-module(request_session).
-author("jianchuan.gys@taobao.com").

-behavior(gen_server).

%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

%% api
-export([start_link/1, start_link/2, infos/1, stop/1,
	set_max_qps/2, start_request/1]).

-define(state_tuple, { 
		tokens_queue_pid,
		pool_id,
		count,
		qps,
		start_time, interval
	}).

-define(default_max_qps, 100).
-define(default_qps_interval, 5000).

-record(state, ?state_tuple).


start_link(Option) ->
	start_link(?default_max_qps, Option).

start_link(MaxQPS, Option) when is_integer(MaxQPS) andalso MaxQPS >= 0 ->
    gen_server:start_link(?MODULE, [MaxQPS, Option], []).

set_max_qps(Pid, MaxQPS) when is_integer(MaxQPS) andalso MaxQPS >= 0 ->
	gen_server:call(Pid, {set_max_qps, MaxQPS}).

start_request(Pid) ->
	gen_server:cast(Pid, wait_for_tokens).

infos(Pid) ->
	gen_server:call(Pid, infos).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init([MaxQPS, Option]) ->
	{ok, Pid} = tokens_queue_manager:new_tokens_queue(MaxQPS),
	Self = self(),
	tokens_queue:set_callback_function(Pid, fun() -> Self ! {tc, wakeup} end),
	PoolId = generate_id:get_id(),

	{ok, {User, Passwd, Host, Port, Database}} = get_option(Option),
	ok = emysql:add_pool(PoolId, 1, User, Passwd, Host, Port, Database, utf8),

	{ok, #state{ 
		tokens_queue_pid 	= Pid,
		pool_id 			= PoolId,
		count 				= 0,
		qps 				= 0,
		start_time 			= get_now_time(),
		interval 			= ?default_qps_interval 
	}, ?default_qps_interval}.

handle_call(infos, _From, State) ->
	Infos = request_simulation_lib:get_infos_from_state(State, ?state_tuple),
	{reply, Infos, State, get_time_left(State)};

handle_call({set_max_qps, MaxQPS}, _From, State = #state{tokens_queue_pid = TokensPid}) ->
	tokens_queue:set_max_cps_cast(TokensPid, MaxQPS),
	{reply, ok, State, get_time_left(State)};

handle_call(_Event, _From, State) ->
	{reply, {error, discard}, State, get_time_left(State)}.

handle_cast(wait_for_tokens, State = #state{tokens_queue_pid = TokensPid}) ->
	tokens_queue:request_tokens_cast(TokensPid),
	{noreply, State, get_time_left(State)};

handle_cast(send_request, State = #state{ pool_id = PoolId, count = Count }) ->
	emysql:execute(PoolId, <<"select 1">>),
	gen_server:cast(self(), wait_for_tokens),
	{noreply, State#state{count = Count + 1}, get_time_left(State)};

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Event, State) ->
	{noreply, State, get_time_left(State)}.

handle_info({tc, wakeup}, State) ->
	gen_server:cast(self(), send_request),
	{noreply, State, get_time_left(State)};

handle_info(timeout, State = #state{count = Count, start_time = StartTime, interval = Interval}) ->
	NowTime = get_now_time(),
	T = NowTime - StartTime,
	Qps = 
		case T > 0 of
			true ->
				Count / T * 1000;
			false ->
				0
		end,
	{noreply, State#state{qps = Qps, count = 0, start_time = NowTime, interval = Interval}, State#state.interval};

handle_info(_Info, State) ->
	{noreply, State, get_time_left(State)}.

terminate(Reason, State = #state{tokens_queue_pid = TokensPid, pool_id = PoolId}) ->
	io:format("Exit ~p Reason ~p State ~p~n", [self(), Reason, State]),

	tokens_queue_manager:delete_tokens_queue(TokensPid),
	emysql:remove_pool(PoolId),

	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

get_option(Option) ->
	Keys = [user, passwd, host, port],
	Values = [X || Y <- Keys, (X = pget(Y, Option)) =/= undefined],
	case length(Values) =:= 4 of
		true ->
			{ok, list_to_tuple(Values ++ [pget(database, Option, hd(Values))])};
		false ->
			{error, required_option_missed}
	end.

pget(Key, List) ->
	proplists:get_value(Key, List).
pget(Key, List, Default) ->
	proplists:get_value(Key, List, Default).

get_time_left(State) ->
	time_left(State#state.start_time, State#state.interval).

%% get now time in ms
get_now_time() ->
	{A, B, C} = now(),
	((A * 1000000 + B) * 1000000 + C) div 1000.

time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, Interval) ->
    case Interval - (get_now_time() - StartTime) of
        Time when Time =< 0 -> 0;
        Time -> Time
    end.
