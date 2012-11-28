-module(request_session).
-author("jianchuan.gys@taobao.com").

-behavior(gen_server).

%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

%% api
-export([start_link/0, start_link/1, infos/1, stop/1,
	set_max_qps/2, start_request/1]).

-define(state_tuple, { 
		tokens_queue_pid
	}).
-define(default_max_qps, 100).

-record(state, ?state_tuple).


start_link() ->
	start_link(?default_max_qps).

start_link(MaxQPS) when is_integer(MaxQPS) andalso MaxQPS >= 0 ->
    gen_server:start_link(?MODULE, [MaxQPS], []).

set_max_qps(Pid, MaxQPS) when is_integer(MaxQPS) andalso MaxQPS >= 0 ->
	gen_server:call(Pid, {set_max_qps, MaxQPS}).

start_request(Pid) ->
	gen_server:cast(Pid, wait_for_tokens).

infos(Pid) ->
	gen_server:call(Pid, infos).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init([MaxQPS]) ->
	{ok, Pid} = tokens_queue_manager:new_tokens_queue(MaxQPS),
	Self = self(),
	tokens_queue:set_callback_function(Pid, fun() -> Self ! {tc, wakeup} end),
	{ok, #state{ 
		tokens_queue_pid = Pid
	}}.

handle_call(infos, _From, State) ->
	Infos = request_simulation_lib:get_infos_from_state(State, ?state_tuple),
	{reply, Infos, State};

handle_call({set_max_qps, MaxQPS}, _From, State = #state{tokens_queue_pid = TokensPid}) ->
	tokens_queue:set_max_cps_cast(TokensPid, MaxQPS),
	{reply, ok, State};

handle_call(_Event, _From, State) ->
	{reply, {error, discard}, State}.

handle_cast(wait_for_tokens, State = #state{tokens_queue_pid = TokensPid}) ->
	tokens_queue:request_tokens_cast(TokensPid),
	{noreply, State};

handle_cast(send_request, State) ->
	gen_server:cast(self(), wait_for_tokens),
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Event, State) ->
	{noreply, State}.

handle_info({tc, wakeup}, State) ->
	gen_server:cast(self(), send_request),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, State = #state{tokens_queue_pid = TokensPid}) ->
	io:format("Exit ~p Reason ~p State ~p~n", [self(), Reason, State]),

	tokens_queue_manager:delete_tokens_queue(TokensPid),

	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
