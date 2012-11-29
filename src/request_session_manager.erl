-module(request_session_manager).
-author("jianchuan.gys@taobao.com").

-behaviour(gen_server).

%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-export([
	start_link/0, 
	infos/0,
	add_work/3,
	del_work/1,
	get_work/1,
	get_work_qps/1
]).

-define(state_tuple, {
		stress_works
	}).


-record(state, ?state_tuple).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

infos() ->
	gen_server:call(?MODULE, infos).

add_work(Name, Option, Links) ->
	gen_server:call(?MODULE, {add_work, Name, Option, Links}).

del_work(Name) ->
	gen_server:call(?MODULE, {del_work, Name}).

get_work(Name) ->
	gen_server:call(?MODULE, {get_work, Name}).

get_work_qps(Name) ->
	gen_server:call(?MODULE, {get_work_qps, Name}).

init([]) ->
	{ok, #state{
		stress_works = ets:new(stress_works, [named_table])
	}}.


handle_call(infos, _From, State) ->
	Infos = request_simulation_lib:get_infos_from_state(State, ?state_tuple),
	{reply, Infos, State};

handle_call({add_work, Name, Option, Links}, _From, State = #state{stress_works = StressWorks}) ->
	Result = 
		case ets:member(StressWorks, Name) of
			true ->
				{error, name_already_used};
			false ->
				Lists = 
					lists:append(
						lists:map(
							fun(Link) ->
								case Link of
									{Count, MaxQPS} when is_integer(Count) andalso is_integer(MaxQPS)
													andalso Count > 0 andalso MaxQPS > 0 ->
										start_connect(Count, MaxQPS, Option);
									_ ->
										[]
								end
							end
						, Links)),
				ets:insert(StressWorks, {Name, Lists}),
				{ok, Lists}
		end,
	{reply, Result, State};

handle_call({del_work, Name}, _From, State = #state{stress_works = StressWorks}) ->
	Result = 
		case ets:lookup(StressWorks, Name) of
			[] ->
				{error, name_not_existed};
			[{Name, Lists}] ->
				remove_connect(Lists),
				ets:delete(StressWorks, Name),
				ok
		end,
	{reply, Result, State};

handle_call({get_work, Name}, _From, State = #state{stress_works = StressWorks}) ->
	Result = 
		case ets:lookup(StressWorks, Name) of
			[] ->
				{error, name_not_existed};
			[{Name, Lists}] ->
				{ok, Lists}
		end,
	{reply, Result, State};

handle_call({get_work_qps, Name}, _From, State = #state{stress_works = StressWorks}) ->
	Result = 
		case ets:lookup(StressWorks, Name) of
			[] ->
				{error, name_not_existed};
			[{Name, Lists}] ->
				{ok,
					lists:map(
						fun(Pid) ->
							Qps = case is_process_alive(Pid) of
								true ->
									proplists:get_value(qps, request_session:infos(Pid));
								false ->
									undefined
							end,
							[{pid, Pid}, {qps, Qps}]
						end
					, Lists)}
		end,
	{reply, Result, State};

handle_call(_Event, _From, State) ->
	{reply, {error, discard}, State}.

handle_cast(_Event, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

start_connect(Count, MaxQPS, Option) ->
	do_start_connect(Count, MaxQPS, Option, []).

do_start_connect(0, _MaxQPS, _Option, Result) ->
	Result;
do_start_connect(Count, MaxQPS, Option, Result) ->
	case request_session_sup:start_child(MaxQPS, Option) of
		{ok, Pid} ->
			request_session:start_request(Pid),
			do_start_connect(Count - 1, MaxQPS, Option, [Pid | Result]);
		_ ->
			do_start_connect(Count - 1, MaxQPS, Option, Result)
	end.

remove_connect(Lists) ->
	lists:foreach(
		fun(Pid) ->
			request_session_sup:delete_child(Pid)
		end
	, Lists).