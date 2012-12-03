-module(request_qps_chart).
-author("jianchuan.gys@taobao.com").

-behavior(gen_server).

-include("request_simulation.hrl").

%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

%% api
-export([start_link/1, infos/1, get_qps_history/1, get_qps_history/2]).

-define(state_tuple, { 
		name,
		qps_chart
	}).


-record(state, ?state_tuple).

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

infos(Pid) ->
	gen_server:call(Pid, infos).

get_qps_history(Pid) ->
	get_qps_history(Pid, ?max_queue_length).

get_qps_history(Pid, HeadCount) ->
	gen_server:call(Pid, {get_qps_history, HeadCount}).

init([Name]) ->
	{ok, emit_update(#state{ name = Name, qps_chart = ets:new(qps_chart, [named_table]) })}.

handle_call(infos, _From, State) ->
	Infos = request_simulation_lib:get_infos_from_state(State, ?state_tuple),
	{reply, Infos, State};

handle_call({get_qps_history, HeadCount}, _From, State = #state{qps_chart = QpsChart}) ->
	QpsHistory = ets:tab2list(QpsChart),
	Result = 
		lists:map(
			fun({Pid, QpsQueue}) ->
				[{pid, Pid}, {qps_history, lists:sublist(lists:reverse(queue:to_list(QpsQueue)), HeadCount)}]
			end
		, QpsHistory),
	{reply, Result, State};

handle_call(_Event, _From, State) ->
	{reply, {error, discard}, State}.

handle_cast(_Event, State) ->
	{noreply, State}.

handle_info(update_qps_chart, State = #state{name = Name, qps_chart = QpsChart}) ->
	case request_session_manager:get_qps(Name) of
		{ok, QPSList} ->
			lists:map(
				fun(QPSInfo) ->
					Pid = pget(pid, QPSInfo),
					QPS = pget(qps, QPSInfo),
					RawQueue =
						case ets:lookup(QpsChart, Pid) of
							[] ->
								queue:new();
							[{_Key, Val}] ->
								Val
						end,
					CurrentQueue = 
						case queue:len(RawQueue) >= ?max_queue_length of
							true ->
								{_, RawQueue0} = queue:out(RawQueue),
								RawQueue0;
							false ->
								RawQueue
						end,
					NextQueue = queue:in(QPS, CurrentQueue),
					ets:insert(QpsChart, {Pid, NextQueue})
				end
			, QPSList);
		{error, _} ->
			ok
	end,
	{noreply, emit_update(State)};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

emit_update(State) ->
    erlang:send_after(?default_qps_interval, self(), update_qps_chart),
    State.

pget(Key, List) ->
	proplists:get_value(Key, List).
