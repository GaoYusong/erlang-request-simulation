-module(request_session_manager).
-author("jianchuan.gys@taobao.com").

-behaviour(gen_server).

-include("request_simulation.hrl").

%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-export([
	start_link/0, 
	infos/0,
	add_work/2,
	add_work/3,
	del_work/1,
	get_works/0,
	set_option/2,
	set_option/3,
	get_option/1,
	get_qps/1,
	get_links/1,
	add_links/2,
	get_qps_history/1,
	get_qps_history/2,
	format_qps_history/1,
	format_qps_history/2
]).

-define(state_tuple, {
		stress_works,
		works_qps_chart
	}).


-record(state, ?state_tuple).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

infos() ->
	gen_server:call(?MODULE, infos).

add_work(Name, Option) ->
	add_work(Name, Option, []).

add_work(Name, Option, Links) when is_list(Links) ->
	gen_server:call(?MODULE, {add_work, Name, Option, Links}, infinity).

del_work(Name) ->
	gen_server:call(?MODULE, {del_work, Name}, infinity).

get_works() ->
	gen_server:call(?MODULE, get_works).

set_option(Name, Option) ->
	set_option(Name, Option, []).

set_option(Name, Option, Links) when is_list(Links) ->
	gen_server:call(?MODULE, {set_option, Name, Option, Links}).

get_option(Name) ->
	gen_server:call(?MODULE, {get_option, Name}).

get_qps(Name) ->
	gen_server:call(?MODULE, {get_qps, Name}, infinity).

get_links(Name) ->
	gen_server:call(?MODULE, {get_links, Name}).

add_links(Name, Links) ->
	gen_server:call(?MODULE, {add_links, Name, Links}, infinity).

get_qps_history(Name) ->
	get_qps_history(Name, ?max_queue_length).

get_qps_history(Name, HeadCount) ->
	gen_server:call(?MODULE, {get_qps_history, Name, HeadCount}, infinity).

format_qps_history(Name) ->
	format_qps_history(Name, ?max_queue_length).
format_qps_history(Name, HeadCount) when is_integer(HeadCount) andalso HeadCount >= 1 ->
	case get_qps_history(Name, HeadCount) of
		{ok, History} ->
			lists:foreach(
				fun(Elem) ->
					io:format("~20s", [pid_to_list(pget(pid, Elem))])
				end, History),
			io:format("~n"),
			%% this is very slow, but it is easy to write
			lists:foreach(
				fun(Elem) ->
					lists:foreach(
						fun(Info) ->
							QpsHistory = pget(qps_history, Info),
							Qps = case length(QpsHistory) < Elem of
								true -> 0.0;
								false -> 
									Qps0 = lists:nth(Elem, QpsHistory),
									case is_number(Qps0) of
										true ->
											float(Qps0);
										false ->
											0.0
									end
							end,
							io:format("~20.2f", [Qps])
						end
					, History),
					io:format("~n")
				end
			, lists:reverse(lists:seq(1, HeadCount))),
			ok;
		Error ->
			Error
	end.

init([]) ->
	{ok, #state{
		stress_works 	= ets:new(stress_works, [named_table]),
		works_qps_chart = ets:new(works_qps_chart, [named_table])
	}}.


handle_call(infos, _From, State) ->
	Infos = request_simulation_lib:get_infos_from_state(State, ?state_tuple),
	{reply, Infos, State};

handle_call({add_work, Name, Option, Links}, _From, State = #state{stress_works = StressWorks, works_qps_chart = WorksQpsChart}) ->
	Result = 
		case ets:member(StressWorks, Name) of
			true ->
				{error, name_already_used};
			false ->
				Lists = do_add_links(Option, Links, []),
				ets:insert(StressWorks, {Name, { Option, Lists } }),

				%% start qps chart process
				ChartPid = case request_qps_chart_sup:start_child(Name) of
					{ok, Pid} -> Pid;
					_ -> undefined
				end,
				ets:insert(WorksQpsChart, {Name, ChartPid}),
				%% 

				{ok, Lists}
		end,
	{reply, Result, State};

handle_call({del_work, Name}, _From, State = #state{stress_works = StressWorks, works_qps_chart = WorksQpsChart}) ->
	Result = check_do(StressWorks, Name,
			fun({TheName, {_Option, Lists}}) ->
				remove_connect(Lists),
				ets:delete(StressWorks, TheName),

				%% stop qps chart process
				case ets:lookup(WorksQpsChart, TheName) of
					[] ->
						ok;
					[{_, Pid}] ->
						request_qps_chart_sup:delete_child(Pid)
				end,
				ets:delete(WorksQpsChart, TheName),
				%%

				ok
			end
		),
	{reply, Result, State};

handle_call(get_works, _From, State = #state{ stress_works = StressWorks }) ->
	{reply, ets:tab2list(StressWorks), State};

handle_call({get_links, Name}, _From, State = #state{stress_works = StressWorks}) ->
	Result = check_do(StressWorks, Name,
			fun({_Name, {_Option, Lists}}) ->
				{ok, Lists}
			end
		),
	{reply, Result, State};

handle_call({add_links, Name, Links}, _From, State = #state{ stress_works = StressWorks }) ->
	Result = check_do(StressWorks, Name,
			fun({ TheName, { Option, Lists } }) ->
				NewLists = do_add_links(Option, Links, Lists),
				ets:insert( StressWorks, { TheName, { Option, NewLists } }),
				{ok, NewLists}
			end
		),
	{reply, Result, State};

handle_call({get_option, Name}, _From, State = #state{ stress_works = StressWorks }) ->
	Result = check_do(StressWorks, Name,
			fun({ _Name, { Option, _Lists }}) ->
				{ok, Option}
			end
		),
	{reply, Result, State};


handle_call({set_option, Name, Option, Links}, _From, State = #state{ stress_works = StressWorks }) ->
	Result = check_do(StressWorks, Name,
			fun ({ TheName, { _LastOption, LastLists}}) ->  
				remove_connect(LastLists),
				Lists = do_add_links(Option, Links, []),
				ets:insert( StressWorks, { TheName, { Option, Lists } } ),
				{ok, Lists}
			end
		),
	{reply, Result, State};


handle_call({get_qps, Name}, _From, State = #state{stress_works = StressWorks}) ->
	Result = check_do(StressWorks, Name,
			fun({_Name, {_Option, Lists}}) ->
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
			end
		),
	{reply, Result, State};

handle_call({get_qps_history, Name, HeadCount}, _From, State = #state{works_qps_chart = WorksQpsChart}) ->
	Result = 
		case ets:lookup(WorksQpsChart, Name) of
			[] ->
				{error, name_not_existed};
			[{_, undefined}] ->
				{error, qps_chart_process_not_stated};
			[{_Key, Pid}] ->
				{ok, request_qps_chart:get_qps_history(Pid, HeadCount)}
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


do_add_links(Option, Links, Lists) ->
	Lists ++ 
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
			, Links)).

check_do(StressWorks, Name, Func) ->
	check_and_do(StressWorks, Name, fun() -> {error, name_not_existed} end, Func).

check_and_do(StressWorks, Name, TrueFunc,  FalseFunc) ->
	case ets:lookup(StressWorks, Name) of
		[] ->
			TrueFunc();
		[Val] ->
			FalseFunc(Val)
	end.

pget(Key, List) ->
	proplists:get_value(Key, List).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

check_work(Name, Option, Lists, Tag)  ->
	io:format("Tag ~p~n", [Tag]),
	?assertEqual(length(Lists), length(supervisor:which_children(request_session_sup))),
	?assertEqual(Lists, element(2, get_links(Name))),
	?assertEqual(Option, element(2, get_option(Name))).

request_session_manager_test() ->
	request_simulation:start(),
	Option = request_simulation_lib:get_env(request_simulation, option, 
		[ {user, "sample"}, {passwd, "test"}, {host, "10.232.64.72"}, {port, 7003}]),

	%% add a work, check metadata, process is created
	%% check add_work, get_links, get_option
	AddWorkRet = add_work("sample", Option, [{5, 100}, {5, 200}]),
	?assertMatch({ok, _}, AddWorkRet),
	{ok, Lists} = AddWorkRet,
	?assertEqual(10, length(Lists)),
	check_work("sample", Option, Lists, "check add_work"),
	

	%% check add_work to existed work name
	?assertEqual({error, name_already_used}, add_work("sample", Option, [])),

	%% test del_work, check metadata, process is deleted
	%% check del_work error, get_links error, get_option error
	?assertEqual(ok, del_work("sample")),
	%% wait request_session_sup remove children list
	timer:sleep(100),
	?assertEqual(0, length(supervisor:which_children(request_session_sup))),
	?assertEqual({error, name_not_existed}, get_links("sample")),
	?assertEqual({error, name_not_existed}, get_option("sample")),

	%% del_work error
	?assertEqual({error, name_not_existed}, del_work("sample")),

	%% check add_work with option error
	?assertEqual({ok, []}, add_work("sample", error, [{5, 100}, {5, 200}])),
	ok = del_work("sample"),

	%% check set_option
	{ok, _} = add_work("sample", Option, [{5, 100}]),
	SetOptionRet = set_option("sample", Option, [{5, 100}, {5, 200}]),
	?assertMatch({ok, _}, SetOptionRet),
	{ok, SetOptionLists} = SetOptionRet,
	?assertEqual(10, length(SetOptionLists)),
	check_work("sample", Option, SetOptionLists, "check set_option"),


	%% check add_links,
	AddLinksRet = add_links("sample", [{5, 100}]),
	?assertMatch({ok, _}, AddLinksRet),
	{ok, AddLinksLists} = AddLinksRet,
	?assertEqual(15, length(AddLinksLists)),
	check_work("sample", Option, AddLinksLists, "check add_links"),

	{ok, QPS} = get_qps("sample"),
	RealQPS = 
		lists:filter(
			fun(Elem) ->
				proplists:get_value(qps, Elem) =/= undefined 
				andalso is_process_alive(proplists:get_value(pid, Elem)) =:= true
			end
		, QPS),
	?assertEqual(QPS, RealQPS),

	?assertEqual({ok, []}, add_work("tarjan", Option, [error])),

	{ok, _} = add_work("sap", Option, [{5, 50}]),
	?assertEqual(3, length(get_works())).



-endif.