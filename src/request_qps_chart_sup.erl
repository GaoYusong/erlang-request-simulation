-module(request_qps_chart_sup).
-author("jianchuan.gys@taobao.com").

-behaviour(supervisor).

-export([init/1]).

-export([start_link/0, start_child/1, delete_child/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Name) ->
	supervisor:start_child(?MODULE, [Name]).

delete_child(Pid) ->
	supervisor:terminate_child(?MODULE, Pid).

init([]) ->
	RestartStategy = {simple_one_for_one, 0, 1},
	{ok, {RestartStategy, [
			{request_qps_chart, {request_qps_chart, start_link, []}, 
			temporary, brutal_kill, worker, [request_qps_chart]}
		]}}.

