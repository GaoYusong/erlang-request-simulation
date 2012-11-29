-module(request_session_sup).
-author("jianchuan.gys@taobao.com").

-behaviour(supervisor).

-export([init/1]).

-export([start_link/0, start_child/1, start_child/2, delete_child/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Option) ->
	do_start_child([Option]).

start_child(MaxCps, Option) ->
	do_start_child([MaxCps, Option]).

delete_child(Pid) ->
	request_session:stop(Pid).

do_start_child(Args) ->
	supervisor:start_child(?MODULE, Args).

init([]) ->
	RestartStategy = {simple_one_for_one, 0, 1},
	{ok, {RestartStategy, [
			{request_session, {request_session, start_link, []}, 
			temporary, brutal_kill, worker, [request_session]}
		]}}.

