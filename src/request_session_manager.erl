-module(request_session_manager).
-author("jianchuan.gys@taobao.com").

-behaviour(gen_server).

%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, infos/0]).

-define(state_tuple, {}).


-record(state, ?state_tuple).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

infos() ->
	gen_server:call(?MODULE, infos).

init([]) ->
	{ok, #state{}}.


handle_call(infos, _From, State) ->
	Infos = request_simulation_lib:get_infos_from_state(State, ?state_tuple),
	{reply, Infos, State};
	
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
