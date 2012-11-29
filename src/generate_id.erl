-module(generate_id).

-behaviour(gen_server).


%% callback function
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

%% api
-export([start_link/0, infos/0, get_id/0]).

-define(state_tuple, { 
		id_count
	}).

-record(state, ?state_tuple).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

infos() ->
	gen_server:call(?MODULE, infos).

get_id() ->
	gen_server:call(?MODULE, get_id).

init([]) ->
	{ok, #state{ id_count = 0 }}.

handle_call(infos, _From, State) ->
	Infos = request_simulation_lib:get_infos_from_state(State, ?state_tuple),
	{reply, Infos, State};

handle_call(get_id, _From, State = #state{ id_count = Id }) ->
	{reply, Id, State#state{ id_count = Id + 1}};

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



