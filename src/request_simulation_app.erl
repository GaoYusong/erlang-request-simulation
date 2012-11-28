-module(request_simulation_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	ok = request_simulation_lib:ensure_started(traffic_control),
	ok = request_simulation_lib:ensure_started(crypto),
	ok = request_simulation_lib:ensure_started(emysql),
    request_simulation_sup:start_link().

stop(_State) ->
    ok.
