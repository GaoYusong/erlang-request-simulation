-module(request_simulation_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    request_simulation_sup:start_link().

stop(_State) ->
    ok.
