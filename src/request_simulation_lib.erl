-module(request_simulation_lib).

-export([
	get_infos_from_state/2, 
	ensure_started/1,
	report_exception/3
]).

get_infos_from_state(State, StateTuple) ->
	{_, Infos} = lists:foldl(
		fun(Info, {Count, Result}) ->
			{Count + 1, [{Info, element(Count + 1, State)} | Result]}
		end,
	{1, []}, tuple_to_list(StateTuple)),
	Infos.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, Reason} ->
          {error, Reason}
    end.

report_exception(Title, Type, What) ->
  Report = [
      Title,
      {type, Type}, {what, What},
      {trace, erlang:get_stacktrace()}
  ],
  error_logger:error_report(Report), 
  ok.	
