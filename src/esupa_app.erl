%%%-------------------------------------------------------------------
%% @doc esupa public API
%% @end
%%%-------------------------------------------------------------------

-module(esupa_app).

-behaviour(application).

-include("common.hrl").
-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1]).
-export([
    get_client/0,
    get_client/1
]).

start(_StartType, _StartArgs) ->
    esupa_sup:start_link().

stop(_State) ->
    ok.

get_client() ->
    get_client(http).

-spec get_client(Type :: http) -> pid().
get_client(Type) ->
    Tab = type_to_tab(Type),
    case ets:first(Tab) of
        '$end_of_table' ->
            ?LOG_ERROR("END OF TABLE ~p", [Tab]),
            {error, "no_free_" ++ atom_to_list(Type) ++ "_clients"};
        Pid ->
            {ok, Pid}
    end.

%% internal functions
type_to_tab(http) -> ?HH_TAB.
