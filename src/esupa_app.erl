%%%-------------------------------------------------------------------
%% @doc esupa public API
%% @end
%%%-------------------------------------------------------------------

-module(esupa_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    esupa_sup:start_link().

stop(_State) ->
    ok.
