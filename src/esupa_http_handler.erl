-module(esupa_http_handler).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-define(SCHEME, "https://").

% API
-export([start_link/2]).
-export([
    request/4
]).
-export([
    get/3
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link(TId, HttpConf) ->
    gen_server:start_link(?MODULE, [TId, HttpConf], []).

request(Pid, Method, Path, Params) ->
    gen_server:call(Pid, {Method, Path, Params}).

init([TId, HttpConf]) ->
    self() ! ready,
    {ok, #{
        hh_tid => TId,
        http_conf => HttpConf
    }}.

handle_call({Method, Path, Params}, _From, #{hh_tid := TId} = State) ->
    true = ets:delete(TId, self()),
    Response = apply(?MODULE, Method, [Path, Params, State]),
    self() ! ready,
    {reply, {ok, Response}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(ready, #{hh_tid := TId} = State) ->

    ?LOG_DEBUG("self ~p", [self()]),

    true = ets:insert(TId, {self()}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% P = erlang:list_to_pid("<0.617.0>").
% esupa_http_handler:request(P, get, "teams?select=*", []).
get(Path, Params, #{http_conf := {Url, Key}}) ->
    {ok, {{_, 200, _}, _Headers, Body}} = httpc:request(
        get,
        {?SCHEME ++ Url ++ Path, [{"Authorization", "Bearer " ++ Key}, {"apikey", Key}]},
        [],
        Params
    ),
    ?LOG_DEBUG("~s", [Body]),
    Body.

%% internal functions