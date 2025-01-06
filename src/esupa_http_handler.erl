-module(esupa_http_handler).

-behaviour(gen_server).

-define(SCHEME, "https://").

% API
-export([start_link/2]).
-export([
    request/5
]).
-export([
    get/4,
    post/4,
    patch/4,
    delete/4
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start_link(TId, HttpConf) ->
    gen_server:start_link(?MODULE, [TId, HttpConf], []).

%%--------------------------------------------------------------------
%% @doc
%% Request supports standard Supabase API
%%
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec request(
    pid(),
    Method :: get | post | patch | delete,
    string(),
    Headers :: [{string(), string()}],
    Body :: jsx:json_text() | undefined
) ->
    term().
request(Pid, Method, Path, Headers, Body) ->
    gen_server:call(Pid, {Method, Path, Headers, Body}).

init([TId, HttpConf]) ->
    self() ! ready,
    {ok, #{
        hh_tid => TId,
        http_conf => HttpConf
    }}.

handle_call({Method, Path, Headers, Body}, _From, #{hh_tid := TId} = State) ->
    true = ets:delete(TId, self()),
    Response = apply(?MODULE, Method, [Path, Headers, Body, State]),
    self() ! ready,
    {reply, Response, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(ready, #{hh_tid := TId} = State) ->
    common:log(debug, proc, http_handler, ok, self()),
    true = ets:insert(TId, {self()}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get(Path, Headers, Body, #{http_conf := {Url, Key}}) ->
    do_request(get, Path, Headers, Url, Key, Body).

post(Path, Headers, Body, #{http_conf := {Url, Key}}) ->
    do_request(post, Path, Headers, Url, Key, Body).

patch(Path, Headers, Body, #{http_conf := {Url, Key}}) ->
    do_request(patch, Path, Headers, Url, Key, Body).

delete(Path, Headers, Body, #{http_conf := {Url, Key}}) ->
    do_request(delete, Path, Headers, Url, Key, Body).

%% internal functions

do_request(Method, Path, Headers, Url, Key, ReqBody) ->
    case
        httpc:request(
            Method,
            {
                ?SCHEME ++ Url ++ Path,
                [
                    {"Authorization", "Bearer " ++ Key},
                    {"apikey", Key},
                    {"Content-Type", "application/json"},
                    {"Accept", "application/json"}
                ] ++ Headers
            },
            [],
            prepare_body(ReqBody)
        )
    of
        {ok, {{_, 200, _}, _Headers, RespBody}} ->
            {ok, jsx:decode(erlang:list_to_binary(RespBody), [{return_maps, true}])};
        {ok, {{_, 404, _}, _Headers, _}} ->
            {error, "not found"};
        {ok, {{_, 400, _}, _Headers, _}} ->
            {error, "bad request"};
        {error, Reason} ->
            common:log(error, proc, http_handler, ok, Reason),
            {error, "internal error"};
        _ ->
            {error, "internal error"}
    end.

prepare_body(undefined) -> [];
prepare_body(Body) when is_binary(Body) -> Body.
