-module(esupa_http_handler).

-behaviour(gen_server).

-define(SCHEME, "https://").

% API
-export([start_link/2]).
-export([
    request/5,
    get/5,
    post/5,
    patch/5,
    delete/5
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
%% Default gen_server:call timeout (5000ms) is too short: concurrent
%% same-host requests can queue behind each other (httpc appears to
%% serialize them rather than truly parallelizing), so a request that's
%% merely slow — not stuck — was crashing the caller with a timeout exit
%% instead of eventually returning a real response.
-define(REQUEST_TIMEOUT, 30_000).

request(Pid, Method, Path, Headers, Body) ->
    gen_server:call(Pid, {Method, Path, Headers, Body}, ?REQUEST_TIMEOUT).

get(Path, Headers, Url, Key, _ReqBody) ->
    do_read(Path, Headers, Url, Key).

post(Path, Headers, Url, Key, ReqBody) ->
    do_write(post, Path, Headers, Url, Key, ReqBody).

patch(Path, Headers, Url, Key, ReqBody) ->
    do_write(patch, Path, Headers, Url, Key, ReqBody).

delete(Path, Headers, Url, Key, _ReqBody) ->
    do_delete(Path, Headers, Url, Key).

init([TId, HttpConf]) ->
    self() ! ready,
    {ok, #{
        hh_tid => TId,
        http_conf => HttpConf
    }}.

handle_call(
    {Method, Path, Headers, Body}, _From, #{hh_tid := TId, http_conf := {Url, Key}} = State
) ->
    true = ets:delete(TId, self()),
    Response = do_request(Method, Path, Headers, Url, Key, Body),
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

%% internal functions

do_request(Method, Path, Headers, Url, Key, ReqBody) ->
    case erlang:apply(?MODULE, Method, [Path, Headers, Url, Key, ReqBody]) of
        {ok, {{_, 200, _}, _Headers, RespBody}} ->
            {ok, jsx:decode(erlang:list_to_binary(RespBody), [{return_maps, true}])};
        {ok, {{_, 404, _}, _Headers, _}} ->
            {error, "not found"};
        {ok, {{_, 400, _}, _Headers, _}} ->
            {error, "bad request"};
        {ok, {{_, 201, Res}, _Headers, _}} ->
            {ok, Res};
        {ok, {{_, 204, Res}, _Headers, _}} ->
            {ok, Res};
        {error, Reason} ->
            common:log(error, proc, http_handler, ok, Reason),
            {error, "internal error"};
        Any ->
            logger:warning("--------------------Any ~p", [Any]),
            {error, "internal error"}
    end.

prepare_body(undefined) -> "";
prepare_body(Body) when is_binary(Body) -> Body.

%% httpc:request/4 is called as (Method, Request, define_options(),
%% define_http_options()) below — i.e. define_options/0 is the 3rd
%% positional arg (HTTPOptions, where timeout/connect_timeout actually
%% belong) and define_http_options/0 is the 4th (Options, a different set
%% that doesn't accept those keys — hence "Invalid option ignored" when
%% they were in the wrong one). Confusing given the names, but that's the
%% existing call shape.
%%
%% HTTPOptions had no timeout at all (infinity), so a hung request
%% (network issue, stuck connection, or just a slow query) would block the
%% calling gen_server worker forever — past even the caller's own
%% ?REQUEST_TIMEOUT gen_server:call bound, since that only times out the
%% caller, not this process. The worker never reaches `self() ! ready`, so
%% it never rejoins the pool: every such hang permanently loses one of the
%% (fixed-size) pool workers. Bounding both here means a hung request now
%% fails and lets the worker recover.
-define(HTTP_TIMEOUT, 25_000).
-define(HTTP_CONNECT_TIMEOUT, 10_000).

define_options() ->
    [{timeout, ?HTTP_TIMEOUT}, {connect_timeout, ?HTTP_CONNECT_TIMEOUT}].

define_http_options() ->
    [].

do_read(Path, Headers, Url, Key) ->
    httpc:request(
        get,
        {
            ?SCHEME ++ Url ++ common:to_list(Path),
            [
                {"Authorization", "Bearer " ++ Key},
                {"apikey", Key},
                %% makes sense only for requests with body
                {"Content-Type", "application/json"},
                %% makes sense when reponse is expected
                {"Accept", "application/json"}
            ] ++ Headers
        },
        %% TODO: check certificates
        define_options(),
        define_http_options()
    ).

do_write(Method, Path, Headers, Url, Key, ReqBody) ->
    httpc:request(
        Method,
        {
            ?SCHEME ++ Url ++ common:to_list(Path),
            [
                {"Authorization", "Bearer " ++ Key},
                {"apikey", Key},
                %% makes sense only for requests with body
                {"Content-Type", "application/json"},
                %% makes sense when reponse is expected
                {"Accept", "application/json"}
            ] ++ Headers,
            % %% makes sense only for POST with JSON body
            "application/json",
            prepare_body(ReqBody)
        },
        %% TODO: check certificates
        define_options(),
        define_http_options()
    ).

do_delete(Path, Headers, Url, Key) ->
    httpc:request(
        delete,
        {
            ?SCHEME ++ Url ++ common:to_list(Path),
            [
                {"Authorization", "Bearer " ++ Key},
                {"apikey", Key},
                %% makes sense only for requests with body
                {"Content-Type", "application/json"},
                %% makes sense when reponse is expected
                {"Accept", "application/json"}
            ] ++ Headers
        },
        %% TODO: check certificates
        define_options(),
        define_http_options()
    ).
