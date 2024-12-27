-module(esupa_http_handler).

-behaviour(gen_server).

-define(SCHEME, "https://").
-define(SELECT, "select=").
-define(DEFAULT_OP, "eq").
-define(DEFAULT_HTTP_OPTIONS, []).

-type supabase_select_api_params() :: #{
    type => Type :: all | all_range | ref_table,
    columns => Columns :: [atom()],
    conditions => Conditions :: [{atom(), term()} | {Key :: atom(), Op :: atom(), Val :: term()}]
}.

-type supabase_insert_update_api_params() :: #{
    % TODO
}.

% API
-export([start_link/2]).
-export([
    request/4
]).
-export([
    get/3
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
%% Currently, only 'eq' is supported
%%
%% @end
%%--------------------------------------------------------------------
-spec request(
    pid(),
    Method :: get | post | update | delete,
    string(),
    Params :: supabase_select_api_params() | supabase_insert_update_api_params()
) ->
    term().
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

% P = erlang:list_to_pid("<0.617.0>").
% esupa_http_handler:request(P, get, "matches?select=*", []).
% esupa_http_handler:request(P, get, "matches?select=*&id=in.(75)", []).
-spec get(string(), supabase_select_api_params(), map()) ->
    {ok, binary() | string()} | {error, string()}.
get(Path0, Params, #{http_conf := {Url, Key}}) ->
    Path = build_path(Path0, Params),
    case
        httpc:request(
            get,
            {?SCHEME ++ Url ++ Path, [
                {"Authorization", "Bearer " ++ Key},
                {"apikey", Key},
                {"Content-Type", "application/json"},
                {"Accept", "application/json"}
            ]},
            [],
            []
        )
    of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, jsx:decode(erlang:list_to_binary(Body), [{return_maps, true}])};
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

%% internal functions

build_path(Path, #{type := all, conditions := Conditions, columns := Columns0} = _Params) ->
    Filters = prepare_filters(Conditions),
    common:log(debug, proc, http_handler, ok, Filters),
    Columns = prepare_columns(Columns0),
    common:log(debug, proc, http_handler, ok, Columns),
    Path ++ Filters ++ ?SELECT ++ Columns.

prepare_filters(Conditions) ->
    lists:foldl(
        fun
            ({Key, Value}, Acc) ->
                Acc ++ common:to_list(Key) ++ "=" ++ ?DEFAULT_OP ++ "." ++ common:to_list(Value) ++
                    "&";
            %% TODO: check all Operators urls
            ({Key, Operator, Value}, Acc) ->
                Acc ++ common:to_list(Key) ++ "=" ++ common:to_list(Operator) ++ "." ++
                    common:to_list(Value) ++ "&"
        end,
        "?",
        Conditions
    ).

prepare_columns(Columns0) ->
    {Columns, _} = lists:foldl(
        fun
            (Column, {Acc, N}) when N =:= 0 -> {Acc ++ common:to_list(Column), N + 1};
            (Column, {Acc, N}) -> {Acc ++ "," ++ common:to_list(Column), N + 1}
        end,
        {"", 0},
        Columns0
    ),
    Columns.
