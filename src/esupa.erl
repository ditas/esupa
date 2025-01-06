-module(esupa).

-include("include/common.hrl").

-export([
    get_client/0
]).

-export([
    supa_from/2,
    supa_select/2,
    supa_insert/2,
    supa_update/2,
    supa_delete/1,

    %% filter methods
    supa_eq/3,
    % supa_neq/3,
    supa_gt/3,
    supa_gte/3,
    supa_lt/3,
    supa_lte/3,
    supa_in/3,
    supa_range/3,
    supa_order/3,
    supa_or/2,

    % supa_is,

    request/2,
    execute/1
]).

-type request_tuple() :: {
    Client :: pid(),
    Method :: get | post | update | delete,
    Req :: string(),
    Headers :: [{string(), term()}]
}.

-type request_tuple_with_body() :: {
    Client :: pid(),
    Method :: get | post | update | delete,
    Req :: string(),
    Headers :: [{string(), term()}],
    Body :: jsx:json_term()
}.

-spec get_client() -> {ok, pid()} | {error, nonempty_string()}.
get_client() ->
    get_client(http).

-spec get_client(Type :: http) -> {ok, pid()} | {error, nonempty_string()}.
get_client(Type) ->
    Tab = type_to_tab(Type),
    case ets:first(Tab) of
        '$end_of_table' ->
            common:log(error, app, ?MODULE, error, '$end_of_table'),
            {error, "no_free_" ++ atom_to_list(Type) ++ "_clients"};
        Pid ->
            {ok, Pid}
    end.

-spec request(Client :: pid(), Method :: get | post | update | delete) -> request_tuple().
request(Client, Method) ->
    {Client, Method, "", []}.

-spec execute(request_tuple() | request_tuple_with_body()) -> term().
execute({Client, Method, Req, Headers}) ->
    esupa_http_handler:request(Client, Method, Req, Headers, undefined);
execute({Client, Method, Req, Headers, Body}) ->
    esupa_http_handler:request(Client, Method, Req, Headers, Body).

-spec supa_from(request_tuple(), string()) -> request_tuple().
supa_from({Client, Method, Req0, Headers}, Table) ->
    {Client, Method, Req0 ++ common:to_list(Table), Headers}.

-spec supa_select(request_tuple(), [string()]) -> request_tuple().
supa_select({Client, Method, Req0, Headers}, []) ->
    {Client, Method, Req0 ++ "?select=" ++ "*", Headers};
supa_select({Client, Method, Req0, Headers}, Columns0) ->
    {Req, _} = lists:foldl(
        fun
            (Column, {Acc, N}) when N =:= 0 -> {Acc ++ common:to_list(Column), N + 1};
            (Column, {Acc, N}) -> {Acc ++ "," ++ common:to_list(Column), N + 1}
        end,
        {Req0 ++ "?select=", 0},
        Columns0
    ),
    {Client, Method, Req, Headers}.

supa_insert({Client, Method, Req, Headers}, Body) ->
    {Client, Method, Req, Headers, jsx:encode(Body)}.

% supa_upsert({Client, Method, Req, Headers}, Body) ->
%     {Client, Method, Req,
%         Headers ++ [{"Prefer", "resolution=merge-duplicates"}],
%         jsx:encode(Body)}.

supa_update({Client, Method, Req0, Headers}, Body) ->
    {Client, Method, Req0 ++ "?", Headers ++ [{"Prefer", "return=minimal"}], jsx:encode(Body)}.

supa_delete({Client, Method, Req0, Headers}) ->
    {Client, Method, Req0 ++ "?", Headers}.

-spec supa_range(request_tuple(), non_neg_integer(), non_neg_integer()) -> request_tuple().
supa_range({Client, Method, Req0, Headers}, Min, Max) ->
    {Client, Method, Req0,
        Headers ++ [{"Range", common:to_list(Min) ++ "-" ++ common:to_list(Max)}]}.

-spec supa_order(request_tuple(), string(), asc | desc) -> request_tuple().
supa_order({Client, Method, Req0, Headers}, Column, Order) ->
    {Client, Method,
        Req0 ++ "&" ++ "order" ++ "=" ++ common:to_list(Column) ++ "." ++ common:to_list(Order),
        Headers}.

-spec supa_eq(request_tuple(), string(), term()) -> request_tuple().
supa_eq({Client, Method, Req0, Headers}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "eq." ++
            common:to_list(Value),
        Headers}.

% -spec supa_neq(request_tuple(), string(), term()) -> request_tuple().
% supa_neq({Client, Method, Req0, Headers}, Column, Value) ->
%     {Client, Method,
%         Req0 ++ prepare_divider(Req0) ++
%         common:to_list(Column) ++ "=" ++ "not.is." ++ common:to_list(Value),
%         Headers}.

-spec supa_gt(request_tuple(), string(), term()) -> request_tuple().
supa_gt({Client, Method, Req0, Headers}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "gt." ++
            common:to_list(Value),
        Headers}.

-spec supa_gte(request_tuple(), string(), term()) -> request_tuple().
supa_gte({Client, Method, Req0, Headers}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "gte." ++
            common:to_list(Value),
        Headers}.

-spec supa_lt(request_tuple(), string(), term()) -> request_tuple().
supa_lt({Client, Method, Req0, Headers}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "lt." ++
            common:to_list(Value),
        Headers}.

-spec supa_lte(request_tuple(), string(), term()) -> request_tuple().
supa_lte({Client, Method, Req0, Headers}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "lte." ++
            common:to_list(Value),
        Headers}.

-spec supa_in(request_tuple(), string(), [term()]) -> request_tuple().
supa_in({Client, Method, Req0, Headers}, Column, Values) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "in.(" ++
            prepare_comma_separated_list(Values) ++ ")",
        Headers}.

-spec supa_or(request_tuple(), [{string(), string(), term()}]) -> request_tuple().
supa_or({Client, Method, Req0, Headers}, ColumnsOperatorsValues) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ "or" ++ "=" ++ "(" ++
            prepare_comma_separated_list(ColumnsOperatorsValues) ++ ")",
        Headers}.

%% internal functions
type_to_tab(http) -> ?HH_TAB.

prepare_comma_separated_list(List) ->
    {Res, _} = lists:foldl(
        fun
            (El, {Acc, N}) when N =:= 0 ->
                {Acc ++ prepare_element(El), N + 1};
            (El, {Acc, N}) ->
                {Acc ++ "," ++ prepare_element(El), N + 1}
        end,
        {"", 0},
        List
    ),
    Res.

prepare_element({Col, Op, Val}) ->
    common:to_list(Col) ++ "." ++ common:to_list(Op) ++ "." ++ common:to_list(Val);
prepare_element(El) ->
    common:to_list(El).

prepare_divider(Req) ->
    case string:slice(Req, length(Req) - 1) of
        "?" -> "";
        _ -> "&"
    end.
