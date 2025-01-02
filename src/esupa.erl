-module(esupa).

-include("include/common.hrl").

-export([
    get_client/0
]).

-export([
    from/2,
    select/2,
    eq/3,
    % neq/3,
    gt/3,
    gte/3,
    lt/3,
    lte/3,
    in/3,
    range/3,
    order/3,

    request/2,
    execute/1
]).

-type request_tuple() :: {
    Client :: pid(),
    Method :: get | post | update | delete,
    Req :: string(),
    Headers :: [{string(), term()}]
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

-spec execute(request_tuple()) -> term().
execute({Client, Method, Req, Headers}) ->
    esupa_http_handler:request(Client, Method, Req, Headers).

-spec from(request_tuple(), string()) -> request_tuple().
from({Client, Method, Req0, Headers}, Table) ->
    {Client, Method, Req0 ++ common:to_list(Table), Headers}.

-spec select(request_tuple(), [string()]) -> request_tuple().
select({Client, Method, Req0, Headers}, []) ->
    {Client, Method, Req0 ++ "?select=" ++ "*", Headers};
select({Client, Method, Req0, Headers}, Columns0) ->
    {Req, _} = lists:foldl(
        fun
            (Column, {Acc, N}) when N =:= 0 -> {Acc ++ common:to_list(Column), N + 1};
            (Column, {Acc, N}) -> {Acc ++ "," ++ common:to_list(Column), N + 1}
        end,
        {Req0 ++ "?select=", 0},
        Columns0
    ),
    {Client, Method, Req, Headers}.

-spec range(request_tuple(), integer(), integer()) -> request_tuple().
range({Client, Method, Req0, Headers}, Min, Max) ->
    {Client, Method, Req0,
        Headers ++ [{"Range", common:to_list(Min) ++ "-" ++ common:to_list(Max)}]}.

-spec order(request_tuple(), string(), asc | desc) -> request_tuple().
order({Client, Method, Req0, Headers}, Column, Order) ->
    {Client, Method,
        Req0 ++ "&" ++ "order" ++ "=" ++ common:to_list(Column) ++ "." ++ common:to_list(Order),
        Headers}.

-spec eq(request_tuple(), string(), term()) -> request_tuple().
eq({Client, Method, Req0, Headers}, Column, Value) ->
    {Client, Method, Req0 ++ "&" ++ common:to_list(Column) ++ "=" ++ "eq." ++ common:to_list(Value),
        Headers}.

% -spec neq(request_tuple(), string(), term()) -> request_tuple().
% neq({Client, Method, Req0, Headers}, Column, Value) ->
%     {Client, Method,
%         Req0 ++ "&" ++ common:to_list(Column) ++ "=" ++ "not.is." ++ common:to_list(Value),
%         Headers}.

-spec gt(request_tuple(), string(), term()) -> request_tuple().
gt({Client, Method, Req0, Headers}, Column, Value) ->
    {Client, Method, Req0 ++ "&" ++ common:to_list(Column) ++ "=" ++ "gt." ++ common:to_list(Value),
        Headers}.

-spec gte(request_tuple(), string(), term()) -> request_tuple().
gte({Client, Method, Req0, Headers}, Column, Value) ->
    {Client, Method,
        Req0 ++ "&" ++ common:to_list(Column) ++ "=" ++ "gte." ++ common:to_list(Value), Headers}.

-spec lt(request_tuple(), string(), term()) -> request_tuple().
lt({Client, Method, Req0, Headers}, Column, Value) ->
    {Client, Method, Req0 ++ "&" ++ common:to_list(Column) ++ "=" ++ "lt." ++ common:to_list(Value),
        Headers}.

-spec lte(request_tuple(), string(), term()) -> request_tuple().
lte({Client, Method, Req0, Headers}, Column, Value) ->
    {Client, Method,
        Req0 ++ "&" ++ common:to_list(Column) ++ "=" ++ "lte." ++ common:to_list(Value), Headers}.

-spec in(request_tuple(), string(), [term()]) -> request_tuple().
in({Client, Method, Req0, Headers}, Column, Values) ->
    {Client, Method,
        Req0 ++ "&" ++ common:to_list(Column) ++ "=" ++ "in.(" ++
            prepare_comma_separated_list(Values) ++ ")",
        Headers}.

%% internal functions
type_to_tab(http) -> ?HH_TAB.

prepare_comma_separated_list(List) ->
    {Res, _} = lists:foldl(
        fun
            (El, {Acc, N}) when N =:= 0 -> {Acc ++ common:to_list(El), N + 1};
            (El, {Acc, N}) -> {Acc ++ "," ++ common:to_list(El), N + 1}
        end,
        {"", 0},
        List
    ),
    Res.
