-module(esupa).

-include("include/common.hrl").

-export([
    get_client/0
]).

-export([
    supa_from/2,
    supa_select/2, supa_join/2,
    supa_insert/2,
    supa_update/2,
    supa_delete/1,

    %% TODO:
    % supa_upsert,

    %% filter methods
    supa_eq/3,
    supa_gt/3,
    supa_gte/3,
    supa_lt/3,
    supa_lte/3,
    supa_in/3,
    supa_range/3,
    supa_order/3,
    supa_or/2,

    %% TODO:
    % supa_is,
    % supa_neq,

    request/2,
    request/3,
    execute/1
]).

-type request() :: {
    Client :: pid(),
    Method :: get | post | patch | delete,
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

-spec request(Client :: pid(), Method :: get | post | patch | delete) -> request().
request(Client, Method) ->
    {Client, Method, "", [], undefined}.

-spec request(Client :: pid(), Method :: get | post | patch | delete, Schema :: string()) -> request().
request(Client, Method, Schema) ->
    {Client, Method, "", [{"Accept-Profile", Schema}], undefined}.

-spec execute(request()) -> term().
execute({Client, Method, Req, Headers, Body}) ->
    esupa_http_handler:request(Client, Method, Req, Headers, Body).

-spec supa_from(request(), string()) -> request().
supa_from({Client, Method, Req0, Headers, Body}, Table) ->
    {Client, Method, Req0 ++ common:to_list(Table), Headers, Body}.

-spec supa_select(request(), [string()]) -> request().
supa_select({Client, Method, Req0, Headers, Body}, []) ->
    {Client, Method, Req0 ++ "?select=" ++ "*", Headers, Body};
supa_select({Client, Method, Req0, Headers, Body}, Columns) ->
    Req = Req0 ++ "?select=" ++ prepare_comma_separated_list(Columns),
    {Client, Method, Req, Headers, Body}.

supa_join({Client, Method, Req0, Headers, Body}, []) ->
    {Client, Method, Req0, Headers, Body};
supa_join({Client, Method, Req0, Headers, Body}, Joins) ->
    Req = recursive_join(Joins, Req0, 0),
    {Client, Method, Req, Headers, Body}.

recursive_join([], Acc, N) ->
    lists:foldr(fun(_, Acc1) -> Acc1 ++ ")" end, Acc, lists:seq(1, N));
    
recursive_join([{Table, []}|Rest], Acc, N) when N =:= 0 ->
    recursive_join(Rest, Acc ++ "," ++ common:to_list(Table) ++ "(", N+1);
recursive_join([{Table, []}|Rest], Acc, N) ->
    recursive_join(Rest, Acc ++ common:to_list(Table) ++ "(", N+1);
    
recursive_join([{Table, Columns}], Acc, N) when N =:= 0 ->
    recursive_join([], Acc ++ "," ++ common:to_list(Table) ++ "(" ++ prepare_comma_separated_list(Columns), N+1);
recursive_join([{Table, Columns}], Acc, N) ->
    recursive_join([], Acc ++ common:to_list(Table) ++ "(" ++ prepare_comma_separated_list(Columns), N+1);
    
recursive_join([{Table, Columns}|Rest], Acc, N) when N =:= 0 ->
    recursive_join(Rest, Acc ++ "," ++ common:to_list(Table) ++ "(" ++ prepare_comma_separated_list(Columns) ++ ",", N+1);
recursive_join([{Table, Columns}|Rest], Acc, N) ->
    recursive_join(Rest, Acc ++ common:to_list(Table) ++ "(" ++ prepare_comma_separated_list(Columns) ++ ",", N+1).

supa_insert({Client, Method, Req, Headers, _Body0}, Body) ->
    {Client, Method, Req, Headers, jsx:encode(Body)}.

supa_update({Client, Method, Req0, Headers, undefined}, Body) ->
    {Client, Method, Req0 ++ "?", Headers ++ [{"Prefer", "return=minimal"}], jsx:encode(Body)};
supa_update({Client, Method, Req0, Headers, Body0}, Body) ->
    BodyBin = jsx:encode(Body),
    {Client, Method, Req0 ++ "?", Headers ++ [{"Prefer", "return=minimal"}],
        <<Body0/binary, BodyBin/binary>>}.

supa_delete({Client, Method, Req0, Headers, Body}) ->
    {Client, Method, Req0 ++ "?", Headers, Body}.

-spec supa_range(request(), non_neg_integer(), non_neg_integer()) -> request().
supa_range({Client, Method, Req0, Headers, Body}, Min, Max) ->
    {Client, Method, Req0,
        Headers ++ [{"Range", common:to_list(Min) ++ "-" ++ common:to_list(Max)}], Body}.

-spec supa_order(request(), string(), asc | desc) -> request().
supa_order({Client, Method, Req0, Headers, Body}, Column, Order) ->
    {Client, Method,
        Req0 ++ "&" ++ "order" ++ "=" ++ common:to_list(Column) ++ "." ++ common:to_list(Order),
        Headers, Body}.

-spec supa_eq(request(), string(), term()) -> request().
supa_eq({Client, Method, Req0, Headers, Body}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "eq." ++
            common:to_list(Value),
        Headers, Body}.

-spec supa_gt(request(), string(), term()) -> request().
supa_gt({Client, Method, Req0, Headers, Body}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "gt." ++
            common:to_list(Value),
        Headers, Body}.

-spec supa_gte(request(), string(), term()) -> request().
supa_gte({Client, Method, Req0, Headers, Body}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "gte." ++
            common:to_list(Value),
        Headers, Body}.

-spec supa_lt(request(), string(), term()) -> request().
supa_lt({Client, Method, Req0, Headers, Body}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "lt." ++
            common:to_list(Value),
        Headers, Body}.

-spec supa_lte(request(), string(), term()) -> request().
supa_lte({Client, Method, Req0, Headers, Body}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "lte." ++
            common:to_list(Value),
        Headers, Body}.

-spec supa_in(request(), string(), [term()]) -> request().
supa_in({Client, Method, Req0, Headers, Body}, Column, Values) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "in.(" ++
            prepare_comma_separated_list(Values) ++ ")",
        Headers, Body}.

-spec supa_or(request(), [{string(), string(), term()}]) -> request().
supa_or({Client, Method, Req0, Headers, Body}, ColumnsOperatorsValues) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ "or" ++ "=" ++ "(" ++
            prepare_comma_separated_list(ColumnsOperatorsValues) ++ ")",
        Headers, Body}.

%% internal functions
type_to_tab(http) -> ?HH_TAB.


% prepare_comma_separated_list_simple(List) ->
%     {Res, _} = lists:foldl(
%         fun
%             (El, {Acc, N}) when N =:= 0 -> {Acc ++ common:to_list(El), N + 1};
%             (El, {Acc, N}) -> {Acc ++ "," ++ common:to_list(El), N + 1}
%         end,
%         {"", 0},
%         List
%     ),
%     Res.

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
