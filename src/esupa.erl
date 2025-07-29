%% @doc Erlang Supabase API Client
%%
%% This module provides a high-level interface for interacting with Supabase's REST API.
%% It supports common database operations like SELECT, INSERT, UPDATE, DELETE with
%% query building capabilities including filters, joins, ordering, and pagination.
%%
%% == Usage ==
%%
%% Basic workflow:
%% 1. Get a client connection
%% 2. Create a request with schema
%% 3. Build query using chain of supa_* functions
%% 4. Execute the request
%%
%% ```
%% {ok, Pid} = esupa:get_client(),
%% A1 = esupa:request(Pid, get, "schema_name"),
%% A2 = esupa:supa_from(A1, "table_name"),
%% A3 = esupa:supa_select(A2, ["field1", "field2"]),
%% A4 = esupa:supa_eq(A3, "id", 123),
%% Result = esupa:execute(A4).
%% '''
%%
%% == Supported Operations ==
%%
%% - Query operations: SELECT with joins, filters, ordering
%% - Data modification: INSERT, UPDATE, DELETE
%% - Filters: eq, gt, gte, lt, lte, in, range, or
%% - Utility: pagination with range, column selection
%%
%% @author ditas
%% @version 1.0.0
-module(esupa).

-include("include/common.hrl").

%% Client management
-export([
    get_client/0
]).

%% Request building and execution
-export([
    request/3,
    execute/1
]).

%% Query building
-export([
    supa_from/2,
    supa_select/2,
    supa_join/2
]).

%% Data modification
-export([
    supa_insert/2,
    supa_update/2,
    supa_delete/1
    %% TODO: supa_upsert/2
]).

%% Filters and conditions
-export([
    supa_eq/3,
    supa_gt/3,
    supa_gte/3,
    supa_lt/3,
    supa_lte/3,
    supa_in/3,
    supa_or/2
]).

%% Query modifiers
-export([
    supa_range/3,
    supa_order/3
    %% TODO: supa_is/3, supa_neq/3
]).

%% @type request(). Represents a Supabase API request.
%%
%% Contains all information needed to execute a request:
%% - Client: PID of the HTTP client process
%% - Method: HTTP method (get, post, patch, delete)
%% - Req: URL path with query parameters
%% - Headers: HTTP headers including schema and preferences
%% - Body: JSON-encoded request body (for POST/PATCH operations)
-type request() :: {
    Client :: pid(),
    Method :: get | post | patch | delete,
    Req :: string(),
    Headers :: [{string(), term()}],
    Body :: jsx:json_term()
}.

%% @doc Get an available HTTP client for making requests.
%%
%% Returns a PID of an available HTTP client from the connection pool.
%% The client is used to execute requests against the Supabase API.
%%
%% @returns {ok, Pid} if a client is available, {error, Reason} otherwise
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

%% @doc Create a new request for the specified schema.
%%
%% Initializes a request object that can be further modified with supa_* functions.
%% The schema parameter specifies which Supabase schema to target.
%%
%% @param Client PID of the HTTP client
%% @param Method HTTP method (get, post, patch, delete)
%% @param Schema Name of the database schema to target
%% @returns A request record ready for further modification
-spec request(Client :: pid(), Method :: get | post | patch | delete, Schema :: string()) ->
    request().
request(Client, Method, Schema) ->
    {Client, Method, "", [{"Accept-Profile", Schema}], undefined}.

%% @doc Execute a built request against the Supabase API.
%%
%% Sends the HTTP request to Supabase and returns the response.
%% This is typically the final step after building a query chain.
%%
%% @param Request The request to execute
%% @returns The API response (format depends on the operation)
-spec execute(request()) -> term().
execute({Client, Method, Req, Headers, Body}) ->
    esupa_http_handler:request(Client, Method, Req, Headers, Body).

%% @doc Specify the table to query from.
%%
%% Sets the target table for the request. This is typically the first
%% function called after creating a request.
%%
%% @param Request The request to modify
%% @param Table Name of the table to query
%% @returns Modified request targeting the specified table
-spec supa_from(request(), string()) -> request().
supa_from({Client, Method, Req0, Headers, Body}, Table) ->
    {Client, Method, Req0 ++ common:to_list(Table), Headers, Body}.

%% @doc Specify which columns to select.
%%
%% Defines which columns should be returned in the query result.
%% If an empty list is provided, all columns (*) will be selected.
%%
%% @param Request The request to modify
%% @param Columns List of column names to select, or [] for all columns
%% @returns Modified request with column selection
-spec supa_select(request(), [string()]) -> request().
supa_select({Client, Method, Req0, Headers, Body}, []) ->
    {Client, Method, Req0 ++ "?select=" ++ "*", Headers, Body};
supa_select({Client, Method, Req0, Headers, Body}, Columns) ->
    Req = Req0 ++ "?select=" ++ prepare_comma_separated_list(Columns),
    {Client, Method, Req, Headers, Body}.

%% @doc Add JOIN clauses to the query.
%%
%% Enables joining with related tables. Each join is specified as a tuple
%% of {TableName, ColumnsList}. Empty column list means select all columns
%% from the joined table.
%%
%% @param Request The request to modify
%% @param Joins List of join specifications: [{Table, Columns}, ...]
%% @returns Modified request with join clauses
supa_join({Client, Method, Req0, Headers, Body}, []) ->
    {Client, Method, Req0, Headers, Body};
supa_join({Client, Method, Req0, Headers, Body}, Joins) ->
    Req = recursive_join(Joins, Req0, 0),
    {Client, Method, Req, Headers, Body}.

recursive_join([], Acc, N) ->
    lists:foldr(fun(_, Acc1) -> Acc1 ++ ")" end, Acc, lists:seq(1, N));
recursive_join([{Table, []} | Rest], Acc, N) when N =:= 0 ->
    recursive_join(Rest, Acc ++ "," ++ common:to_list(Table) ++ "(", N + 1);
recursive_join([{Table, []} | Rest], Acc, N) ->
    recursive_join(Rest, Acc ++ common:to_list(Table) ++ "(", N + 1);
recursive_join([{Table, Columns}], Acc, N) when N =:= 0 ->
    recursive_join(
        [],
        Acc ++ "," ++ common:to_list(Table) ++ "(" ++ prepare_comma_separated_list(Columns),
        N + 1
    );
recursive_join([{Table, Columns}], Acc, N) ->
    recursive_join(
        [], Acc ++ common:to_list(Table) ++ "(" ++ prepare_comma_separated_list(Columns), N + 1
    );
recursive_join([{Table, Columns} | Rest], Acc, N) when N =:= 0 ->
    recursive_join(
        Rest,
        Acc ++ "," ++ common:to_list(Table) ++ "(" ++ prepare_comma_separated_list(Columns) ++ ",",
        N + 1
    );
recursive_join([{Table, Columns} | Rest], Acc, N) ->
    recursive_join(
        Rest,
        Acc ++ common:to_list(Table) ++ "(" ++ prepare_comma_separated_list(Columns) ++ ",",
        N + 1
    ).

%% @doc Insert data into the table.
%%
%% Converts the request to an INSERT operation with the provided data.
%% The data will be JSON-encoded before sending.
%%
%% @param Request The request to modify
%% @param Body Data to insert (will be JSON-encoded)
%% @returns Modified request for INSERT operation
supa_insert({Client, Method, Req, Headers, _Body0}, Body) ->
    {Client, Method, Req, Headers, jsx:encode(Body)}.

%% @doc Update records in the table.
%%
%% Converts the request to an UPDATE operation. Use with filter functions
%% to specify which records to update. Sets "return=minimal" preference
%% to avoid returning updated data by default.
%%
%% @param Request The request to modify
%% @param Body Data to update (will be JSON-encoded)
%% @returns Modified request for UPDATE operation
supa_update({Client, Method, Req0, Headers, undefined}, Body) ->
    {Client, Method, Req0 ++ "?", Headers ++ [{"Prefer", "return=minimal"}], jsx:encode(Body)};
supa_update({Client, Method, Req0, Headers, Body0}, Body) ->
    BodyBin = jsx:encode(Body),
    {Client, Method, Req0 ++ "?", Headers ++ [{"Prefer", "return=minimal"}],
        <<Body0/binary, BodyBin/binary>>}.

%% @doc Delete records from the table.
%%
%% Converts the request to a DELETE operation. Use with filter functions
%% to specify which records to delete.
%%
%% @param Request The request to modify
%% @returns Modified request for DELETE operation
supa_delete({Client, Method, Req0, Headers, Body}) ->
    {Client, Method, Req0 ++ "?", Headers, Body}.

%% @doc Add pagination range to the request.
%%
%% Limits the result set to a specific range of records, useful for pagination.
%% The range is 0-based and inclusive of both Min and Max.
%%
%% @param Request The request to modify
%% @param Min Starting index (0-based)
%% @param Max Ending index (inclusive)
%% @returns Modified request with range limitation
-spec supa_range(request(), non_neg_integer(), non_neg_integer()) -> request().
supa_range({Client, Method, Req0, Headers, Body}, Min, Max) ->
    {Client, Method, Req0,
        Headers ++ [{"Range", common:to_list(Min) ++ "-" ++ common:to_list(Max)}], Body}.

%% @doc Add ordering to the query results.
%%
%% Sorts the result set by the specified column in ascending or descending order.
%% Multiple order clauses can be chained.
%%
%% @param Request The request to modify
%% @param Column Name of the column to sort by
%% @param Order Sort direction (asc or desc)
%% @returns Modified request with ordering
-spec supa_order(request(), string(), asc | desc) -> request().
supa_order({Client, Method, Req0, Headers, Body}, Column, Order) ->
    {Client, Method,
        Req0 ++ "&" ++ "order" ++ "=" ++ common:to_list(Column) ++ "." ++ common:to_list(Order),
        Headers, Body}.

%% @doc Add equality filter to the query.
%%
%% Filters results where the specified column equals the given value.
%%
%% @param Request The request to modify
%% @param Column Name of the column to filter
%% @param Value Value to match (will be converted to string)
%% @returns Modified request with equality filter
-spec supa_eq(request(), string(), term()) -> request().
supa_eq({Client, Method, Req0, Headers, Body}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "eq." ++
            common:to_list(Value),
        Headers, Body}.

%% @doc Add greater-than filter to the query.
%%
%% Filters results where the specified column is greater than the given value.
%%
%% @param Request The request to modify
%% @param Column Name of the column to filter
%% @param Value Value to compare against
%% @returns Modified request with greater-than filter
-spec supa_gt(request(), string(), term()) -> request().
supa_gt({Client, Method, Req0, Headers, Body}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "gt." ++
            common:to_list(Value),
        Headers, Body}.

%% @doc Add greater-than-or-equal filter to the query.
%%
%% Filters results where the specified column is greater than or equal to the given value.
%%
%% @param Request The request to modify
%% @param Column Name of the column to filter
%% @param Value Value to compare against
%% @returns Modified request with greater-than-or-equal filter
-spec supa_gte(request(), string(), term()) -> request().
supa_gte({Client, Method, Req0, Headers, Body}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "gte." ++
            common:to_list(Value),
        Headers, Body}.

%% @doc Add less-than filter to the query.
%%
%% Filters results where the specified column is less than the given value.
%%
%% @param Request The request to modify
%% @param Column Name of the column to filter
%% @param Value Value to compare against
%% @returns Modified request with less-than filter
-spec supa_lt(request(), string(), term()) -> request().
supa_lt({Client, Method, Req0, Headers, Body}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "lt." ++
            common:to_list(Value),
        Headers, Body}.

%% @doc Add less-than-or-equal filter to the query.
%%
%% Filters results where the specified column is less than or equal to the given value.
%%
%% @param Request The request to modify
%% @param Column Name of the column to filter
%% @param Value Value to compare against
%% @returns Modified request with less-than-or-equal filter
-spec supa_lte(request(), string(), term()) -> request().
supa_lte({Client, Method, Req0, Headers, Body}, Column, Value) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "lte." ++
            common:to_list(Value),
        Headers, Body}.

%% @doc Add IN filter to the query.
%%
%% Filters results where the specified column value is in the given list of values.
%%
%% @param Request The request to modify
%% @param Column Name of the column to filter
%% @param Values List of values to match against
%% @returns Modified request with IN filter
-spec supa_in(request(), string(), [term()]) -> request().
supa_in({Client, Method, Req0, Headers, Body}, Column, Values) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ common:to_list(Column) ++ "=" ++ "in.(" ++
            prepare_comma_separated_list(Values) ++ ")",
        Headers, Body}.

%% @doc Add OR condition to the query.
%%
%% Creates an OR condition with multiple column-operator-value combinations.
%% Each tuple should contain {Column, Operator, Value} where Operator is
%% a string like "eq", "gt", "lt", etc.
%%
%% @param Request The request to modify
%% @param ColumnsOperatorsValues List of {Column, Operator, Value} tuples
%% @returns Modified request with OR condition
-spec supa_or(request(), [{string(), string(), term()}]) -> request().
supa_or({Client, Method, Req0, Headers, Body}, ColumnsOperatorsValues) ->
    {Client, Method,
        Req0 ++ prepare_divider(Req0) ++ "or" ++ "=" ++ "(" ++
            prepare_comma_separated_list(ColumnsOperatorsValues) ++ ")",
        Headers, Body}.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @private
%% @doc Convert client type to ETS table name.
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

%% @private
%% @doc Convert list of elements to comma-separated string.
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

%% @private
%% @doc Format individual element for URL parameter.
prepare_element({Col, Op, Val}) ->
    common:to_list(Col) ++ "." ++ common:to_list(Op) ++ "." ++ common:to_list(Val);
prepare_element(El) ->
    common:to_list(El).

%% @private
%% @doc Determine appropriate URL parameter divider (? or &).
prepare_divider(Req) ->
    case string:slice(Req, length(Req) - 1) of
        "?" -> "";
        _ -> "&"
    end.
