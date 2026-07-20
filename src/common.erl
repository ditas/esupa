-module(common).

-include_lib("kernel/include/logger.hrl").

-define(LOG_MAP(Type, Source, Result, Reason), #{
    id => log_id(),
    type => Type,
    source => Source,
    result => Result,
    reason => Reason
}).

-export([
    update_map/2,
    to_binary/1,
    log/5,
    to_list/1,
    url_encode/1
]).

-spec update_map([{term(), term()}], map()) -> map().
update_map(KVList, Map) ->
    lists:foldl(
        fun({K, V}, Acc) ->
            maps:put(K, V, Acc)
        end,
        Map,
        KVList
    ).

-spec to_binary(atom() | binary() | string() | number() | list()) -> binary().
to_binary(Var) when is_list(Var) -> list_to_binary(Var);
to_binary(Var) when is_atom(Var) -> atom_to_binary(Var);
to_binary(Var) when is_integer(Var) -> integer_to_binary(Var);
to_binary(Var) when is_float(Var) -> float_to_binary(Var);
to_binary(Var) when is_binary(Var) -> Var.

-spec to_list(Var :: string() | binary() | float() | integer() | atom()) -> string().
to_list(Var) when is_list(Var) -> Var;
to_list(Var) when is_binary(Var) -> binary_to_list(Var);
to_list(Var) when is_float(Var) -> float_to_list(Var);
to_list(Var) when is_integer(Var) -> integer_to_list(Var);
to_list(Var) when is_atom(Var) -> atom_to_list(Var).

%% @doc Percent-encode a value for safe inclusion in a query string.
%%
%% Encodes every byte except RFC 3986's "unreserved" set (ALPHA / DIGIT /
%% "-" / "." / "_" / "~"). This deliberately also encodes "+", which RFC
%% 3986 doesn't strictly require escaping but which query-string parsers
%% following the application/x-www-form-urlencoded convention (as many
%% do, including whatever sits in front of PostgREST) decode as a literal
%% space — e.g. an ISO8601 timestamp's "+00:00" offset would otherwise
%% arrive as " 00:00" and fail to parse.
-spec url_encode(Var :: string() | binary() | float() | integer() | atom()) -> string().
url_encode(Var) ->
    Bin =
        case is_binary(Var) of
            true -> Var;
            false -> unicode:characters_to_binary(to_list(Var))
        end,
    lists:flatten([encode_byte(B) || B <- binary_to_list(Bin)]).

encode_byte(B) when
    (B >= $A andalso B =< $Z) orelse
        (B >= $a andalso B =< $z) orelse
        (B >= $0 andalso B =< $9) orelse
        B =:= $- orelse B =:= $. orelse B =:= $_ orelse B =:= $~
->
    B;
encode_byte(B) ->
    io_lib:format("%~2.16.0B", [B]).

-spec log(
    debug | error | warning,
    Type :: app | proc,
    Source :: atom(),
    Result :: ok | error,
    Reason :: term()
) -> ok.
log(debug, Type, Source, Result, Reason) ->
    ?LOG_DEBUG(?LOG_MAP(Type, Source, Result, Reason));
log(error, Type, Source, Result, Reason) ->
    ?LOG_ERROR(?LOG_MAP(Type, Source, Result, Reason));
log(warning, Type, Source, Result, Reason) ->
    ?LOG_WARNING(?LOG_MAP(Type, Source, Result, Reason));
log(info, Type, Source, Result, Reason) ->
    ?LOG_INFO(?LOG_MAP(Type, Source, Result, Reason)).

%% internal functions

log_id() ->
    integer_to_list(erlang:monotonic_time()).
