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
    to_list/1
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
    ?LOG_WARNING(?LOG_MAP(Type, Source, Result, Reason)).

-spec to_list(Var :: string() | binary() | float() | integer() | atom()) -> string().
to_list(Var) when is_list(Var) -> Var;
to_list(Var) when is_binary(Var) -> binary_to_list(Var);
to_list(Var) when is_float(Var) -> float_to_list(Var);
to_list(Var) when is_integer(Var) -> integer_to_list(Var);
to_list(Var) when is_atom(Var) -> atom_to_list(Var).

%% internal functions

log_id() ->
    integer_to_list(erlang:monotonic_time()).
