-module(common).

-export([
    update_map/2,
    to_binary/1,
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

to_list(Var) when is_list(Var) -> Var;
to_list(Var) when is_binary(Var) -> binary_to_list(Var);
to_list(Var) when is_float(Var) -> float_to_list(Var);
to_list(Var) when is_integer(Var) -> integer_to_list(Var);
to_list(Var) when is_atom(Var) -> atom_to_list(Var).

%% internal functions
