-module(common).

-export([
    update_map/2,
    to_binary/1
]).

update_map(KVList, Map) ->
    lists:foldl(fun({K, V}, Acc) ->
        maps:put(K, V, Acc) 
    end, Map, KVList).

to_binary(Var) when is_list(Var) -> list_to_binary(Var);
to_binary(Var) when is_atom(Var) -> atom_to_binary(Var);
to_binary(Var) when is_integer(Var) -> integer_to_binary(Var);
to_binary(Var) when is_float(Var) -> float_to_binary(Var);
to_binary(Var) when is_binary(Var) -> Var.