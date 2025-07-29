# esupa

An OTP application to simplify requests to [Supabase API](https://supabase.com/docs/guides/api)

## Build
```
$ rebar3 as <profile> release
$ ./_build/<profile>/esupa/bin/esupa console | daemon
```

## Usage
```
{ok, Pid} = esupa:get_client().
A1 = esupa:request(Pid, get, "schema_name").
A2 = esupa:supa_from(A1, "table_name").
A3 = esupa:supa_select(A2, ["optional", "fields", "to", "select"]).

A3 = esupa:supa_select(A2, ["optional", "fields", "to", "select", "another_table_name("another_table_field1", "another_table_field2")"]).

A4 = esupa:supa_eq(A3, "id", 123).
```

For another schema use:
`A2 = esupa:supa_from(A1, "persona_page_full", "another_schema_name").`
