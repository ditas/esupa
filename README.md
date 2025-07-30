# esupa

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-25.0%2B-red.svg)](http://www.erlang.org)

**esupa** is an Erlang/OTP library that provides a comprehensive client interface for [Supabase](https://supabase.com), supporting both REST API operations and real-time WebSocket connections for PostgreSQL change streams.

## Features

### 🚀 HTTP REST API Client
- **Complete CRUD Operations**: SELECT, INSERT, UPDATE, DELETE
- **Advanced Querying**: Filters, joins, ordering, pagination
- **Connection Pooling**: Efficient HTTP client management
- **Schema Support**: Multi-schema database operations
- **Type Safety**: Comprehensive type specifications

### 🔄 Real-time WebSocket Integration
- **PostgreSQL Change Streams**: Listen to database changes in real-time
- **Automatic Reconnection**: Robust connection handling with failover
- **Event Filtering**: Subscribe to specific tables, schemas, and operations
- **Connection Pooling**: Multiple WebSocket connection management
- **Heartbeat Support**: Automatic connection health monitoring

## Installation

Add `esupa` to your `rebar.config` dependencies:

```erlang
{deps, [
    {esupa, {git, "https://github.com/your-org/esupa.git", {branch, "main"}}}
]}.
```

## Quick Start

### 1. Configuration

Configure your Supabase connection in `sys.config`:

```erlang
{esupa, [
    {base_url, "your-project.supabase.co"},
    {rest_url, "/rest/v1/"},
    {ws_url, "/realtime/v1/websocket"},
    {key, "your-anon-key"},
    {http_handler_pool_size, 10},
    {max_ws_handler_pool_size, 5}
]}
```

### 2. Basic HTTP API Usage

```elixir
# Get a client connection
{ok, client} = :esupa.get_client()

# Build and execute a query
result = :esupa.request(client, get, 'public')
    |> :esupa.supa_from('users')
    |> :esupa.supa_select(['id', 'name', 'email'])
    |> :esupa.supa_eq('active', true)
    |> :esupa.supa_order('created_at', desc)
    |> :esupa.supa_range(0, 10)
    |> :esupa.execute()
```

### 3. Real-time WebSocket Usage

```erlang
%% Subscribe to table changes
esupa_websocket_service:subscribe(
    self(),                           % Receiver PID
    {"public", "users", "INSERT", ""}, % {Schema, Table, Event, Filter}
    1                                 % Number of subscribers
),

%% Handle incoming changes
receive
    #{<<"event">> := <<"INSERT">>, <<"new">> := NewRecord} ->
        io:format("New user created: ~p~n", [NewRecord])
end.
```

## HTTP API Reference

### Connection Management

#### `get_client() -> {ok, pid()} | {error, string()}`

Retrieves an available HTTP client from the connection pool.

```erlang
{ok, Client} = esupa:get_client().
```

### Request Building

#### `request(Client, Method, Schema) -> request()`

Creates a new request targeting the specified schema.

```erlang
Request = esupa:request(Client, get, "public").
```

**Parameters:**
- `Client`: HTTP client PID
- `Method`: HTTP method (`get`, `post`, `patch`, `delete`)
- `Schema`: Database schema name

### Query Operations

#### `supa_from(Request, Table) -> request()`

Specifies the target table for the operation.

```erlang
Request2 = esupa:supa_from(Request, "users").
```

#### `supa_select(Request, Columns) -> request()`

Defines which columns to return. Use `[]` for all columns.

```erlang
%% Select specific columns
Request3 = esupa:supa_select(Request2, ["id", "name", "email"]),

%% Select all columns
Request3 = esupa:supa_select(Request2, []).
```

#### `supa_join(Request, Joins) -> request()`

Adds JOIN operations with related tables.

```erlang
%% Join with profiles table
Joins = [{"profiles", ["avatar_url", "bio"]}],
Request4 = esupa:supa_join(Request3, Joins).
```

### Filters and Conditions

#### Comparison Operators

```erlang
%% Equality
Request = esupa:supa_eq(Request, "status", "active"),

%% Greater than
Request = esupa:supa_gt(Request, "age", 18),

%% Greater than or equal
Request = esupa:supa_gte(Request, "score", 100),

%% Less than
Request = esupa:supa_lt(Request, "price", 50),

%% Less than or equal
Request = esupa:supa_lte(Request, "quantity", 10),

%% IN clause
Request = esupa:supa_in(Request, "category", ["electronics", "books"]).
```

#### Complex Conditions

```erlang
%% OR conditions
OrConditions = [
    {"age", "gte", "21"},
    {"verified", "eq", "true"}
],
Request = esupa:supa_or(Request, OrConditions).
```

### Data Modification

#### `supa_insert(Request, Data) -> request()`

Inserts new records into the table.

```erlang
UserData = #{
    <<"name">> => <<"John Doe">>,
    <<"email">> => <<"john@example.com">>,
    <<"active">> => true
},
Request = esupa:supa_insert(Request, UserData).
```

#### `supa_update(Request, Data) -> request()`

Updates existing records. Combine with filters to specify which records to update.

```erlang
UpdateData = #{<<"status">> => <<"inactive">>},
Request = esupa:request(Client, patch, "public")
    |> esupa:supa_from("users")
    |> esupa:supa_eq("id", 123)
    |> esupa:supa_update(UpdateData).
```

#### `supa_delete(Request) -> request()`

Deletes records. Use with filters to specify which records to delete.

```erlang
Request = esupa:request(Client, delete, "public")
    |> esupa:supa_from("users")
    |> esupa:supa_eq("active", false)
    |> esupa:supa_delete().
```

### Query Modifiers

#### `supa_order(Request, Column, Direction) -> request()`

Orders results by the specified column.

```erlang
%% Ascending order
Request = esupa:supa_order(Request, "created_at", asc),

%% Descending order
Request = esupa:supa_order(Request, "updated_at", desc).
```

#### `supa_range(Request, Min, Max) -> request()`

Implements pagination by limiting the result range.

```erlang
%% Get records 0-19 (first page, 20 items)
Request = esupa:supa_range(Request, 0, 19),

%% Get records 20-39 (second page, 20 items)
Request = esupa:supa_range(Request, 20, 39).
```

### Execution

#### `execute(Request) -> term()`

Executes the built request and returns the response.

```erlang
Result = esupa:execute(Request).
```

## WebSocket Real-time API

### Subscription Management

#### `esupa_websocket_service:subscribe(ReceiverPid, Request, NumSubscribers)`

Subscribes to real-time database changes.

**Parameters:**
- `ReceiverPid`: Process that will receive change notifications
- `Request`: Tuple of `{Schema, Table, Event, Filter}`
- `NumSubscribers`: Number of WebSocket connections to establish

**Events:**
- `"INSERT"`: New records created
- `"UPDATE"`: Existing records modified
- `"DELETE"`: Records deleted
- `"*"`: All change events

**Example:**
```erlang
%% Listen to all INSERT events on users table
esupa_websocket_service:subscribe(
    self(),
    {"public", "users", "INSERT", ""},
    1
),

%% Listen to UPDATE events for specific user
esupa_websocket_service:subscribe(
    self(),
    {"public", "users", "UPDATE", "id=eq.123"},
    1
).
```

### Message Handling

WebSocket messages are delivered to the subscribing process with the following structure:

```erlang
receive
    #{
        <<"event">> := Event,           % "INSERT", "UPDATE", or "DELETE"
        <<"schema">> := Schema,         % Database schema
        <<"table">> := Table,           % Table name
        <<"commit_timestamp">> := TS,   % Transaction timestamp
        <<"new">> := NewRecord,         % New record data (INSERT/UPDATE)
        <<"old">> := OldRecord          % Previous record data (UPDATE/DELETE)
    } ->
        handle_change(Event, NewRecord, OldRecord)
end.
```

## Complete Examples

### User Management System

```erlang
-module(user_manager).
-export([create_user/1, get_active_users/0, listen_for_new_users/0]).

create_user(UserData) ->
    {ok, Client} = esupa:get_client(),
    esupa:request(Client, post, "public")
        |> esupa:supa_from("users")
        |> esupa:supa_insert(UserData)
        |> esupa:execute().

get_active_users() ->
    {ok, Client} = esupa:get_client(),
    esupa:request(Client, get, "public")
        |> esupa:supa_from("users")
        |> esupa:supa_select(["id", "name", "email", "created_at"])
        |> esupa:supa_eq("active", true)
        |> esupa:supa_order("created_at", desc)
        |> esupa:supa_range(0, 49)  % First 50 users
        |> esupa:execute().

listen_for_new_users() ->
    esupa_websocket_service:subscribe(
        self(),
        {"public", "users", "INSERT", ""},
        1
    ),
    user_change_loop().

user_change_loop() ->
    receive
        #{<<"event">> := <<"INSERT">>, <<"new">> := NewUser} ->
            io:format("New user registered: ~s~n", [
                maps:get(<<"email">>, NewUser, <<"unknown">>)
            ]),
            user_change_loop();
        Other ->
            io:format("Received: ~p~n", [Other]),
            user_change_loop()
    after 30000 ->
        io:format("No new users in the last 30 seconds~n"),
        user_change_loop()
    end.
```

### Blog Post System with Real-time Comments

```erlang
-module(blog_system).
-export([get_post_with_comments/1, listen_for_comments/1]).

get_post_with_comments(PostId) ->
    {ok, Client} = esupa:get_client(),
    esupa:request(Client, get, "public")
        |> esupa:supa_from("posts")
        |> esupa:supa_select([
            "id", "title", "content", "created_at",
            "comments(id,content,author,created_at)"
        ])
        |> esupa:supa_eq("id", PostId)
        |> esupa:execute().

listen_for_comments(PostId) ->
    Filter = "post_id=eq." ++ integer_to_list(PostId),
    esupa_websocket_service:subscribe(
        self(),
        {"public", "comments", "INSERT", Filter},
        1
    ),
    comment_loop(PostId).

comment_loop(PostId) ->
    receive
        #{<<"event">> := <<"INSERT">>, <<"new">> := Comment} ->
            io:format("New comment on post ~p: ~s~n", [
                PostId,
                maps:get(<<"content">>, Comment, <<"...">>)
            ]),
            comment_loop(PostId)
    end.
```

## Configuration Options

### HTTP Configuration

```erlang
{esupa, [
    {base_url, "your-project.supabase.co"},
    {rest_url, "/rest/v1/"},
    {key, "your-anon-or-service-key"},
    {http_handler_pool_size, 10},
    {httpc_options, [
        {keep_alive_timeout, 120000},
        {max_pipeline_length, 10},
        {max_sessions, 20}
    ]}
]}
```

### WebSocket Configuration

```erlang
{esupa, [
    {ws_url, "/realtime/v1/websocket"},
    {max_ws_handler_pool_size, 5}
]}
```

## Building and Running

### Development Build

```bash
$ rebar3 compile
$ rebar3 shell
```

### Release Build

```bash
# Local development release
$ rebar3 as local release
$ ./_build/local/rel/esupa/bin/esupa console

# Test environment
$ rebar3 as test release
$ ./_build/test/rel/esupa/bin/esupa console

# Production release
$ rebar3 as prod release
$ ./_build/prod/rel/esupa/bin/esupa daemon
```

## Testing

```bash
# Run unit tests
$ rebar3 eunit

# Run common tests
$ rebar3 ct

# Run all tests with coverage
$ rebar3 cover
```

## Documentation

Generate documentation using ExDoc:

```bash
$ rebar3 ex_doc
```

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests (`rebar3 eunit ct`)
5. Format code (`rebar3 fmt`)
6. Run linter (`rebar3 lint`)
7. Commit your changes (`git commit -am 'Add amazing feature'`)
8. Push to the branch (`git push origin feature/amazing-feature`)
9. Open a Pull Request

## License

This project is licensed under the Apache License 2.0 - see the [LICENSE.md](LICENSE.md) file for details.

## Dependencies

- **Erlang/OTP 24+**: Required runtime
- **gun**: HTTP/WebSocket client
- **jsx**: JSON encoding/decoding
- **recon**: Production debugging tools

## Changelog

### v0.6.0
- Current stable release
- Full HTTP REST API support
- WebSocket real-time subscriptions
- Connection pooling
- Automatic reconnection handling

## Support

- **Issues**: [GitHub Issues](https://github.com/your-org/esupa/issues)
- **Documentation**: Generated docs available in `/docs`
- **Examples**: See `/examples` directory for more use cases

---

Made with ❤️ for the Erlang and Supabase communities.
