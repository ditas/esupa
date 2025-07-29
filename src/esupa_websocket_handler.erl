-module(esupa_websocket_handler).

-behaviour(gen_server).

-include("include/common.hrl").

-define(DEFAULT_PORT, 443).
-define(DEFAULT_HB_TIMEOUT, 30000).
-define(HEARTBEAT_BASE, #{
    <<"event">> => <<"heartbeat">>,
    <<"topic">> => <<"phoenix">>,
    <<"payload">> => #{},
    <<"ref">> => undefined
}).

%% API
-export([
    start_link/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start_link(WSConf, ReceiverPid, Request) ->
    gen_server:start_link(?MODULE, [WSConf, ReceiverPid, Request], []).

init([WSConf, ReceiverPid, Request]) ->
    self() ! ready,
    {ok, #{
        ws_conf => WSConf,
        receiver => ReceiverPid,
        request => Request
    }}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% esupa_websocket_service:subscribe(self(),{"public","matches","update","id=eq.2"},1).
% recon_trace:calls({esupa_websocket_handler,'handle_info', fun(_) -> return_trace() end},100).
handle_info(ready, State) ->
    self() ! init_ws,
    {noreply, State};
handle_info(init_ws, #{ws_conf := {BaseUrl, WSUrl, Key, _HttpOptions}} = State) ->
    {ok, ConnPid} = gun:open(BaseUrl, ?DEFAULT_PORT, #{
        transport => tls,
        % tls_opts => [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}, {server_name_indication, "*.supabase.co"}]
        tls_opts => [{verify, verify_none}],
        protocols => [http]
    }),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, WSUrl ++ "?apikey=" ++ Key, []),
    {noreply, common:update_map([{conn_pid, ConnPid}, {stream_ref, StreamRef}], State)};
handle_info(
    {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers},
    #{
        request := {Schema, Table, Event, Filter},
        stream_ref := StreamRef
    } = State
) ->
    SchemaBin = common:to_binary(Schema),
    TableBin = common:to_binary(Table),
    RefBin = common:to_binary(erlang:monotonic_time()),
    ChannelMsg = #{
        event => <<"phx_join">>,
        topic => erlang:iolist_to_binary([<<"realtime:">>, SchemaBin, <<":">>, TableBin]),
        payload => #{
            <<"config">> => #{
                <<"postgres_changes">> => [
                    #{
                        <<"event">> => common:to_binary(Event),
                        <<"schema">> => SchemaBin,
                        <<"table">> => TableBin,
                        <<"filter">> => common:to_binary(Filter)
                    }
                ]
            }
        },
        ref => RefBin
    },
    ok = gun:ws_send(ConnPid, StreamRef, {text, jsx:encode(ChannelMsg)}),
    {noreply, common:update_map([{ref, RefBin}, {timer_count, 0}], State)};
handle_info(
    {gun_ws, ConnPid, StreamRef, {text, JSON}},
    #{
        stream_ref := StreamRef,
        conn_pid := ConnPid,
        receiver := ReceiverPid,
        timer_count := TimerCount
    } = State
) ->
    _ =
        case jsx:decode(JSON) of
            #{<<"event">> := Event} when Event =:= <<"phx_reply">> andalso TimerCount =:= 0 ->
                erlang:send_after(?DEFAULT_HB_TIMEOUT, self(), heartbeat);
            Data ->
                handle_incoming(ReceiverPid, Data)
        end,
    {noreply, maps:put(timer_count, TimerCount + 1, State)};
handle_info(heartbeat, #{conn_pid := ConnPid, stream_ref := StreamRef} = State) ->
    RefBin = common:to_binary(erlang:monotonic_time()),
    BaseHBMessage = ?HEARTBEAT_BASE,
    ok = gun:ws_send(ConnPid, StreamRef, {text, jsx:encode(maps:put(ref, RefBin, BaseHBMessage))}),
    _TRef = erlang:send_after(?DEFAULT_HB_TIMEOUT, self(), heartbeat),
    {noreply, State};
handle_info({gun_down, ConnPid, _, _, _}, #{conn_pid := ConnPid, receiver := ReceiverPid, request := Request} = State) ->
    esupa_websocket_service:subscribe(ReceiverPid, Request, 1),
    {stop, normal, State};
handle_info(Msg, State) ->
    common:log(warning, proc, websocker_handler, ok, Msg),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec handle_incoming(pid(), term()) -> ok.
handle_incoming(ReceiverPid, Data) ->
    ReceiverPid ! Data,
    ok.
