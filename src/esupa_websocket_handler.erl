-module(esupa_websocket_handler).

-behaviour(gen_server).

-include("common.hrl").
-include_lib("kernel/include/logger.hrl").

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
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

start_link(WSConf, ReceiverPid, Request) ->
    gen_server:start_link(?MODULE, [WSConf, ReceiverPid, Request], []).

init([WSConf, ReceiverPid, Request]) ->
    self() ! ready,
    {ok, #{
        ws_conf => WSConf,
        receiver => ReceiverPid,
        request => Request
    }}.

handle_call(Msg, From, State) ->
    {reply, ok, State}.

handle_cast(Msg, State) ->
    {noreply, State}.      

% esupa_websocket_service:subscribe(self(),{"public","matches","update","id=eq.134"},1).
% recon_trace:calls({esupa_websocket_handler,'handle_info', fun(_) -> return_trace() end},100).

handle_info(ready, State) ->
    self() ! init_ws,
    {noreply, State};
handle_info(init_ws, #{ws_conf := {BaseUrl, WSUrl, Key, _HttpOptions}} = State) ->
    {ok, ConnPid} = gun:open(BaseUrl, ?DEFAULT_PORT, #{
        transport => tls,
        %% Note: default protocols are: [http2,http]
        %% but `http2` will complicate matters here and
        %% is not really needed for our purposes; we just
        %% want to bring up the websocket as quickly
        %% as possible.
        protocols => [http]
    }),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, WSUrl ++ "?apikey=" ++ Key, []),
    {noreply, common:update_map([{conn_pid, ConnPid}, {stream_ref, StreamRef}], State)};
handle_info({gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers}, #{
        request := {Schema, Table, Event, Filter},
        stream_ref := StreamRef
    } = State) ->
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
handle_info({gun_ws, ConnPid, StreamRef, {text, JSON}}, #{stream_ref := StreamRef, conn_pid := ConnPid, receiver := ReceiverPid, timer_count := TimerCount} = State) ->
    
    ?LOG_EMERGENCY("------------------------------jsx:decode(JSON) ~p", [jsx:decode(JSON)]),
    
    case jsx:decode(JSON) of
        #{<<"event">> := Event} when Event =:= <<"phx_reply">> andalso TimerCount =:= 0 -> 
            erlang:send_after(?DEFAULT_HB_TIMEOUT, self(), heartbeat);
        Data ->
            handle_incoming(ReceiverPid, Data)
    end,
    {noreply, maps:put(timer_count, TimerCount + 1, State)};
handle_info(heartbeat, #{conn_pid := ConnPid, stream_ref := StreamRef} = State) ->

    ?LOG_ALERT("HB ~p", [State]),

    RefBin = common:to_binary(erlang:monotonic_time()),

    ?LOG_ALERT("HB RefBin ~p", [RefBin]),

    BaseHBMessage = ?HEARTBEAT_BASE,

    ?LOG_ALERT("HB BaseHBMessage ~p", [BaseHBMessage]),

    ok = gun:ws_send(ConnPid, StreamRef, {text, jsx:encode(maps:put(ref, RefBin, BaseHBMessage))}),
    _TRef = erlang:send_after(?DEFAULT_HB_TIMEOUT, self(), heartbeat),
    {noreply, State};
handle_info(Msg, State) ->
    ?LOG_ERROR("Unhandled message ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_incoming(ReceiverPid, Data) ->
    ?LOG_DEBUG("~p", [Data]),
    ReceiverPid ! Data.


