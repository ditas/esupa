-module(esupa_websocket_service).

-behaviour(gen_server).

-include("include/common.hrl").

% API
-export([start_link/1]).

-export([
    subscribe/3
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

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

subscribe(ReceiverPid, Request, NumberOfSubscribers) ->
    gen_server:cast(?MODULE, {subscribe, ReceiverPid, Request, NumberOfSubscribers}).

init(State) ->
    process_flag(trap_exit, true),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(
    {subscribe, _ReceiverPid, _Request, NumberOfSubscribers},
    #{available_pool_size := PoolSize} = State
) when
    NumberOfSubscribers > PoolSize
->
    common:log(error, proc, websocket_service, error, "too many subscribers"),
    {stop, too_many_subscribers, State};
handle_cast(
    {subscribe, ReceiverPid, Request, NumberOfSubscribers},
    #{available_pool_size := PoolSize, ws_config := WSConf} = State
) ->
    ok = lists:foreach(
        fun(_I) ->
            {ok, _Pid} = esupa_websocket_handler:start_link(WSConf, ReceiverPid, Request)
        end,
        lists:seq(0, NumberOfSubscribers - 1)
    ),
    {noreply, maps:put(pull_size, PoolSize - NumberOfSubscribers, State)};
handle_cast(Msg, State) ->
    common:log(warning, proc, websocket_service, ok, Msg),
    {noreply, State}.

handle_info({'DOWN', _Mref, process, _ConnPid, Reason}, State) ->
    logger:error("Websocket Connection Failed ~p", [Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions
