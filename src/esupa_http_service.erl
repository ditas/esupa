-module(esupa_http_service).

-behaviour(gen_server).

-include("include/common.hrl").

-define(DEFAULT_HTTP_PROFILE, one_long_live_conn_per_process).

% API
-export([start_link/1]).

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

init(State) ->
    process_flag(trap_exit, true),
    HHTId = ets:new(?HH_TAB, [public, set, named_table]),
    self() ! init,
    {ok, maps:put(tid, HHTId, State)}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    init, #{pool_size := PoolSize, tid := HHTId, http_config := {Url, Key, HttpcOptions}} = State
) ->
    ok = httpc:set_options(HttpcOptions, ?DEFAULT_HTTP_PROFILE),
    ok = lists:foreach(
        fun(_I) ->
            {ok, _Pid} = esupa_http_handler:start_link(HHTId, {Url, Key})
        end,
        lists:seq(0, PoolSize - 1)
    ),
    {noreply, State};
handle_info({'DOWN', _Mref, process, _ConnPid, Reason}, State) ->
    common:log(error, proc, http_service, error, Reason),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions
