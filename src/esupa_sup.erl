%%%-------------------------------------------------------------------
%% @doc esupa top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(esupa_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(APP, esupa).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, BaseUrl} = application:get_env(?APP, base_url),
    {ok, ProjectUrl} = application:get_env(?APP, project_url),
    {ok, Key} = application:get_env(?APP, key),
    {ok, HttpcOptions} = application:get_env(?APP, httpc_options),

    {ok, MaxWSPoolSize} = application:get_env(?APP, max_ws_handler_pool_size),
    {ok, WSUrl} = application:get_env(?APP, ws_url),

    {ok, HttpPoolSize} = application:get_env(?APP, http_handler_pool_size),
    {ok, RestUrl} = application:get_env(?APP, rest_url),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => http_pool_service,
            start =>
                {esupa_http_service, start_link, [
                    common:update_map(
                        [
                            {pool_size, HttpPoolSize},
                            {http_config, {
                                common:to_list(ProjectUrl) ++ common:to_list(BaseUrl) ++
                                    common:to_list(RestUrl),
                                common:to_list(Key),
                                HttpcOptions
                            }}
                        ],
                        #{}
                    )
                ]},
            modules => [esupa_http_service],
            restart => permanent,
            type => worker
        },
        #{
            id => websocket_pool_service,
            start =>
                {esupa_websocket_service, start_link, [
                    common:update_map(
                        [
                            {available_pool_size, MaxWSPoolSize},
                            {ws_config, {
                                common:to_list(ProjectUrl),
                                common:to_list(BaseUrl),
                                common:to_list(WSUrl),
                                common:to_list(Key),
                                HttpcOptions
                            }}
                        ],
                        #{}
                    )
                ]},
            modules => [esupa_websocket_service],
            restart => permanent,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
