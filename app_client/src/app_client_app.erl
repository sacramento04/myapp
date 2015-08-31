-module(app_client_app).

-behavious(application).
-behavious(supervisor).

-export([
    start/0,
    start/2,
    stop/1
]).

-export([
    init/1
]).

-include("app_client_app.hrl").


start () ->
    application:start(?MODULE).
    
start (_Type, _StartArgs) ->
    file:make_dir(?LOG_DIR),
    inets:start(),
    Result = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    
    start_client_app_log(),
    start_client_socket_sup(),
    
    Result.
    
stop (_State) ->
    ok.
    
init ([]) ->
    {ok, {{one_for_one, 0, 1}, []}}.
    
start_client_app_log () ->
    {ok, _} = supervisor:start_child(
        ?MODULE, 
        {
            client_app_log, 
            {client_app_log, start_link, []}, 
            permanent, 
            16#FFFFFFFF, 
            worker, 
            [client_app_log]
        }
    ).
    
start_client_socket_sup () ->
    {ok, _} = supervisor:start_child(
        ?MODULE,
        {
            client_socket_sup,
            {client_socket_sup, start_link, []},
            permanent,
            16#FFFFFFFF,
            supervisor,
            [client_socket_sup]
        }
    ).