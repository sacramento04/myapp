-ifdef(debug).
    -define(IS_DEBUG, true).
    -define(DEBUG(Msg, Args), io:format("[debug] - " ++ Msg, Args)).
    -define(TRACE(Call), io:format("[trace][~p,~p]~n    code: ~s ~n    return: ~w~n", [?MODULE, ?LINE, ??Call, Call])).
-else.
    -define(IS_DEBUG, false).
    -define(DEBUG(Msg, Args), ok).
    -define(TRACE(Call), ok).
-endif.

-define(INFO(Msg, Args), client_app_log:write(info, Msg, Args)).
-define(ERROR(Msg, Args), client_app_log:write(error, Msg, Args)).
-define(WARNING(Msg, Args), client_app_log:write(warning, Msg, Args)).

-define(LOG_DIR, "./log/").

-record(
    client_state, 
    {
        id = 0,
        socket = undefined
    }
).