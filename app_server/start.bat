@echo off
cls
erl +P 102400 -boot start_sasl -pa ebin -s app_server_app start -name jarvis_server@192.168.24.88 -setcookie jarvis -env ERL_MAX_ETS_TABLES 100000
pause