-module(constants).
-compile(export_all).

get_string(invalid_con_command) -> "Wrong arguments for CON";
get_string(_) -> "ERROR".
