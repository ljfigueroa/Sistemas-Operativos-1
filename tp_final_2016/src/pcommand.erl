-module(pcommand).

-include("pcommand_interface.hrl").
-include("game_interface.hrl").
-include("user_interface.hrl").
-compile(export_all).


parse(RawString) ->
    [Name | Args] = string:tokens(RawString, " "),
    case Name of
	"CON" -> pcommand(con, Args);
	"LSG" -> pcommand(lgs, Args);
	"NEW" -> pcommand(new, Args);
	"ACC" -> pcommand(acc, Args);
	"PLA" -> pcommand(pla, Args);
	"OBS" -> pcommand(obs, Args);
	"LEA" -> pcommand(lea, Args);
	"BYE" -> pcommand(bye, Args);
	_     -> error
    end.

pcommand(con, [Name | []]) ->
    {ok, con, #pcommand{id=con, name=Name}};
pcommand(lgs, [CmdID | []]) ->
    {ok, #pcommand{id=lgs, cmd_id=CmdID}};
pcommand(new, [CmdID | []]) ->
    {ok, #pcommand{id=new, cmd_id=CmdID}};
pcommand(acc, [CmdID, GameID | []]) ->
    {ok, #pcommand{id=acc, cmd_id=CmdID, game_id=str2int(GameID)}};
pcommand(pla, [CmdID, GameID, PlayMove | []]) ->
    {ok, #pcommand{id=pla, cmd_id=CmdID, game_id=str2int(GameID), move=str2int(PlayMove)}};
pcommand(obs, [CmdID, GameID | []]) ->
    {ok, #pcommand{id=obs, cmd_id=CmdID, game_id=str2int(GameID)}};
pcommand(lea, [CmdID, GameID | []]) ->
    {ok, #pcommand{id=lea, cmd_id=CmdID, game_id=str2int(GameID)}};
pcommand(bye, []) ->
    {ok, #pcommand{id=bye}};
pcommand(_, _) ->
    error.


str2int(Str) ->
    erlang:list_to_integer(Str).

parse_connect(RawString) ->
    [Cmd, Name | _ ] = string:tokens(RawString, " "),
    case Cmd of
	"CON" ->  {ok, #pcommand{id=con, name=Name}};
	_  -> error
    end.


format_type(ok) ->
    "OK";
format_type(error) ->
    "ERROR";
format_type(T) ->
    io_lib:format("ERROR(~p)", [T]).


format_response(lgs, {GameMap})->
    format_game_list(maps:values(GameMap)).

format_game_list(List) ->
    Fun = fun(G) -> format_game(G) end,
    G2S = lists:map(Fun, List),  %% stringify list of games
    JG = string:join(G2S, ", "), %% join list to make a single string
    io_lib:format("[~s]", [JG]). %% wrap the list of games with []


format_game(Game) ->
    Id = Game#game.id,
    Player1 = format_user(Game#game.p1),
    Player2 = format_user(Game#game.p2),
    State = io_lib:format("~p", [Game#game.state]),
    io_lib:format("{id=~p, p1=~s, p2=~s, state=~s}", [Id, Player1, Player2, State]).


format_user(undefined) ->
    "undefined";
format_user(User) ->
    User#user.name.


format_response({R})->
    io_lib:format("~p", [R]);
format_response(_) ->
    "".

%% @doc
%% param T
format(T, C = #pcommand{id=lgs, cmd_id=CmdId}, Response)->
    io_lib:format("~s ~s ~s", [format_type(T), CmdId, format_response(lgs, Response)]);
format(T, C = #pcommand{id=new, cmd_id=CmdId}, Response) ->
    io_lib:format("~s ~s ~s", [format_type(T), CmdId, format_response(Response)]);
format(T, C = #pcommand{id=acc, cmd_id=CmdId}, Response) ->
    io_lib:format("~s ~s ~s", [format_type(T), CmdId, format_response(Response)]);
format(T, C = #pcommand{id=pla, cmd_id=CmdId}, Response) ->
    io_lib:format("~s ~s ~s", [format_type(T), CmdId, format_response(Response)]);
format(T, C = #pcommand{id=obs, cmd_id=CmdId}, Response) ->
    io_lib:format("~s ~s ~s", [format_type(T), CmdId, format_response(Response)]);
format(T, C = #pcommand{id=lea, cmd_id=CmdId}, Response) ->
    io_lib:format("~s ~s ~s", [format_type(T), CmdId, format_response(Response)]);
format(T, C = #pcommand{id=bye}, Response) ->
    io_lib:format("~s ~s", [format_type(T), format_response(Response)]);
format(_,_,_) ->
    "TODO".
