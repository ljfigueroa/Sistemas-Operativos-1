-module(pcommand).
-include("pcommand_interface.hrl").
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
    {ok, #pcommand{id=acc, cmd_id=CmdID}};
pcommand(pla, [CmdID, GameID, PlayMove | []]) ->
    {ok, #pcommand{id=pla, cmd_id=CmdID, game_id=GameID, move=PlayMove}};
pcommand(obs, [CmdID, GameID | []]) ->
    {ok, #pcommand{id=obs, cmd_id=CmdID, game_id=GameID}};
pcommand(lea, [CmdID, GameID | []]) ->
    {ok, #pcommand{id=lea, cmd_id=CmdID}};
pcommand(bye, []) ->
    {ok, #pcommand{id=bye}};
pcommand(_, _) ->
    error.
