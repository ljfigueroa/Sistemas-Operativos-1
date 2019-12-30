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
pcommand(lgs, [CmdId | []]) ->
    {ok, #pcommand{id=lgs, cmdId=CmdId}};
pcommand(new, [CmdId | []]) ->
    {ok, #pcommand{id=new, cmdId=CmdId}};
pcommand(acc, [CmdId, GameId | []]) ->
    {ok, #pcommand{id=acc, cmdId=CmdId}};
pcommand(pla, [CmdId, GameId, PlayMove | []]) ->
    {ok, #pcommand{id=pla, cmdId=CmdId, gameId=GameId, move=PlayMove}};
pcommand(obs, [CmdId, GameId | []]) ->
    {ok, #pcommand{id=obs, cmdId=CmdId, gameId=GameId}};
pcommand(lea, [CmdId, GameId | []]) ->
    {ok, #pcommand{id=lea, cmdId=CmdId}};
pcommand(bye, []) ->
    {ok, #pcommand{id=bye}};
pcommand(_, _) ->
    error.
