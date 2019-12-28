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
    #pcommand{id=lgs, cmdId=CmdId};
pcommand(new, [CmdId | []]) ->
    #pcommand{id=new, cmdId=CmdId};
pcommand(acc, [CmdId, GameId | []]) ->
    #pcommand{id=acc, cmdId=CmdId};
pcommand(pla, [CmdId, GameId, PlayMove | []]) ->
    #pcommand{id=pla, cmdId=CmdId, gameId=GameId, move=PlayMove};
pcommand(obs, [CmdId, GameId | []]) ->
    #pcommand{id=obs, cmdId=CmdId, gameId=GameId};
pcommand(lea, [CmdId, GameId | []]) ->
    #pcommand{id=lea, cmdId=CmdId};
pcommand(bye, []) ->
    #pcommand{id=bye};
pcommand(_, _) ->
    error.
