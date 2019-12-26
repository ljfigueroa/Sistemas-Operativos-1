#!/bin/bash
erlc games.erl tgames.erl user.erl tusers.erl && erl -eval "ok=tgames:start()."
rm *.beam
