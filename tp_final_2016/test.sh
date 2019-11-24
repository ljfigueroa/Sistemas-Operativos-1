#!/bin/bash
erlc games.erl tgames.erl && erl -eval "tgames:start()."
rm *.beam
