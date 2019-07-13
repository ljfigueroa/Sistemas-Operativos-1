#!/bin/bash
erlc constants.erl user.erl pbalance.erl games.erl server.erl client.erl && erl -eval "client:start()."
rm *.beam
