#!/bin/bash
erlc pbalance.erl games.erl server.erl client.erl && erl -eval "client:start()."
