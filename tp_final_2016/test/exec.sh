#!/bin/bash
rm *.beam
erlc *.erl && erl -eval "client:start()."
rm *.beam
