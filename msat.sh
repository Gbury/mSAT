#!/bin/sh

exec dune exec --profile=release src/main/main.exe -- $@
