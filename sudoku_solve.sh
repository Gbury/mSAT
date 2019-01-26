#!/bin/sh

#exec dune exec src/sudoku/sudoku_solve.exe -- $@
exec dune exec --profile=release src/sudoku/sudoku_solve.exe -- $@
