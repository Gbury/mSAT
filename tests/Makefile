
test-icnf:
	@for i in regression/*.icnf ; do \
		echo "test problem $$i"; \
		./icnf-solve/icnf_solve.exe --check $$i > regression/.`basename $$i`.out 2>/dev/null ; \
		diff regression/.`basename $$i`.out regression/.`basename $$i`.ref \
		  || ( echo "mismatch for $$i" ; exit 1) ; \
	 done


test-sudoku:
	@for i in sudoku/sudoku.txt ; do \
	  	echo "test problem $$i"; \
		./../src/sudoku/sudoku_solve.exe $$i ; \
	done
