SOLVER =../sat_solve.native
OPTIONS =-t 30s -s 1G
CMD = $(SOLVER) $(OPTIONS)

LOGDIR := logs
LOG := $(shell  date +'$(LOGDIR)/sat_%Y-%m-%d-%H%M')
CNF := $(shell  find ./ -name "*.cnf" -type f)
DONE := $(CNF:.cnf=.done)

all: $(LOG) $(DONE)
	@find ./ -name "*.done" -type f -delete
	@echo "All done."

$(LOG):
	@mkdir -p $(LOGDIR)
	@touch $(LOG)
	@echo "Log file : $(LOG)"
	@echo "Date : `date +'%H:%M %d-%m-%Y'`" | tee -a $(LOG)
	@echo "Command : '$(CMD)'" | tee -a $(LOG)
	@echo "Commit : `git rev-parse HEAD`" | tee -a $(LOG)

%.done : %.cnf $(LOG)
	@./run_prover "$(CMD)" "$*" "$(LOG)"
	@touch $*.done

clean:
	@find ./ -name "*.done" -type f -delete
