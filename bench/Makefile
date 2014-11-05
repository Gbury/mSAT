SOLVER 	:=../sat_solve.native
OPTIONS :=-t 30s -s 1G

LOGDIR 	:= $(shell echo "./logs/`git rev-parse HEAD`")
INDEX	:= $(shell echo "./$(LOGDIR)/index")
CNF 	:= $(shell find -L ./ -name "*.cnf" -type f)
DONE 	:= $(addprefix $(LOGDIR)/raw/, $(CNF:.cnf=.done))

PROVER := $(shell echo "./$(LOGDIR)/prover")
CMD    := $(PROVER) $(OPTIONS)

all: $(INDEX)

$(INDEX): $(PROVER) $(DONE)
	@echo "Bench ended for commit `git rev-parse HEAD`" | tee -a $(INDEX)
	@echo "Ended at `date +'%H:%M:%S %d/%m/%Y'`" | tee -a $(INDEX)

$(PROVER): $(SOLVER)
	@mkdir -p $(LOGDIR)/raw
	@cp $(SOLVER) $(PROVER)
	@echo "Bench started for commit `git rev-parse HEAD`"

$(SOLVER):
	cd .. && $(MAKE)

$(LOGDIR)/raw/%.done : $(PROVER)
	@mkdir -p $(dir $@)
	@echo "solving problem $*.cnf..."
	@./run_prover "$(CMD)" "$*.cnf" "$@"

