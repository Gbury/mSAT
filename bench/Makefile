SOLVER 	:=../sat_solve.native
OPTIONS :=-t 30s -s 1G

LOGDIR 	:= $(shell echo "logs/`git rev-parse HEAD`")
LOGDIRE := $(shell echo "logs\/`git rev-parse HEAD`")
INDEX	:= $(shell echo "$(LOGDIR)/index")
CNF 	:= $(shell find ./ -name "*.cnf" -type f)
DONE 	:= $(addprefix $(LOGDIR)/raw/, $(CNF:.cnf=.done))

PROVER := $(shell echo "$(LOGDIR)/prover")
CMD    := $(PROVER) $(OPTIONS)

all: $(INDEX)
	@echo "All done."

$(INDEX): $(PROVER) $(DONE)
	@echo "Ended at `date +'%H:%M:%S %d/%m/%Y'`" > $(INDEX)
	@echo "Index created"

$(PROVER):
	@echo "Creating log directory..." && mkdir -p $(LOGDIR)/raw
	@echo "Copying prover..." && cp $(SOLVER) $(PROVER)

$(LOGDIR)/raw/%.done : $(PROVER)
	@mkdir -p $(dir $@)
	@echo "solving problem $*.cnf..."
	@./run_prover "$(CMD)" "$*.cnf" "$@"

