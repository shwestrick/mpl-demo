MPL=/afs/cs/academic/class/15210-s17/mpl/build/bin/mpl -mlb-path-var "COMPILER mpl"
MLTON=mlton -mlb-path-var "COMPILER mlton"
FLAGS=-default-type int32

.PHONY: run.mpl run.mlton
run.mpl:
	$(MPL) $(FLAGS) -output $@ sources.mlb

run.mlton:
	$(MLTON) $(FLAGS) -output $@ sources.mlb
