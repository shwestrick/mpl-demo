MPL=/afs/cs/academic/class/15210-s17/mpl/build/bin/mpl -mlb-path-var "COMPILER mpl"
#MPL=/afs/cs/academic/class/15210-s17/mpl-flat/build/bin/mpl -mlb-path-var "COMPILER mpl"
FLAGS=-default-type int64

.PHONY: run.mpl run.mlton
run.mpl:
	$(MPL) $(FLAGS) -output $@ sources.mlb
