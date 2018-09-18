#!/bin/bash
source ./model-parameters.sh $MODEL
parallel --env _ --gnu -n 4 ./run-generate-new-variables.lisp {} ::: ${TIMES[*]}
