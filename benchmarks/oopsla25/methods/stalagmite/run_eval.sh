#!/bin/bash

# NOTE: This script needs to be run inside the Stalagmite docker container.
# Follow the instructions in the README.

TIMEOUT_SEC=600   # 10 minutes

results_table="/stalagmite/data/results.csv"

echo "subject,time_ms,status" > $results_table

mine_and_refine() {
  echo -n "Mining grammar of subject $1 ... "  
  
  mkdir -p /stalagmite/data/subjects/$1  
  start_time=$(date +%s%3N)
  python3 eval.py --subject "$1" --mine &> "logs/eval_mine_log_$1"
  exit_code=$?
  timeout --signal=SIGTERM --kill-after=5s $TIMEOUT_SEC python3 eval.py --subject "$1" --refine &> "logs/eval_refine_log_$1"
  end_time=$(date +%s%3N)
  elapsed_time=$((end_time - start_time))

  echo "$1,$elapsed_time,$exit_code" >> $results_table

  initial_grammar_file="/stalagmite/subjects/$1/initial_grammar.json"
  refined_grammar_final="/stalagmite/subjects/$1/refined_grammar_final.json"
  # If timeout kills refinement, we need to copy the grammar.
  output_refined_grammar_file="/stalagmite/data/subjects/$1/1/grammars/refined_grammar_final.json" # that's the important path
  if [ ! -f "$refined_grammar_final" ]; then
      echo "Refinement preempted -- file does not exist: $refined_grammar_final. Fixing."
      cp $initial_grammar_file $refined_grammar_final
      cp $initial_grammar_file $output_refined_grammar_file
  fi

  echo "done after $elapsed_time ms with status $exit_code"
}
