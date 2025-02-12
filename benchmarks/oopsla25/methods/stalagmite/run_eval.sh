#!/bin/bash

# NOTE: This script needs to be run inside the Stalagmite docker container.
# Follow the instructions in the README.

results_table="/staminag/data/paper/results.csv"

echo "subject,time_ms,status" > $results_table

mine_and_refine() {
  echo -n "Mining grammar of subject $1 ... "  
  
  start_time=$(date +%s%3N)
  python3 eval.py --subject "$1" --mine &> "logs/eval_mine_log_$1"  
  python3 eval.py --subject "$1" --refine &> "logs/eval_mine_log_$1"
  exit_code=$?
  end_time=$(date +%s%3N)
  elapsed_time=$((end_time - start_time))

  echo "$1,$elapsed_time,$exit_code" >> $results_table

  echo "done after $elapsed_time ms with status $exit_code"
}
