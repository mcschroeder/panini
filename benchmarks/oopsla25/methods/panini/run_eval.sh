#!/bin/bash

subjects_dir="../../subjects"
results_dir="results"
results_table="$results_dir/results.csv"

cabal build -v0 panini
cabal build -v0 regex

mkdir -p $results_dir

echo "subject,time_seconds,status" > $results_table

printf "%-10s %-10s %-8s %-30s\n" "Subject" "Time (s)" "Status" "Regex"
printf "%-10s %-10s %-8s %-30s\n" "----------" "----------" "--------" "--------------------"

for file in "$subjects_dir"/*.py; do
  filename=$(basename -- "$file")
  subject="${filename%.*}"
  outfile="$results_dir/$subject.out"  

  printf "%-10s " $subject

  start_time=$(date +%s)
  cabal run -v0 panini -- --no-color "$file" > $outfile 2>&1
  exit_code=$?
  end_time=$(date +%s)
  elapsed_time=$((end_time - start_time))

  echo "$subject,$elapsed_time,$exit_code" >> $results_table

  printf "%-10s %-8s " $elapsed_time $exit_code

  if [ "$exit_code" -eq 0 ]; then
    sed -n 's/.*∈ \(.*\)}.*/\1/p; s/.*= "\(.*\)".*/\1/p; s/.*: (s:𝕊) → .*/\.\*/p; s/.*: {s:𝕊 | false} → .*/\[\]/p' $outfile | head -n 1 > $results_dir/$subject.regex
    cat $results_dir/$subject.regex
  else
    printf "\n"
  fi  
done
