#!/bin/bash

# NOTE: This script needs to be run inside the Mimid VM.
# Follow the instructions in the README.

subjects_dir="/subjects"
results_dir="/vagrant/results"
results_table="$results_dir/results.csv"

mkdir -p $results_dir

echo "subject,time_seconds,status" > $results_table

for file in "$subjects_dir"/*.c; do
  filename=$(basename -- "$file")
  subject="${filename%.*}"

  echo "Mining grammar of subject $subject ..."

  cd "/home/vagrant/mimid/Cmimid"
  cp "$subjects_dir/$subject.c" "examples/$subject.c"
  echo "{\"[start]\": \"<start>\",\"[grammar]\":$(cat "$subjects_dir/$subject.grammar")}" > "examples/$subject.grammar"

  start_time=$(date +%s)
  make build/$subject.pgrammar
  exit_code=$?
  end_time=$(date +%s)
  elapsed_time=$((end_time - start_time))

  echo "$subject,$elapsed_time,$exit_code" >> $results_table

  echo "Subject $subject done after $elapsed_time seconds; status: $exit_code"

  jq '."[grammar]"' build/$subject.pgrammar > "$results_dir/$subject.grammar"
done
