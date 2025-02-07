#!/bin/bash

SAMPLE_COUNT=10000
subjects_dir="../../subjects"
results_dir="results"
results_table="$results_dir/results.csv"

mkdir -p $results_dir

echo "subject,time_seconds,status" > $results_table

printf "%-10s %-10s %-8s\n" "Subject" "Time (s)" "Status"
printf "%-10s %-10s %-8s\n" "----------" "----------" "--------"

for file in "$subjects_dir"/*.py; do
  filename=$(basename -- "$file")
  subject="${filename%.*}"
  golden=$subjects_dir/$subject.regex
  output=$results_dir/$subject.grammar

  printf "%-10s " $subject

  start_time=$(date +%s)
  java -cp target/ttt-eval-1.0-SNAPSHOT.jar org.example.Main $file $golden $SAMPLE_COUNT $output
  exit_code=$?
  end_time=$(date +%s)
  elapsed_time=$((end_time - start_time))

  echo "$subject,$elapsed_time,$exit_code" >> $results_table

  printf "%-10s %-8s\n" $elapsed_time $exit_code
done
