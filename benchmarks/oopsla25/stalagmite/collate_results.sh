#!/bin/bash

accuracy_dir="stalagmite/output/paper/accuracy/csv"
time_file="stalagmite/output/paper/time.csv"
output_file="results.csv"

echo "subject,time_seconds,precision,recall" > "$output_file"

while IFS=, read -r subject time_seconds; do
  if [[ "$subject" == "subject" ]]; then
    continue
  fi
  
  accuracy_file="$accuracy_dir/accuracy_${subject}.csv"
  if [[ -f "$accuracy_file" ]]; then
    read precision recall <<< $(awk -F, '$1=="refined" {print $2, $3}' "$accuracy_file")
  else
    precision=0
    recall=0
  fi
  echo "$subject,$time_seconds,$precision,$recall" >> "$output_file"

done < "$time_file"
