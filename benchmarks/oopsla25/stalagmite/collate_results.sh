#!/bin/bash

# Directory paths
accuracy_dir="stalagmite/output/paper/accuracy/csv"
time_file="stalagmite/output/paper/time.csv"
output_file="results.csv"

# Initialize the output CSV with the header
echo "subject,precision,recall,time_s" > "$output_file"

for file in "$accuracy_dir"/accuracy_*.csv; do
    subject=$(basename "$file" | sed 's/accuracy_\(.*\)\.csv/\1/')
    read precision recall <<< $(awk -F, '$1=="refined" {print $2, $3}' "$file")
    time_sec=$(awk -F, -v subject="$subject" '$1 == subject {print $2}' "$time_file")
    echo "$subject,$precision,$recall,$time_sec" >> "$output_file"
done
