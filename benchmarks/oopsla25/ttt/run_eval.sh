#!/bin/bash

SAMPLE_COUNT=10000
subjects_dir="../subjects"
output_dir="output"
results_file="results.csv"

echo "subject,time_ms,precision,recall"
echo "subject,time_ms,precision,recall" > $results_file

mkdir -p $output_dir

for file in "$subjects_dir"/*.py; do
  if [[ -f "$file" ]]; then    
    filename=$(basename -- "$file")
    subject="${filename%.*}"
    golden=$subjects_dir/$subject.regex
    output=$output_dir/$subject.grammar
    echo -n "$subject,"
    echo -n "$subject," >> $results_file
    result=$(java -cp target/ttt-eval-1.0-SNAPSHOT.jar org.example.Main $file $golden $SAMPLE_COUNT $output)
    echo $result
    echo $result >> $results_file
  fi
done
