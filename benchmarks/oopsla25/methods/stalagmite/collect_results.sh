#!/bin/bash

# stalagmite/output/subjects/000/1/grammars/refined_grammar_final.json
grammars_dir="stalagmite.tmp/output/grammars/refined"
results_table="stalagmite.tmp/output/results.csv"
results_dir="results"

mkdir -p $results_dir

cp $results_table "$results_dir/results.csv"

base_dir="stalagmite.tmp/output/subjects"
for path in $(find "$base_dir" -type f -name "refined_grammar_final.json"); do
  # Get the directory 3 levels above the file
  dir=$(dirname "$path")           # .../1/grammars
  dir=$(dirname "$dir")            # .../1
  dir=$(dirname "$dir")            # .../000 or 001 or whatever
  subject=$(basename "$dir")       # Extracts just '000', '001', etc.
  echo "Found refined grammar in subject: $subject"
  cp $path "$results_dir/$subject.grammar"
done