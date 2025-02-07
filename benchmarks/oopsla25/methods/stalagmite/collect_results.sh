#!/bin/bash

grammars_dir="stalagmite/output/paper/grammars/refined"
results_table="stalagmite/output/paper/results.csv"
results_dir="results"

mkdir -p $results_dir

mv $results_table "$results_dir/results.csv"

for file in "$grammars_dir"/*.json; do
  if [[ "$file" =~ refined_grammar_([a-zA-Z0-9_]+)\.json ]]; then
    subject="${BASH_REMATCH[1]}"
    if [[ "$subject" =~ ^(calc|json|lisp|tinyc)$ ]]; then
      continue
    fi

    cp $file "$results_dir/$subject.grammar"

  fi
done
