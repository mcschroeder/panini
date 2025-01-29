#!/bin/bash

source_dir="../../../tests/eval_c"
dest_dir="stalagmite/data/paper/grammars/golden"

for file in "$source_dir"/*.regex; do
  if [[ -f "$file" ]]; then
    filename=$(basename -- "$file")
    dir_name="${filename%.*}"
    echo "Prepare golden grammar for $dir_name"
    golden_file="$dest_dir/golden_grammar_${dir_name}.json"
    cabal run -v0 regex -- -f posix -t fuzzingbook -o $golden_file "$file"
  fi
done
