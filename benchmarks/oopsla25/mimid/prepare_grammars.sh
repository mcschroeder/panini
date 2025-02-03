#!/bin/bash

source_dir="../../../tests/eval_c"
dest_dir="subjects"

mkdir -p $dest_dir

for file in "$source_dir"/*.regex; do
  if [[ -f "$file" ]]; then
    filename=$(basename -- "$file")
    subject="${filename%.*}"
    echo "Prepare ${subject}.grammar"
    golden_file="$dest_dir/${subject}.grammar"
    cabal run -v0 regex -- -f posix -t fuzzingbook -o $golden_file "$file"
    echo "{\"[start]\": \"<start>\",\"[grammar]\":$(cat "$golden_file")}" > "$golden_file"
  fi
done
