#!/bin/bash

subjects_dir="../../subjects"
dest_dir="subjects"

SAMPLE_COUNT=20
SAMPLE_DEPTH=1000

mkdir -p "$dest_dir"

for file in "$subjects_dir"/*.py; do
  if [[ -f "$file" ]]; then    
    filename=$(basename -- "$file")
    subject="${filename%.*}"    
    echo "Prepare $filename"
    dest_file="$dest_dir/$subject.py"
    cp "$file" "$dest_file"
    echo -e "#!/usr/bin/env python3\n\n$(cat "$dest_file")" > "$dest_file"
    chmod +x "$dest_file"
    sample_dir="${dest_dir}/${subject}_samples"
    mkdir -p $sample_dir
    python3 gensamples.py --source "$subjects_dir/${subject}.grammar" --outdir "${sample_dir}" --count $SAMPLE_COUNT --depth $SAMPLE_DEPTH
  fi
done
