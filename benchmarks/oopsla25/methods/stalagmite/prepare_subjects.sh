#!/bin/bash

subjects_dir="../../subjects"
dest_dir="stalagmite/subjects"
template_dir="template"
eval_script="stalagmite/eval/run_eval.sh"

cp run_eval.sh $eval_script

for file in "$subjects_dir"/*.c; do
  if [[ -f "$file" ]]; then    
    filename=$(basename -- "$file")
    subject="${filename%.*}"
    echo "Prepare $filename"
    mkdir -p "$dest_dir/$subject"
    cp "$file" "$dest_dir/$subject/"
    cp -r "$template_dir"/* "$dest_dir/$subject/"
    for file2 in "$dest_dir/$subject"/*; do
      sed -i "" "s/__SUBJECT__/$subject/g" "$file2"
      if [[ $subject =~ ^[0-9] ]]; then
        sed -i "" "s/__SUBJECT_FUNC__/f$subject/g" "$file2"
      else
        sed -i "" "s/__SUBJECT_FUNC__/$subject/g" "$file2"
      fi
    done
    echo "mine_and_refine $subject" >> $eval_script
  fi
done
