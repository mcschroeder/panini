#!/bin/bash

subjects_dir="../../subjects"
dest_dir="stalagmite.tmp/subjects"
template_dir="template"
eval_script="stalagmite.tmp/eval/run_eval.sh"

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
    tmp=$dest_dir/$subject/${subject}.c
    cp $tmp $dest_dir/$subject/${subject}_orig.c
    echo '#include "klee/klee.h"' > $dest_dir/$subject/${subject}_symex.c
    cat $tmp >> $dest_dir/$subject/${subject}_symex.c
    if [ "$subject" = "cgidecode" ]; then
        # cgi_decode => use special symex blob (init_hex_values() + check return code)
	    cat $dest_dir/$subject/cgidecode_symex_blob.c >> $dest_dir/$subject/${subject}_symex.c
    else
        # Not cgi_decode => normal symex blob
    	cat $dest_dir/$subject/symex_blob.c >> $dest_dir/$subject/${subject}_symex.c
    fi


    rm $tmp
    echo "mine_and_refine $subject" >> $eval_script
  fi
done
