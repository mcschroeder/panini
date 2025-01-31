#!/bin/bash

source_dir="../../../tests/eval_c"
dest_dir="stalagmite/subjects"
template_dir="template"
eval_script="stalagmite/eval/run_eval.sh"

cat <<EOF > $eval_script
#!/bin/bash
echo "subject,time_seconds" > "/staminag/data/paper/time.csv"
mine_and_refine() {
  echo -n "\$1 "
  start_time=\$(date +%s)
  echo -n "mine "
  timeout 1h python3 eval.py --subject "\$1" --mine &> "logs/eval_mine_log_\$1"
  echo -n "refine "
  python3 eval.py --subject "\$1" --refine &> "logs/eval_mine_log_\$1"
  end_time=\$(date +%s)
  elapsed_time=\$((end_time - start_time))
  echo -n "(\$elapsed_time sec) "
  echo "\$1,\$elapsed_time" >> "/staminag/data/paper/time.csv"
  echo -n "data "
  python3 eval.py --subject "\$1" --data &> "logs/eval_mine_log_\$1"
  data_file="/staminag/data/paper/accuracy/csv/accuracy_\$1.csv"
  if [[ -f "\$data_file" ]]; then
    read precision recall <<< \$(awk -F, '\$1=="refined" {print \$2, \$3}' "\$data_file")
  else
    precision=0
    recall=0
  fi
  printf "(%.2f P, %.2f R)\n" \$precision \$recall
}
EOF

for file in "$source_dir"/*.c; do
  if [[ -f "$file" ]]; then    
    filename=$(basename -- "$file")
    subject="${filename%.*}"
    echo "Prepare $filename"
    mkdir -p "$dest_dir/$subject"
    cp "$file" "$dest_dir/$subject/"
    cp -r "$template_dir"/* "$dest_dir/$subject/"
    for file2 in "$dest_dir/$subject"/*; do
      sed -i "" "s/__SUBJECT__/$subject/g" "$file2"
    done
    echo "mine_and_refine $subject" >> $eval_script
  fi
done
