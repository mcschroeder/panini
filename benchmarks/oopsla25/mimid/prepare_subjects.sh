#!/bin/bash

source_dir="../../../tests/eval_c"
dest_dir="subjects"
eval_script="$dest_dir/run_eval.sh"

mkdir -p $dest_dir

cat <<EOF > $eval_script
#!/bin/bash
echo "subject,time_seconds,precision,recall" > "/vagrant/results.csv"
mine_and_eval() {
  echo "Mining \$1 ..."
  cd /home/vagrant/mimid/Cmimid
  cp /vagrant/subjects/\$1.c examples/\$1.c
  cp /vagrant/subjects/\$1.grammar examples/\$1.grammar
  start_time=\$(date +%s)
  timeout 1h make build/\$1.grammar
  end_time=\$(date +%s)
  elapsed_time=\$((end_time - start_time))
  echo -n "\$1,\$elapsed_time" >> "/vagrant/results.csv"
  echo "Mining \$1 DONE (\$elapsed_time sec)"

  echo "Computing precision for \$1 ..."
  make build/\$1.fuzz
  if [[ -f build/\$1.fuzz ]]; then
    precision=\$(grep -oP '\d+/(\d+)' build/\$1.fuzz | awk -F/ '{print \$1/\$2}')
  else
    precision=0
  fi
  echo -n ",\$precision" >> "/vagrant/results.csv"
  printf "Computing precision for \$1 DONE (%.2f)\n" \$precision

  echo "Computing recall for \$1 ..."
  make build/\$1.precision
  if [[ -f build/\$1.precision ]]; then
    recall=\$(grep -oP '\d+/(\d+)' build/\$1.precision | awk -F/ '{print \$1/\$2}')
  else
    recall=0
  fi
  echo ",\$recall" >> "/vagrant/results.csv"
  printf "Computing recall for \$1 DONE (%.2f)\n" \$recall
}
EOF

for file in "$source_dir"/*.c; do
  if [[ -f "$file" ]]; then    
    filename=$(basename -- "$file")
    subject="${filename%.*}"
    echo "Prepare $filename"
    cat "template/pre.c" "$file" "template/post.c" > "$dest_dir/$filename"
    sed -i "" "s/__SUBJECT__/$subject/g" "$dest_dir/$filename"
    echo "mine_and_eval $subject" >> $eval_script
  fi
done
