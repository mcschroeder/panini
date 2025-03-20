#!/bin/bash

# NOTE: This script needs to be run inside the TreeVada Docker container.
# Follow the instructions in the README.

subjects_dir="/panini/subjects"
results_dir="/panini/results"
results_table="$results_dir/results.csv"

cp /panini/unpickle.py /treevada-artifact/treevada

mkdir -p $results_dir

echo "subject,time_ms,status" > $results_table

for file in "$subjects_dir"/*.py; do
  filename=$(basename -- "$file")
  subject="${filename%.*}"
  sample_dir="$subjects_dir/${subject}_samples"
  log_file="$subjects_dir/${subject}.log"

  echo "Mining grammar of subject $subject ..."

  cd /treevada-artifact/treevada
  start_time=$(date +%s%3N)
  python3 search.py external "${file}" "${sample_dir}" "${log_file}"
  exit_code=$?
  end_time=$(date +%s%3N)
  elapsed_time=$((end_time - start_time))

  echo "$subject,$elapsed_time,$exit_code" >> $results_table

  echo "Subject $subject done after $elapsed_time ms; status: $exit_code"

  if [[ -e "${log_file}.gramdict" ]]; then
    python3 unpickle.py --input "${log_file}.gramdict" --output "${results_dir}/${subject}.grammar"
  fi
done
