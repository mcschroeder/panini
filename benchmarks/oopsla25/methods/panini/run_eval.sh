#!/bin/bash

# NOTE: this is intended to be run from within the Panini Docker container
# see README for more information

subjects_dir="../../subjects"
results_dir="../../methods/panini/results"
results_table="$results_dir/results.csv"

#cabal build panini
#cabal build regex

mkdir -p $results_dir

echo "subject,time_ms,status,sat" > $results_table

printf "%-10s %-10s %-8s %-8s %-30s\n" "Subject" "Time (ms)" "Status" "Verified" "Regex" 
printf "%-10s %-10s %-8s %-8s %-30s\n" "----------" "----------" "--------" "--------" "--------------------"

for file in "$subjects_dir"/*.py; do
  if [ -e "${file%.py}.pan" ]; then
    file="${file%.py}.pan"
  fi
  filename=$(basename -- "$file")
  subject="${filename%.*}"
  outfile="$results_dir/$subject.out"  

  printf "%-10s " $subject

  start_time=$(date +%s)
  cabal run -v0 panini -- --no-color "$file" --dump-smt > $outfile 2>&1
  exit_code=$?
  end_time=$(date +%s)
  elapsed_time=$((end_time - start_time))

  echo "$subject,$elapsed_time,$exit_code" >> $results_table

  printf "%-10s %-8s " $elapsed_time $exit_code

  if [ "$exit_code" -eq 0 ]; then

    if grep -q "UNVERIFIED" $outfile; then
      echo -n ",0" >> $results_table
      printf "%-8s" "0"
    else
      echo -n ",1" >> $results_table
      printf "%-8s" "1"
    fi

    sed -n '$ s/.*∈ \(.*\)}.*/\1/p; s/.*= "\(.*\)".*/\1/p; s/.*: (s:𝕊) → .*/\.\*/p; s/.*: {s:𝕊 | false} → .*/\[\]/p' $outfile | head -n 1  > $results_dir/$subject.regex
    cat $results_dir/$subject.regex


    
  #   if [ "$(cat $results_dir/$subject.regex)" == "[]" ]; then
  #     echo '{"<start>":[]}' > "$results_dir/$subject.grammar"
  #   else
  #     cabal run -v0 regex -- -f posix -t fuzzingbook -o "$results_dir/$subject.grammar" "$results_dir/$subject.regex"
  #   fi
  else
    printf "\n"
  fi  

  if [ -e "$subjects_dir/$subject.smt" ]; then
    mv "$subjects_dir/$subject.smt" "$results_dir/$subject.smt"
  fi

done
