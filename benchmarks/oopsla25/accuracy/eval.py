import csv
from collections import defaultdict
import statistics
from compare_grammars import compare_grammars, load_grammar
import os

subjects_dir="/benchmark/subjects"
methods_dir="/benchmark/methods"
methods = ["mimid","panini","stalagmite","ttt"]

output_file = "/benchmark/accuracy/results/results_by_subject.csv"
output_agg_file = "/benchmark/accuracy/results/results_by_category.csv"

#####################################################################

FUZZ_DEPTH = 1000
FUZZ_COUNT = 1000

def compute_results(method):
  print(f"{method}", flush=True)
  
  results = {}  

  results_dir = methods_dir + "/" + method + "/results"  
  with open(results_dir + "/results.csv", mode='r') as file:
    results = {}
    for row in csv.DictReader(file):
      subject = row['subject']
      results[subject] = {}
      results[subject]['time_ms'] = int(row['time_ms'])
      results[subject]['status'] = int(row['status'])

  for subject in results:
    print(f"\t{subject}", end = "\t", flush=True)
    result = results[subject]

    if result['status'] != 0:
      result['precision'] = 0
      result['recall'] = 0
      print("-\t-", flush=True)
      continue

    results_dir = methods_dir + "/" + method + "/results"
    with open(results_dir + "/" + subject + ".grammar") as f:
      mined_grammar = load_grammar(f)
    with open(subjects_dir + "/" + subject + ".grammar") as f:
      golden_grammar = load_grammar(f)
    
    p_val, p_inv = compare_grammars(mined_grammar, golden_grammar, FUZZ_DEPTH, FUZZ_COUNT)
    p_tot = len(p_val) + len(p_inv)
    p = len(p_val)/p_tot if p_tot > 0 else 1.0
    result['precision'] = p
    print(f"{p}", end = "\t", flush=True)

    r_val, r_inv = compare_grammars(golden_grammar, mined_grammar, FUZZ_DEPTH, FUZZ_COUNT)
    r_tot = len(r_val) + len(r_inv)
    r = len(r_val)/r_tot if r_tot > 0 else 1.0
    result['recall'] = r
    print(f"{r}", flush=True)
  
  return results

#####################################################################

def aggregate_results(results, subjects_by_category):
  results_agg = {}

  for category in subjects_by_category:
    num_subjects = len(subjects_by_category[category])
    time_ms_list = []
    precision_list = []
    recall_list = []
    num_errors = 0
    for subject in subjects_by_category[category]:
      if subject not in results:
        continue
      result = results[subject]
      time_ms_list.append(result['time_ms'])
      if result['status'] == 0:
        precision_list.append(result['precision'])
        recall_list.append(result['recall'])
      else:
        num_errors += 1
    assert len(precision_list) == len(recall_list) == len(time_ms_list)
    if len(precision_list) > 0:
      results_agg[category] = {
        'num_subjects': num_subjects,
        'num_errors': num_errors,
        'success_rate': (num_subjects - num_errors) / num_subjects,
        'precision_mean': statistics.mean(precision_list),
        'recall_mean': statistics.mean(recall_list),
        'time_ms_mean': statistics.mean(time_ms_list),
        'time_ms_stdev': statistics.stdev(time_ms_list)
      }

  return results_agg

#####################################################################

print(f"Compute all results for {methods} ...", flush=True)

results = {}
for method in methods:
  results[method] = compute_results(method)

os.makedirs(os.path.dirname(output_file), exist_ok=True)
with open(output_file, 'w') as file:
  writer = csv.writer(file)
  writer.writerow([
    "method",
    "subject",
    "time_ms",
    "status",
    "precision",
    "recall"
  ])
  for method in results:
    for subject in results[method]:
      writer.writerow([
        method, 
        subject, 
        results[method][subject]['time_ms'],
        results[method][subject]['status'],
        results[method][subject]['precision'],
        results[method][subject]['recall']
      ])

print(f"All results written to {output_file}", flush=True)

# results = {}
# with open(output_file, 'r') as file:
#   for row in csv.DictReader(file):
#     if not row['method'] in results:
#       results[row['method']] = {}
#     results[row['method']][row['subject']] = {}
#     results[row['method']][row['subject']]['time_ms'] = int(row['time_ms'])
#     results[row['method']][row['subject']]['status'] = int(row['status'])
#     results[row['method']][row['subject']]['precision'] = float(row['precision'])
#     results[row['method']][row['subject']]['recall'] = float(row['recall'])

#####################################################################

print("Aggregating results by subject category ...", flush=True)

subjects_by_category = defaultdict(list)
with open(subjects_dir + "/categories.csv", mode='r') as file:
  for row in csv.DictReader(file):
    subjects_by_category[row['category']].append(row['subject'])

subjects_by_category = dict(subjects_by_category)

results_agg = {}
for method in methods:
  results_agg[method] = aggregate_results(results[method], subjects_by_category)

os.makedirs(os.path.dirname(output_agg_file), exist_ok=True)
with open(output_agg_file, 'w') as file:
  writer = csv.writer(file)
  writer.writerow([
    "method",
    "category",
    "num_subjects",
    "num_errors",
    "success_rate",
    "precision_mean",
    "recall_mean",
    "time_ms_mean",
    "time_ms_stdev"
  ])
  for method in results_agg:
    for category in results_agg[method]:
      result = results_agg[method][category]
      writer.writerow([
        method,
        category,
        result['num_subjects'],
        result['num_errors'],
        result['success_rate'],
        result['precision_mean'],
        result['recall_mean'],
        result['time_ms_mean'],
        result['time_ms_stdev']
      ])

print(f"All aggregated results written to {output_agg_file}", flush=True)
