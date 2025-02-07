import csv
from collections import defaultdict
import statistics
from compare_grammars import compare_grammars, load_grammar

subjects_dir="/benchmark/subjects"
methods_dir="/benchmark/methods"
methods = ["mimid","panini","stalagmite","ttt"]

output_file = "/benchmark/accuracy/results/results_by_subject.csv"
output_agg_file = "/benchmark/accuracy/results/results_by_category.csv"

#####################################################################

FUZZ_DEPTH = 1000
FUZZ_COUNT = 1000

def compute_precision(method, subject):
  results_dir = methods_dir + "/" + method + "/results"
  mined_grammar = load_grammar(results_dir + "/" + subject + ".grammar")
  golden_grammar = load_grammar(subjects_dir + "/" + subject + ".grammar")
  valid, invalid = compare_grammars(mined_grammar, golden_grammar, FUZZ_DEPTH, FUZZ_COUNT)
  total = valid + invalid
  return valid/total if total > 0 else 0

def compute_recall(method, subject):
  results_dir = methods_dir + "/" + method + "/results"
  mined_grammar = load_grammar(results_dir + "/" + subject + ".grammar")
  golden_grammar = load_grammar(subjects_dir + "/" + subject + ".grammar")
  valid, invalid = compare_grammars(golden_grammar, mined_grammar, FUZZ_DEPTH, FUZZ_COUNT)
  total = valid + invalid
  return valid/total if total > 0 else 0

#####################################################################

print(f"Computing results ...")

results = {}

for method in methods:
  print(f"{method}")

  results_dir = methods_dir + "/" + method + "/results"
  
  with open(results_dir + "/results.csv", mode='r') as file:
    results[method] = {}
    for row in csv.DictReader(file):
      subject = row['subject']
      results[method][subject] = {}
      results[method][subject]['time_seconds'] = row['time_seconds']
      results[method][subject]['status'] = row['status']

  for subject in results[method]:
    print(f"\t{subject}", end = "\t")
    result = results[method][subject]    
    p = compute_precision(method, subject) if result['status'] == 0 else 0
    print(f"{p}", end = "\t")
    r = compute_recall(method, subject) if result['status'] == 0 else 0
    print(f"{r}")
    result['precision'] = p
    result['recall'] = r

with open(output_file, 'w') as file:
  writer = csv.writer(file)
  writer.writerow([
    "method",
    "subject",
    "time_seconds",
    "status",
    "precision",
    "recall"
  ])
  for result in results:
    for method in methods:
      for subject in results[method]:
        writer.writerow([
          method, 
          subject, 
          results[method][subject]['time_seconds'],
          results[method][subject]['status'],
          results[method][subject]['precision'],
          results[method][subject]['recall']
        ])

print(f"Written results to {output_file}")

#####################################################################

print("Aggregating results by subject category ...")

subjects_by_category = defaultdict(list)
with open(subjects_dir + "/categories.csv", mode='r') as file:
  for row in csv.DictReader(file):
    subjects_by_category[row['category']].append(row['subject'])

subjects_by_category = dict(subjects_by_category)

results_agg = {}
for method in methods:
  results_agg[method] = {}
  for category in subjects_by_category:
    num_subjects = len(subjects_by_category[category])
    time_seconds_list = []
    precision_list = []
    recall_list = []
    precision_list_without_errors = []
    recall_list_without_errors = []
    num_errors = 0
    for subject in subjects_by_category[category]:
      result = results[method][subject]
      time_seconds_list.append(result['time_seconds'])
      precision_list.append(result['precision'])
      recall_list.append(result['recall'])
      if result['status'] == 0:
        precision_list_without_errors.append(result['precision'])
        recall_list_without_errors.append(result['recall'])
      else:
        num_errors += 1
    results_agg[method][category] = {
      'num_subjects': num_subjects,
      'time_seconds_mean': statistics.mean(time_seconds_list),
      'time_seconds_stdev': statistics.stdev(time_seconds_list),
      'precision_mean': statistics.mean(precision_list),
      'recall_mean': statistics.mean(recall_list),
      'num_errors': num_errors,
      'precision_mean_without_errors': statistics.mean(precision_list),
      'recall_mean_without_errors': statistics.mean(recall_list)
    }

with open(output_agg_file, 'w') as file:
  writer = csv.writer(file)
  writer.writerow([
    "method",
    "category",
    "num_subjects",
    "time_seconds_mean",
    "time_seconds_stdev",
    "precision_mean",
    "recall_mean",
    "num_errors",
    "precision_mean_without_errors",
    "recall_mean_without_errors"
  ])
  for result in results_agg:
    for method in methods:
      for category in results_agg[method][category]:
        result = results_agg[method][category]
        writer.writerow([
          method,
          category,
          result['num_subjects'],
          result['time_seconds_mean'],
          result['time_seconds_stdev'],
          result['precision_mean'],
          result['recall_mean'],
          result['num_errors'],
          result['precision_mean_without_errors'],
          result['recall_mean_without_errors']
        ])

print(f"Written aggregated results to {output_agg_file}")
