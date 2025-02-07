# Evaluation of Panini on the Panini benchmark dataset

```shell
./run_eval.sh
```

| File                  | Description
|-----------------------|-------------
| `results/*.out`       | raw Panini output for subject
| `results/*.regex`     | regex extracted from Panini output for subject
| `results/*.grammar`   | Fuzzing Book grammar for subject, converted with `regex` tool
| `results/results.csv` | CSV with `subject`, `time_seconds`, and `status` (exit code)
