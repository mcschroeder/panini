# Evaluation of Panini on the Panini benchmark dataset

## 1. Build Docker container for Panini

First, if you have not already done so, you need to build the Docker container for Panini.

```shell
docker build -f ../../../../Dockerfile -t panini ../../../../
```

## 2. Run evaluation inside Panini Docker container

Now drop into a shell inside the Docker container and run the evaluation script:

```console
$ docker run -it --entrypoint /bin/bash -v $(realpath ../../):/benchmark panini
root@35e756ca1de1:/panini# /benchmark/methods/panini/run_eval.sh
```

This should take 5-10 minutes. Afterwards, this directory (outside the Docker container) will contain a `results` folder with all the inferred grammars and other information.

| File                  | Description
|-----------------------|-------------
| `results/*.out`       | raw Panini output for subject
| `results/*.regex`     | regex extracted from Panini output for subject
| `results/*.grammar`   | Fuzzing Book grammar for subject, converted with `regex` tool
| `results/results.csv` | CSV with `subject`, `time_ms`, and `status` (exit code)
