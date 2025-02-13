# Evaluation of TTT on the Panini benchmark dataset

This is a guide on how to evaluate the TTT algorithm (Isberner et al. 2014) on the Panini benchmark dataset. We have implemented TTT using the LearnLib Java framework for automata learning (<https://learnlib.de>).

The positive sample inputs required by the learning algorithm are derived from our ground truth grammars (just like in the Mimid evaluation). By default, we use 10.000 samples per subject (this number can be changed in the `run_eval.sh` script). This simulates a best-case scenario; in a real-world setting, sample inputs would have to first be mined via some other means, e.g., parser directed fuzzing.

## 1. Compiler Docker image and run evaluation

```shell
docker build -t ttt .
docker run -v $(realpath ../../):/benchmark ttt
```

This will take about 30 minutes to an hour. The inferred grammars can be found in the `results` directory, which will also contain a `results.csv` with the collected run times and exit codes.

## References

* Malte Isberner, Falk Howar, and Bernhard Steffen. 2014. The TTT Algorithm: A Redundancy-Free Approach to Active Automata Learning. RV 2014. <https://doi.org/10.1007/978-3-319-11164-3_26>
