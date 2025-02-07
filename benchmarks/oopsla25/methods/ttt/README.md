# Evaluation of TTT on the Panini benchmark dataset

This is a guide on how to evaluate the TTT algorithm (Isberner et al. 2014) on the Panini benchmark dataset. We have implemented TTT using the LearnLib Java framework for automata learning (<https://learnlib.de>).

The positive sample inputs required by the learning algorithm are derived from our ground truth grammars (just like in the Mimid evaluation), which should give an indication of performance under optimal conditions. By default, we use 10.000 samples per subject (this number can be changed in the `run_eval.sh` script).

## 1. Compile Java test runner

This benchmark requires Maven and JDK 13.

```shell
mvn package
```

## 2. Run evaluation

```shell
./run_eval.sh
```

This will take about 1-2 hours. The inferred grammars can be found in the `results` directory, which will also contain a `results.csv` with the collected run times and exit codes.

Note that TTT requires known positive input samples from which to infer a grammar. Our implementation generates up to 10.000 sample inputs for each subject from its regex golden grammar. This simulates a best-case scenario; in a real-world setting, sample inputs would have to first be mined via some other means, e.g., parser directed fuzzing.

## References

* Malte Isberner, Falk Howar, and Bernhard Steffen. 2014. The TTT Algorithm: A Redundancy-Free Approach to Active Automata Learning. RV 2014. <https://doi.org/10.1007/978-3-319-11164-3_26>
