# OOPSLA 2025 Evaluation

This directory contains all materials necessary to reproduce the comparative evaluation of grammar inference approaches from our OOPSLA'25 paper.

| Location         | Description
|------------------|-------------
| `accuracy/`      | Scripts to compute precision and recall (see __Evaluation__ step 2).
| `methods/`       | Methods under evaluation (see __Evaluation__ step 1).
| `subjects/`      | Ad hoc parser subjects (see __Subjects__).
| `README.md`      | This document.

## Evaluation

1) Go into the subdirectory of each approach (`methods/panini/`, `methods/mimid/`, `methods/stalagmite/`, `methods/ttt/`) and follow the instructions in the respective `README.md`. You should end up with a `methods/*/results/` folder for each method, containing the inferred grammars of all subjects and additional information about each inference attempt. This might altogether take about 5-6 hours.

    > The `methods/*/results_paper/` folders contain archived versions of the inferred grammars from our evaluation run. The results in our paper are based on these.

2) Go into the `accuracy/` subdirectory and follow the instructions in `README.md` to compute precision and recall of all grammars in all `methods/*/results/` folders. This will produce an `accuracy/results/` folder containing the final results per individual subject and method as well as aggregated by subject category. This will take about 15-30 minutes.

    > The `accuracy/results_paper/` folder contains archived versions of the final results as they appear in our paper.

## Subjects

The `subjects` folder contains the 202 ad hoc parsers for regular languages on which we evaluate the different inference approaches. Each parser was originally written in Python and then manually translated to C. Each parser is compilable/executable stand-alone, reading input from `stdin` or a file, and returning exit code 0 on success.

The `subjects` folder also contains *ground truth* in the form of golden grammars. These describe the actual input language of each parser (i.e., all and only those inputs that the parser accepts without error). They are needed to compute precision and recall and to derive sample inputs required by some of the methods (Mimid and TTT). The golden grammars are given as both a POSIX regular expression and in the format used by *The Fuzzing Book* (Zeller et al. 2024), the latter converted from the former using the `regex` tool from our `regex-algebra` package.

To facilitate a structured analysis, we classified the parsers within our benchmark dataset into three overarching categories based on their features, which can be found in `categories.csv`. See the paper for more details on this point.

| File             | Description
|------------------|-------------
| `*.py`           | Python source of ad hoc parser.
| `*.c`            | C source of ad hoc parser.
| `*.regex`        | The parser's true input grammar as a POSIX regular expression.
| `*.grammar`      | The parser's true input grammar in Fuzzing Book format.
| `categories.csv` | Classification of parsers based on structural features.

## References

* Andreas Zeller, Rahul Gopinath, Marcel Böhme, Gordon Fraser, and Christian Holler. 2024. The Fuzzing Book. <https://www.fuzzingbook.org>
