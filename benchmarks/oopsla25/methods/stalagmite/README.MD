# Evaluation of Stalagmite on the Panini benchmark dataset

This is a step-by-step guide on how to evaluate the Stalagmite grammar mining system (Bettscheider and Zeller 2024) on the Panini benchmark dataset. The Stalagmite artifact runs inside a Docker container (<https://www.docker.com>).

## 1. Download and unpack Stalagmite

```shell
git clone https://github.com/leonbett/stalagmite stalagmite.tmp
cd stalagmite.tmp && git checkout b7d97faa43f093e9c2c12569348a4b30b6131a9c && cd ..
```

## 2. Prepare evaluation subjects for Stalagmite

Stalagmite requires elaborate scaffolding in order to be able to infer the grammar of a program. The `prepare_subjects.sh` script copies the C subjects from the benchmark dataset into the `stalagmite/subjects` folder and prepares the scaffolding around them using the stuff from the `template` folder. It also sets up a the `run_eval.sh` script for running the Stalagmite evaluation on our subjects inside the Docker container.

```shell
./prepare_subjects.sh
```

## 3. Patch Stalagmite

This patch ensures that Stalagmite's grammar refinement step does not run into an endless loop if a fuzz input crashes the program (i.e., we simply count crash-causing inputs as invalid inputs). It also changes the Dockerfile to run our custom `run_eval.sh` script when starting the container.

You can apply it by running the following command:

```shell
patch -d stalagmite.tmp -p1 < patch.diff
```

## 4. Run evaluation and collect results

The evaluation is run inside a Docker container using the following commands:

```shell
cd stalagmite.tmp
sudo chmod -R 777 .
git rev-parse HEAD > klee-examples.commit
docker build . -t symbolic-grammar-mining --platform linux/amd64
mkdir output
docker run -it -v $(pwd)/output:/stalagmite/data --platform linux/amd64 symbolic-grammar-mining
```

This will take about 3-4 hours. To collect all the results in one place, continue with the following commands:

```shell
cd ..
./collect_results.sh
```

The mined grammars will now be in the `results` directory, which will also contain a `results.csv` with the collected run times and exit codes.

## References

* Leon Bettscheider and Andreas Zeller. 2024. Look Ma, No Input Samples! Mining Input Grammars from Code with Symbolic Parsing. FSE 2024. <https://doi.org/10.1145/3663529.3663790>
