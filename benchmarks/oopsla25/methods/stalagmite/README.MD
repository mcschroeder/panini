# Evaluation of Stalagmite on the Panini benchmark dataset

This is a step-by-step guide on how to evaluate the Stalagmite grammar mining system (Bettscheider and Zeller 2024) on the Panini benchmark dataset. The Stalagmite artifact runs inside a Docker container (<https://www.docker.com>).

## 1. Download and unpack Stalagmite artifact

Download the artifact from <https://doi.org/10.5281/zenodo.11186613> and unpack it into a subdirectory named `stalagmite`.

```shell
curl -o stalagmite.zip https://zenodo.org/api/records/11186613/files/stalagmite.zip/content
unzip stalagmite.zip -d .
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
patch -d stalagmite -p1 < patch.diff
```

## 4. Run evaluation and collect results

The evaluation is run inside a Docker container using the following commands:

```shell
cd stalagmite
sudo chmod -R 777 .
docker build . -t symbolic-grammar-mining
mkdir output
docker run -it -v $(pwd)/output:/staminag/data symbolic-grammar-mining
```

This will take about 2-3 hours. To collect all the results in one place, continue with the following commands:

```shell
cd ..
./collect_results.sh
```

The mined grammars will now be in the `results` directory, which will also contain a `results.csv` with the collected run times and exit codes.

## References

* Leon Bettscheider and Andreas Zeller. 2024. Look Ma, No Input Samples! Mining Input Grammars from Code with Symbolic Parsing. FSE 2024. <https://doi.org/10.1145/3663529.3663790>
