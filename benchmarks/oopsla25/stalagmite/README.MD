# Evaluation of Stalagmite on the Panini benchmark dataset

This is a step-by-step guide on how to evaluate the Stalagmite grammar mining system on the Panini benchmark dataset.

References:

* Leon Bettscheider and Andreas Zeller. 2024. Look Ma, No Input Samples! Mining Input Grammars from Code with Symbolic Parsing. FSE 2024. https://doi.org/10.1145/3663529.3663790

* Andreas Zeller, Rahul Gopinath, Marcel Böhme, Gordon Fraser, and Christian Holler. 2024. The Fuzzing Book. https://www.fuzzingbook.org

## 1. Download and unpack Stalagmite artifact

Download the artifact from https://doi.org/10.5281/zenodo.11186613 and unpack it into a subdirectory named `stalagmite`.

```shell
curl -o stalagmite.zip https://zenodo.org/api/records/11186613/files/stalagmite.zip/content
unzip stalagmite.zip -d .
```

## 2. Prepare evaluation subjects for Stalagmite

Stalagmite requires elaborate scaffolding in order to be able to infer the grammar of a program. The `prepare_subjects.sh` script copies the C subjects from the Panini evaluation dataset into the `stalagmite/subjects` folder and prepares the scaffolding around them using the stuff in the `template` folder. It also sets up a custom `run_eval.sh` script for running the Stalagmite evaluation on our subjects.

```shell
./prepare_subjects.sh
```

## 3. Prepare golden grammars

The ground truth of the Panini evaluation dataset is in the form of regular expressions. Stalagmite's recall computation requires a golden grammar in Fuzzing Book format. The `prepare_grammars.sh` script uses the `regex` tool from our `regex-algebra` package to do the conversion and copies the grammars into the right location.

```shell
./prepare_grammars.sh
```

## 4. Patch evaluation scripts

This patch

  1. ensures that we can calculate precision and recall of grammars with <1000 productions;
  2. changes the Dockerfile to run our own `run_eval.sh` script when starting the container.

You can apply it by running the following command:

```shell
patch -d stalagmite -p1 < patch_container.diff 
```

## 5. Run evaluation

The evaluation is run inside a Docker container using the following commands:

```shell
cd stalagmite
sudo chmod -R 777 .
docker build . -t symbolic-grammar-mining
mkdir output
docker run -it -v $(pwd)/output:/staminag/data symbolic-grammar-mining
```

After everything is done (about TBD hours), the results can be found in the `stalagmite/output` folder. To produce a single `results.csv`, run the following command:

```shell
cd ..
./collate_results.sh
```
