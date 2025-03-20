# Evaluation of TreeVada on the Panini benchmark dataset

This is a step-by-step guide on how to evaluate the TreeVada grammar inference tool (Arefin et al., 2024) on the Panini benchmark dataset. The TreeVada artifact runs inside a Docker container (<https://www.docker.com>).

## 1. Prepare subjects

We need to move the test subjects into the right location and make sure they are executable standalone. We also need to generate the training set of input samples required by TreeVada for each test subject.

```shell
./prepare_subjects.sh
```

## 2. Pull and run Docker container

```shell
docker pull marefin/treevada:v2
docker run --rm -it --platform linux/amd64 -v $(pwd):/panini marefin/treeva
da:v2
```

From here on out, all commands are run inside the Docker container.

## 3. Run evaluation

```console
[root@77095aca9b64 treevada]# /panini/run_eval.sh
```

This will take about 2-3 hours.

The mined grammars will now be in the `results` directory, which will also contain a `results.csv` with the collected run times and exit codes.

## References

* Arefin, Mohammad Rifat, Suraj Shetiya, Zili Wang, and Christoph Csallner. 2024. Fast Deterministic Black-box Context-free Grammar Inference. ICSE 2024. <https://doi.org/10.1145/3597503.3639214>
