# Computing precision and recall of inferred grammars

Our procedure is closely modeled after the precision and recall calculations performed by the Mimid and Stalagmite artifacts: to compute precision, we generate up to 1.000 inputs from the inferred grammar and check how many the golden grammar accepts; to compute recall, we generate up to 1.000 inputs from the golden grammar and check how many the inferred grammar accepts.

Run the following commands to compute precision and recall for the inferred grammars of each subject by each method, as well as some aggregate statistics:

```shell
docker build -t grammar-accuracy .
docker run -v $(realpath ../):/benchmark grammar-accuracy
```

This will take about 15-30 minutes. The results will appear in the `results` directory.
