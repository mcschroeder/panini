# Computing precision and recall of inferred grammars

To compute precision, the inferred grammar is used to generate up to 1.000 inputs which are fed into a parser for the golden grammar.

To compute recall, the golden grammar is used to generate up to 1.000 inputs which are fed into a parser for the inferred grammar. The inputs are generated 

To ensure a fair comparison, the same inputs are used for all

```
docker build -t grammar-accuracy .
docker run -v $(realpath ../..):/benchmark grammar-accuracy
```
