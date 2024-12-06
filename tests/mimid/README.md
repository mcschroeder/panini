# Mimid

1. In the `PymimiBook.ipynb` notebook, insert the following snippet at the end of section __2.2.2.1 Golden Grammar__:

```
cgidecode_golden_panini = {
  "<START>": [
    "<cgidecode-s>"
  ],
  "<cgidecode-s>": [
      '<cgidecode>',
      '<cgidecode><cgidecode-s>'],
  "<cgidecode>": ["<single_char>", "%<hex_digit><hex_digit>"
  ],
  "<hex_digit>": list(string.hexdigits),
  "<single_char>": [c for c in string.printable if c != '%']
}
cgidecode_golden = cgidecode_golden_panini
```

2. Run the notebook up to and including __2.2.2.3 Mimid__.

# TreeVada

1. Use the input samples from section __2.2.2.2  Samples__ of the Mimid notebook as the training set (one sample per file).

2. Run TreeVada `search.py` on `cgidecode.py` with the input samples as training set.

3. Use the TreeVada `eval.py` command to print out the unpickled grammar in the log file.

4. Manually translate the TreeVada grammar to the Mimid grammar format and use it in place of the Mimid-derived grammar to compute precision and recall in section ___2.2.2.3 Mimid__ of the Mimid notebook.
