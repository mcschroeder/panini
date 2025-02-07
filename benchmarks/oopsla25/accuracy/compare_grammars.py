# Adapted from the precision/recall calculations of Stalagmite.

import argparse
import json
import fuzzingbook.Parser as P
from common import LimitFuzzer, tree_to_str
from collections import Counter

# what proportion of strings in the source grammar are accepted by the target grammar?
def compare_grammars(source_grammar, target_grammar, max_depth: int, max_count: int):
  source_fuzzer = LimitFuzzer(source_grammar)
  source_inputs = Counter()  
  while len(source_inputs) < max_count and all(count < max_count * len(source_inputs) for _,count in source_inputs.most_common(1)):
    inp, _ = source_fuzzer.fuzz("<start>", max_depth=max_depth)
    source_inputs[inp] += 1

  if len(source_inputs) == 0:
    return 1.0,[],[]

  target_parser = P.IterativeEarleyParser(P.non_canonical(target_grammar), start_symbol="<start>")  
  valid = []
  invalid = []
  for inp in source_inputs:
    parsed = False
    try:
      result = target_parser.parse(inp)
      for tree in result:
          s = tree_to_str(tree)
          if s == inp:
            parsed = True
            break
    except SyntaxError:
      pass
    if parsed:
      valid.append(inp)
    else:
      invalid.append(inp)
  
  assert len(valid)+len(invalid) == len(source_inputs)
  return valid, invalid

# The Fuzzing Book parser has a quirk where the start symbol of a grammar 
# must only go to a single alternative; we make sure this is always the case.
# Additionally, we ensure that the start symbol is always lowercase <start>.
def load_grammar(file):
  grammar = json.load(file)  
  assert "<start_>" not in grammar
  old_start = "<start>" if "<start>" in grammar else "<START>"
  grammar["<start_>"] = grammar[old_start]
  del grammar[old_start]
  grammar["<start>"] = [["<start_>"]]
  return grammar

def main():
  ap = argparse.ArgumentParser()
  ap.add_argument("--source", required=True, type=str, help="grammar file")
  ap.add_argument("--target", required=True, type=str, help="grammar file")
  ap.add_argument('--count', required=True, type=int, help='maximum count of inputs to be generated')
  ap.add_argument('--depth', required=True, type=int, help='maximum depth to be used by generation')
  args = ap.parse_args()

  with open(args.source) as f:
    source_grammar = load_grammar(f)
  
  with open(args.target) as f:
    target_grammar = load_grammar(f)
  
  valid, invalid = compare_grammars(source_grammar, target_grammar, args.depth, args.count)
  print(f"{len(valid)},{len(invalid)}")

if __name__ == '__main__':
  main()
