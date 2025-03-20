# NOTE: this is intended to be run inside the TreeVada Docker container

from grammar import Grammar, Rule
import argparse
import pickle
from typing import Dict
import json

def main():
  ap = argparse.ArgumentParser()
  ap.add_argument("--input", required=True, type=str, help="pickled gramdict")
  ap.add_argument("--output", required=True, type=str, help="fuzzingbook grammar")
  args = ap.parse_args()

  grammar_dict : Dict[str, Rule] = {}
  with open(args.input, "rb") as f:
    grammar_dict = pickle.load(f)
  
  fb_dict = {}
  for key, rule in grammar_dict.items():
    fb_dict[f"<{key}>"] = [[t[1:-1] if t.startswith('"') else f"<{t}>" for t in p ] for p in rule.bodies]
  with open(args.output, "w") as f:
    json.dump(fb_dict, f)

if __name__ == '__main__':
  main()
