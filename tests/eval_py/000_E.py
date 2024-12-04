import sys

def f000(s: str):
  assert len(s) == 0

if __name__ == '__main__':
    print(f000(sys.argv[1]))
