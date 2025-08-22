# Panini

Static Grammar Inference for Ad Hoc Parsers

## Installation / Development

* Panini is written in Haskell. You need __GHC 9.6.6__ and __cabal 3.12.1.0__ to
  compile it. The recommended way to set up a Haskell environment is with
  [GHCup](https://www.haskell.org/ghcup/).

* To run Panini, you also need the [Z3 SMT solver](https://github.com/Z3Prover/z3)
  somewhere on your `PATH`. If you are on a Mac with [Homebrew](https://brew.sh),
  you can `brew install z3`.

* To run Panini directly from source, simply use `cabal run panini -- <options>`,
  where `<options>` are any command-line options you want to pass on to the
  `panini` executable.

* To start an interactive development session in GHCi, use `cabal repl`. You can
  then use `:main` as if invoking the `panini` executable (e.g., `:main --help`).

* To build a standalone `panini` executable, run `cabal build panini`. This will
  put the binary somewhere in the `dist-newstyle` directory. To find out where,
  use `cabal exec which panini`.

* To build and install `panini` on your `PATH`, run

  ```shell
  cabal clean
  export GIT_ROOT=$(pwd); cabal install panini
  ```

* You can also build and run Panini using [Docker](https://docker.com).

  ```shell
  docker build -t panini .
  docker run panini --version
  ```  

## Usage

* `panini <FILE>` will run type and grammar inference on `<FILE>` and report all
  inferred type signatures:

  ```console
  > panini benchmarks/oopsla25/subjects/980_parser.py
  parser : {s:𝕊 | s ∈ a|[^a]b.*} → 𝟙
  ```

* If you run `panini` without any arguments on an interactive terminal, it
  launches into the Panini REPL. Use `:help` to display a list of available
  commands.

  ```console
  > panini
  Panini 0.1 (bd4bf70)
  Type :help for more information.
  Panini> 
  ```

* The `--trace` flag, available in all modes, outputs (very) detailed internal
  diagnostics and debugging information to `stderr`. The `--trace-file` flag
  writes the same information quietly to a log file.

* For further information and additional options, consult `panini --help`.

## The Python frontend

* Panini includes an *experimental* Python frontend and is able to directly
  analyze a subset of Python source code. You can inspect the Python-to-λΣ
  transpilation using `--trace` or provide the `--save-pan` flag to create a
  standalone `.pan` file from the Python source.

## The λΣ Language

* λΣ is a simple λ-calculus in A-normal form with a refinement type system in
  the style of *Liquid Types*. For an extensive description of the syntax and
  semantics of the core calculus, please refer to the paper.

* Panini programs are made up of three kinds of statements:

  * An *assumption* `x : t` assigns the λΣ type `t` to the name `x`. Assumptions
    are taken as-is; they are the axioms of the system. Apart from possibly
    being syntactically incorrect, an assumption can never fail.

      ```panini
      length : (s:𝕊) → { n:ℤ | n ≥ 0 ∧ n = |s| }
      ```

  * A *definition* `x = e` defines `x` to be the λΣ term `e` and causes Panini
    to try to infer the most precise λΣ type for `e`.

    If the defined name was previously assumed to have a certain type `t₀`, then
    Panini checks if the inferred type `t₁` of the new definition is a subtype
    of the previously assumed type, `t₀ <: t₁`. The inferred type for `x`
    replaces any previously assumed type in the global context.

      ```panini
      f : {v:ℤ | ?} → 𝟙
      f = λx:ℤ.
        let p = equals x 1 in
        assert p
      ```

  * An *import statement* `import m` loads the module `m` which brings all of
    its assumptions and definitions into scope. As of now, there is only a
    single global namespace.
  
    Also as of now, Panini modules are simply files: if the current working
    directory is `foo`, then `import base` loads the file `foo/base`.

    ```panini
    import base
    import ./python/3.11.4
    ```

* For examples of Panini programs, look at the `tests` directory.

### Symbols

Panini fully supports and makes heavy use of Unicode. If you're using VS Code,
the [Unicode LaTeX Input](https://marketplace.visualstudio.com/items?itemName=gao-shuhua.vsc-unicode-latex)
extension is quite handy to easily input Unicode symbols.

Alternatively, you can use the following ASCII equivalents for common symbols:

| Unicode | ASCII    | LaTeX     | Description
|---------|----------|-----------|-------------
| `λ`     | `\`      | `\lambda` | lambda abstraction
| `→`     | `->`     | `\to`     | function arrow
| `𝟙`     | `unit`   | `\bb1`    | unit type
| `𝔹`     | `bool`   | `\bbB`    | Boolean type
| `ℤ`     | `int`    | `\bbZ`    | integer type
| `ℕ`     | `nat`    | `\bbN`    | natural number type (unused)
| `𝕊`     | `string` | `\bbS`    | string type
| `ℂ𝕙`    | `char`   | `\bbC\bbh` | character type
| `∧`     | `/\`     | `\wedge`  | conjunction
| `∨`     | `\/`     | `\vee`    | disjunction
| `¬`     | `~`      | `\neg`    | negation
| `≠`     | `/=`     | `\ne`     | inequality
| `≤`     | `<=`     | `\le`     | less than or equal
| `≥`     | `>=`     | `\ge`     | greater than or equal
| `⇒` or `⟹` | `==>` | `\Rightarrow` or `\implies` | implication
| `⇔` or `⟺` | `<=>` | `\Leftrightarrow` or `\iff` | iff
| `∈`     | `\in`    | `\in`     | included in
| `∉`     | `\notin` | `\notin`  | not included in
| `∋`     | `\ni`    | `\ni`     | includes
| `∌`     | `\notni` | N/A       | does not include

Panini also has a `--no-unicode` flag which outputs only ASCII (but still permits Unicode input).

## Citation

If you want to cite Panini, please use the following reference:
```
@article{schroeder:panini,
  author = {Michael Schr\"{o}der and J\"{u}rgen Cito},
  title = {Static Inference of Regular Grammars for Ad Hoc Parsers},
  journal = {Proceedings of the ACM on Programming Languages},
  volume = {9},
  number = {OOPSLA2},  
  date = {2025-10},
  doi = {10.1145/3763054}
}
```
