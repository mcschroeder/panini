# Panini

Grammar Inference for Ad Hoc Parsers

## Installation / Development

* Panini is written in Haskell. You need __GHC 9.6.2__ and __cabal 3.10.1.0__ to
  compile it. The recommended way to set up a Haskell environment is with
  [GHCup](https://www.haskell.org/ghcup/).

* To run Panini, you also need the [Z3 SMT solver](https://github.com/Z3Prover/z3)
  somewhere on your `PATH`. If you are on a Mac with [Homebrew](https://brew.sh),
  you can `brew install z3`.

* To run Panini directly from source, simply use `cabal run panini -- <options>`,
  where `<options>` are any command-line options you want to pass on to the
  `panini` executable.

* To start an interactive development session in GHCi, use `cabal repl`. To run
  Panini from within the current GHCi session, you first need to load the main
  executable module with `:load app/Main.hs`. You can then use `:main` as if
  invoking the `panini` executable (e.g., `:main --help`).

* To build a standalone `panini` executable, run `cabal build exe:panini`. This
  will put the binary somewhere in the `dist-newstyle` directory. You can use
  `cabal exec which panini` to find out exactly where and copy the `panini`
  binary to somewhere on your `PATH`.

  (Alternatively, you could run `cabal install exe:panini` to build and install
  the `panini` executable in one step, but this is currently not recommended due
  to arcane technical issues related to out-of-place builds.)

## Usage

* `panini <FILE>` will run type and grammar inference on `<FILE>` and report all
  inferred type signatures:

  ```console
  > panini tests/T1.in
  f : {s:𝕊 | s ∈ (a | (Σ∖a)bΣ*)} → 𝟙
  ```

* If you run `panini` without any arguments on an interactive terminal, it
  launches into the Panini REPL. Use `:help` to display a list of available
  commands.

  ```console
  > panini
  Panini> 
  ```

* `panini --test` runs Panini in test mode: by default, all files matching
  `tests/*.pan` will be processed and their outputs compared against the
  corresponding `tests/*.out` or `tests/*.err` files.

  ```console
  > panini --test
  tests/T1.pan ... OK
  tests/T2.pan ... OK
  tests/T3.pan ... OK
  ...
  ```

* The `--trace` flag, available in all modes, outputs (very) detailed internal
  diagnostics and debugging information to `stderr`. The `--trace-file` flag
  writes the same information quietly to a log file.

* For further information and additional options, consult `panini --help`.

## The λΣ Language

* Eventually, Panini will be able to analyze Python source code directly. For
  now, it expects all input to be in a domain-specific intermediate
  representation for ad hoc parsers, built around a core calculus called λΣ.
  
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

| Unicode | ASCII    | LaTex     | Description
|---------|----------|-----------|-------------
| `λ`     | `\`      | `\lambda` | lambda abstraction
| `→`     | `->`     | `\to`     | function arrow
| `𝟙`     | `unit`   | `\bb1`    | unit type
| `𝔹`     | `bool`   | `\bbB`    | Boolean type
| `ℤ`     | `int`    | `\bbZ`    | integer type
| `ℕ`     | `nat`    | `\bbN`    | natural number type (unused)
| `𝕊`     | `string` | `\bbS`    | string type
| `∧`     | `/\`     | `\wedge`  | conjunction
| `∨`     | `\/`     | `\vee`    | disjunction
| `¬`     | `~`      | `\neg`    | negation
| `≠`     | `/=`     | `\ne`     | inequality
| `≤`     | `<=`     | `\le`     | less than or equal
| `≥`     | `>=`     | `\ge`     | greater than or equal
| `⇒` or `⟹` | `==>` | `\Rightarrow` or `\implies` | implication
| `⇔` or `⟺` | `<=>` | `\Leftrightarrow` or `\iff` | iff


Panini also has a `--no-unicode` flag which outputs only ASCII (but still permits Unicode input).
