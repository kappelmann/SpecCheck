# SpecCheck

SpecCheck is a [QuickCheck](https://en.wikipedia.org/wiki/QuickCheck)-like testing framework for Isabelle/ML.
You can use it to write input generators for and properties about ML functions.
It helps you to identify bugs code by printing counterexamples on failure.

## Authors
- Lukas Bulwahn
- Nicolai Schaffroth
- Sebastian Willenbrink

## Quick Usage
1. Import `Spec_Check.thy` into your environment
2. Write specifications using the ML invocation: `checkGen ctxt (gen, printer) (name, prop)` where
  * `ctxt` is the desired context
  * `gen` is the random value generator used for the test. See `generator.ML` for some sample generators.
  * `printer` converts values into strings to show the failing inputs
  * `name` is the shown name of the test
  * `prop` is the property to be tested. See property.ML

More complicated testing examples can be found, for example, in `tester.ML` and `test.thy`
[here](https://gitlab.lrz.de/ga85wir/bachelorthesis/).

A deprecated alternative allows you to specify tests using strings:
- `check_property "ALL x. P x"` where `P x` is some ML code evaluating to a boolean

Examples can be found in `Examples.thy`.
**Note:** this mechanism might be removed anytime soon.
      
## Dependency Graph      
![Dependency Graph](./dependencies.svg)

Update graphic with: "dot -Tsvg dependencies.dot -o dependencies.svg"

## Notes

SpecCheck is heavily based on the [QCheck](https://github.com/league/qcheck), a testing framework for Standard ML by
[Christopher League](https://contrapunctus.net/league/).
As Isabelle/ML provides a rich and uniform ML platform, some features where removed or adapted, in particular:

1. Isabelle/ML provides common data structures, which we can use in the
tool's implementation for storing data and printing output.

2. Implementations in Isabelle/ML checked with this tool commonly use Isabelle/ML's `int` type
(which corresponds ML's `IntInf.int`), but do not use other integer types in ML such as ML's `Int.int`,
`Word.word`, and others.

3. As Isabelle makes heavy use of parallelism, we avoid reference types.

## Next Steps
  - Remove all references and store the neccessary random seed in the
    Isabelle's context.
  - Simplify some existing random generators.
    The original ones from Christopher League are so complicated to
    support many integer types uniformly.

## License

  The source code originated from Christopher League's QCheck, which is
  licensed under the 2-clause BSD license. The current source code is
  licensed under the compatible 3-clause BSD license of Isabelle.

