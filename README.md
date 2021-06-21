# SpecCheck

SpecCheck is a [QuickCheck](https://en.wikipedia.org/wiki/QuickCheck)-like testing framework for Isabelle/ML.
You can use it to write input generators for and properties about ML functions.
It helps you to identify bugs by printing counterexamples on failure.

## Quick Usage
1. Import `Spec_Check.Spec_Check` into your environment.
2. Write specifications using the ML invocation: `check show gen name prop ctxt seed` where
  * `show` converts values into `Pretty.T` types to show the failing inputs. See `src/show`.
  * `gen` is the value generator used for the test. See `src/generators`.
  * `name` is the shown name of the test
  * `prop` is the property to be tested. See `property.ML`
  * `seed` is the initial seed for the generator.

You can also choose to omit the show method for rapid testing or add a shrinking method a la
QuickCheck to get better counterexamples. See `src/spec_check.ML`.

A deprecated alternative allows you to specify tests using strings:
1. Import `Spec_Check_Dynamic.Dynamic` into your environment.
2. `check_property "ALL x. P x"` where `P x` is some ML code evaluating to a boolean

Examples can be found in `examples/`.

## Notes

SpecCheck is based on [QCheck](https://github.com/league/qcheck), a testing framework for Standard ML by
[Christopher League](https://contrapunctus.net/league/).
As Isabelle/ML provides a rich and uniform ML platform, some features where removed or adapted, in particular:

1. Isabelle/ML provides common data structures, which we can use in the
tool's implementation for storing data and printing output.

2. Implementations in Isabelle/ML checked with this tool commonly use Isabelle/ML's `int` type
(which corresponds ML's `IntInf.int`), but do not use other integer types in ML such as ML's `Int.int`,
`Word.word`, and others.

3. As Isabelle makes heavy use of parallelism, we avoid reference types.

## Next Steps

* Implement sizing methods (cf. QuickCheck's `sized`)
* Implement shrinking methods for commonly used types

## License

The source code originated from Christopher League's QCheck, which is
licensed under the 2-clause BSD license. The current source code is
licensed under the compatible 3-clause BSD license of Isabelle.

## Authors

* Lukas Bulwahn
* Nicolai Schaffroth
* Sebastian Willenbrink
* Kevin Kappelmann
