chapter "SpecCheck"

session Spec_Check2 in src = "Pure" +

description
\<open>SpecCheck is a specification-based testing environment for ML programs. It is based on QCheck
(\<^url>\<open>https://github.com/league/qcheck/\<close>) by Christopher League (\<^url>\<open>https://contrapunctus.net/\<close>). It
got adapted and extended to fit into the Isabelle/ML framework and resemble the very successful
QuickCheck (\<^url>\<open>https://en.wikipedia.org/wiki/QuickCheck\<close>) more closely.\<close>
directories
  generators
  output_styles
  "show"
  shrink

theories
  Generators
  Output_Style
  Show
  Spec_Check

session Spec_Check_Dynamic in "src/dynamic" = "Spec_Check2" +
description
\<open>Dynamical generation of generators and show functions for SpecCheck tests specified in string
formats.\<close>
theories
  Dynamic

session Spec_Check_Examples in "src/examples" = "Spec_Check_Dynamic" +
theories
  Examples
