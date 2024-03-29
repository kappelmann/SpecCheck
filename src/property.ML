(*  Title:      Tools/Spec_Check/property.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

The base module of testable properties.
A property is the type of values that SpecCheck knows how to test.
Properties not only test whether a given predicate holds, but, for example, can also have
preconditions.
*)

signature SPEC_CHECK_PROPERTY =
sig

  type 'a pred = 'a -> bool
  (*the type of values testable by SpecCheck*)
  type 'a prop
  (*transforms a predicate into a testable property*)
  val prop : 'a pred -> 'a prop
  (*implication for properties: if the first argument evaluates to false, the test case is
    discarded*)
  val implies : 'a pred -> 'a prop -> 'a prop
  (*convenient notation for `implies` working on predicates*)
  val ==> : 'a pred * 'a pred -> 'a prop

end

structure Spec_Check_Property : SPEC_CHECK_PROPERTY =
struct

type 'a pred = 'a -> bool
type 'a prop = 'a -> Spec_Check_Base.result_single

fun apply f x = Spec_Check_Base.Result (f x)
  (*testcode may throw arbitrary exceptions; interrupts must not be caught!*)
  handle exn => if Exn.is_interrupt exn then Exn.reraise exn else Spec_Check_Base.Exception exn

fun prop f x = apply f x

fun implies cond prop x =
  if cond x
  then prop x
  else Spec_Check_Base.Discard

fun ==> (p1, p2) = implies p1 (prop p2)

end
