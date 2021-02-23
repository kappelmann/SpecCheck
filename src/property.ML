(*  Title:      Tools/Spec_Check/property.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

The base module of testable properties.
A property is a the type of values that SpecCheck knows how to test.
Properties not only test whether a given predicate holds, but, for example, can also track argument
distributions and runtimes and have preconditions.
*)

signature PROPERTY =
sig

  type 'a pred = 'a -> bool
  (* the type of values testable by SpecCheck *)
  type 'a prop
  (* transforms a predicate into a testable property *)
  val property : 'a pred -> 'a prop
  (* implication for properties: if the first argument evaluates to false, the test case is discarded *)
  val implies : 'a pred -> 'a prop -> 'a prop
  (* convenient notation for `implies` working on predicates *)
  val ==> : 'a pred * 'a pred -> 'a prop
  
  datatype result = Result of bool | Discard | Exception of exn
  
  type stats = { count : int, generated : bool, timing : Timing.timing }
  (* runs a property for a given test case and returns the result and the updated the statistics *)
  val test : 'a prop -> 'a * stats -> result * stats
  (* returns an empty stats object for generated or fixed tests *)
  val empty_stats : bool -> stats

end

structure Property : PROPERTY =
struct

datatype result = Result of bool | Discard | Exception of exn

type stats = { count : int, generated : bool, timing : Timing.timing }

fun empty_stats gen = {
    count = 0,
    generated = gen,
    timing = {
      cpu = Time.zeroTime,
      elapsed = Time.zeroTime,
      gc = Time.zeroTime
    }
  }

type 'a pred = 'a -> bool
type 'a prop = 'a -> result

fun apply f x =
  Result (f x)
  (* Testcode may throw arbitrary exceptions. Interrupts MUST not be catched! *)
  handle x => if Exn.is_interrupt x then Exn.reraise x else Exception x

fun property f x = apply f x

fun implies cond prop x =
  if cond x
  then prop x
  else Discard

fun ==> (p1, p2) = implies p1 (property p2)

fun add_timings {elapsed = elapsed1, cpu = cpu1, gc = gc1} {elapsed = elapsed2, cpu = cpu2, gc = gc2} =
  { elapsed = elapsed1 + elapsed2,
    cpu = cpu1 + cpu2,
    gc = gc1 + gc2}

fun test prop (input, {count, generated, timing}) =
  let
    val (time, result) = Timing.timing (fn () => prop input) ()
    val stats = {count = count + 1, generated = generated, timing = add_timings timing time}
  in (result, stats) end

end
