(*  Title:      Tools/Spec_Check/random.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

A Lehmer random number generator:
https://en.wikipedia.org/wiki/Lehmer_random_number_generator
We use int to avoid any float imprecision problems (and the seed is an int anyway).
The parameters "a" and "m" are selected according to the recommendation in above article;
they are an an improved version of the so-called "minimal standard" (MINSTD) generator.

This file only contains those functions that rely on the internal integer representation of rand.
*)

signature RANDOM =
sig
type rand
(* creates a new random seed *)
val new : unit -> rand
(* creates a new random seed from a given one *)
val next : rand -> rand
(* Use this function for reproducible randomness; inputs \<le> 0 are mapped to 1 *)
val deterministic_seed : int -> rand

(* range_int (x,y) r returns a value in [x;y]; both x and y are inclusive! *)
val range_int : int * int -> rand -> int * rand
(* range_real (x,y) r returns a value in [x;y); that is, y will never be returned! *)
val range_real : real * real -> rand -> real * rand
(* real r returns a real in [0;1); theoretically, with 2^31 equidistant discrete values *)
val real : rand -> real * rand
(* split a seed into two new independent seeds *)
val split : rand -> rand * rand
end

structure Random : RANDOM  =
struct

type rand = int

val a = 48271
val m = 2147483647 (* 2^31 - 1 *)

fun next seed = (seed * a) mod m

(* TODO: Time is not sufficiently random when polled rapidly! *)
fun new () =
  Time.now ()
  |> Time.toMicroseconds
  |> (fn x => Int.max (1, x mod m)) (* The seed must be within [1;m) *)
  |> next (* Not strictly necessary *)

fun deterministic_seed r = Int.max (1, r mod m)

fun range_int (min, max) r =
  if min > max
  then Runtime.exn_trace (raise Fail ("Range_Int: " ^ @{make_string} min ^ " > " ^ @{make_string} max))
  else (min + ((max - min) * r) div m, next r)

(* TODO: In theory, the current implementation could return two seeds directly adjacent in the
sequence of the pseudorandom number generator. Practically, however, it should be good enough. *)
fun split r =
  let
    val r0 = next r
    val r1 = r - r0
  in
    (next r0, next r1)
  end

fun real r = ((Real.fromInt r) / (Real.fromInt m), next r)

fun range_real (min, max) r =
  if min > max
  then raise Fail ("Range_Real: " ^ @{make_string} min ^ " > " ^ @{make_string} max)
  else real r |>> (fn x => min + x * (max - min))

end
