(*  Title:      Tools/Spec_Check/property.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Conditional properties that can track argument distribution.
*)

signature PROPERTY =
sig

type 'a pred = 'a -> bool
type 'a prop
val pred : 'a pred -> 'a prop
val pred2 : ('a * 'b) pred -> 'b -> 'a prop
val implies : 'a pred * 'a prop -> 'a prop
val ==> : 'a pred * 'a pred -> 'a prop

(*Results*)
datatype result = Result of bool | Discard | Exception of exn

type stats = { count : int, generated : bool, timing : Timing.timing }
val test : 'a prop -> 'a * stats -> result * stats
val empty_stats : bool -> stats

end

structure Property : PROPERTY =
struct

datatype result = Result of bool | Discard | Exception of exn
type stats = { count : int, generated : bool, timing : Timing.timing }
fun empty_stats gen =
  { count = 0, generated = gen,
    timing = {cpu = Time.zeroTime, elapsed = Time.zeroTime, gc = Time.zeroTime} }

type 'a pred = 'a -> bool
type 'a prop = 'a -> result

fun apply f x =
  Result (f x)
  (* Testcode may throw arbitrary exceptions. Interrupts MUST not be catched! *)
  handle x => if Exn.is_interrupt x then Exn.reraise x else Exception x

fun pred f x = apply f x
fun pred2 f z = pred (fn x => f (x, z))

fun implies (cond, prop) x =
  if cond x
  then prop x
  else Discard

fun ==> (p1, p2) = implies (p1, pred p2)

fun add_timings {elapsed = elapsed1, cpu = cpu1, gc = gc1} {elapsed = elapsed2, cpu = cpu2, gc = gc2} =
  {elapsed = elapsed1 + elapsed2,
   cpu = cpu1 + cpu2,
   gc = gc1 + gc2}

fun test prop (input, {count, generated, timing}) =
  let
    val (time, result) = Timing.timing (fn () => prop input) ()
    val stats = {count = count + 1, generated = generated, timing = add_timings timing time}
  in
  (result,stats)
  end
  

end
