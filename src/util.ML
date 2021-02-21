(*  Title:      Tools/Spec_Check/utils.ML
    Author:     Kevin Kappelmann

Some utility functions that maybe should be put somewhere else in Pure.
*)

signature UTIL =
sig

datatype ('a,'b) either = Left of 'a | Right of 'b

(* Fold a sequence until it stops. *)
val fold_seq : ('a -> 'b -> 'b) -> 'a Seq.seq -> 'b -> 'b

(* Fold a sequence for a limited number of steps or until the sequence stops.
If the passed number of steps is negative, the complete sequence will be folded. *)
val fold_seq_bounded : int -> ('a -> 'b -> 'b) -> 'a Seq.seq -> 'b -> 'b

end

structure Util : UTIL =
struct

datatype ('a,'b) either = Left of 'a | Right of 'b

fun fold_seq f seq acc = case Seq.pull seq of
    SOME (v, seq) => fold_seq f seq (f v acc)
  | NONE => acc

fun fold_seq_bounded steps f seq acc =
  let val seq = if steps < 0 then seq else Seq.take steps seq
  in fold_seq f seq acc end

end