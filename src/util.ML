(*  Title:      Tools/Spec_Check/util.ML
    Author:     Kevin Kappelmann

Some utility functions that maybe should be put somewhere else in Pure.
*)

signature UTIL =
sig

datatype ('a,'b) either = Left of 'a | Right of 'b

(*folds a sequence until it stops*)
val fold_seq : ('a -> 'b -> 'b) -> 'a Seq.seq -> 'b -> 'b

(*Folds a sequence for a limited number of steps or until the sequence stops.
If the passed number of steps is negative, the complete sequence will be folded.*)
val fold_seq_bounded : int -> ('a -> 'b -> 'b) -> 'a Seq.seq -> 'b -> 'b

val spaces : string list -> string

val pwriteln : string -> unit
val pwarning : string -> unit

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

val spaces = space_implode " "

fun pretty_output output =
  space_explode " "
  #> map Pretty.str
  #> Pretty.breaks
  #> pair 0
  #> Pretty.blk
  #> output

val pwriteln = pretty_output Pretty.writeln

val pwarning = pretty_output (Output.warning o Pretty.string_of)

end