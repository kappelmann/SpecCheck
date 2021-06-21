(*  Title:      Tools/Spec_Check/util.ML
    Author:     Kevin Kappelmann

Some utility functions that maybe should be put somewhere else in Pure.
*)

signature SPEC_CHECK_UTIL =
sig

datatype ('a,'b) either = Left of 'a | Right of 'b

val spaces : string list -> string

val pwriteln : string -> unit
val pwarning : string -> unit

end

structure Spec_Check_Util : SPEC_CHECK_UTIL =
struct

datatype ('a,'b) either = Left of 'a | Right of 'b

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