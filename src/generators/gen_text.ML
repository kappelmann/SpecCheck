(*  Title:      Tools/Spec_Check/generators/gen_text.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Random generators for chars and strings.
*)

signature SPEC_CHECK_GEN_TEXT = sig

  val range_char : char * char -> char Spec_Check_Gen_Types.gen
  val char : char Spec_Check_Gen_Types.gen                   

  val char_of : string -> char Spec_Check_Gen_Types.gen

  val string : int Spec_Check_Gen_Types.gen -> char Spec_Check_Gen_Types.gen ->
    string Spec_Check_Gen_Types.gen

  val substring : string Spec_Check_Gen_Types.gen -> substring Spec_Check_Gen_Types.gen

  val cochar : (char, 'b) Spec_Check_Gen_Types.cogen
  val costring : (string, 'b) Spec_Check_Gen_Types.cogen
  val cosubstring : (substring, 'b) Spec_Check_Gen_Types.cogen

  val digit : char Spec_Check_Gen_Types.gen
  val lowercase_letter : char Spec_Check_Gen_Types.gen
  val uppercase_letter : char Spec_Check_Gen_Types.gen
  val letter : char Spec_Check_Gen_Types.gen
end

structure Spec_Check_Gen_Text : SPEC_CHECK_GEN_TEXT =
struct

open Spec_Check_Gen_Base

type char = Char.char
type string = String.string
type substring = Substring.substring

fun range_char (lo, hi) = map Char.chr (range_int (Char.ord lo, Char.ord hi))
val char = range_char (Char.minChar, Char.maxChar)

fun char_of s =
  one_of (Vector.tabulate (String.size s, fn i => lift (String.sub (s, i))))

fun string length_g g = list length_g g #>> CharVector.fromList

fun substring gen r =
  let
    val (s, r) = gen r
    val (i, r) = range_int (0, String.size s) r
    val (j, r) = range_int (0, String.size s - i) r
  in
    (Substring.substring (s, i, j), r)
  end

fun cochar c =
  if Char.ord c = 0 then variant 0
  else cochar (Char.chr (Char.ord c div 2)) o variant 1

fun cosubstring s = colist cochar (Substring.explode s)

fun costring s = cosubstring (Substring.full s)

val digit = range_char (#"0", #"9")

val lowercase_letter = range_char (#"a", #"z")
val uppercase_letter = range_char (#"A", #"Z")
val letter = one_ofL [lowercase_letter, uppercase_letter]

end
