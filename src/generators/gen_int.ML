(*  Title:      Tools/Spec_Check/generators/gen_int.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Random generators for ints.
*)

signature SPEC_CHECK_GEN_INT = sig

  (*pos m generates an integer in [1, m]*)
  val pos : int -> int Spec_Check_Gen_Types.gen
  (*neg m generates an integer in [m, 1]*)
  val neg : int -> int Spec_Check_Gen_Types.gen
  (*nonneg m generates an integer in [0, m]*)
  val nonneg : int -> int Spec_Check_Gen_Types.gen
  (*nonpos m generates an integer in [m, 0]*)
  val nonpos : int -> int Spec_Check_Gen_Types.gen

  val coint : (int, 'b) Spec_Check_Gen_Types.cogen

end

structure Spec_Check_Gen_Int : SPEC_CHECK_GEN_INT =
struct

open Spec_Check_Gen_Base

fun pos m = range_int (1, m)
fun neg m = range_int (m, ~1)
fun nonneg m = range_int (0, m)
fun nonpos m = range_int (m, 0)

fun coint n =
  if n = 0 then variant 0
  else if n < 0 then coint (~n) o variant 1
  else coint (n div 2) o variant 2

end
