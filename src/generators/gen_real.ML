(*  Title:      Tools/Spec_Check/generators/gen_real.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Random generators for reals.
*)

signature SPEC_CHECK_GEN_REAL = sig

  (*range_real (x,y) r returns a value in [x, y]*)
  val range_real : real * real -> real Spec_Check_Gen_Types.gen

  val real : real Spec_Check_Gen_Types.gen

  val real_pos : real Spec_Check_Gen_Types.gen
  val real_neg : real Spec_Check_Gen_Types.gen

  val real_nonneg : real Spec_Check_Gen_Types.gen
  val real_nonpos : real Spec_Check_Gen_Types.gen

  val real_finite : real Spec_Check_Gen_Types.gen
end

structure Spec_Check_Gen_Real : SPEC_CHECK_GEN_REAL =
struct

open Spec_Check_Gen_Base
open Spec_Check_Gen_Text

fun range_real (min, max) r =
  if min > max
  then
    raise Fail (Spec_Check_Util.spaces["Range_Real:", string_of_real min, ">", string_of_real max])
  else Spec_Check_Random.real_unit r |>> (fn s => min + (s * max - s * min))

val digits = string (range_int (1, Real.precision)) (range_char (#"0", #"9"))

val {exp=minExp,...} = Real.toManExp Real.minPos
val {exp=maxExp,...} = Real.toManExp Real.posInf

val ratio = 99

fun mk r =
  let
    val (a, r) = digits r
    val (b, r) = digits r
    val (e, r) = range_int (minExp div 4, maxExp div 4) r
    val x = String.concat [a, ".", b, "E", Int.toString e]
  in
    (the (Real.fromString x), r)
  end

val real_pos = one_ofWL ((ratio, mk) ::
  List.map ((pair 1) o lift) [Real.posInf, Real.maxFinite, Real.minPos, Real.minNormalPos])

val real_neg = map Real.~ real_pos

val real_nonneg = one_ofWL [(1, lift 0.0), (ratio, real_pos)]
val real_nonpos = one_ofWL [(1, lift 0.0), (ratio, real_neg)]

val real = one_ofL [real_nonneg, real_nonpos]

val real_finite = filter Real.isFinite real

end
