(*  Title:      Tools/Spec_Check/generator.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Random generators for Isabelle/ML's types.
*)

signature GEN_FUNCTION = sig
  val function : ('a, 'b) Gen_Types.cogen -> 'b Gen_Types.gen -> ('a -> 'b) Gen_Types.gen
  val function' : 'b Gen_Types.gen -> (''a -> 'b) Gen_Types.gen
end

structure Gen_Function : GEN_FUNCTION =
struct

open Gen_Base

fun function cogen gen r =
  let
    val (r1, r2) = Random.split r
    fun g x = fst (cogen x gen r1)
  in (g, r2) end

fun function' gen r =
  let
    val (external, internal) = Random.split r
    val seed = Unsynchronized.ref internal
    val table = Unsynchronized.ref []
    fun new_entry k =
      let
        val (new_val, new_seed) = gen (!seed)
        val _ =  seed := new_seed
        val _ = table := AList.update (op =) (k, new_val) (!table)
      in new_val end
  in
    (fn v1 =>
      case AList.lookup (op =) (!table) v1 of
        NONE => new_entry v1
      | SOME v2 => v2, external)
  end

end
