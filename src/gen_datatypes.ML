(*  Title:      Tools/Spec_Check/generator.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Random generators for Isabelle/ML's types.
*)

signature GEN_DATATYPES = sig
  include GEN_BASE
  (* text generators *)
  val char : char gen
  val charRange : char * char -> char gen
  val charFrom : string -> char gen
  val charByType : (char -> bool) -> char gen
  val string : int gen -> char gen -> string gen
  val substring : string gen -> substring gen
  val cochar : (char, 'b) co
  val costring : (string, 'b) co
  val cosubstring : (substring, 'b) co
  (* integer generators *)
  val int : int gen
  val int_pos : int gen
  val int_neg : int gen
  val int_nonpos : int gen
  val int_nonneg : int gen
  val coint : (int, 'b) co
  (* real generators *)
  val real : real gen
  val real_frac : real gen
  val real_pos : real gen
  val real_neg : real gen
  val real_nonpos : real gen
  val real_nonneg : real gen
  val real_finite : real gen
  (* function generators *)
  val function_const : 'c * 'b gen -> ('a -> 'b) gen
  val function_strict : int -> ''a gen * 'b gen -> (''a -> 'b) gen
  val function_lazy : ''a gen * 'b gen -> (''a -> 'b) gen
  val unit : unit gen
  val ref' : 'a gen -> 'a Unsynchronized.ref gen
end

structure Gen_Datatypes : GEN_DATATYPES =
struct

open Gen_Base

(* text *)

type char = Char.char
type string = String.string
type substring = Substring.substring


fun charRange (lo, hi) = map Char.chr (Random.range_int (Char.ord lo, Char.ord hi))
val char = charRange (Char.minChar, Char.maxChar)

fun charFrom s =
  one_of (Vector.tabulate (String.size s, fn i => lift (String.sub (s, i))))

fun charByType p = filter p char

fun string length_g g = list length_g g #>> CharVector.fromList

fun substring gen r =
  let
    val (s, r') = gen r
    val (i, r'') = Random.range_int (0, String.size s) r'
    val (j, r''') = Random.range_int (0, String.size s - i) r''
  in
    (Substring.substring (s, i, j), r''')
  end

fun cochar c =
  if Char.ord c = 0 then variant 0
  else variant 1 o cochar (Char.chr (Char.ord c div 2))

fun cosubstring s = Substring.foldr (fn (c,v) => cochar c o v) (variant 0) s

fun costring s = cosubstring (Substring.full s)

(* integers *)
val digit = charRange (#"0", #"9")
val nonzero = string (lift 1) (charRange (#"1", #"9"))
fun digits' n = string (Random.range_int (0, n-1)) digit
fun digits n = map2 (curry op^) nonzero (digits' n)

val maxDigits = 64
val ratio = 49

fun pos_or_neg f r =
  let
    val (s, r') = digits maxDigits r
  in (f (the (Int.fromString s)), r') end

val int_pos = pos_or_neg I
val int_neg = pos_or_neg Int.~
val zero = lift 0

val int_nonneg = one_ofWL [(1, zero), (ratio, int_pos)]
val int_nonpos = one_ofWL [(1, zero), (ratio, int_neg)]
val int = one_ofL [int_nonneg, int_nonpos]

fun coint n =
  if n = 0 then variant 0
  else if n < 0 then variant 1 o coint (~ n)
  else variant 2 o coint (n div 2)

(* reals *)
val digits = string (Random.range_int (1, Real.precision)) (charRange (#"0", #"9"))

fun real_frac r =
  let val (s, r') = digits r
  in (the (Real.fromString s), r') end

val {exp=minExp,...} = Real.toManExp Real.minPos
val {exp=maxExp,...} = Real.toManExp Real.posInf

val ratio = 99

fun mk r =
  let
    val (a, r') = digits r
    val (b, r'') = digits r'
    val (e, r''') = Random.range_int  (minExp div 4, maxExp div 4) r''
    val x = String.concat [a, ".", b, "E", Int.toString e]
  in
    (the (Real.fromString x), r''')
  end

val real_pos = one_ofWL (List.map ((pair 1) o lift)
    [Real.posInf, Real.maxFinite, Real.minPos, Real.minNormalPos] @ [(ratio, mk)])

val real_neg = map Real.~ real_pos

val real_nonneg = one_ofWL [(1, lift 0.0), (ratio, real_pos)]
val real_nonpos = one_ofWL [(1, lift 0.0), (ratio, real_neg)]

val real = one_ofL [real_nonneg, real_nonpos]

val real_finite = filter Real.isFinite real

(* more function generators *)

fun function_const (_, gen2) r =
  let
    val (v, r') = gen2 r
  in (fn _ => v, r') end;

fun function_strict size (gen1, gen2) r =
  let
    val (def, r') = gen2 r
    val (table, r'') = list (lift size) (zip gen1 gen2) r'
  in (fn v1 => the_default def (AList.lookup (op =) table v1), r'') end;

fun function_lazy (gen1, gen2) r =
  let
    val (initial1, r') = gen1 r
    val (initial2, internal) = gen2 r'
    val seed = Unsynchronized.ref internal
    val table = Unsynchronized.ref [(initial1, initial2)]
    fun new_entry k =
      let
        val (new_val, new_seed) = gen2 (!seed)
        val _ =  seed := new_seed
        val _ = table := AList.update (op =) (k, new_val) (!table)
      in new_val end
  in
    (fn v1 =>
      case AList.lookup (op =) (!table) v1 of
        NONE => new_entry v1
      | SOME v2 => v2, r')
  end;

(* unit *)

fun unit r = ((), r);

(* references *)

fun ref' gen r =
  let val (value, r') = gen r
  in (Unsynchronized.ref value, r') end;

end
