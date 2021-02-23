(*  Title:      Tools/Spec_Check/generators/base_generator.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Basic utility functions to create and combine generators.
*)

signature GEN_BASE =
sig
  include GEN_TYPES
  
  (* generator that always returns the passed value *)
  val lift : 'a -> 'a gen
  
  val zip : 'a gen -> 'b gen -> ('a * 'b) gen
  val zip3 : 'a gen -> 'b gen -> 'c gen -> ('a * 'b * 'c) gen
  val zip4 : 'a gen -> 'b gen -> 'c gen -> 'd gen -> ('a * 'b * 'c * 'd) gen
  val map : ('a -> 'b) -> 'a gen -> 'b gen
  val map2 : ('a -> 'b -> 'c) -> 'a gen -> 'b gen -> 'c gen
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a gen -> 'b gen -> 'c gen -> 'd gen
  val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a gen -> 'b gen -> 'c gen -> 'd gen -> 'e gen
  
  (* ensures that all generated values fulfill the predicate; loops if the predicate is never
  satisfied by the generated values. *)
  val filter : ('a -> bool) -> 'a gen -> 'a gen
  
  (* bernoulli random variable with parameter p \<in> [0,1] *)
  val bernoulli : real -> bool gen
  
  (* random variable following a binomial distribution with parameters p \<in> [0,1] and n \\<ge> 0 *)
  val binom_dist : real -> int -> int gen
  
  (* range_real (x,y) r returns a value in [x;y] *)
  val range_real : real * real -> real gen

  (* range_int (x,y) r returns a value in [x;y] *)
  val range_int : int * int -> int gen
  
  (* randomly generates one of the given values *)
  val elements : 'a vector -> 'a gen
  (* randomly uses one of the given generators *)
  val one_of : 'a gen vector -> 'a gen
  
  (* randomly generates one of the given values *)
  val elementsL : 'a list -> 'a gen
  (* randomly uses one of the given generators *)
  val one_ofL : 'a gen list -> 'a gen
  
  (* chooses one of the given generators with a weighted random distribution. *)
  val one_ofW : (int * 'a gen) vector -> 'a gen
  (* chooses one of the given values with a weighted random distribution. *)
  val elementsW : (int * 'a) vector -> 'a gen
  
  (* chooses one of the given generators with a weighted random distribution. *)
  val one_ofWL : (int * 'a gen) list -> 'a gen
  (* chooses one of the given values with a weighted random distribution. *)
  val elementsWL : (int * 'a) list -> 'a gen
  
  (* creates a vector of length as returned by the passed int generator *)
  val vector : int gen -> 'a gen -> 'a vector gen
  
  (* creates a list of length as returned by the passed int generator *)
  val list : int gen -> 'a gen -> 'a list gen
  (* generates elements until the passed (generator) predicate fails;
  returns a list of all values that satisfied the predicate *)
  val unfold_while : ('a -> bool gen) -> 'a gen -> 'a list gen
  (* generates a random permutation of the given list *)
  val shuffle : 'a list -> 'a list gen
  
  (* generates SOME value if passed bool generator returns true and NONE otherwise *)
  val option : bool gen -> 'a gen -> 'a option gen
  
  (* Creates a sequence of values using the passed generator, which depend on the current index
  of the sequence.
  TODO Sebastian: what do you mean by the following line?
  Can be used for stateful generators but the performance suffers when compared to the genS type. *)
  val seq : (int -> 'a option gen) -> 'a Seq.seq gen
  
  val unit : unit gen
  val ref_gen : 'a gen -> 'a Unsynchronized.ref gen
  
  (* `variant i g` creates the `i`th variant of a given generator. It raises an error if `i` is negative. *)
  val variant : (int, 'b) cogen
  val cobool : (bool, 'b) cogen
  val colist : ('a, 'b) cogen -> ('a list, 'b) cogen
  val cooption : ('a, 'b) cogen -> ('a option, 'b) cogen

end

structure Gen_Base : GEN_BASE =
struct

open Gen_Types

fun lift obj r = (obj, r)

fun zip g1 g2 =
  g1 #-> (fn x1 => g2 #-> (fn x2 => pair (x1, x2)))
fun zip3 g1 g2 g3 =
  zip g1 (zip g2 g3) #-> (fn (x1, (x2, x3)) => pair (x1, x2, x3))
fun zip4 g1 g2 g3 g4 =
  zip (zip g1 g2) (zip g3 g4) #-> (fn ((x1, x2), (x3, x4)) => pair (x1, x2, x3, x4))

fun map f g = g #>> f
fun map2 f = map (uncurry f) oo zip
fun map3 f = map (fn (a,b,c) => f a b c) ooo zip3
fun map4 f = map (fn (a,b,c,d) => f a b c d) oooo zip4

fun filter p gen r =
  let fun loop (x, r) = if p x then (x, r) else loop (gen r)
  in loop (gen r) end

fun bernoulli p r = let val (x,r) = Random.real r in (x <= p, r) end

fun binom_dist _ 0 r = (0,r)
  | binom_dist p n r =
    let fun int_of_bool b = if b then 1 else 0
    in
      bernoulli p r
      |>> int_of_bool
      ||> binom_dist p (n-1)
      |> (fn (x,(acc,r)) => (acc + x, r))
    end

fun range_real (min, max) r =
  if min > max
  then raise Fail ("Range_Real: " ^ @{make_string} min ^ " > " ^ @{make_string} max)
  else Random.real r |>> (fn s => min + (s * max - s * min))

fun range_int (min, max) r =
  if min > max
  then Runtime.exn_trace (raise Fail ("Range_Int: " ^ @{make_string} min ^ " > " ^ @{make_string} max))
  else Random.real r |>> (fn s => Int.min (min + Real.floor (s * Real.fromInt (max - min + 1)), max))
    
fun one_of v r =
  let val (i, r) = range_int (0, Vector.length v - 1) r
  in Vector.sub (v, i) r end

fun one_ofW (v : (int * 'a gen) vector) r =
  let
    val weight_total = Vector.foldl (fn ((freq,_),acc) => freq + acc) 0 v
    fun selectGen _ (_, Util.Right gen) = Util.Right gen
      | selectGen rand ((weight, gen), Util.Left acc) =
      let val acc = acc + weight
      in if acc < rand
         then Util.Left acc
         else Util.Right gen
      end
    val (threshhold, r) = range_int (1, weight_total) r
    val gen = case Vector.foldl (selectGen threshhold) (Util.Left 0) v of
        Util.Right gen => gen
      | _ => raise Fail "Error in one_ofW - did you pass an empty vector or invalid frequencies?"
  in gen r end

fun elements v = one_of (Vector.map lift v)
fun elementsW v = one_ofW (Vector.map (fn p => p ||> lift) v)

fun one_ofL l = one_of (Vector.fromList l)
fun one_ofWL l = one_ofW (Vector.fromList l)
fun elementsL l = elements (Vector.fromList l)
fun elementsWL l = elementsW (Vector.fromList l)

fun list length_g g r =
  let val (l, r) = length_g r
  in fold_map (K g) (map_range I l) r end

fun unfold_while bool_g_of_elem g r =
  let
    fun unfold_while_accum (xs, r) =
      let
        val (x, r) = g r
        val (continue, r) = bool_g_of_elem x r
      in
        if continue
        then unfold_while_accum (x::xs, r)
        else (xs, r)
      end
  in unfold_while_accum ([], r) end

fun shuffle xs r =
  let
    val (ns, r) = list (fn r => (length xs, r)) (Random.real) r
    val real_ord = make_ord (op <=)
    val xs = ListPair.zip (ns, xs) |> sort (real_ord o apply2 fst) |> List.map snd
  in (xs, r) end

fun vector length_g g = list length_g g #>> Vector.fromList

fun option bool_g g r =
  case bool_g r of
    (false, r) => (NONE, r)
  | (true, r) => map SOME g r

fun seq gen_of_idx r =
  let
    val (r1, r2) = Random.split r
    fun gen_next idx r () = case gen_of_idx idx r of
      (NONE, _) => NONE
    | (SOME v, r) => SOME (v, Seq.make (gen_next (idx + 1) r))
  in (Seq.make (gen_next 0 r1), r2) end

val unit = lift ()

fun ref_gen gen r = let val (value, r) = gen r
  in (Unsynchronized.ref value, r) end

fun variant i g r =
  if i < 0 then raise Subscript
  else
    let
      fun nth i r =
        let val (r1, r2) = Random.split r
        in if i = 0 then r1 else nth (i-1) r2 end
    in g (nth i r) end

fun cobool false = variant 0
  | cobool true = variant 1

fun colist _ [] = variant 0
  | colist co (x::xs) = colist co xs o co x o variant 1

fun cooption _ NONE = variant 0
  | cooption co (SOME x) = co x o variant 1

end
