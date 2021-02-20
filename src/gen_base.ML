(*  Title:      Tools/Spec_Check/base_generator.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Basic utility functions to create and combine generators.
*)

signature GEN_BASE =
sig
(* 'a gen consumes a random seed and returns an unused one along with a generated 'a value *)
type 'a gen = Random.rand -> 'a * Random.rand
(* ('a,'b) genS, when compared to `gen`, also consumes a state and returns a modified state.
  TODO Sebastian: Currently almost no usage.
  Theoretically useful for functions that accept stateful gens.
  More lightweight than sequences from Seq. *)
type ('a, 's) genS = ('s * Random.rand) -> 'a * ('s * Random.rand)

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

(* Finite sequence of values
   Pass initial state and state-update/next function
   New state is passed to gen, therefore sequence is:
   [gen (next state), gen (next (next state)), ...] until "next state" returns NONE
   Can be used for stateful generators but the performance suffers when compared to the genS type.
*)
val seq_of_gen : int -> 'a gen -> Random.rand -> 'a Seq.seq
(* There is no fold predefined for finite seq? Nor a halting version for infinite seq?
   first argument is the limit, if less than 0 limit is disabled. *)
val fold_seq : int -> ('a -> 'b -> 'b) -> 'a Seq.seq -> 'b -> 'b

type ('a, 'b) co = 'a -> 'b gen -> 'b gen
val variant : (int, 'b) co
val arrow : ('a, 'b) co * 'b gen -> ('a -> 'b) gen
val cobool : (bool, 'b) co
val colist : ('a, 'b) co -> ('a list, 'b) co
val coopt : ('a, 'b) co -> ('a option, 'b) co

(* bernoulli random variable with parameter p \<in> [0,1] *)
val bernoulli : real -> bool gen

(* random variable following a binomial distribution with parameters p \<in> [0,1] and n \\<ge> 0 *)
val binom_dist : real -> int -> int gen
end

structure Gen_Base : GEN_BASE =
struct

type 'a gen = Random.rand -> 'a * Random.rand
type ('a,'b) genS = ('b * Random.rand) -> 'a * ('b * Random.rand)

type ('a, 'b) reader = 'b -> ('a * 'b) option

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

fun one_of v r =
  let val (i, r) = Random.range_int (0, Vector.length v - 1) r
  in Vector.sub (v, i) r end

local
datatype ('a,'b) either = Left of 'a | Right of 'b
in
fun one_ofW (v : (int * 'a gen) vector) r =
  let
    val weight_total = Vector.foldl (fn ((freq,_),acc) => freq + acc) 0 v
    fun selectGen _ (_, Right gen) = Right gen
      | selectGen rand ((weight, gen), Left acc) =
      let val acc = acc + weight
      in if acc < rand
         then Left acc
         else Right gen
      end
    val (threshhold, r) = Random.range_int (1, weight_total) r
    val gen = case Vector.foldl (selectGen threshhold) (Left 0) v of
        Right gen => gen
      | _ => raise Fail "Error in one_ofW - did you pass an empty vector or invalid frequencies?"
  in gen r end
end

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

fun seq_of_gen limit gen r =
  let
    fun aux 0 _ _ _ = NONE
      | aux limit gen r () =
          let val (v,r) = gen r
          in SOME (v, Seq.make (aux (limit - 1) gen r)) end
  in Seq.make (aux limit gen r) end

fun fold_seq limit f seq acc =
  if limit = 0
  then acc
  else
    case Seq.pull seq of
      SOME (v,seq) => fold_seq (limit - 1) f seq (f v acc)
    | NONE => acc


type ('a, 'b) co = 'a -> 'b gen -> 'b gen

fun variant v g r =
  let
    fun nth (i, r) =
      let val (r1, r2) = Random.split r
      in if i = 0 then r1 else nth (i-1, r2) end
  in g (nth (v, r)) end

fun arrow (cogen, gen) r =
  let
    val (r1, r2) = Random.split r
    fun g x = fst (cogen x gen r1)
  in (g, r2) end

fun cobool false = variant 0
  | cobool true = variant 1

fun colist _ [] = variant 0
  | colist co (x::xs) = variant 1 o co x o colist co xs

fun coopt _ NONE = variant 0
  | coopt co (SOME x) = variant 1 o co x

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

end
