(*  Title:      Tools/Spec_Check/base_generator.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Basic random generators.
*)

signature GEN_BASE =
sig
(* 'a gen consume a random seed and return an unused one.
('a,'b) genS additionally consume a state and return a modified state *)
type 'a gen = Random.rand -> 'a * Random.rand
type ('a,'b) genS = ('b * Random.rand) -> 'a * ('b * Random.rand)

val lift : 'a -> 'a gen
val gen_prop : ('a -> bool) -> 'a gen -> 'a gen (* Ensure that all generated values fulfill property *)
val select : 'a vector -> 'a gen
val selectW : (real * 'a) vector -> 'a gen
val choose : 'a gen vector -> 'a gen
val choose' : (int * 'a gen) vector -> 'a gen (* TODO obsolete? Very bad performance for large numbers *)
(* Choose based on weight of generators. Total of weights need not be 1.0, therefore:
  chooseW [(1.0,x),(3.0,y)] == chooseW [(0.25,x),(0.75,y)] *)
val chooseW : (real * 'a gen) vector -> 'a gen

(* Copy of above with list instead of vector *)
val selectL : 'a list -> 'a gen
val selectWL : (real * 'a) list -> 'a gen
val chooseL : 'a gen list -> 'a gen
val chooseL' : (int * 'a gen) list -> 'a gen
val chooseWL : (real * 'a gen) list -> 'a gen

val filter : ('a -> bool) -> 'a gen -> 'a gen
val shuffle : 'a list -> 'a list gen

val zip : ('a gen * 'b gen) -> ('a * 'b) gen
val zip3 : ('a gen * 'b gen * 'c gen) -> ('a * 'b * 'c) gen
val zip4 : ('a gen * 'b gen * 'c gen * 'd gen) -> ('a * 'b * 'c * 'd) gen
val map : ('a -> 'b) -> 'a gen -> 'b gen
val map2 : ('a * 'b -> 'c) -> ('a gen * 'b gen) -> 'c gen
val map3 : ('a * 'b * 'c -> 'd) -> ('a gen * 'b gen * 'c gen) -> 'd gen
val map4 : ('a * 'b * 'c * 'd -> 'e) -> ('a gen * 'b gen * 'c gen * 'd gen) -> 'e gen

val flip : real -> bool gen
val binom_dist : real -> int -> int gen
val binom_dist_avg : real -> int -> int gen
val list : bool gen -> 'a gen -> 'a list gen
val option : bool gen -> 'a gen -> 'a option gen
val vector : (int * (int -> 'a) -> 'b) -> int gen * 'a gen -> 'b gen

type ('a, 'b) co = 'a -> 'b gen -> 'b gen
val variant : (int, 'b) co
val arrow : ('a, 'b) co * 'b gen -> ('a -> 'b) gen
val cobool : (bool, 'b) co
val colist : ('a, 'b) co -> ('a list, 'b) co
val coopt : ('a, 'b) co -> ('a option, 'b) co

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

end

structure Gen_Base : GEN_BASE =
struct

(* Generators *)
type 'a gen = Random.rand -> 'a * Random.rand
type ('a,'b) genS = ('b * Random.rand) -> 'a * ('b * Random.rand)

fun gen_prop prop gen r =
  let val (value,r) = gen r
  in
  if prop value
  then (value,r)
  else gen_prop prop gen r
  end

fun flip prob r = let val (x,r) = Random.real r in (x < prob, r) end

fun binom_dist _ 0 r = (0,r)
  | binom_dist p n r =
    let fun int_of_bool b = if b then 1 else 0 in
      flip p r
      |>> int_of_bool
      ||> binom_dist p (n-1)
      |> (fn (x,(acc,r)) => (acc + x, r))
    end

fun binom_dist_avg p avg r = binom_dist p (Real.toInt IEEEReal.TO_NEAREST (Real.fromInt avg / p)) r

type ('a, 'b) reader = 'b -> ('a * 'b) option

fun lift obj r = (obj, r)

(* Isabelle does not use vectors? *)
fun choose v r =
  let
    val (i, r) = Random.range_int (0, Vector.length v - 1) r
  in
  Vector.sub (v, i) r
  end

fun choose' v =
  let
    fun explode ((freq, gen), acc) = List.tabulate (freq, fn _ => gen) @ acc
  in
  choose (Vector.fromList (Vector.foldr explode [] v))
  end

local
datatype ('a,'b) either = Left of 'a | Right of 'b
in
fun chooseW (v : (real * 'a gen) vector) r =
  let
    val weight_total = Vector.foldl (fn ((freq,_),acc) => freq + acc) 0.0 v
    fun selectGen _ (_,Left gen) = Left gen
      | selectGen rand ((weight,gen),Right acc) =
      let val acc = acc + weight
      in
      if rand < acc
      then Left gen
      else Right acc      
      end
    val (threshhold,r) = Random.range_real (0.0,weight_total) r
    val gen = case Vector.foldl (selectGen threshhold) (Right 0.0) v of
        Left gen => gen
      | Right _ => raise Fail "Error in chooseF"
  in
  gen r
  end
end

fun select v = choose (Vector.map lift v)
fun selectW v = chooseW (Vector.map (fn (x,y) => (x,lift y)) v)

fun chooseL l = choose (Vector.fromList l)
fun chooseL' l = choose' (Vector.fromList l)
fun chooseWL l = chooseW (Vector.fromList l)
fun selectL l = select (Vector.fromList l)
fun selectWL l = selectW (Vector.fromList l)

fun zip (g1, g2) = g1 #-> (fn x1 => g2 #-> (fn x2 => pair (x1, x2)))

fun zip3 (g1, g2, g3) =
  zip (g1, zip (g2, g3)) #-> (fn (x1, (x2, x3)) => pair (x1, x2, x3))

fun zip4 (g1, g2, g3, g4) =
  zip (zip (g1, g2), zip (g3, g4)) #-> (fn ((x1, x2), (x3, x4)) => pair (x1, x2, x3, x4))

fun map f g = apfst f o g

fun map2 f = map f o zip
fun map3 f = map f o zip3
fun map4 f = map f o zip4

fun filter p gen r =
  let
    fun loop (x, r) = if p x then (x, r) else loop (gen r)
  in
    loop (gen r)
  end

fun shuffle [] r = ([],r)
  | shuffle (x::xs) r =
    let val (index,r) = Random.range_int (0, length (x::xs) - 1) r
        val (xs,r) = shuffle xs r
    in
    (take index xs @ [x] @ drop index xs, r)
    end

fun list flip g r =
  case flip r of
      (true, r) => ([], r)
    | (false, r) =>
      let
        val (x,r) = g r
        val (xs,r) = list flip g r
      in (x::xs, r) end

fun option flip g r =
  case flip r of
    (true, r) => (NONE, r)
  | (false, r) => map SOME g r

fun vector tabulate (int, elem) r =
  let
    val (n, r) = int r
    val p = Unsynchronized.ref r
    fun g _ =
      let
        val (x,r) = elem(!p)
      in x before p := r end
  in
    (tabulate(n, g), !p)
  end

fun seq_of_gen limit gen r =
  let
    fun aux 0 _ _ _ = NONE
      | aux limit gen r () =
          let val (v,r) = gen r in
          SOME (v, Seq.make (aux (limit - 1) gen r)) end
  in
    Seq.make (aux limit gen r)
  end


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
  in
    g (nth (v, r))
  end

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

end

