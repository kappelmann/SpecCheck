
signature GEN_TERM = sig
  include GEN_BASE
  (* more generators *)
  val term : int -> term gen
  val term_fol : real * real * real -> int -> int -> term gen
  val term_fol_structure : int -> int -> term gen
  val term_fol_map : real * real * real -> term -> term gen
  val term_det : (int * int * 'a -> term * int * 'a) -> 'a -> term
  val term_det_path : ((term * int) list -> 'a -> term * int * 'a) -> 'a -> term
  val free : int -> term gen
  val var : int -> term gen
  val const : int -> term gen
  val typ : int -> typ gen
end

structure Gen_Term =
struct

open Gen_Datatypes

(* types and terms *)

val sort_string = selectL ["sort1", "sort2", "sort3"];
val type_string = selectL ["TCon1", "TCon2", "TCon3"];
val tvar_string = selectL ["a", "b", "c"];

val func_string = selectL ((Char.ord #"f" upto Char.ord #"h") |> List.map (Char.chr #> Char.toString));
val const_string = selectL ((Char.ord #"a" upto Char.ord #"f" - 1) |> List.map (Char.chr #> Char.toString));
val var_string = selectL ((Char.ord #"x" upto Char.ord #"z") |> List.map (Char.chr #> Char.toString));
val index = selectL [0, 1, 2, 3];
val bound_index = selectL [0, 1, 2, 3];

val sort = list (flip (1.0/3.0)) sort_string;

fun typ n =
  let
    fun type' m = map Type (zip (type_string, list (flip 0.25) (typ m)))
    val tfree = map TFree (zip (tvar_string, sort))
    val tvar = map TVar (zip (zip (tvar_string, index), sort))
  in
    if n = 0 then chooseL [tfree, tvar]
    else chooseL [type' (n div 2), tfree, tvar]
  end;

fun term n =
  let
    val const = map Const (zip (const_string, typ 10))
    val free = map Free (zip (var_string, typ 10))
    val var = map Var (zip (zip (var_string, index), typ 10))
    val bound = map Bound bound_index
    fun abs m = map Abs (zip3 (var_string, typ 10, term m))
    fun app m = map (op $) (zip (term m, term m))
  in
    if n = 0 then chooseL [const, free, var, bound]
    else chooseL [const, free, var, bound, abs (n - 1), app (n div 2)]
  end;

datatype 'a node = Node of 'a * 'a node list

fun term_fol1 num_sym r =
  let
    fun ins target value (Node (node,children)) =
      if node = target
      then Node (node, Node (value, [])::children)
      else Node (node, List.map (ins target value) children)
    fun ins_at_random i (tree,r) =
      let val (target,r) = Random.range_int (0,i-1) r
      in
      (ins target i tree, r)
      end
    (* Abuse Bound to store the number of args without specifying a type *)
    fun conv_node (Node (_, [])) r = (Bound 0,r)
      | conv_node (Node (_, args)) r =
        (Bound (List.length args),r)
        |> fold (fn x => fn (acc,r) => conv_node x r |>> (fn x => acc $ x)) args 
  in
  fold ins_at_random (1 upto (num_sym - 1)) (Node (0,[]), r)
  |-> conv_node
  end

fun zip_list xs ys = case (xs,ys) of
     ([],_) => []
   | (_,[]) => []
   | (x::xs,y::ys) => (x,y) :: zip_list xs ys

fun clamp (lower,upper) x = if x < lower then lower else if x > upper then upper else x

fun get_num_args avg_args num_sym r =
  binom_dist 0.5 (avg_args * 2) r
  |>> clamp (0,num_sym)

fun distribute_num_sym num_args num_sym r =
  let fun f (acc,r) = Random.range_int (0,num_args) r |>> (fn x => x::acc)
      val (intervals,r) = fold (fn _ => f) (1 upto num_args - 1) ([],r) |>> Library.sort int_ord
      val intervals = 0::intervals @ [num_sym]
      val args_size = zip_list (tl intervals) intervals |> List.map (fn (x,y) => x - y)
  in (args_size,r) end

fun term_fol_structure avg_args num_sym r =
  if num_sym <= 1
  then (Bound 0, r)
  else let
    val num_sym = num_sym - 1 (* Function *)
    val (num_args,r) = get_num_args avg_args num_sym r
    (* Abuse Bound to store the number of args without specifying a type *)
    val (func,r) = (Bound num_args,r)
    val (args_size,r) =
      if num_args = 0
      then ([],r) (* This drops some symbols => lower total than requested by num_sym *)
      else distribute_num_sym num_args num_sym r
    val (args,r) =
      fold (fn x => fn (acc,r) => term_fol_structure avg_args x r |>> (fn y => y :: acc)) args_size ([],r)
  in (Term.list_comb (func,args),r) end

fun ifMult _ _ [] [x] = x
  | ifMult rand cumulativ (f::fs) (x::xs) =
    let val c = cumulativ + f in
    if rand < c then x
    else ifMult rand c fs xs
    end
  | ifMult _ _ _ _ = raise Fail "Invalid arguments to ifMult"

fun generic_type 0 = TVar (("'a", 0), [])
  | generic_type n =
      let val name = "'" ^ Char.toString (Char.chr (Char.ord #"a" + n)) in
      Type ("fun", [TVar ((name,0), []), generic_type (n-1)]) end

(* Implement more sophisticated selection. Remember already used variables and their types. *)
fun name 0 r = string (lift 1, charRange (#"a", #"e")) r
  | name n r = string (lift 1, charRange (#"f",#"j")) r |>> (fn str => str ^ @{make_string} n)
fun var n r1 =
  let val (r1,r2) = Random.split r1
      val (r1,r3) = Random.split r1
  in
  (Var ((name n r1 |> fst, index r2 |> fst), generic_type n), r3)
  end
fun const n r1 =
  let val (r1,r2) = Random.split r1 in
  (Const (name n r1 |> fst, generic_type n), r2)
  end
fun free n r1 =
  let val (r1,r2) = Random.split r1 in
  (Free (name n r1 |> fst, generic_type n), r2)
  end

fun term_fol_map (freq_bound, freq_free, freq_var) term r =
  let fun aux bounds types term r =
        let val (rand,r) = Random.real r
            val ifMult = ifMult rand 0.0 [freq_bound, freq_free, freq_var]
        in case term of
            Bound n =>
              ifMult [lift (Bound 0), free n, var n, const n] r
          | fst$snd =>
            let val (fst,r) = aux bounds types fst r
                val (snd,r) = aux bounds types snd r
            in (fst$snd, r) end
          | _ => raise Fail "Term must only consist of $ and Bounds where index = number of args"
        end
  in aux [] [] term r end

fun term_fol freq avg_args num_sym r =
  term_fol_structure avg_args num_sym r
  |-> term_fol_map freq

fun incr_nth (x::xs) 0 = x+1 :: xs
  | incr_nth [] n = incr_nth (0::[]) n
  | incr_nth (x::xs) n = x :: incr_nth xs (n-1)

fun term_det f init_acc =
  let fun aux indices height state =
    let
      (* indices stores the number of symbols so far at each height *)
      val indices = incr_nth indices height
      val index = nth indices height
      val (sym,num_args,state) = f height index state
      fun fold_aux (acc, indices, state) =
        aux indices (height + 1) state
        |> (fn (x,y,z) => (x :: acc,y,z))
(*      val () = writeln (@{make_string} sym ^ @{make_string} num_args ^ @{make_string} indices)*)
      val (args,indices,acc) =
        fold (K fold_aux) (1 upto num_args) ([],indices,state)
(*      val () = writeln (@{make_string} args) *)
    in
    (fold (fn x => fn y => y $ x) (rev args) sym, indices, acc)
    end
  in
  aux [] 0 init_acc |> (fn (x,_,_) => x)
  end

fun term_det_path f init_acc =
  let fun aux path state =
    let
      val (sym,num_args,state) = f path state
      fun fold_aux i (acc, state) =
        aux ((sym,i)::path) state
        |> (fn (x,y) => (x :: acc,y))
(*      val () = writeln (@{make_string} sym ^ @{make_string} num_args ^ @{make_string} indices)*)
      val (args,acc) =
        fold fold_aux (1 upto num_args) ([],state)
(*      val () = writeln (@{make_string} args) *)
    in
    (fold (fn x => fn y => y $ x) (rev args) sym, acc)
    end
  in
  aux [] init_acc |> (fn (x,_) => x)
  end

end
