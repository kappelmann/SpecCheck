(* Implements generators for terms and their types *)
signature GEN_TERM = sig
  include GEN_BASE

  (* Reuse previously generated values. Pass a reuse-probability and an initial value list as state *)
  val reuse : 'a gen -> real -> 'a list -> Random.rand -> 'a Seq.seq

(* Symbol and type generators
  symbols have no unique type but are a subset of term, therefore the sym_gens have type "term gen" *)
  val free : int -> term gen
  val var : int -> term gen
  val const : int -> term gen
  val typ : int -> typ gen

  val free' : real -> int -> Random.rand -> term Seq.seq
  val var' : real -> int -> Random.rand -> term Seq.seq
  val const' : real -> int -> Random.rand -> term Seq.seq

(* def_sym_gen (weight_free, weight_var, weight_const) num_args
  The weights specify the ratio of symbols (Free, Var, Const) in relation to each other in the term.
  Sum of weights need not be \<le> 1 and each weight can take arbitrary values. *)
  val def_sym_gen : (real * real * real) -> int -> term gen
(* def_sym_seq requires an additional reuse probability *)
  val def_sym_seq : (real * real * real) -> real -> int -> Random.rand -> term Seq.seq


(* Term generators *)

(* "term_fol symbol_gen avg_args num_sym" returns a random generator for terms
  symbol_gen takes number of arguments and returns a symbol. TODO No state is passed around. Use ref instead.
  avg_args specifies the average number of arguments of every function.
  num_sym specifies the upper bound on the amount of symbols. Term may contain less symbols*)
  val term_fol : (int -> term gen) -> int -> int -> term gen

(* "term_det f init_state" where "f height index state" returns "(symbol, num_args, new_state)"
  generates a term deterministically by applying f to every node andd expanding that node depending
  on the num_args that f returns.
  Traversal is: function \<rightarrow> first argument \<rightarrow> ... \<rightarrow> last argument
  This is later transformed to the Isabelle representation of ((f $ x) $ y) $ z
  See pdf in repository for more details.
  
  Arguments of f:
  height describes the distance from the root. Starts at 0
  index describes the !GLOBAL! index in that layer. Starts at 1.
    The left subtree influences the right subtree!
  state is passed according to the above traversal
  
  Return value of f:
  symbol is the function to which the arguments are generated next. Symbols are terms in Isabelle!
  num_args specifies how many arguments are required for the symbol. 0 to terminate this subtree
  new_state is passed according to the above traversal
  *)
  val term_det : (int -> int -> 'a -> term * int * 'a) -> 'a -> term

(* See above. f now takes a (Symbol * index_of_arg) list which specifies the path from the root to
  the current node *)
  val term_det_path : ((term * int) list -> 'a -> term * int * 'a) -> 'a -> term
end

structure Gen_Term : GEN_TERM =
struct

open Gen_Datatypes

local
fun reuse_aux gen threshhold state r () =
  let
    val (random_perc,r) = Random.real r
    val ((next,r),state) =
      if random_perc < threshhold andalso not (null state)
      then (selectL state r, state)
      else let val (v,r) = gen r in ((v,r), v :: state) end
  in
  SOME (next, Seq.make (reuse_aux gen threshhold state r))
  end  
in
fun reuse gen threshhold state r = Seq.make (reuse_aux gen threshhold state r)
end
(* Symbols and types *)

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


fun generic_type 0 = TVar (("'a", 0), [])
  | generic_type n =
      let val name = "'" ^ Char.toString (Char.chr (Char.ord #"a" + n)) in
      Type ("fun", [TVar ((name,0), []), generic_type (n-1)]) end

(* Implement more sophisticated selection. Remember already used variables and their types. *)
fun name prefix n r =
  string (lift 5, charRange (#"a",#"z")) r
  |>> (fn str => prefix ^ @{make_string} n ^ "_" ^ str)
fun var n r1 =
  let val (r1,r2) = Random.split r1
      val (r1,r3) = Random.split r1
  in
  (Var ((name "v" n r1 |> fst, index r2 |> fst), generic_type n), r3)
  end
fun const n r1 =
  let val (r1,r2) = Random.split r1 in
  (Const (name "c" n r1 |> fst, generic_type n), r2)
  end
fun free n r1 =
  let val (r1,r2) = Random.split r1 in
  (Free (name "f" n r1 |> fst, generic_type n), r2)
  end

fun var' perc n = reuse (var n) perc []
fun const' perc n = reuse (const n) perc []
fun free' perc n = reuse (free n) perc []

(* Construct a symbol generator by using chooseWL over a list of weights and generators *)
fun def_sym_gen (weight_free, weight_var, weight_const) n =
  chooseWL [(weight_free, free n),
            (weight_var, var n),
            (weight_const, const n)]

fun def_sym_seq (weight_free, weight_var, weight_const) prob n r =
  let
    fun aux state r () =
      let
        val (perc,r) = Random.range_real (0.0, weight_free + weight_var + weight_const) r
        val index =
          if perc < weight_free then 0
          else if perc < weight_free + weight_var then 1
          else 2
        val (value,seq) = nth state index |> Seq.pull |> (fn SOME x => x)
        val state = nth_map index (K seq) state
      in                             
      SOME (value, Seq.make (aux state r))
      end
    val ((r1,r2),(r3,r4)) = Random.split r |>> Random.split ||> Random.split
  in
  aux [free' prob n r1,
       var' prob n r2,
       const' prob n r3] r4
  |> Seq.make
  end

(* Term generators *)

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

fun term_fol_map sym_gen term r =
  let
    fun aux bounds types term r =
      case term of
        Bound n =>
          sym_gen n r
      | fst$snd =>
        let val (fst,r) = aux bounds types fst r
            val (snd,r) = aux bounds types snd r
        in (fst$snd, r) end
      | _ => raise Fail "Term must only consist of $ and Bounds where index = number of args"
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
