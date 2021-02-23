(* Implements generators for terms and their types.
By default all generated symbols and terms have dummyT as their type.
Infer the type with Syntax.check_term with an appropriate context if needed.
TODO implement wrapper that automatically collects all consts + vars *)
signature GEN_TERM = sig

  (* Symbol and type generators
   symbols have no unique type but are a subset of term, therefore the sym_gens have type "term gen"
   "free id args" returns a Free symbol with the pattern "f<args>_<id>". That is, args is used in
   the name to identify the type and id is used as an identifier differentiating it from other Frees. *)
  val free : int -> int -> term
  val var : int -> int -> term
  val const : int -> int -> term
  val typ : int -> typ Gen_Types.gen

  (* def_sym_gen (weight_free, weight_var, weight_const) max_id num_args
  max_id specifies the largest id used for name generation. It picks a random value from [0,max_id)
  The weights specify the ratio of symbols (Free, Var, Const) in relation to each other in the term. *)
  val def_sym_gen : (int * int * int) -> int -> int -> term Gen_Types.gen
  (* "def_sym_genS weights reuse_prob num_args" is a stateful variant that reuses previously generated
   symbols according to reuse_prob. To do so it uses a state. *)
  val def_sym_genS : int * int * int -> real -> int ->
    (term, ((int list * int list * int list) * Random.rand)) Gen_Types.gen_state

  (* Term generators *)

  (* "term_det f init_state" where "f height index state" returns "(symbol, num_args, new_state)"
  generates a term deterministically by applying f to every node and expanding that node depending
  on the num_args that f returns.
  Traversal is: function \<rightarrow> first argument \<rightarrow> ... \<rightarrow> last argument
  This is later transformed to the Isabelle representation of ((f $ x) $ y) $ z
  See pdf in repository for more details.
  
  Arguments of f:
  height describes the distance from the root. Starts at 0
  index describes the !GLOBAL! index in that layer. Starts at 1.
  Therefore the left subtree influences the right subtree!
  state is passed according to the above traversal
  
  Return value of f:
  symbol is the function to which the arguments are generated next. Symbols are terms in Isabelle!
  num_args specifies how many arguments are required for the symbol. 0 to terminate this subtree
  new_state is passed according to the traversal above
  *)
  val term_det : (int -> (term, int * 's) Gen_Types.gen_state) ->
    (term, 's) Gen_Types.gen_state

  (* See above. f now takes a (Symbol * index_of_arg) list which specifies the path from the root to
  the current node *)
  val term_det_path : ((term * int) list -> 'a -> term * int * 'a) -> 'a -> term

  (* "term_fol symbol_gen avg_args num_sym" returns a random generator for terms
  symbol_gen takes number of arguments and returns a symbol.
  No state is passed around. Use term_det or ref if you need to.
  avg_args specifies the average number of arguments of every function.
  num_sym specifies the upper bound on the amount of symbols. Term may contain less symbols.
  TODO remove as it is not as useful as thought? *)
  val term_fol : (int -> term Gen_Types.gen) -> int -> int -> term Gen_Types.gen
end

structure Gen_Term : GEN_TERM =
struct

open Gen_Base

(* Symbols and types *)

val sort_string = elementsL ["sort1", "sort2", "sort3"];
val type_string = elementsL ["TCon1", "TCon2", "TCon3"];
val tvar_string = elementsL ["a", "b", "c"];

val func_string = elementsL ((Char.ord #"f" upto Char.ord #"h") |> List.map (Char.chr #> Char.toString));
val const_string = elementsL ((Char.ord #"a" upto Char.ord #"f" - 1) |> List.map (Char.chr #> Char.toString));
val var_string = elementsL ((Char.ord #"x" upto Char.ord #"z") |> List.map (Char.chr #> Char.toString));
val index = elementsL [0, 1, 2, 3];
val bound_index = elementsL [0, 1, 2, 3];

val sort = unfold_while (K (bernoulli (1.0/3.0))) sort_string;

fun typ n =
  let
    fun type' m = zip type_string (unfold_while (K (bernoulli 0.25)) (typ m)) |> map Type
    val tfree = zip tvar_string sort |> map TFree
    val tvar = zip (zip tvar_string index) sort |> map TVar
  in
    if n = 0 then one_ofL [tfree, tvar]
    else one_ofL [type' (n div 2), tfree, tvar]
  end;

fun generic_type char 0 =
      let val name = "'" ^ Char.toString (Char.chr (Char.ord #"a" + char)) in
      TVar ((name,0), []) end
  | generic_type char n =
      let val name = "'" ^ Char.toString (Char.chr (Char.ord #"a" + char)) in
      Type ("fun", [TVar ((name,0), []), generic_type (char+1) (n-1)]) end

fun var id args = Var (("v" ^ string_of_int args ^ "_" ^ string_of_int id, 0), dummyT)
fun const id args = Const ("c" ^ string_of_int args ^ "_" ^ string_of_int id, dummyT)
fun free id args = Free ("f" ^ string_of_int args ^ "_" ^ string_of_int id, dummyT)

(* Construct a symbol generator by using chooseWL over a list of weights and generators *)
fun def_sym_gen (weight_free, weight_var, weight_const) max_id num_args r =
  let
    val (gen,r) = elementsWL [(weight_free, free),
                            (weight_var, var),
                            (weight_const, const)] r
    val (id,r) = range_int (0, max_id) r
    val symbol = gen id num_args
  in
  (symbol, r)
  end

(* nth_map has no default *)
fun incr_nth (x::xs) 0 = x+1 :: xs
  | incr_nth [] n = incr_nth (0::[]) n
  | incr_nth (x::xs) n = x :: incr_nth xs (n-1)

fun def_sym_genS (weight_free, weight_var, weight_const) reuse_prob arg (state as (idfree,idvar,idconst),r) =
  let
    val (reuse_perc,r) = Random.real r
    val (perc,r) = range_int (0, weight_free + weight_var + weight_const) r
    fun f xs = nth xs arg handle Subscript => 0
    val (r1,r) = Random.split r
    val reuse = reuse_perc < reuse_prob
    fun range id =
      if reuse
      then range_int (0,id+1) r1 |> fst
      else id + 1
    val (sym,state) =
      if perc < weight_free
      then
        if reuse
        then (free (f idfree |> range) arg, state)
        else (free (f idfree) arg, (incr_nth idfree arg, idvar, idconst))
      else if perc < weight_free + weight_var
      then
        if reuse
        then (var (f idvar |> range) arg, state)
        else (var (f idvar) arg, (idfree, incr_nth idvar arg,idconst))
      else
        if reuse
        then (const (f idconst |> range) arg, state)
        else (const (f idconst) arg, (idfree, idvar, incr_nth idconst arg))
  in
  (sym,(state,r))
  end

(* Term generators *)

fun clamp (lower,upper) x =
   if x < lower then lower
   else if x > upper then upper
   else x

fun get_num_args avg_args num_sym r =
  binom_dist 0.5 (avg_args * 2) r
  |>> clamp (0,num_sym)

fun distribute_num_sym num_args num_sym r =
  let fun f (acc,r) = range_int (0,num_args - 1) r |>> (fn x => x::acc)
      val (intervals,r) = fold (fn _ => f) (1 upto num_args - 1) ([],r) |>> Library.sort int_ord
      val intervals = 0::intervals @ [num_sym]
      val args_size = ListPair.zip (tl intervals, intervals) |> List.map (fn (x,y) => x - y)
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

fun term_det gen state =
  let fun build_tree indices height state =
    let
      (* indices stores the number of nodes visited so far at each height *)
      val indices = incr_nth indices height
      val index = nth indices height
      (* generate the symbol for the current node *)
      val (sym, (num_args, state)) = gen height (index, state)
      fun fold_arg (children, indices, state) =
        build_tree indices (height + 1) state
        |> (fn (child, indices, state) => (child :: children, indices, state))
      (* generate the subtrees for each argument *)
      val (children, indices, state) = fold (K fold_arg) (1 upto num_args) ([] ,indices, state)
    in (fold (fn x => fn y => y $ x) (rev children) sym, indices, state) end
  in
    build_tree [] 0 state
    |> (fn (x, _, state) => (x, state))
  end

fun term_det_path f init_acc =
  let fun aux path state =
    let
      val (sym, num_args, state) = f path state
      fun fold_aux i (acc, state) =
        aux ((sym,i)::path) state
        |> (fn (x,y) => (x :: acc,y))
      val (args,acc) =
        fold fold_aux (1 upto num_args) ([],state)
    in
    (fold (fn x => fn y => y $ x) (rev args) sym, acc)
    end
  in
    aux [] init_acc
    |> fst
  end

end
