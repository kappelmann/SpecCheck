(*  Title:      Tools/Spec_Check/spec_check.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Specification-based testing of ML programs with random values.
*)

signature SPEC_CHECK =
sig
  include CONFIGURATION
  val check_gen : Proof.context -> string ->
                  'a Generator.gen -> ('a -> string) option ->
                  'a Property.prop -> Random.rand -> unit
  val check_gen_shrink : Proof.context -> string ->
                         'a Generator.gen -> ('a -> string) option -> ('a -> 'a list) ->
                         'a Property.prop -> Random.rand -> unit

  val check_property : Proof.context -> string -> unit
end;

structure Spec_Check : SPEC_CHECK =
struct
open Configuration

(* testing functions *)

fun check ctxt stats_initial seq shrink name show prop =
  let
    fun apply_show x = case show of NONE => NONE | SOME f => SOME (f x)
    val print_result = Output_Style.get_style ctxt name
    fun test_input input stats = Property.test prop (input, stats)

    fun try_shrink (obj, result) (stats, badobjs) =
      let
        fun is_failure input =
          if test_input input stats |> fst |> Property.failure
          then SOME (input, result)
          else NONE
      in
        case get_first is_failure (shrink obj) of
          SOME (obj, result) => try_shrink (obj, result) (stats, badobjs)
        | NONE => obj :: badobjs
      end

    fun iter obj (stats, badobjs) =
      (* Target fulfilled, skip the rest *)
      if #count stats >= Config.get ctxt Configuration.gen_target
      then (stats, badobjs)
      else
      (* Test obj, if failure then attempt to shrink *)
        let
          val (result, stats') = test_input obj stats
          val badobjs' =
            if Property.failure result
            then try_shrink (obj, result) (stats, badobjs)
            else badobjs
        in (stats', badobjs') end
  in
    Generator.fold_seq iter seq (stats_initial, [])
    ||> map apply_show
    |> print_result
  end

fun limit ctxt gen = Generator.seq_of_gen
  (Config.get ctxt Configuration.gen_target * Config.get ctxt Configuration.max_discard_ratio) gen

fun check_gen ctxt name gen show prop r =
  check ctxt {count = 0, tags = [("__GEN", 0)]} (limit ctxt gen r) (fn _ => [])
    name show prop

fun check_gen_shrink ctxt name gen show shrink prop r =
  check ctxt {count=0, tags=[("__GEN", 0)]} (limit ctxt gen r) shrink
    name show prop

fun check_one ctxt name show prop obj =
  check ctxt Property.stats (Seq.of_list [obj]) (fn _ => [])
    name show prop

(*call the compiler and pass resulting type string to the parser*)
fun determine_type ctxt s =
  let
    val return = Unsynchronized.ref "return"
    val context : ML_Compiler0.context =
     {name_space = #name_space ML_Env.context,
      print_depth = SOME 1000000,
      here = #here ML_Env.context,
      print = fn r => return := r,
      error = #error ML_Env.context}
    val _ =
      Context.setmp_generic_context (SOME (Context.Proof ctxt))
        (fn () =>
          ML_Compiler0.ML context
            {line = 0, file = "generated code", verbose = true, debug = false} s) ()
  in
    Gen_Construction.parse_pred (! return)
  end;

(*call the compiler and run the test*)
fun run_test ctxt s =
  Context.setmp_generic_context (SOME (Context.Proof ctxt))
    (fn () =>
      ML_Compiler0.ML ML_Env.context
        {line = 0, file = "generated code", verbose = false, debug = false} s) ();

(*split input into tokens*)
fun input_split s =
  let
    fun dot c = c = #"."
    fun space c = c = #" "
    val (head, code) = Substring.splitl (not o dot) (Substring.full s)
  in
   (String.tokens space (Substring.string head),
    Substring.string (Substring.dropl dot code))
  end;

(*create the function from the input*)
fun make_fun s =
  let
    val scan_param = Scan.one (fn s => s <> ";")
    fun parameters s = Scan.repeat1 scan_param s
    val p = $$ "ALL" |-- parameters
    val (split, code) = input_split s
    val stop = Scan.stopper (fn _ => ";") (fn s => s = ";");
    val (params, _) = Scan.finite stop p split
  in "fn (" ^ commas params ^ ") => " ^ code end;

(*read input and perform the test*)
fun gen_check_property check ctxt s =
  let
    val func = make_fun s
    val (_, ty) = determine_type ctxt func
  in run_test ctxt (check ctxt "Check" (ty, func)) end;

val check_property = gen_check_property Gen_Construction.build_check
(*val check_property_safe = gen_check_property Gen_Construction.safe_check*)

(*perform test for specification function*)
fun gen_check_property_f check ctxt s =
  let
    val (name, ty) = determine_type ctxt s
  in run_test ctxt (check ctxt name (ty, s)) end;

val check_property_f = gen_check_property_f Gen_Construction.build_check
(*val check_property_safe_f_ = gen_check_property_f Gen_Construction.safe_check*)

end;

fun check_property s = Spec_Check.check_property (Context.the_local_context ()) s;

