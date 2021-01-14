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
  val check_seq : Proof.context -> string ->
                         'a Seq.seq -> ('a -> string) option -> ('a -> 'a list) ->
                         'a Property.prop -> unit
  val check_property : Proof.context -> string -> unit
end;

structure Spec_Check : SPEC_CHECK =
struct
open Configuration
open Property

(* testing functions *)
fun check ctxt stats_init name show seq shrink prop =
  let
    fun apply_show x = case show of NONE => NONE | SOME f => SOME (f x)
    val print_result = Output_Style.get_style ctxt name
    fun test_input input stats = Property.test prop (input, stats)

    fun try_shrink obj =
      let
        fun is_failure input =
          case test_input input (empty_stats true) |> fst of
            Result true => false
          | Discard => false
          | Result false => true
          | Exception exn => (warning "Exception raised during test:"; Runtime.exn_trace (fn () => raise exn))
      in
        (* Quasi Depth-first search, return once end of current branch is reached *)
        case find_first is_failure (shrink obj) of
          SOME obj => try_shrink obj
        | NONE => obj
      end

    exception EarlyReturn of Property.stats * 'a list
    fun iter obj (stats, badobjs) =
      (* Target fulfilled, skip the rest *)
      if #count stats >= Config.get ctxt Configuration.gen_target
      then raise EarlyReturn (stats, badobjs) (* Avoid traversing the whole lazy sequence *)
      else
      (* Test obj, if failure then attempt to shrink *)
        let
          val (result, stats) = test_input obj stats
          val badobjs =
            case result of
              Result true => badobjs
            | Discard => badobjs
            | Result false => try_shrink obj :: badobjs
            | Exception exn => (warning "Exception raised during test:"; Runtime.exn_trace (fn () => raise exn))
        in (stats, badobjs) end
  in
    Timing.timing (fn () =>
      (Generator.fold_seq iter seq (stats_init, []) handle EarlyReturn x => x)
      ||> map apply_show) ()
    |-> print_result
  end          

fun limit ctxt gen = Generator.seq_of_gen
  (Config.get ctxt Configuration.gen_target * Config.get ctxt Configuration.max_discard_ratio) gen

fun check_gen ctxt name gen show prop r =
  check ctxt (Property.empty_stats true) name show (limit ctxt gen r) (fn _ => [])
    prop

fun check_gen_shrink ctxt name gen show shrink prop r =
  check ctxt (Property.empty_stats true) name show (limit ctxt gen r) shrink
    prop

fun check_seq ctxt name seq show shrink prop  =
  check ctxt (Property.empty_stats false) name show seq shrink prop

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

