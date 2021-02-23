(*  Title:      Tools/Spec_Check/spec_check.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Specification-based testing of ML programs with random values.

TODO: generator based tests probably should return a new seed?
*)

signature SPEC_CHECK =
sig

  val check_base : 'a Generator.gen -> string -> 'a Property.prop -> Proof.context ->
    Random.rand -> unit
  val check : 'a Show.show -> 'a Generator.gen -> string -> 'a Property.prop -> Proof.context ->
    Random.rand -> unit
  val check_shrink : 'a Show.show -> 'a Shrink.shrink -> 'a Generator.gen -> string ->
    'a Property.prop -> Proof.context -> Random.rand -> unit

  val check_seq_base : 'a Seq.seq -> string -> 'a Property.prop -> Proof.context -> unit
  val check_seq : 'a Show.show -> 'a Seq.seq -> string -> 'a Property.prop -> Proof.context -> unit

  val check_list_base : 'a list -> string -> 'a Property.prop -> Proof.context -> unit
  val check_list : 'a Show.show -> 'a list -> string -> 'a Property.prop -> Proof.context -> unit
  
end

structure Spec_Check : SPEC_CHECK =
struct

open Property

fun try_shrink prop shrink input =
  let
    fun is_failure input = case Property.test prop (input, empty_stats true) |> fst of
        Result true => false
      | Discard => false
      | Result false => true
      | Exception exn =>
          (warning "Exception raised during test:"; Runtime.exn_trace (fn () => raise exn))
  in
    (* Always try the first successful branch and abort without backtracking once no further shrink is possible. *)
    case find_first is_failure (shrink input) of
      (* recursively shrink *)
      SOME input => try_shrink prop shrink input
    | NONE => input
  end

fun run init_stats opt_show shrink seq name prop ctxt =
  let
    fun apply_show xs = case opt_show of
        SOME show => Util.Left (map show xs)
      | NONE => Util.Right (length xs)
    val print_result = Output_Style.get_style ctxt name
    val max_success = Config.get ctxt Configuration.max_success
    (* number of counterexamples to generate before stopping the test *)
    val num_counterexamples =
      let val conf_num_counterexamples = Config.get ctxt Configuration.num_counterexamples
      in if conf_num_counterexamples > 0 then conf_num_counterexamples else ~1 end
    
    exception EarlyReturn of Property.stats * ('a list * int)
    fun run_test input (stats, state_counterexamples) =
      (* stop the test run if enough successful tests were run or counterexamples were found *)
      if #count stats >= max_success orelse snd state_counterexamples = num_counterexamples
      then raise EarlyReturn (stats, state_counterexamples)
      else
      (* Test input and attempt to shrink on failure *)
        let
          val (result, stats) = Property.test prop (input, stats)
          val new_counterexamples_state = case result of
              Result true => state_counterexamples
            | Discard => state_counterexamples
            | Result false =>
                state_counterexamples
                |>> (fn counterexamples => try_shrink prop shrink input :: counterexamples)
                ||> (curry (op +) 1)
            | Exception exn => (warning ("Exception raised during test: " ^ name);
                  warning (@{make_string} exn);
                  raise EarlyReturn (stats, state_counterexamples |>> (fn cs => input :: cs)))
        in (stats, new_counterexamples_state) end
  in
    (* TODO Perhaps use ML_Profiling.profile_time to get more accurate results? Far slower *)
    Timing.timing (fn _ =>
      (Util.fold_seq run_test seq (init_stats, ([], 0)) handle EarlyReturn x => x)
      ||> apply_show o fst) ()
    |-> print_result
  end

(* Limit is set here instead of in the check function above because only randomly generated values
require a limit *)
fun limit gen ctxt =
  let
    val steps = Config.get ctxt Configuration.max_success *
      Config.get ctxt Configuration.max_discard_ratio
    fun opt_gen i = if i >= steps then Generator.lift NONE else Generator.map SOME gen
  in fst o Generator.seq opt_gen end

fun check_seq_base seq = run (Property.empty_stats false) NONE Shrink.none seq
fun check_seq show = run (Property.empty_stats false) (SOME show) Shrink.none

fun check_list_base xs = check_seq_base (Seq.of_list xs)
fun check_list show = check_seq show o Seq.of_list

fun check_base gen name prop ctxt r =
  run (Property.empty_stats true) NONE Shrink.none (limit gen ctxt r) name prop ctxt

fun check show gen name prop ctxt r =
  run (Property.empty_stats true) (SOME show) Shrink.none (limit gen ctxt r) name prop ctxt

fun check_shrink show shrink gen name prop ctxt r =
  run (Property.empty_stats true) (SOME show) shrink (limit gen ctxt r) name prop ctxt

end
