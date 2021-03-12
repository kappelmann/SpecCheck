(*  Title:      Tools/Spec_Check/spec_check.ML
    Author:     Lukas Bulwahn, Nicolai Schaffroth, and Kevin Kappelmann TU Muenchen
    Author:     Christopher League

Specification-based testing of ML programs.
*)

signature SPEC_CHECK =
sig

  (*tries to shrink a given (failing) input to a smaller, failing input*)
  val try_shrink : 'a Property.prop -> 'a Shrink.shrink -> 'a -> int -> Spec_Check_Base.stats ->
    ('a * Spec_Check_Base.stats)

  (*runs a property for a given test case and returns the result and the updated the statistics*)
  val run_a_test : 'a Property.prop -> 'a -> Spec_Check_Base.stats ->
    Spec_Check_Base.result_single * Spec_Check_Base.stats

  (*returns the new state after executing the test*)
  val check_style : 'a Output_Style.output_style -> ('a Show.show) option ->  'a Shrink.shrink ->
    ('a, 's) Generator.gen_state -> string -> 'a Property.prop -> Proof.context -> 's -> 's
  val check_shrink : 'a Show.show -> 'a Shrink.shrink -> ('a, 's) Generator.gen_state -> string ->
    'a Property.prop -> Proof.context -> 's -> 's
  val check : 'a Show.show -> ('a, 's) Generator.gen_state -> string -> 'a Property.prop ->
    Proof.context -> 's -> 's
  val check_base : ('a, 's) Generator.gen_state -> string -> 'a Property.prop -> Proof.context ->
    's -> 's

  (*returns all unprocessed elements of the sequence*)
  val check_seq_style : 'a Output_Style.output_style -> ('a Show.show) option -> 'a Seq.seq ->
    string -> 'a Property.prop -> Proof.context -> 'a Seq.seq
  val check_seq : 'a Show.show -> 'a Seq.seq -> string -> 'a Property.prop -> Proof.context ->
    'a Seq.seq
  val check_seq_base : 'a Seq.seq -> string -> 'a Property.prop -> Proof.context -> 'a Seq.seq

  (*returns all unused elements of the list*)
  val check_list_style : 'a Output_Style.output_style -> ('a Show.show) option -> 'a list ->
    string -> 'a Property.prop -> Proof.context -> 'a Seq.seq
  val check_list : 'a Show.show -> 'a list -> string -> 'a Property.prop -> Proof.context ->
    'a Seq.seq
  val check_list_base : 'a list -> string -> 'a Property.prop -> Proof.context -> 'a Seq.seq

end

structure Spec_Check : SPEC_CHECK =
struct

open Spec_Check_Base

fun run_a_test prop input {
    num_success_tests,
    num_failed_tests,
    num_discarded_tests,
    num_recently_discarded_tests,
    num_success_shrinks,
    num_failed_shrinks,
    timing
  } =
  let
    val (time, result) = Timing.timing (fn () => prop input) ()
    val is_success = case result of Result is_success => is_success | _ => false
    val is_discard = case result of Discard => true | _ => false
    val is_failure = not is_discard andalso not is_success

    val num_success_tests = num_success_tests + (if is_success then 1 else 0)
    val num_failed_tests = num_failed_tests + (if is_failure then 1 else 0)
    val num_discarded_tests = num_discarded_tests + (if is_discard then 1 else 0)
    val num_recently_discarded_tests = if is_discard then num_recently_discarded_tests + 1 else 0
    val timing = add_timing timing time

    val stats = {
      num_success_tests = num_success_tests,
      num_failed_tests = num_failed_tests,
      num_discarded_tests = num_discarded_tests,
      num_recently_discarded_tests = num_recently_discarded_tests,
      num_success_shrinks = num_success_shrinks,
      num_failed_shrinks = num_failed_shrinks,
      timing = timing
    }
  in (result, stats) end

fun add_num_success_shrinks stats n = {
  num_success_tests = #num_success_tests stats,
  num_failed_tests = #num_failed_tests stats,
  num_discarded_tests = #num_discarded_tests stats,
  num_recently_discarded_tests = #num_recently_discarded_tests stats,
  num_success_shrinks = #num_success_shrinks stats + n,
  num_failed_shrinks = #num_failed_shrinks stats,
  timing = #timing stats
}

fun add_num_failed_shrinks stats n = {
  num_success_tests = #num_success_tests stats,
  num_failed_tests = #num_failed_tests stats,
  num_discarded_tests = #num_discarded_tests stats,
  num_recently_discarded_tests = #num_recently_discarded_tests stats,
  num_success_shrinks = #num_success_shrinks stats,
  num_failed_shrinks = #num_failed_shrinks stats + n,
  timing = #timing stats
}

fun try_shrink prop shrink input max_shrinks stats =
  let
    fun is_failure input = case run_a_test prop input empty_stats |> fst of
        Result is_success => not is_success
      | Discard => false
      | Exception _ => false (*do not count exceptions as a failure because the shrinker might
                               just have broken some invariant of the function*)
    fun find_first_failure xq pulls_left stats =
      if pulls_left <= 0
      then (NONE, pulls_left, stats)
      else
        case Seq.pull xq of
          NONE => (NONE, pulls_left, stats)
        | SOME (x, xq) =>
          if is_failure x
          then (SOME x, pulls_left - 1, add_num_success_shrinks stats 1)
          else find_first_failure xq (pulls_left - 1) (add_num_failed_shrinks stats 1)
  in
    (*always try the first successful branch and abort without backtracking once no further
      shrink is possible.*)
    case find_first_failure (shrink input) max_shrinks stats of
      (*recursively shrink*)
      (SOME input, shrinks_left, stats) => try_shrink prop shrink input shrinks_left stats
    | (NONE, _, stats) => (input, stats)
  end

fun test output_style init_stats show_opt shrink opt_gen name prop ctxt state =
  let
    val max_discard_ratio = Config.get ctxt Configuration.max_discard_ratio
    val max_success = Config.get ctxt Configuration.max_success
    (*number of counterexamples to generate before stopping the test*)
    val num_counterexamples =
      let val conf_num_counterexamples = Config.get ctxt Configuration.num_counterexamples
      in if conf_num_counterexamples > 0 then conf_num_counterexamples else ~1 end
    val max_shrinks = Config.get ctxt Configuration.max_shrinks

    fun run_tests opt_gen state stats counterexamples =
      (*stop the test run if enough (successful) tests were run or counterexamples were found*)
      if #num_success_tests stats >= max_success then (Success stats, state)
      else if num_tests stats >= max_success orelse
              #num_failed_tests stats = num_counterexamples
      then (Failure (stats, failure_data counterexamples), state)
      else (*test input and attempt to shrink on failure*)
        let val (input_opt, state) = opt_gen state
        in
          case input_opt of
            NONE =>
              if #num_failed_tests stats > 0
              then (Failure (stats, failure_data counterexamples), state)
              else (Success stats, state)
          | SOME input =>
            let val (result, stats) = run_a_test prop input stats
            in
              case result of
                Result true => run_tests opt_gen state stats counterexamples
              | Result false =>
                  let val (counterexample, stats) = try_shrink prop shrink input max_shrinks stats
                  in run_tests opt_gen state stats (counterexample :: counterexamples) end
              | Discard =>
                  if #num_recently_discarded_tests stats > max_discard_ratio
                  then
                    if #num_failed_tests stats > 0
                    then (Failure (stats, failure_data counterexamples), state)
                    else (Gave_Up stats, state)
                  else run_tests opt_gen state stats counterexamples
              | Exception exn =>
                  (Failure (stats, failure_data_exn (input :: counterexamples) exn), state)
            end
        end
  in
    Timing.timing (fn _ => run_tests opt_gen state init_stats []) ()
    |> uncurry (apfst o output_style show_opt ctxt name)
    |> snd
  end

fun check_style style show_opt shrink =
  test style empty_stats show_opt shrink o Generator.map SOME

fun check_shrink show = check_style Output_Style.default (SOME show)
fun check show = check_shrink show Shrink.none

fun check_base gen = check_style Output_Style.default NONE Shrink.none gen

fun check_seq_style style show_opt xq name prop ctxt =
  test style empty_stats show_opt Shrink.none Generator.of_seq name prop ctxt xq

fun check_seq show = check_seq_style Output_Style.default (SOME show)
fun check_seq_base xq = check_seq_style Output_Style.default NONE xq

fun check_list_style style show = check_seq_style style show o Seq.of_list
fun check_list show = check_seq show o Seq.of_list
fun check_list_base xs = check_seq_base (Seq.of_list xs)

end
