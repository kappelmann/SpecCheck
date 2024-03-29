(*  Title:      Tools/Spec_Check/output_style/output_style_custom.ML
    Author:     Lukas Bulwahn, Nicolai Schaffroth, Kevin Kappelmann TU Muenchen
    Author:     Christopher League

Custom-made, QuickCheck-inspired output style for SpecCheck.
*)

structure Spec_Check_Output_Style_Custom : SPEC_CHECK_OUTPUT_STYLE =
struct

open Spec_Check_Base

fun print_success stats =
  let
    val num_success_tests = string_of_int (#num_success_tests stats)
    val num_discarded_tests = #num_discarded_tests stats
    val discarded_str =
      if num_discarded_tests = 0
      then "."
      else Spec_Check_Util.spaces [";", string_of_int num_discarded_tests,  "discarded."]
  in
    implode ["OK, passed ", num_success_tests, " tests", discarded_str]
    |> Spec_Check_Util.pwriteln
  end

fun print_gave_up stats =
  let
    val num_success_tests = string_of_int (#num_success_tests stats)
    val num_discarded_tests = string_of_int (#num_discarded_tests stats)
  in
    Spec_Check_Util.spaces ["Gave up! Passed only", num_success_tests, "test(s);", num_discarded_tests,
      "discarded test(s)."]
    |> Spec_Check_Util.pwarning
  end

fun print_failure_data ctxt show_opt failure_data =
  case #the_exception failure_data of
    SOME exn =>
      cat_lines ["Exception during test run:", exnMessage exn]
      |> Spec_Check_Util.pwarning
  | NONE => case show_opt of
      NONE => ()
    | SOME show =>
      let
        val sort_counterexamples = Config.get ctxt Spec_Check_Configuration.sort_counterexamples
        val maybe_sort = if sort_counterexamples then sort (int_ord o apply2 size) else I
        val counterexamples =
          #counterexamples failure_data
          |> map (Pretty.string_of o show)
          |> maybe_sort
      in fold (fn x => fn _ => Spec_Check_Util.pwarning x) counterexamples () end

fun print_failure ctxt show_opt (stats, failure_data) =
  ((Spec_Check_Util.spaces ["Failed! Falsified (after", string_of_int (num_tests stats), "test(s) and ",
    string_of_int (num_shrinks stats), "shrink(s)):"] |> Spec_Check_Util.pwarning);
  print_failure_data ctxt show_opt failure_data)

fun print_stats ctxt stats total_time =
  let
    val show_stats = Config.get ctxt Spec_Check_Configuration.show_stats
    (*the time spent in the test function in relation to the total time spent;
      the latter includes generating test cases and overhead from the framework*)
    fun show_time {elapsed, ...} =
      implode ["Time: ", Time.toString elapsed, "s (run) / ", Time.toString (#elapsed total_time),
        "s (total)"]
  in
    if not show_stats
    then ()
    else Spec_Check_Util.pwriteln (show_time (#timing stats))
  end

fun style show_opt ctxt name total_time result =
  let val stats = stats_of_result result
  in
    Spec_Check_Util.pwriteln (Spec_Check_Util.spaces ["Testing:", name]);
    (case result of
      Success _ => print_success stats
    | Gave_Up _ => print_gave_up stats
    | Failure data => print_failure ctxt show_opt data);
    print_stats ctxt stats total_time
  end

end
