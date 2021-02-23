(*  Title:      Tools/Spec_Check/output_style.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Custom-made output styles for SpecCheck.
*)

structure Output_Style_Custom : OUTPUT_STYLE =
struct

val identifier = "Custom"

fun style ctxt name total_time (stats, counterexamples) =
  let
    val (showable_counterexamples, count_counterexamples) = case counterexamples of
        Util.Left xs => (xs, length xs)
      | Util.Right size => ([], size)
    val max_success = Config.get ctxt Configuration.max_success
    val sort_counterexamples = Config.get ctxt Configuration.sort_counterexamples
    val show_stats = Config.get ctxt Configuration.show_stats
    val num_counterexamples = Config.get ctxt Configuration.num_counterexamples
  
    val resultw = 8
    val countw = 20
    val timew = 20
  
    fun result {count = 0, ...} = "No Test"
      | result {count, generated, ...} =
          if count_counterexamples <> 0
          then "Failed"
          else if generated andalso count < max_success
          then "Bad Gen"
          else "Passed"
  
    fun ratio (0, _) = "(0/0 passed)"
      | ratio (total, 0) = "(" ^ string_of_int total ^ " passed)"
      | ratio (total, fails) =
          "(" ^ string_of_int (total - fails) ^ "/" ^ string_of_int total ^  " passed)"
    
    (* Print the time spent in the test function in relation to total time spent.
       That includes generating test cases and overhead from the framework *)
    fun time {cpu, elapsed, gc} =
      "(" ^ Time.toString elapsed ^ "s / " ^ Time.toString (#elapsed total_time) ^ "s)"
  
    val result =
      let val more_stats = if not show_stats then "" else
            StringCvt.padRight #" " countw (ratio (#count stats, count_counterexamples)) ^
            StringCvt.padRight #" " timew (time (#timing stats))
      in
      StringCvt.padRight #" " resultw (result stats) ^ more_stats ^ name
      end
  
    fun print_counter counterexamples =
      let
        val maybe_sort = if sort_counterexamples then sort (int_ord o apply2 size) else I
        val counterexamples =
          counterexamples
          |> take num_counterexamples
          |> maybe_sort
      in
        case counterexamples of
            [] => (warning "No counterexample to show; config spec_check_num_counterexamples \<le> 0")
          | [e] => (warning ("Counterexample:"); warning e)
          | es => (warning "Counterexamples:"; fold (fn x => fn _ => warning x) es ())
      end
  in
    if count_counterexamples = 0
    then writeln result
    else if null showable_counterexamples
    then warning result
    else (warning result; print_counter showable_counterexamples)
  end

end
