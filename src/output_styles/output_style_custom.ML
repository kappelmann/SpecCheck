(*  Title:      Tools/Spec_Check/output_style.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Custom-made output styles for SpecCheck.
*)

structure Output_Style_Custom : OUTPUT_STYLE =
struct

open Configuration

val identifier = "Custom"

fun style ctxt name total_time (stats, badobjs) =
  let
    val (badobjs',count_badobjs) = case badobjs of
        Util.Left xs => (xs, length xs)
      | Util.Right size => ([], size)
    val target = Config.get ctxt gen_target
    val sort_examples = Config.get ctxt sort_examples
    val show_stats = Config.get ctxt show_stats
    val limit = Config.get ctxt examples
  
    val resultw = 8
    val countw = 20
    val timew = 20
  
    fun result {count = 0, ...} = "No Test"
      | result {count, generated, ...} =
          if count_badobjs <> 0
          then "Failed"
          else if generated andalso count < target
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
            StringCvt.padRight #" " countw (ratio (#count stats, count_badobjs)) ^
            StringCvt.padRight #" " timew (time (#timing stats))
      in
      StringCvt.padRight #" " resultw (result stats) ^ more_stats ^ name
      end
  
    fun print_counter badobjs =
      let
        val maybe_sort = if sort_examples then sort (int_ord o apply2 size) else I
        val badobjs =
          badobjs
          |> take limit
          |> maybe_sort
      in
        case badobjs of
            [] => (warning "No counterexample. No random generator used?")
          | [e] => (warning ("Counterexample:"); warning e)
          | es => (warning "Counterexamples:"; fold (fn x => fn _ => warning x) es ())
      end
  in
    if count_badobjs = 0
    then writeln result
    else if null badobjs'
    then warning result
    else (warning result; print_counter badobjs')
  end

end
