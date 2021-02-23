(*  Title:      Tools/Spec_Check/output_style.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Perl output styles for SpecCheck.
*)

structure Output_Style_Perl : OUTPUT_STYLE =
struct

open Configuration

val identifier = "Perl"

fun style ctxt name timing (stats, badobjs) =
  let
    val (badobjs',count_badobjs) = case badobjs of
        Util.Left xs => (xs, length xs)
      | Util.Right size => ([], size)
    val max_success = Config.get ctxt max_success
    val namew = Config.get ctxt column_width
    val sort_counterexamples = Config.get ctxt sort_counterexamples
    val show_stats = Config.get ctxt show_stats
    val num_counterexamples = Config.get ctxt num_counterexamples

    val resultw = 8
    val countw = 20
    val allw = namew + resultw + countw + 2

    val maybe_sort = if sort_counterexamples then sort (int_ord o apply2 size) else I

    fun result {count = 0, ...} = "dubious"
      | result {count, generated, ...} =
          if count_badobjs <> 0 then "FAILED"
          else if generated andalso count < max_success then "dubious"
          else "ok"

    fun ratio (0, _) = "(0/0 passed)"
      | ratio (count, 0) = "(" ^ string_of_int count ^ " passed)"
      | ratio (count, n) =
          "(" ^ string_of_int (count - n) ^ "/" ^ string_of_int count ^  " passed)"

    val update =
      "\r" ^ StringCvt.padRight #"." namew name ^ "." ^
      StringCvt.padRight #" " resultw (result stats) ^
      StringCvt.padRight #" " countw (ratio (#count stats, count_badobjs))

    fun err badobjs =
      case maybe_sort (take num_counterexamples badobjs) of
          [] => ()
        | es => (warning "Counterexamples:"; fold (fn x => fn _ => warning x) es ())

  in
    if count_badobjs = 0
    then writeln update
    else (warning update; err badobjs')
  end

end
