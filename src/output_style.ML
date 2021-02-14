(*  Title:      Tools/Spec_Check/output_style.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Output styles for presenting Spec_Check's results.
*)

structure Output_Style : sig
  datatype ('a, 'b) either = Left of 'a | Right of 'b
  type output_style = Proof.context -> string -> Timing.timing -> Property.stats * (string list, int) either  -> unit
  
  val register_style : string -> output_style -> theory -> theory
  val get_style : Proof.context -> string -> Timing.timing -> Property.stats * (string list, int) either -> unit
end = struct

open Configuration

datatype ('a, 'b) either = Left of 'a | Right of 'b

type ('a, 'b) reader = 'b -> ('a * 'b) option
type 'a rep = ('a -> string) option

type output_style = Proof.context -> string -> Timing.timing -> Property.stats * (string list, int) either -> unit


structure Style = Theory_Data
(
  type T = output_style Symtab.table
  val empty = Symtab.empty
  val extend = I
  fun merge data : T = Symtab.merge (K true) data
)

fun get_style ctxt =
  let val name = Config.get ctxt Configuration.style in
    (case Symtab.lookup (Style.get (Proof_Context.theory_of ctxt)) name of
      SOME style => style ctxt
    | NONE => error ("No style called " ^ quote name ^ " found"))
  end

fun register_style name style = Style.map (Symtab.update (name, style))


(* perl style *)

fun perl_style ctxt name timing (stats, badobjs) =
  let
    val (badobjs',count_badobjs) = case badobjs of
        Left xs => (xs, length xs)
      | Right size => ([], size)
    val target = Config.get ctxt gen_target
    val namew = Config.get ctxt column_width
    val sort_examples = Config.get ctxt sort_examples
    val show_stats = Config.get ctxt show_stats
    val limit = Config.get ctxt examples

    val resultw = 8
    val countw = 20
    val allw = namew + resultw + countw + 2

    val maybe_sort = if sort_examples then sort (int_ord o apply2 size) else I

    fun result {count = 0, ...} = "dubious"
      | result {count, generated, ...} =
          if count_badobjs <> 0 then "FAILED"
          else if generated andalso count < target then "dubious"
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
      case maybe_sort (take limit badobjs) of
          [] => ()
        | es => (warning "Counterexamples:"; fold (fn x => fn _ => warning x) es ())

  in
    if count_badobjs = 0
    then writeln update
    else (warning update; err badobjs')
  end

val perl_style = register_style "Perl" perl_style

val _ = Theory.setup perl_style;


(* CM style: meshes with CM output; highlighted in sml-mode *)

fun cm_style ctxt name timing ({count, ...} : Property.stats, badobjs) =
  let
    val (badobjs',count_badobjs) = case badobjs of
        Left xs => (xs, length xs)
      | Right size => ([], size)
    fun pad wd = StringCvt.padLeft #"0" wd o Int.toString
    val gen_target = Config.get ctxt gen_target
    val _ = writeln ("[testing " ^ name ^ "... ")
  in
  case (count, count_badobjs) of
    (0, 0) => warning ("no valid cases generated]")
  | (n, 0) => writeln (
        if n >= gen_target then "ok]"
        else "ok on " ^ string_of_int n ^ "; " ^ string_of_int gen_target ^ " required]")
  | (_, m) =>
      let
        val wd = size (string_of_int (length badobjs'))
        fun each (e, i) = warning (name ^ ":" ^ pad wd i ^ ".0 Error: " ^ e)
      in
        (warning "FAILED]"; map each (badobjs' ~~ (1 upto m)); ())
      end
  end

val cm_style = register_style "CM" cm_style

val _ = Theory.setup cm_style;


(* Custom style *)

fun custom_style ctxt name total_time (stats, badobjs) =
  let
    val (badobjs',count_badobjs) = case badobjs of
        Left xs => (xs, length xs)
      | Right size => ([], size)
    val target = Config.get ctxt gen_target
    val sort_examples = Config.get ctxt sort_examples
    val show_stats = Config.get ctxt show_stats
    val limit = Config.get ctxt examples
  
    val resultw = 8
    val countw = 20
    val timew = 20
    val allw = resultw + countw + timew + 2
  
    fun result {count = 0, ...} = "No Test"
      | result {count, generated, ...} =
          if count_badobjs <> 0
          then "Failed"
          else if generated andalso count < target
          then "Bad Gen"
          else "Passed"
  
    fun ratio (0, _) = "(0/0 passed)"
      | ratio (count, 0) = "(" ^ string_of_int count ^ " passed)"
      | ratio (count, n) =
          "(" ^ string_of_int (count - n) ^ "/" ^ string_of_int count ^  " passed)"
    
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

val custom_style = register_style "Custom" custom_style

val _ = Theory.setup custom_style;

end
