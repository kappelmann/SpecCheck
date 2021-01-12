(*  Title:      Tools/Spec_Check/output_style.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Output styles for presenting Spec_Check's results.
*)

structure Output_Style : sig
  (* status prints an update so the progress of a running test can be inspected.
     finish prints the final result *)
  type output_style = Proof.context -> string ->
    {status : string option * Property.result * (Property.stats  * string option list) -> unit,
     finish: Property.stats * string option list -> unit}

  val register_style : string -> output_style -> theory -> theory
  val get_style : Proof.context -> string ->
                  {finish: Property.stats * string option list -> unit,
                   status: string option * Property.result * (Property.stats * string option list) -> unit}
end = struct

open Configuration

type ('a, 'b) reader = 'b -> ('a * 'b) option
type 'a rep = ('a -> string) option

type output_style = Proof.context -> string ->
  {status: string option * Property.result * (Property.stats * string option list) -> unit,
   finish: Property.stats * string option list -> unit}


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

val perl_style =
  register_style "Perl"
    (fn ctxt => fn tag =>
      let
        val target = Config.get ctxt gen_target
        val namew = Config.get ctxt column_width
        val sort_examples = Config.get ctxt sort_examples
        val show_stats = Config.get ctxt show_stats
        val limit = Config.get ctxt examples

        val resultw = 8
        val countw = 20
        val allw = namew + resultw + countw + 2

        val maybe_sort = if sort_examples then sort (int_ord o apply2 size) else I

        fun result ({count = 0, ...}, _) _ = "dubious"
          | result (_ : Property.stats, badobjs) false = if null badobjs then "ok" else "FAILED"
          | result ({count, tags}, badobjs) true =
              if not (null badobjs) then "FAILED"
              else if AList.defined (op =) tags "__GEN" andalso count < target then "dubious"
              else "ok"

        fun ratio (0, _) = "(0/0 passed)"
          | ratio (count, 0) = "(" ^ string_of_int count ^ " passed)"
          | ratio (count, n) =
              "(" ^ string_of_int (count - n) ^ "/" ^ string_of_int count ^  " passed)"

        fun update (stats, badobjs) donep =
          "\r" ^ StringCvt.padRight #"." namew tag ^ "." ^
          StringCvt.padRight #" " resultw (result (stats, badobjs) donep) ^
          StringCvt.padRight #" " countw (ratio (#count stats, length badobjs))

        fun status (_, result, (stats, badobjs)) =
          if Property.failure result then warning (update (stats, badobjs) false) else ()

        fun prtag count (tag, n) first =
          if String.isPrefix "__" tag then ("", first)
          else
             let
               val ratio = round ((real n / real count) * 100.0)
             in
               (((if first then "" else StringCvt.padRight #" " allw "\n") ^
                 StringCvt.padLeft #" " 3 (string_of_int ratio) ^ "% " ^ tag),
               false)
             end

        fun prtags ({count, tags} : Property.stats) =
          if show_stats then cat_lines (fst (fold_map (prtag count) tags true)) else ""

        fun err badobjs =
          case maybe_sort (take limit (map_filter I badobjs)) of
              [] => ()
            | es => (warning "Counterexamples:"; fold (fn x => fn _ => warning x) es ())

        fun finish (stats, badobjs) =
          if null badobjs
          then writeln (update (stats, badobjs) true ^ prtags stats)
          else (warning (update (stats, badobjs) true); err badobjs)
      in
        {status = status, finish = finish}
      end)

val _ = Theory.setup perl_style;


(* CM style: meshes with CM output; highlighted in sml-mode *)

val cm_style =
  register_style "CM"
    (fn ctxt => fn tag =>
      let
        fun pad wd = StringCvt.padLeft #"0" wd o Int.toString
        val gen_target = Config.get ctxt gen_target
        val _ = writeln ("[testing " ^ tag ^ "... ")
        fun finish ({count, ...} : Property.stats, badobjs) =
          (case (count, badobjs) of
            (0, []) => warning ("no valid cases generated]")
          | (n, []) => writeln (
                if n >= gen_target then "ok]"
                else "ok on " ^ string_of_int n ^ "; " ^ string_of_int gen_target ^ " required]")
          | (_, es) =>
              let
                val wd = size (string_of_int (length es))
                fun each (NONE, _) = ()
                  | each (SOME e, i) = warning (tag ^ ":" ^ pad wd i ^ ".0 Error: " ^ e)
              in
                (warning "FAILED]"; map each (es ~~ (1 upto (length es))); ())
              end)
      in
        {status = K (), finish = finish}
      end)

val _ = Theory.setup cm_style;



(* Custom style *)

fun custom_style ctxt name =
let
  val target = Config.get ctxt gen_target
  val sort_examples = Config.get ctxt sort_examples
  val show_stats = Config.get ctxt show_stats
  val limit = Config.get ctxt examples

  val resultw = 8
  val countw = 20
  val allw = resultw + countw + 2

  fun result ({count = 0, ...}, _) = "No Test"
    | result ({count, tags}, badobjs) =
        if not (null badobjs)
        then "Failed"
        else if AList.defined (op =) tags "__GEN" andalso count < target
        then "Bad Gen"
        else "Passed"

  fun ratio (0, _) = "(0/0 passed)"
    | ratio (count, 0) = "(" ^ string_of_int count ^ " passed)"
    | ratio (count, n) =
        "(" ^ string_of_int (count - n) ^ "/" ^ string_of_int count ^  " passed)"

  fun update (stats, badobjs) =
    StringCvt.padRight #" " resultw (result (stats, badobjs)) ^
    StringCvt.padRight #" " countw (ratio (#count stats, length badobjs)) ^
    name

  (* TODO investigate this function further. First non-generator tag is printed? *)
  fun prtag count (tag, n) first =
    if String.isPrefix "__" tag
    then ("", first)
    else
       let
         val ratio = round ((real n / real count) * 100.0)
       in
         (((if first then "" else StringCvt.padRight #" " allw "\n") ^
           StringCvt.padLeft #" " 3 (string_of_int ratio) ^ "% " ^ tag),
         false)
       end

  fun prtags ({count, tags} : Property.stats) =
    if show_stats
    then fold_map (prtag count) tags true |> fst |> cat_lines
    else ""

  fun print_counter badobjs =
    let
      val maybe_sort = if sort_examples then sort (int_ord o apply2 size) else I
      val badobjs =
        badobjs
        |> map_filter I
        |> take limit
        |> maybe_sort (* TODO When can this occur? *)
    in
      case badobjs of
          [] => ()
        | es => (warning "Counterexamples:"; fold (fn x => fn _ => warning x) es ())
    end

  fun finish (stats, badobjs) =
    if null badobjs
    then writeln (update (stats, badobjs) ^ prtags stats)
    else (warning (update (stats, badobjs)); print_counter badobjs)
  in
  {status = K (), finish = finish}
  end

val custom_style = register_style "Custom" custom_style

val _ = Theory.setup custom_style;

end
