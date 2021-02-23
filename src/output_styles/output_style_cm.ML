(*  Title:      Tools/Spec_Check/output_style.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

CM output styles for SpecCheck.
*)

structure Output_Style_CM : OUTPUT_STYLE =
struct

open Configuration

val identifier = "CM"

(* CM style: meshes with CM output; highlighted in sml-mode *)
fun style ctxt name timing ({count, ...} : Property.stats, badobjs) =
  let
    val (badobjs',count_badobjs) = case badobjs of
        Util.Left xs => (xs, length xs)
      | Util.Right size => ([], size)
    fun pad wd = StringCvt.padLeft #"0" wd o Int.toString
    val max_success = Config.get ctxt max_success
    val _ = writeln ("[testing " ^ name ^ "... ")
  in
  case (count, count_badobjs) of
    (0, 0) => warning ("no valid cases generated]")
  | (n, 0) => writeln (
        if n >= max_success then "ok]"
        else "ok on " ^ string_of_int n ^ "; " ^ string_of_int max_success ^ " required]")
  | (_, m) =>
      let
        val wd = size (string_of_int (length badobjs'))
        fun each (e, i) = warning (name ^ ":" ^ pad wd i ^ ".0 Error: " ^ e)
      in
        (warning "FAILED]"; map each (badobjs' ~~ (1 upto m)); ())
      end
  end

end
