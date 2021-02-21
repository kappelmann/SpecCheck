(*  Title:      Tools/Spec_Check/output_style.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Output styles for presenting Spec_Check's results.
*)

signature OUTPUT_STYLE =
sig
  type output_style = Proof.context -> string -> Timing.timing -> Property.stats * (string list, int) Util.either  -> unit
  val register_style : string -> output_style -> theory -> theory
  val get_style : Proof.context -> string -> Timing.timing -> Property.stats * (string list, int) Util.either -> unit
end

structure Output_Style : OUTPUT_STYLE =
struct

open Util
open Configuration

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

(* Register the pre-defined styles *)
val perl_style = register_style Output_Style_Perl.identifier Output_Style_Perl.style
val _ = Theory.setup perl_style

val cm_style = register_style Output_Style_CM.identifier Output_Style_CM.style
val _ = Theory.setup cm_style

val custom_style = register_style Output_Style_Custom.identifier Output_Style_Custom.style
val _ = Theory.setup custom_style

end
