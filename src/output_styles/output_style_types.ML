(*  Title:      Tools/Spec_Check/styles/style_types.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Shared types for SpecCheck output styles.
*)
signature OUTPUT_STYLE_TYPES =
sig
  type output_style = Proof.context -> string -> Timing.timing -> 
    Property.stats * (string list, int) Util.either  -> unit
end

structure Output_Style_Types : OUTPUT_STYLE_TYPES =
struct

type output_style = Proof.context -> string -> Timing.timing ->
  Property.stats * (string list, int) Util.either -> unit

end

signature OUTPUT_STYLE =
sig
  val style : Output_Style_Types.output_style
  val identifier : string
end
