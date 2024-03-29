(*  Title:      Tools/Spec_Check/output_styles/output_style_types.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Shared types for SpecCheck output styles.
*)
signature SPEC_CHECK_OUTPUT_STYLE_TYPES =
sig
  type 'a output_style = 'a Spec_Check_Show.show option -> Proof.context -> string ->
    Timing.timing -> 'a Spec_Check_Base.result -> unit
end

structure Spec_Check_Output_Style_Types : SPEC_CHECK_OUTPUT_STYLE_TYPES =
struct

type 'a output_style = 'a Spec_Check_Show.show option -> Proof.context -> string -> Timing.timing ->
  'a Spec_Check_Base.result -> unit

end

signature SPEC_CHECK_OUTPUT_STYLE =
sig
  val style : 'a Spec_Check_Output_Style_Types.output_style
end
