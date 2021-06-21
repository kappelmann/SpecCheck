(*  Title:      Tools/Spec_Check/output_styles/output_style.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Output styles for presenting SpecCheck results.
*)

signature SPEC_CHECK_DEFAULT_OUTPUT_STYLE =
sig
  include SPEC_CHECK_OUTPUT_STYLE_TYPES
  val default : 'a output_style
end

structure Spec_Check_Default_Output_Style : SPEC_CHECK_DEFAULT_OUTPUT_STYLE =
struct

open Spec_Check_Output_Style_Types
val default = Spec_Check_Output_Style_Custom.style

end
