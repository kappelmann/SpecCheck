(*  Title:      Tools/Spec_Check/output_styles/output_style.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Output styles for presenting Spec_Check's results.
*)

signature OUTPUT_STYLE =
sig
  include OUTPUT_STYLE_TYPES
  val default : 'a output_style
end

structure Output_Style : OUTPUT_STYLE =
struct

open Output_Style_Types
val default = Output_Style_Custom.style

end
