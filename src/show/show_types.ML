(*  Title:      Tools/Spec_Check/show/show_types.ML
    Author:     Kevin Kappelmann

Shared type definitions for SpecCheck showable types.
*)

signature SPEC_CHECK_SHOW_TYPES =
sig
  type 'a show = 'a -> Pretty.T
end

structure Spec_Check_Show_Types : SPEC_CHECK_SHOW_TYPES =
struct

type 'a show = 'a -> Pretty.T

end
