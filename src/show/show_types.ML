(*  Title:      Tools/Spec_Check/show/show_types.ML
    Author:     Kevin Kappelmann

Shared type definitions for SpecCheck showable types.
*)

signature SHOW_TYPES =
sig
  type 'a show = 'a -> string
end

structure Show_Types : SHOW_TYPES =
struct

type 'a show = 'a -> string

end
