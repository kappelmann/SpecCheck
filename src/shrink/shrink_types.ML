(*  Title:      Tools/Spec_Check/show/show_types.ML
    Author:     Kevin Kappelmann

Shared type definitions for SpecCheck shrinkable types.
*)

signature SHRINK_TYPES =
sig
  type 'a shrink = 'a -> 'a Seq.seq
end

structure Shrink_Types : SHRINK_TYPES =
struct

type 'a shrink = 'a -> 'a Seq.seq

end
