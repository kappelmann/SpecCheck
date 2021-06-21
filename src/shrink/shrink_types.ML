(*  Title:      Tools/Spec_Check/shrink/shrink_types.ML
    Author:     Kevin Kappelmann

Shared type definitions for SpecCheck shrinkable types.
*)

signature SPEC_CHECK_SHRINK_TYPES =
sig
  type 'a shrink = 'a -> 'a Seq.seq
end

structure Spec_Check_Shrink_Types : SPEC_CHECK_SHRINK_TYPES =
struct

type 'a shrink = 'a -> 'a Seq.seq

end
