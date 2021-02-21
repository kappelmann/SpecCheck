(*  Title:      Tools/Spec_Check/base_types.ML
    Author:     Kevin Kappelmann

Shared type definitions for SpecCheck. 
*)

signature GEN_TYPES =
sig
  (* 'a gen consumes a random seed and returns an unused one along with a generated 'a value *)
  type 'a gen = Random.rand -> 'a * Random.rand
  
  (* ('a,'b) genS, when compared to `gen`, also consumes a state and returns a modified state.
  TODO Sebastian: Currently almost no usage.
  Theoretically useful for functions that accept stateful gens.
  More lightweight than sequences from Seq. *)
  type ('a, 's) genS = ('s * Random.rand) -> 'a * ('s * Random.rand)
  
  (* a cogenerator produces new generators depending on an input element and an existing generator.  *)
  type ('a, 'b) cogen = 'a -> 'b gen -> 'b gen

end

structure Gen_Types : GEN_TYPES =
struct

type 'a gen = Random.rand -> 'a * Random.rand

type ('a,'b) genS = ('b * Random.rand) -> 'a * ('b * Random.rand)

type ('a, 'b) cogen = 'a -> 'b gen -> 'b gen

end
