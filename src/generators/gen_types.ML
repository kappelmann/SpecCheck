(*  Title:      Tools/Spec_Check/generators/gen_types.ML
    Author:     Kevin Kappelmann

Shared type definitions for SpecCheck generators.
*)

signature GEN_TYPES =
sig

  (*consumes a state and returns a new state along with a generated value*)
  type ('a, 's) gen_state = 's -> 'a * 's
  (*'a gen consumes a random seed and returns an unused one along with a generated 'a value*)
  type 'a gen = ('a, Random.rand) gen_state

  (*a cogenerator produces new generators depending on an input element and an existing generator.*)
  type ('a, 'b, 's) cogen_state = 'a -> ('b, 's) gen_state -> ('b, 's) gen_state

  (*a cogenerator produces new generators depending on an input element and an existing generator.*)
  type ('a, 'b) cogen = ('a, 'b, Random.rand) cogen_state

end

structure Gen_Types : GEN_TYPES =
struct

type ('a, 's) gen_state = 's -> 'a * 's

type 'a gen = ('a, Random.rand) gen_state

type ('a, 'b, 's) cogen_state = 'a -> ('b, 's) gen_state -> ('b, 's) gen_state

type ('a, 'b) cogen = ('a, 'b, Random.rand) cogen_state

end
