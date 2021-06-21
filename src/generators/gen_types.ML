(*  Title:      Tools/Spec_Check/generators/gen_types.ML
    Author:     Kevin Kappelmann

Shared type definitions for SpecCheck generators.
*)

signature SPEC_CHECK_GEN_TYPES =
sig

  (*consumes a state and returns a new state along with a generated value*)
  type ('a, 's) gen_state = 's -> 'a * 's
  (*consumes a random seed and returns an unused one along with a generated value*)
  type 'a gen = ('a, Spec_Check_Random.rand) gen_state

  (*a cogenerator produces new generators depending on an input element and an existing generator.*)
  type ('a, 'b, 's) cogen_state = 'a -> ('b, 's) gen_state -> ('b, 's) gen_state

  (*a cogenerator produces new generators depending on an input element and an existing generator.*)
  type ('a, 'b) cogen = ('a, 'b, Spec_Check_Random.rand) cogen_state

end

structure Spec_Check_Gen_Types : SPEC_CHECK_GEN_TYPES =
struct

type ('a, 's) gen_state = 's -> 'a * 's

type 'a gen = ('a, Spec_Check_Random.rand) gen_state

type ('a, 'b, 's) cogen_state = 'a -> ('b, 's) gen_state -> ('b, 's) gen_state

type ('a, 'b) cogen = ('a, 'b, Spec_Check_Random.rand) cogen_state

end
