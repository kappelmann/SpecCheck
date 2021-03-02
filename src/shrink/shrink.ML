(* Structure containing all shrink functions. *)
structure Shrink =
struct
open Shrink_Types

fun none _ = []

fun term t =
  case t of 
    t1 $ t2 => t1 :: t2 :: (term t1 @ term t2)
  | Abs (_,_,t) => t :: term t
  | _ => []


end