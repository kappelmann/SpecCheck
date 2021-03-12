(*  Title:      Tools/Spec_Check/shrink/shrink_base.ML
    Author:     Kevin Kappelmann

Basic utility functions to create and combine shrink functions.
*)

signature SHRINK_BASE =
sig
  include SHRINK_TYPES

  val none : 'a shrink

  val product : 'a shrink -> 'b shrink -> ('a * 'b) shrink
  val product3 : 'a shrink -> 'b shrink -> 'c shrink -> ('a * 'b * 'c) shrink
  val product4 : 'a shrink -> 'b shrink -> 'c shrink -> 'd shrink -> ('a * 'b * 'c * 'd) shrink

  val int : int shrink

  val list : 'a shrink -> ('a list shrink)
  val list' : ('a list) shrink

  val term : term shrink

end

structure Shrink_Base : SHRINK_BASE =
struct
open Shrink_Types

fun none _ = []

fun product_list [] [] = []
  | product_list _ [] = []
  | product_list [] _ = []
  | product_list (x::xs) ys = map (pair x) ys @ product_list xs ys

fun product shrinkA shrinkB (a, b) =
  let
    val shrinkedA = shrinkA a
    val shrinkedB = shrinkB b
  in map (rpair b) shrinkedA @ map (pair a) shrinkedB @ product_list (shrinkA a) (shrinkB b) end

fun product3 shrinkA shrinkB shrinkC (a, b, c) =
  product shrinkA shrinkB (a, b)
  |> rpair (shrinkC c)
  |-> product_list
  |> map (fn ((a,b),c) => (a,b,c))

fun product4 shrinkA shrinkB shrinkC shrinkD (a, b, c, d) =
  product3 shrinkA shrinkB shrinkC (a, b, c)
  |> rpair (shrinkD d)
  |-> product_list
  |> map (fn ((a,b,c),d) => (a,b,c,d))

fun int i =
  let
    fun pred i s = (i <> 0, s)
    fun next i = let val half = Int.quot (i, 2) in (half, half) end
  in 0 :: (Generator.unfold_while pred next i |> rev o fst) end

(*Note: shrinking with large lists and element shrinkers will explode very quickly*)
fun list _ [] = []
  | list elem_shrink (x::xs) =
    let val elems = elem_shrink x @ [x]
    in  [] :: flat (map (fn xs => map (fn x => x :: xs) elems) (list elem_shrink xs)) end

fun list' xs = list none xs

fun term t =
  case t of 
    t1 $ t2 => let val prod = product term term (t1, t2)
      in t1 :: t2 :: flat (map (fn (t1, t2) => [t1, t2, t1 $ t2]) prod) end
  | Abs (x, T, t) => t :: flat (map (fn t => [t, Abs (x, T, t)]) (term t))
  | _ => []

end
