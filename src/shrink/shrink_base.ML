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

fun list _ [] = []
  | list elem_shrink [x] = [] :: map single (elem_shrink x)
  | list elem_shrink (x::xs) =
      [] :: flat (map (fn xs => map (fn x => x :: xs) (elem_shrink x @ [x])) (list elem_shrink xs))

fun list' xs = list none xs

(*TODO: improve this shrinker*)
fun term t =
  case t of 
    t1 $ t2 => t1 :: t2 :: (term t1 @ term t2)
  | Abs (_,_,t) => term t
  | _ => []

end
