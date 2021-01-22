(*  Title:      Tools/Spec_Check/spec_check.ML
    Author:     Lukas Bulwahn and Nicolai Schaffroth, TU Muenchen
    Author:     Christopher League

Specification-based testing of ML programs with random values.
*)

signature SPEC_CHECK =
sig
  include CONFIGURATION
  val check_gen : Proof.context -> string ->
                  'a Generator.gen -> ('a -> string) option ->
                  'a Property.prop -> Random.rand -> unit
  val check_gen_shrink : Proof.context -> string ->
                         'a Generator.gen -> ('a -> string) option -> ('a -> 'a list) ->
                         'a Property.prop -> Random.rand -> unit
  val check_seq : Proof.context -> string ->
                         'a Seq.seq -> ('a -> string) option -> ('a -> 'a list) ->
                         'a Property.prop -> unit
end;

structure Spec_Check : SPEC_CHECK =
struct
open Configuration
open Property

(* testing functions *)
fun check ctxt stats_init name show seq shrink prop =
  let
    fun apply_show x = case show of NONE => NONE | SOME f => SOME (f x)
    val print_result = Output_Style.get_style ctxt name
    fun test_input input stats = Property.test prop (input, stats)

    fun try_shrink obj =
      let
        fun is_failure input =
          case test_input input (empty_stats true) |> fst of
            Result true => false
          | Discard => false
          | Result false => true
          | Exception exn => (warning "Exception raised during test:"; Runtime.exn_trace (fn () => raise exn))
      in
        (* Quasi Depth-first search, return once end of current branch is reached *)
        case find_first is_failure (shrink obj) of
          SOME obj => try_shrink obj
        | NONE => obj
      end

    exception EarlyReturn of Property.stats * 'a list
    fun iter obj (stats, badobjs) =
      (* Target fulfilled, skip the rest *)
      if #count stats >= Config.get ctxt Configuration.gen_target
      then raise EarlyReturn (stats, badobjs) (* Avoid traversing the whole lazy sequence *)
      else
      (* Test obj, if failure then attempt to shrink *)
        let
          val (result, stats) = test_input obj stats
          val badobjs =
            case result of
              Result true => badobjs
            | Discard => badobjs
            | Result false => try_shrink obj :: badobjs
            | Exception exn => (warning ("Exception raised during test: " ^ name); Runtime.exn_trace (fn () => Exn.reraise exn))
        in (stats, badobjs) end
  in
    Timing.timing (fn () =>
      (Generator.fold_seq (~1) iter seq (stats_init, []) handle EarlyReturn x => x)
      ||> map apply_show) ()
    |-> print_result
  end          

(* Limit is set here instead of in the check function above because only randomly generated values require a limit *)
fun limit ctxt gen =
  Generator.seq_of_gen
    (Config.get ctxt Configuration.gen_target * Config.get ctxt Configuration.max_discard_ratio)
    gen

fun check_gen ctxt name gen show prop r =
  check ctxt (Property.empty_stats true) name show (limit ctxt gen r) (fn _ => [])
    prop

fun check_gen_shrink ctxt name gen show shrink prop r =
  check ctxt (Property.empty_stats true) name show (limit ctxt gen r) shrink
    prop

fun check_seq ctxt name seq show shrink prop  =
  check ctxt (Property.empty_stats false) name show seq shrink prop

end;

