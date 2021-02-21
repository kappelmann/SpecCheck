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
    fun apply_show xs =
      case show of
        SOME f => Util.Left (map f xs)
      | NONE => Util.Right (length xs)
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
        (* Always try the first successful branch, abort without backtracking once no further shrink is possible. *)
        case find_first is_failure (shrink obj) of
          SOME obj => try_shrink obj
        | NONE => obj
      end

    exception EarlyReturn of Property.stats * 'a list
    fun iter obj (stats, badobjs) =
      (* Target fulfilled, skip the rest and avoid traversing the whole lazy sequence *)
      if #count stats >= Config.get ctxt Configuration.gen_target
      then raise EarlyReturn (stats, badobjs)
      else
      (* Test obj, if failure then attempt to shrink *)
        let
          val (result, stats) = test_input obj stats
          val badobjs =
            case result of
              Result true => badobjs
            | Discard => badobjs
            | Result false => try_shrink obj :: badobjs
            | Exception exn =>
                (warning ("Exception raised during test: " ^ name);
                 warning (@{make_string} exn);
                 raise EarlyReturn (stats, obj :: badobjs))
        in (stats, badobjs) end
  in
  (* TODO Perhaps use ML_Profiling.profile_time to get more accurate results? Far slower *)
    Timing.timing (fn () =>
      (Util.fold_seq iter seq (stats_init, []) handle EarlyReturn x => x)
      ||> apply_show) ()
    |-> print_result
  end

(* Limit is set here instead of in the check function above because only randomly generated values
require a limit *)
fun limit ctxt g =
  let
    val steps = Config.get ctxt Configuration.gen_target *
      Config.get ctxt Configuration.max_discard_ratio
    fun opt_g i = if i >= steps then Generator.lift NONE else Generator.map SOME g
  in fst o Generator.seq opt_g end

fun check_gen ctxt name gen show prop r =
  check ctxt (Property.empty_stats true) name show (limit ctxt gen r) (fn _ => []) prop

fun check_gen_shrink ctxt name gen show shrink prop r =
  check ctxt (Property.empty_stats true) name show (limit ctxt gen r) shrink prop

fun check_seq ctxt name seq show shrink prop  =
  check ctxt (Property.empty_stats false) name show seq shrink prop

end;

