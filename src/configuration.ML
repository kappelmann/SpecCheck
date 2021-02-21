signature CONFIGURATION =
sig

  (* Number of generated inputs for test *)
  val gen_target : int Config.T
  (* Maximum number of discarded tests per successful test before giving up *)
  val max_discard_ratio : int Config.T
  (* Number of counterexamples shown *)
  val examples : int Config.T
  (* Sort counter examples by size *)
  val sort_examples : bool Config.T
  (* Print timing etc. depending on style *)
  val show_stats : bool Config.T
  val column_width : int Config.T
  val style : string Config.T
  
end

structure Configuration : CONFIGURATION =
struct

val gen_target = Attrib.setup_config_int \<^binding>\<open>spec_check_gen_target\<close> (K 100)

val max_discard_ratio = Attrib.setup_config_int \<^binding>\<open>spec_check_max_discard_ratio\<close> (K 10)

val examples = Attrib.setup_config_int \<^binding>\<open>spec_check_examples\<close> (K 5)

val sort_examples = Attrib.setup_config_bool \<^binding>\<open>spec_check_sort_examples\<close> (K true)

val show_stats = Attrib.setup_config_bool \<^binding>\<open>spec_check_show_stats\<close> (K true)

val column_width = Attrib.setup_config_int \<^binding>\<open>spec_check_column_width\<close> (K 22)

val style = Attrib.setup_config_string \<^binding>\<open>spec_check_style\<close> (K "Custom")

end