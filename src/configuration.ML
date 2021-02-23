signature CONFIGURATION =
sig

  (* Maximum number of successful tests before succeeding *)
  val max_success : int Config.T
  (* Maximum number of discarded tests per successful test before giving up *)
  val max_discard_ratio : int Config.T
  (* Number of counterexamples shown *)
  val num_counterexamples : int Config.T
  (* Sort counterexamples by size *)
  val sort_counterexamples : bool Config.T
  (* Print timing etc. depending on style *)
  val show_stats : bool Config.T
  val column_width : int Config.T
  val style : string Config.T
  
end

structure Configuration : CONFIGURATION =
struct

val max_success = Attrib.setup_config_int \<^binding>\<open>spec_check_max_success\<close> (K 100)

val max_discard_ratio = Attrib.setup_config_int \<^binding>\<open>spec_check_max_discard_ratio\<close> (K 10)

val num_counterexamples = Attrib.setup_config_int \<^binding>\<open>spec_check_num_counterexamples\<close> (K 1)

val sort_counterexamples =
  Attrib.setup_config_bool \<^binding>\<open>spec_check_sort_counterexamples\<close> (K true)

val show_stats = Attrib.setup_config_bool \<^binding>\<open>spec_check_show_stats\<close> (K true)

val column_width = Attrib.setup_config_int \<^binding>\<open>spec_check_column_width\<close> (K 22)

val style = Attrib.setup_config_string \<^binding>\<open>spec_check_style\<close> (K "Custom")

end
