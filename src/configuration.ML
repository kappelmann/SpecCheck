signature CONFIGURATION =
sig
  val gen_target : int Config.T
  val gen_max : int Config.T
  val examples : int Config.T
  val sort_examples : bool Config.T
  val show_stats : bool Config.T
  val column_width : int Config.T
  val style : string Config.T
end

structure Configuration =
struct
(* Number of generated inputs for test *)
val gen_target = Attrib.setup_config_int \<^binding>\<open>spec_check_gen_target\<close> (K 100)
(* How many inputs should be generated when some inputs are discarded because of properties *)
val gen_max = Attrib.setup_config_int \<^binding>\<open>spec_check_gen_max\<close> (K 1000)
(* Number of counterexamples shown *)
val examples = Attrib.setup_config_int \<^binding>\<open>spec_check_examples\<close> (K 5)

(* Sort counter examples by default *)
val sort_examples = Attrib.setup_config_bool \<^binding>\<open>spec_check_sort_examples\<close> (K true)
val show_stats = Attrib.setup_config_bool \<^binding>\<open>spec_check_show_stats\<close> (K true)
val column_width = Attrib.setup_config_int \<^binding>\<open>spec_check_column_width\<close> (K 22)
val style = Attrib.setup_config_string \<^binding>\<open>spec_check_style\<close> (K "Perl")
end