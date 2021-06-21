\<^marker>\<open>creator "Kevin Kappelmann"\<close>
section \<open>SpecCheck\<close>
theory Spec_Check
imports
  Spec_Check_Generators
  Spec_Check_Show
  Spec_Check_Shrink
  Spec_Check_Output_Style
begin
paragraph \<open>Summary\<close>
text \<open>The SpecCheck (specification based) testing environment and Lecker testing framework.\<close>

ML_file \<open>spec_check.ML\<close>
ML_file \<open>lecker.ML\<close>

end