# install.packages("usethis")
library(usethis)
#This is the package used for connecting R to Github
#I can also use the push and pull commands to edit on Github

library(tidyverse)
library(haven)
#wave 1
a_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_indresp_protect.dta")
a_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w1/a_child_protect.dta")

#wave 2
b_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w2/b_indresp_protect.dta")
b_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w2/b_child_protect.dta")
b_indall <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w2/b_indall_protect.dta")
b_egoalt <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w2/b_egoalt_protect.dta")

#wave 3
c_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w3/c_indresp_protect.dta")

#wave 4
d_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w4/d_indresp_protect.dta")

#wave 5
e_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w5/e_indresp_protect.dta")
e_newborn <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w5/e_newborn_protect.dta")
e_child <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w5/e_child_protect.dta")

#wave6
f_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w6/f_indresp_protect.dta")
f_newborn <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w6/f_newborn_protect.dta")

#wave 7
g_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w7/g_indresp_protect.dta")

#wave 8
h_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w8/h_indresp_protect.dta")

#wave 9
i_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w9/i_indresp_protect.dta")
i_hhresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w9/i_hhresp_protect.dta")

#wave 10
j_indresp <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_w10/j_indresp_protect.dta")

#xwavedata
xwave <- read_dta("S:/Questionnaires/UKHLS/Understanding_)Societyw1-9_SpecialLicence/ukhls_wx/xwavedat_protect.dta")

j_indresp %>% count(j_ch1bm)
xwave %>% count(ch1bm_dv)
xwave %>% 
  filter(ch1by_dv >= 2007, sex == 2) %>% 
  count(ch1by_dv)

j_indresp %>% count(j_lchmorn)

empfert3 <- select(c_indresp, pidp, c_sex, c_jbsamr, c_cjob, c_empchk, c_notempchk, c_nunmpsp_dv, c_nnewborn, c_finfut)
waves3 <- left_join(full_sample, empfert3, by= "pidp")

waves3 %>% count(c_nunmpsp_dv)
waves3 %>% count(c_nnewborn)
waves3 %>% count(c_finfut)

empfert5 <- select(e_indresp, pidp, e_sex, e_jbsamr, e_cjob, e_empchk, e_notempchk, e_nunmpsp_dv, e_nnewborn, e_finfut)
waves5 <- left_join(full_sample, empfert5, by= "pidp")

waves5 %>% count(e_nunmpsp_dv)
waves5 %>% count(e_nnewborn)
waves5 %>% count(e_finfut)

empfert6 <- select(f_indresp, pidp, f_sex, f_jbsamr, f_cjob, f_empchk, f_notempchk, f_nunmpsp_dv, f_nnewborn, f_finfut, f_jbsec)
waves6 <- left_join(full_sample, empfert6, by= "pidp")

waves6 %>% count(f_nunmpsp_dv)
waves6 %>% count(f_nnewborn)
waves6 %>% count(f_finfut)

empfert7 <- select(g_indresp, pidp, g_sex, g_jbsamr, g_cjob, g_empchk, g_notempchk, g_nunmpsp_dv, g_nnewborn, g_finfut)
waves7 <- left_join(full_sample, empfert7, by= "pidp")

waves7 %>% count(g_nunmpsp_dv)
waves7 %>% count(g_nnewborn)
waves7 %>% count(g_finfut)

empfert8 <- select(h_indresp, pidp, h_sex, h_jbsamr, h_cjob, h_empchk, h_notempchk, h_nunmpsp_dv, h_nnewborn, h_finfut, h_jbsec)
waves8 <- left_join(full_sample, empfert8, by= "pidp")

waves8 %>% count(h_nunmpsp_dv)
waves8 %>% count(h_nnewborn)
waves8 %>% count(h_finfut)

empfert9 <- select(i_indresp, pidp, i_sex, i_jbsamr, i_cjob, i_empchk, i_notempchk, i_nunmpsp_dv, i_nnewborn, i_finfut)
waves9 <- left_join(full_sample, empfert9, by= "pidp")

waves9 %>% count(i_nunmpsp_dv)
waves9 %>% count(i_nnewborn)
waves9 %>% count(i_sex)
waves9 %>% count(i_finfut)
