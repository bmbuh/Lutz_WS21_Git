# Created 07/01/2021

library(tidyverse)
library(Hmisc)
# install.packages("rms")
library(rms)
# install.packages("stargazer")
library(stargazer)
# install.packages("MASS")
library(MASS)
# install.packages("arsenal")
library(arsenal)
# install.packages("ggalluvial")
library(ggalluvial)

# Creating the data set ---------------------------------------------------
# My dependent variable is the change in number of intended children between waves
# 5 and 10. This is calculated by using the variables e_lchmorn - j_lchmorn 
# controlling for NA in wave 10. This is only for women of child bearing age in 
# wave 5

#wave 1 : base characteristics
# a_fert_fin <- dplyr::select(a_indresp, pidp, a_mlstat)
# a_fert_fin %>% count(a_mlstat)
# xwavefert <- dplyr::select(xwave, pidp, evermar_dv)
# 
# a_fert_fin <- 
#   left_join(a_fert_fin, xwavefert, by = "pidp")

#Wave 5 : this is to dplyr::select my sample: age < 45, sex = F, 
e_fert_fin <- dplyr::select(e_indresp, pidp, e_sex, e_dvage, e_finfut, e_lchmor, e_lchmorn, e_nnatch, e_marstat, e_jbhas) %>% 
  filter(e_dvage <=45, e_sex == 2, !is.na(e_lchmorn))%>% 
  mutate(e_finfut = ifelse(e_finfut == 2, 0, 1))%>% 
  mutate(e_jbhas = ifelse(e_jbhas == 1, 1, 0))

e_fert_fin %>% 
  count(e_lchmorn)


#Wave 10
j_fert_fin <- dplyr::select(j_indresp, pidp, j_finfut, j_lchmorn, j_nnatch, j_nnewborn, j_dvage, j_qfhigh_dv, j_cohab_dv, j_marstat, j_marstat_dv, j_jbsec) %>% 
  mutate(j_lchmorn = ifelse(j_lchmorn <= 0, 0, j_lchmorn))

j_fert_fin %>% 
  count(j_lchmorn)

#Combine the two waves
fert_int <- 
  left_join(e_fert_fin, j_fert_fin, by = "pidp") %>% 
  # left_join(.,a_fert_fin, by = "pidp") %>% 
  mutate(e_lchmorn = ifelse(e_lchmorn < 0, NA, e_lchmorn )) %>% 
  mutate(j_lchmorn = ifelse(j_lchmorn < 0, NA, j_lchmorn )) %>% 
  mutate(intchange = j_lchmorn - e_lchmorn) %>% 
  filter(!is.na(intchange)) %>% 
  mutate(childborn = j_nnatch - e_nnatch) %>% 
  mutate(childborn = ifelse(childborn < 0, 0, childborn)) %>% 
  mutate(childborn = ifelse(childborn > 1, 0, childborn)) %>%
  mutate(e_marstat = ifelse(e_marstat == 3 , 2, e_marstat)) %>% 
  mutate(married = ifelse(e_marstat == 2, "married", "unmarried"))%>% 
  mutate(j_marstat = ifelse(j_marstat == 3 , 2, j_marstat)) %>% 
  mutate(married2 = ifelse(j_marstat == 2, "married", "unmarried")) %>% 
  unite(marchng, married, married2, sep = "-", remove = FALSE) %>% 
  mutate(marchng = ifelse(marchng == "unmarried-married", "got_married", ifelse(marchng == "married-married", "stay_married", ifelse(marchng == "unmarried-unmarried", "stay_single", "got_divorced")))) %>% 
  mutate(maryes = ifelse(j_marstat_dv == 1, 1, 0))

fert_int %>% 
  count(intchange)

#create the categorical indep. variable by creating new variable dep.intchange         
dep_fert_int <- fert_int %>% 
  dplyr::select(pidp, intchange) %>% 
  mutate(dep.intchange1 = ifelse(intchange < 0, 1 , ifelse(intchange == 0, 2, 3))) %>% #ordered so "less" comes first (1 = less, 2 = same, 3 = more)
  mutate(dep.intchange2 = ifelse(intchange < 0, 5 , ifelse(intchange == 0, 4, 6))) %>% #ordered so "same" comes first (4 = same, 5 = less, 6 = more)
  mutate(dep.intchange3 = ifelse(intchange < 0, 8 , ifelse(intchange == 0, 9, 7))) %>% #ordered so "more" comes first (7 = more, 8 = less, 9 = same)
  dplyr::select(-intchange)


#code to create new variable based on ISCED educational attainment levels split into high/medium/low  
fert_int <- fert_int %>% 
  mutate(j_qfhigh_dv = ifelse(j_qfhigh_dv < -1, NA, j_qfhigh_dv )) %>% 
  mutate(j_qfhigh_dv = ifelse(j_qfhigh_dv == 96 , NA, j_qfhigh_dv ))%>% 
  mutate(isced = case_when(
  j_qfhigh_dv <= 4 ~ "high",
  j_qfhigh_dv <= 14 & j_qfhigh_dv >=5 & j_qfhigh_dv != 12 & j_qfhigh_dv != 13 ~ "medium",
  j_qfhigh_dv >=12 & j_qfhigh_dv <= 13 ~ "low")) %>% 
  mutate(isced = fct_relevel(isced, c("high", "medium", "low"))) # this is in order to keep graphs in the correct high, medium, low order

fert_int %>% count(j_finfut, e_finfut)
fert_int %>% count(j_cohab_dv)
fert_int %>% count(j_cohab_dv, marchng)



# Fin Fut Waves ---------------------------------------------------------
#Take out the variables of financial future certainty for each wave and add them to the data frame fert_int
#Wave 5

#Wave 6
f_fert_fin <- dplyr::select(f_indresp, pidp, f_finfut, f_jbhas, f_jbsec) %>% 
  mutate(f_finfut = ifelse(f_finfut == 2, 0, 1)) %>% 
  mutate(f_jbhas = ifelse(f_jbhas == 1, 1, 0))

#Wave 7
g_fert_fin <- dplyr::select(g_indresp, pidp, g_finfut, g_jbhas)%>% 
  mutate(g_finfut = ifelse(g_finfut == 2, 0, 1))%>% 
  mutate(g_jbhas = ifelse(g_jbhas == 1, 1, 0))

#Wave 8
h_fert_fin <- dplyr::select(h_indresp, pidp, h_finfut, h_jbhas, h_jbsec)%>% 
  mutate(h_finfut = ifelse(h_finfut == 2, 0, 1))%>% 
  mutate(h_jbhas = ifelse(h_jbhas == 1, 1, 0))

#Wave 9
i_fert_fin_hh <-  
  dplyr::select(i_hhresp, i_hidp, i_fihhmnnet4_dv)

i_fert_fin <- dplyr::select(i_indresp, pidp, i_hidp, i_finfut, i_jbhas)%>% 
  mutate(i_finfut = ifelse(i_finfut == 2, 0, 1))%>% 
  mutate(i_jbhas = ifelse(i_jbhas == 1, 1, 0)) 

i_fert_fin <- 
  left_join(i_fert_fin, i_fert_fin_hh, by = "i_hidp") %>% 
  dplyr::select(-i_hidp) %>% 
  rename("mn_inc" = "i_fihhmnnet4_dv")

mn_inc <- 
  dplyr::select(i_fert_fin, pidp, mn_inc)

#This step adds together multiple waves in order to create a ratio of years spent with a positive financial outlook
finfut <- 
  left_join(e_fert_fin, f_fert_fin, by = "pidp") %>% 
  left_join(.,g_fert_fin, by = "pidp") %>% 
  left_join(.,h_fert_fin, by = "pidp") %>% 
  left_join(.,i_fert_fin, by = "pidp") %>%
  left_join(.,j_fert_fin, by = "pidp") %>% 
  dplyr::select(pidp, e_finfut, f_finfut, g_finfut, h_finfut, i_finfut) %>% 
  mutate(sum = rowSums(.[2:6], na.rm = TRUE), #This creates a sum of all the years with positive or negative outlook
         mean = rowMeans(.[2:6], na.rm = TRUE)) %>% #This creates a ratio of years spent with a positive outlook
  rename("ratio.finfut" = "mean") %>% 
  dplyr::select(pidp, ratio.finfut)

#This step adds a ratio for time spent employed versus not employed
jbhas <- 
  left_join(e_fert_fin, f_fert_fin, by = "pidp") %>% 
  left_join(.,g_fert_fin, by = "pidp") %>% 
  left_join(.,h_fert_fin, by = "pidp") %>% 
  left_join(.,i_fert_fin, by = "pidp") %>%
  left_join(.,j_fert_fin, by = "pidp") %>% 
  dplyr::select(pidp, e_jbhas, f_jbhas, g_jbhas, h_jbhas, i_jbhas) %>% 
  mutate(sum = rowSums(.[2:6], na.rm = TRUE), #This creates a sum of all the years with positive or negative outlook
         mean = rowMeans(.[2:6], na.rm = TRUE)) %>% #This creates a ratio of years spent with a positive outlook
  rename("ratio.jbhas" = "mean")%>% 
  dplyr::select(pidp, ratio.jbhas)

#to add variables about perceived job security
# The question is "How likely are you to lose your job in the next 12 months?"
# "Very likely" and "likely" are recoded as 1 and "Unlikely" and "Very Unlikely' are recoded a 0
jbsec <- 
  left_join(f_fert_fin, h_fert_fin, by = "pidp") %>% 
  left_join(., j_fert_fin, by = "pidp") %>% 
  dplyr::select(pidp, f_jbsec, h_jbsec, j_jbsec) %>% 
  mutate(f_jbsec = ifelse(f_jbsec < 0, NA, ifelse(f_jbsec <= 2, 1, 0))) %>% 
  mutate(h_jbsec = ifelse(h_jbsec < 0, NA, ifelse(h_jbsec <= 2, 1, 0))) %>% 
  mutate(j_jbsec = ifelse(j_jbsec < 0, NA, ifelse(j_jbsec <= 2, 1, 0)))


fert_int <- 
  left_join(fert_int, finfut, by = "pidp") %>% 
  left_join(., jbhas, by = "pidp") %>% 
  left_join(., dep_fert_int, by = "pidp") %>% 
  left_join(., jbsec, by = "pidp") %>% 
  left_join(., mn_inc, by = "pidp") %>% 
  dplyr::select(-j_jbsec.x)

fert_int %>% count(intchange)

# ordinal logisitc regression -----------------------------------------

remove(mlr_fert_int)

#final cleaned data for the ordinal logistic regression
mlr_fert_int <- fert_int %>% 
  dplyr::select(pidp, intchange, dep.intchange1, dep.intchange2, dep.intchange3, ratio.jbhas, ratio.finfut, isced, childborn, marchng, e_dvage, mn_inc, f_jbsec, h_jbsec, j_jbsec.y, j_cohab_dv, e_lchmorn, j_lchmorn) %>% 
  mutate(childborn = as_factor(childborn)) %>% 
  mutate(ID = row_number()) %>% 
  mutate(marchng = as_factor(marchng)) %>% 
  mutate(dep.intchange1 = as.factor(dep.intchange1)) %>% 
  mutate(dep.intchange2 = as.factor(dep.intchange2)) %>% 
  mutate(dep.intchange3 = as.factor(dep.intchange3)) %>% 
  mutate(e_dvage = as.integer(e_dvage)) %>% 
  mutate(mn_inc = as.numeric(mn_inc)) %>% 
  filter(!is.na(mn_inc)) %>% 
  mutate(isced = as.character(isced)) %>% 
  mutate(marchng = fct_relevel(marchng, c("got_divorced", "stay_single", "stay_married", "got_married"))) %>% 
  mutate(childborn = recode(childborn, "0" = "No", "1" = "Yes")) %>% 
  mutate(marchng = recode(marchng, "got_divorced" = "Got Divorced", "stay_single" =  "Stayed single", 
                          "stay_married" =  "Stayed married", "got_married" = "Got Married")) %>%
  mutate(dep.intchange1 = recode(dep.intchange1, "1" = "Less intended", "2" = "Same intended", "3" = "More intended")) %>% 
  mutate(isced = fct_relevel(isced, c("low", "medium", "high"))) %>% 
  mutate(e_lchmorn = as.factor(e_lchmorn)) %>% 
  mutate(e_lchmorn = recode(e_lchmorn, "5" = "5+", "7" = "5+", "8" = "5+", "10" = "5+")) %>% 
  mutate(e_lchmorn = fct_relevel(e_lchmorn, c("1", "2", "3", "4", "5+"))) %>% 
  mutate(isced = recode(isced, "low" = "Low education", "medium" = "Medium education", "high" = "High education"))

  str(mlr_fert_int)


# mlr_fert_int_labels <- c(pidp = "pidp", marchng = "Change in Marriage Status", e_dvage = "Age", childborn = "Had a child",
               # ratio.jbhas = "Ratio of waves spent employed", ratio.finfut =  "Ratio waves with positive financial outlook")

mycontrols <- tableby.control(test = FALSE)
dsmlr <-arsenal::tableby(dep.intchange1 ~ childborn + e_dvage + ratio.jbhas + ratio.finfut + marchng + isced, data = mlr_fert_int, control = mycontrols)
labels(dsmlr) <-  c(marchng = "Change in Marriage Status", e_dvage = "Age at Wave 5", childborn = "Had a child",
                    ratio.jbhas = "Ratio of waves spent employed", ratio.finfut =  "Ratio waves with positive financial outlook", isced = "Educational attainment")
summary(dsmlr)
write2word(dsmlr, "dsmlr.doc")

mult_reg <- lm(intchange ~ childborn + ratio.jbhas + ratio.finfut + e_dvage + marchng + mn_inc, data = mlr_fert_int)
print(mult_reg)
stargazer(mult_reg,
          title = "Multiple regression analysis",
          dep.var.caption = "Change in number of intended children: Wave 5 - 10",
          dep.var.labels = c("Number intended"),
          column.labels = c(),
          covariate.labels = c("Had a child", "Ratio of waves spent employed", "Ratio waves with positive financial outlook",
                               "Age", "Stayed single", "Stayed married", "Got Married", "Medium education", "High education", "Monthly income"),
          notes.label = "Significance levels",
          type = "html",
          out = "mult_reg.doc")


str(mlr_fert_int)
  # xtab(~dep.intchange + marchng, mlr_fert_int)

#Ordinal logistic regression
## Dep: Less, Same, or More intended children between W5-W10
##Ind: Ratio of time spent in employment, Ratio of waves spent with positive outlook, age, married changes status
print(ologit <- lrm(dep.intchange1 ~ childborn + ratio.jbhas + ratio.finfut + e_dvage + marchng + , data = mlr_fert_int)) #ref: less int.num
print(ologit2 <- lrm(dep.intchange2 ~ childborn + ratio.jbhas + ratio.finfut + e_dvage + marchng, data = mlr_fert_int))#ref: same int.num
print(ologit3 <- lrm(dep.intchange3 ~ childborn + ratio.jbhas + ratio.finfut + e_dvage + marchng, data = mlr_fert_int))
# print(ologit)
# summary(ologit)
# ologit2 <- lrm(dep.intchange2 ~ childborn + ratio.jbhas + ratio.finfut + e_dvage + marchng, data = mlr_fert_int, rel)
# print(ologit2)

summary(olr.model.1 <- polr(dep.intchange1 ~ childborn + ratio.jbhas + ratio.finfut + e_dvage + marchng + isced,
                            data = mlr_fert_int, method = "logistic" ))
summary(olr.model.2 <- polr(dep.intchange2 ~ childborn + ratio.jbhas + ratio.finfut + e_dvage + marchng + isced, #+ j_cohab_dv, #+ f_jbsec + h_jbsec + j_jbsec.y,
                            data = mlr_fert_int, method = "logistic" ))
summary(olr.model.3 <- polr(dep.intchange3 ~ childborn + ratio.jbhas + ratio.finfut + e_dvage + marchng + isced, #+ j_cohab_dv, #+ f_jbsec+ h_jbsec + j_jbsec.y,
                            data = mlr_fert_int, method = "logistic" ))

#for whatever reason the mn_inc variable won't work
#summary(olr.model.4 <- polr(dep.intchange1 ~ childborn + ratio.jbhas + ratio.finfut + e_dvage + marchng + isced + mn_inc, 
                            # data = mlr_fert_int, method = "logistic" ))


stargazer(olr.model.1, olr.model.2, olr.model.3,
          title = "Ordinal logistic regression",
          dep.var.caption = "Change in number of intended children: Wave 5 - 10",
          dep.var.labels = c("Less intended", "Same intended", "More intended"),
          column.labels = c(),
          covariate.labels = c("Had a child", "Ratio of waves spent employed", "Ratio waves with positive financial outlook",
                               "Age", "Stayed single", "Stayed married", "Got Married", "Medium education", "High education"),
          notes.label = "Significance levels",
          type = "html",
          out = "test_stargazer.doc")

# fitted <- predict(ologit, newdata = mlr_fert_int, type = "fitted.ind")
# colMeans(fitted)

# mlr_fert_int %>% 
#   count(ratio.jbhas)
# 
# head(mlr_fert_int)
# 
# mlrdata <- mlogit.data(mlr_fert_int, choice = "dep.intchange", shape = "wide")
# mlrdata[1:20,]
# 
# mlogit.model1 <- mlogit(dep.intchange ~ 1| childborn + marchng + isced + ratio.jbhas + ratio.finfut + e_dvage, data = mlrdata, reflevel = "more")
# summary(mlogit.model1)
# exp(coef(mlogit.model1))
# 
# mlogit.model2 <- mlogit(dep.intchange ~ 1 | ratio.finfut, data = mlrdata, reflevel = "same")
# summary(mlogit.model2)
# 
# exp(coef(mlogit.model1))


# graphs ------------------------------------------------------------------

data.frame%>% 
  group_by(childborn) %>% 
  ggplot(aes(y = ID, axis1 = e_lchmorn, axis2 = dep.intchange1)) +
  geom_alluvium(aes(fill = childborn), width = 1/12)+
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Num Intended W5", "Change in W10"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme(aspect.ratio = 1) +
  labs(fill = "Child born") +
  ylab("Quantity") +
  ggtitle("Changes in number of intended children")  +
  ggsave("lutz_alluvial_intention_change.png")

data.frame <- mlr_fert_int %>% 
  count(e_lchmorn, dep.intchange1, childborn) %>% mutate(ID = row_number()) %>% 
  mutate(dep.intchange1 = recode(dep.intchange1, "Less intended" = "Less", "Same intended" = "Same", "More intended" = "More"))
  
  
#graph of education and partnership status changes in the data  
fert_int %>% 
  filter(!is.na(marchng), !is.na(isced)) %>% 
  ggplot(aes(x=marchng, fill = isced)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Educational Attainment terciles") +
  xlab("Marriage Status between wave 5 and 10") + 
  ylab("Quantity") +
  ggtitle("Relationship between partnership change and education", subtitle = "Women with fertility intentions") +
  ggsave("lutz_edu_marchng.png")

ggplot(data = mlr_fert_int) +
  geom_bar(mapping = aes(x=e_dvage, fill = e_lchmorn)) +
  scale_fill_brewer(palette = "Dark2") +
  theme(aspect.ratio = 1) +
  labs(fill = "Num Intend") +
  xlab("Age") + 
  ylab("Population size")+
  ggtitle("Age structure, by number of intended children", subtitle =  "Wave 5") +
  ggsave("lutz_age_lchmorn.png")
