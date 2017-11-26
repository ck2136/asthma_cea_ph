#HEEMOD TUTORIAL FILE

## USING THE ASTHMA MODEL WITH ASTHMA CONTROL AS STATES

## Couple of Options that will allow user to feel comfortable 
#1. Show Survival Plots of Original Study that is being used...
#2. Allow for check on AIC for different distributions

rm(list=ls())
library(heemod)
library(ggplot2) # for plotting
library(parallel)

# define model parameters

par_mod <- define_parameters(
  age_base = 36,
  age_cycle = model_time + age_base # age of individuals given a cycle is going to be the age at baseline + the time it takes for the model
)

# add the mortality table (all cause mortality for US)
par_mod <- modify(
  par_mod,
  
  sex_indiv = "MLE", # MLE => male in the WHO database
  p_death_all = get_who_mr(
    age = age_cycle,
    sex = sex_indiv,
    country = "USA",
    local = TRUE
  )
)

# death probability defined as funciton of age and sex from the WHO database


# common probabilities across both controlled an uncontrolled group

par_mod <- modify(
  par_mod,
  
  #prob con/uncon
  p_con_PH = 0.37418,
  p_con_UC = 0.025496,
  p_unc_PH = 0.007121,
  p_unc_UC = 0.005768,
  
  #mortality that is common across everyone 
  p_asthma_related_mortality_uc = 0.00552,
  p_asthma_related_mortality_c = 0.0001534,
  p_add_risk_death_hosp = 0.01165, 
  
  
  #exacerbations controlled
  p_oc_burst_uc = 0.10005,
  p_er_visit_uc = 0.0057,
  p_hosp_uc = 0.0023,
  
  #exacerbations uncontrolled
  p_oc_burst_c = 0.00963,
  p_er_visit_c = 0.0023,
  p_hosp_c = 0.0023
)


par_mod <- modify(
  par_mod,
  
  cost_noex = 113,
  cost_oc = 147,
  cost_er = 673,
  cost_hosp = 11209,
  cost_ph1 = 100,
  cost_ph1_cycle = ifelse(state_time == 1,
                          cost_ph1,
                          0),
  
  cost_ph_rec = 25
  
)

# other parameters such as utilities and discount rate

par_mod <- modify(
  par_mod,
  
  dr = 0.03,
  utility_ne = 0.67,
  utility_oc = 0.57,
  utility_er = 0.45,
  utility_hosp = 0.33
)



# Transition probabilities

#Transition probabilities for PH+UC

mat_ph <- define_transition(
  state_names = c("UC_NE","UC_OC","UC_HOSP","UC_ER","UC_OD","UC_AD","C_NE","C_OC","C_HOSP","C_ER","C_OD","C_AD"),
  
  C, p_oc_burst_uc, p_hosp_uc, p_er_visit_uc, p_death_all, p_asthma_related_mortality_uc, p_con_PH, 0, 0, 0, 0, 0, #UC_NE ->
  C, 0, 0, 0, p_death_all, 0, 0, 0, 0, 0, 0, 0,  #UC_OC ->
  C, 0, 0, 0, p_death_all, 0, 0, 0, 0, 0, 0, 0,  #UC_HOSP ->
  C, 0, 0, 0, p_death_all, 0, 0, 0, 0, 0, 0, 0,  #UC_ER ->
  0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,  #UC_OD ->
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,  #UC_AD ->
  
  p_unc_PH, 0, 0, 0, 0, 0, C, p_oc_burst_c, p_hosp_c, p_er_visit_c, p_death_all, p_asthma_related_mortality_c, #C_NE ->
  0, 0, 0, 0, 0, 0, C, 0, 0, 0, p_asthma_related_mortality_c, 0,  #C_OC ->
  0, 0, 0, 0, 0, 0, C, 0, 0, 0, p_asthma_related_mortality_c, 0,  #C_HOSP ->
  0, 0, 0, 0, 0, 0, C, 0, 0, 0, p_asthma_related_mortality_c, 0,  #C_ER ->
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,  #C_OD ->
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1  #C_AD ->
)


#PH USUAL CARE
mat_uc <- define_transition(
  state_names = c("UC_NE","UC_OC","UC_HOSP","UC_ER","UC_OD","UC_AD","C_NE","C_OC","C_HOSP","C_ER","C_OD","C_AD"),
  
  C, p_oc_burst_uc, p_hosp_uc, p_er_visit_uc, p_death_all, p_asthma_related_mortality_uc, p_con_UC, 0, 0, 0, 0, 0, #UC_NE ->
  C, 0, 0, 0, p_death_all, 0, 0, 0, 0, 0, 0, 0,  #UC_OC ->
  C, 0, 0, 0, p_death_all, 0, 0, 0, 0, 0, 0, 0,  #UC_HOSP ->
  C, 0, 0, 0, p_death_all, 0, 0, 0, 0, 0, 0, 0,  #UC_ER ->
  0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,  #UC_OD ->
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,  #UC_AD ->
  
  p_unc_UC, 0, 0, 0, 0, 0, C, p_oc_burst_c, p_hosp_c, p_er_visit_c, p_death_all, p_asthma_related_mortality_c, #C_NE ->
  0, 0, 0, 0, 0, 0, C, 0, 0, 0, p_asthma_related_mortality_c, 0,  #C_OC ->
  0, 0, 0, 0, 0, 0, C, 0, 0, 0, p_asthma_related_mortality_c, 0,  #C_HOSP ->
  0, 0, 0, 0, 0, 0, C, 0, 0, 0, p_asthma_related_mortality_c, 0,  #C_ER ->
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,  #C_OD ->
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1  #C_AD ->
)


# State Values

state_UC_NE <- define_state(
  cost_treat = dispatch_strategy(
    ph = cost_noex + cost_ph1_cycle + cost_ph_rec ,
    uc = cost_noex
  ),
  cost_total = discount(cost_treat, r = dr),
  qaly = utility_ne
)

state_UC_OC <- define_state(
  cost_treat = dispatch_strategy(
    ph = cost_oc + cost_ph1_cycle + cost_ph_rec,
    uc = cost_oc
  ),
  cost_total = discount(cost_treat, r = dr),
  qaly = utility_oc
)
state_UC_HOSP <- define_state(
  cost_treat = dispatch_strategy(
    ph = cost_hosp + cost_ph1_cycle + cost_ph_rec,
    uc = cost_hosp
  ),
  cost_total = discount(cost_treat, r = dr),
  qaly = utility_hosp
)
state_UC_ER <- define_state(
  cost_treat = dispatch_strategy(
    ph = cost_er + cost_ph1_cycle + cost_ph_rec,
    uc = cost_er
  ),
  cost_total = discount(cost_treat, r = dr),
  qaly = utility_er
)
state_UC_OD <- define_state(
  cost_treat = 0,
  cost_total = 0,
  qaly = 0
)
state_UC_AD <- define_state(
  cost_treat = 0,
  cost_total = 0,
  qaly = 0
)
state_C_NE <- define_state(
  cost_treat = dispatch_strategy(
    ph = cost_noex + cost_ph1_cycle + cost_ph_rec,
    uc = cost_noex
  ),
  cost_total = discount(cost_treat, r = dr),
  qaly = utility_ne
)

state_C_OC <- define_state(
  cost_treat = dispatch_strategy(
    ph = cost_oc + cost_ph1_cycle + cost_ph_rec,
    uc = cost_oc
  ),
  cost_total = discount(cost_treat, r = dr),
  qaly = utility_oc
)
state_C_HOSP <- define_state(
  cost_treat = dispatch_strategy(
    ph = cost_hosp + cost_ph1_cycle + cost_ph_rec,
    uc = cost_hosp
  ),
  cost_total = discount(cost_treat, r = dr),
  qaly = utility_hosp
)
state_C_ER <- define_state(
  cost_treat = dispatch_strategy(
    ph = cost_er + cost_ph1_cycle + cost_ph_rec,
    uc = cost_er
  ),
  cost_total = discount(cost_treat, r = dr),
  qaly = utility_er
)
state_C_OD <- define_state(
  cost_treat = 0,
  cost_total = 0,
  qaly = 0
)
state_C_AD <- define_state(
  cost_treat = 0,
  cost_total = 0,
  qaly = 0
)


#strategies

strat_ph <- define_strategy(
  transition = mat_ph,
  
  UC_NE = state_UC_NE,
  UC_OC = state_UC_OC,
  UC_HOSP = state_UC_HOSP,
  UC_ER = state_UC_ER,
  UC_OD = state_UC_OD,
  UC_AD = state_UC_AD,
  C_NE = state_C_NE,
  C_OC = state_C_OC,
  C_HOSP = state_C_HOSP,
  C_ER = state_C_ER,
  C_OD = state_C_OD,
  C_AD = state_C_AD
)

strat_uc <- define_strategy(
  transition = mat_uc,
  
  UC_NE = state_UC_NE,
  UC_OC = state_UC_OC,
  UC_HOSP = state_UC_HOSP,
  UC_ER = state_UC_ER,
  UC_OD = state_UC_OD,
  UC_AD = state_UC_AD,
  C_NE = state_C_NE,
  C_OC = state_C_OC,
  C_HOSP = state_C_HOSP,
  C_ER = state_C_ER,
  C_OD = state_C_OD,
  C_AD = state_C_AD
)

detectCores() # 4 cores?
use_cluster(4)
status_cluster(verbose = TRUE)
res_mod <- run_model(
  parameters = par_mod,
  
  ph = strat_ph,
  uc = strat_uc,
  
  cycles = 10,
  
  cost = cost_total,
  effect = qaly
)

summary(res_mod)


# DSA Uncertainty analysis

def_dsa <- define_dsa(
  
  age_base, 10, 40,
  p_asthma_related_mortality_uc, 0.001, 0.01,
  cost_ph_rec, 10, 100,
  cost_ph1, 50, 1000,
  dr, 0, 0.5,
  utility_ne, 0.3, 0.7
)

res_dsa <- run_dsa(res_mod, dsa = def_dsa)

#plot of evolution over time of counts by state and values
plot(res_mod) +
  scale_colour_discrete(name="Transition\nStates",
                    breaks=c("UC_NE","UC_OC","UC_HOSP","UC_ER","UC_OD","UC_AD","C_NE","C_OC","C_HOSP","C_ER","C_OD","C_AD"),
                    labels=c("UnCtrl No Exac", "UnCtrl OC Burst", "UnCtrl Hospitalization", "UnCtrl ER", "UnCtrl Other Death", "UnCtrl Asthma Death", "Ctrl No Exac", "Ctrl OC Burst", "Ctrl Hospitalization", "Ctrl ER", "Ctrl Other Death", "Ctrl Asthma Death")) +
  ggtitle(label = "Propeller Health + Usual Care vs. Usual Care Alone Markov Evolution") +
  theme(plot.title = element_text(hjust = 0.5))


plot(res_dsa)
plot(res_dsa, type = "ce") +
  scale_color_brewer(name = "Treatment", palette = "Set1") +
  facet_wrap(~. .strategy_names) +
  xlab("Incremental QALYs")


sum<-summary(res_mod)

# Tornado plot ... may need to play with this 
plot(res_dsa, type = "simple")
plot(res_dsa, type = "difference")


#PSA set up all of the parameters later...

def_psa <- define_psa(
  age_base ~ normal(mean = 20, sd = 5),
  p_asthma_related_mortality_uc ~binomial(prob = 0.25, size = 500),
  cost_ph_rec ~ gamma(mean = 25, sd = 100),
  cost_ph1 ~ gamma(mean = 150, sd = 200),
  dr ~ binomial(prob = 0.05, size = 100),
  utility_ne ~ normal(mean = 0.67, sd = 0.15)
)

res_psa <- run_psa(res_mod, psa = def_psa, N = 1000)
plot(res_psa)
summary(res_psa)
close_cluster()
