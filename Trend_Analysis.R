library(tidyverse)
library(broom)


# prog_group <- read_csv("cRNFL_small_2ormoreScans_MAPS.csv") %>% distinct() %>% drop_na() %>%
#   group_by(Firstname, Eye) %>%
#   distinct(Diff_date, .keep_all = TRUE) %>% # I am taking one test from each day; add something to choose best quality of the day
#   ungroup() 

prog_group <- read_csv("cRNFL_large_2ormoreScans_MAPS.csv") %>% distinct() %>% drop_na() %>%
  group_by(Firstname, Eye) %>%
  distinct(Diff_date, .keep_all = TRUE) %>%
  ungroup()

prog_trend <- prog_group %>% filter(n_tests_total > 3,
                                    n_tests_long > 0)

maps_eyes <- read_csv("MAPS_diagnosis_progGrades.csv") %>%
  mutate(Study_Eye = if_else(Eye =="OD","R", "L")) %>%
  select(ID, Study_Eye, Dx, MAPS_Group)

prog_trend <- left_join(prog_trend, maps_eyes,
                        by = c("Firstname"="ID","Eye" = "Study_Eye")) %>% drop_na() %>%
  mutate(Diff_date_yr = Diff_date / 365)

trend_distinct_eyes <- prog_trend %>% group_by(Firstname, Eye) %>%
  filter(Diff_date ==0) %>%
  select(Firstname, Eye, DOB,n_tests_total, Dx, MAPS_Group) %>% ungroup()


## G metric
G_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(RNFLMean_G ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

G_models <- G_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

G_trend <- left_join(G_models, trend_distinct_eyes,
                     by = c("Firstname", "Eye"))

write_csv(G_trend,"small_cRNFL_G_trend_MAPS.csv")
write_csv(G_trend,"large_cRNFL_G_trend_MAPS.csv")


## T metric
T_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(RNFLMean_T ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

T_models <- T_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

T_trend <- left_join(T_models, trend_distinct_eyes,
                     by = c("Firstname", "Eye"))

write_csv(T_trend,"small_cRNFL_T_trend_MAPS.csv")
write_csv(T_trend,"large_cRNFL_T_trend_MAPS.csv")

## TI metric
TI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(RNFLMean_TI ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

TI_models <- TI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

TI_trend <- left_join(TI_models, trend_distinct_eyes,
                     by = c("Firstname", "Eye"))

write_csv(TI_trend,"small_cRNFL_TI_trend_MAPS.csv")
write_csv(TI_trend,"large_cRNFL_TI_trend_MAPS.csv")


## TS metric
TS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(RNFLMean_TS ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

TS_models <- TS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

TS_trend <- left_join(TS_models, trend_distinct_eyes,
                      by = c("Firstname", "Eye"))

write_csv(TS_trend,"small_cRNFL_TS_trend_MAPS.csv")
write_csv(TS_trend,"large_cRNFL_TS_trend_MAPS.csv")

## N metric
N_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(RNFLMean_N ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

N_models <- N_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

N_trend <- left_join(N_models, trend_distinct_eyes,
                      by = c("Firstname", "Eye"))

write_csv(N_trend,"small_cRNFL_N_trend_MAPS.csv")
write_csv(N_trend,"large_cRNFL_N_trend_MAPS.csv")

## NI metric
NI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(RNFLMean_NI ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

NI_models <- NI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

NI_trend <- left_join(NI_models, trend_distinct_eyes,
                     by = c("Firstname", "Eye"))

write_csv(NI_trend,"small_cRNFL_NI_trend_MAPS.csv")
write_csv(NI_trend,"large_cRNFL_NI_trend_MAPS.csv")

## NS metric
NS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(RNFLMean_NS ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

NS_models <- NS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

NS_trend <- left_join(NS_models, trend_distinct_eyes,
                      by = c("Firstname", "Eye"))

write_csv(NS_trend,"small_cRNFL_NS_trend_MAPS.csv")
write_csv(NS_trend,"large_cRNFL_NS_trend_MAPS.csv")


## Analysis

G_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres))  

G_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                  prog5 = sum(sig_prog5),
                                                                  prog2p5 = sum(sig_prog2p5),
                                                                  prog5_thres = sum(sig_prog5_wThres),
                                                                  prog2p5_thres = sum(sig_prog2p5_wThres)) 

T_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres))  

T_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                  prog5 = sum(sig_prog5),
                                                                  prog2p5 = sum(sig_prog2p5),
                                                                  prog5_thres = sum(sig_prog5_wThres),
                                                                  prog2p5_thres = sum(sig_prog2p5_wThres))

TI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres))  

TI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                  prog5 = sum(sig_prog5),
                                                                  prog2p5 = sum(sig_prog2p5),
                                                                  prog5_thres = sum(sig_prog5_wThres),
                                                                  prog2p5_thres = sum(sig_prog2p5_wThres)) 

TS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                    prog5 = sum(sig_prog5),
                                                                    prog2p5 = sum(sig_prog2p5),
                                                                    prog5_thres = sum(sig_prog5_wThres),
                                                                    prog2p5_thres = sum(sig_prog2p5_wThres))  

TS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres)) 


N_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres))  

N_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                  prog5 = sum(sig_prog5),
                                                                  prog2p5 = sum(sig_prog2p5),
                                                                  prog5_thres = sum(sig_prog5_wThres),
                                                                  prog2p5_thres = sum(sig_prog2p5_wThres))

NI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                    prog5 = sum(sig_prog5),
                                                                    prog2p5 = sum(sig_prog2p5),
                                                                    prog5_thres = sum(sig_prog5_wThres),
                                                                    prog2p5_thres = sum(sig_prog2p5_wThres))  

NI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres)) 

NS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                    prog5 = sum(sig_prog5),
                                                                    prog2p5 = sum(sig_prog2p5),
                                                                    prog5_thres = sum(sig_prog5_wThres),
                                                                    prog2p5_thres = sum(sig_prog2p5_wThres))  

NS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres)) 



## Macular Volume Metrics ##


prog_group <- read_csv("mac_volume_2ormoreScans_MAPS.csv") %>% distinct() %>% drop_na() %>%
  group_by(Firstname, Eye) %>%
  distinct(Diff_date, .keep_all = TRUE) %>%
  ungroup()

prog_trend <- prog_group %>% filter(n_tests_total > 3,
                                    n_tests_long > 0)

maps_eyes <- read_csv("MAPS_diagnosis_progGrades.csv") %>%
  # mutate(Study_Eye = if_else(Eye =="OD","R", "L")) %>%
  select(ID, Eye, Dx, MAPS_Group)

prog_trend <- left_join(prog_trend, maps_eyes,
                        by = c("Firstname"="ID","Eye" = "Eye")) %>% drop_na() %>%
  mutate(Diff_date_yr = Diff_date / 365)

trend_distinct_eyes <- prog_trend %>% group_by(Firstname, Eye) %>%
  filter(Diff_date ==0) %>%
  select(Firstname, Eye, DOB,n_tests_total, Dx, MAPS_Group) %>% ungroup()


## Ret_G metric
Ret_G_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`Retina Value G` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

Ret_G_models <- Ret_G_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

Ret_G_trend <- left_join(Ret_G_models, trend_distinct_eyes,
                     by = c("Firstname", "Eye"))

write_csv(Ret_G_trend,"mac_volume_Ret_G_trend_MAPS.csv")

Ret_G_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres))  

Ret_G_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                  prog5 = sum(sig_prog5),
                                                                  prog2p5 = sum(sig_prog2p5),
                                                                  prog5_thres = sum(sig_prog5_wThres),
                                                                  prog2p5_thres = sum(sig_prog2p5_wThres)) 


## Ret_TI metric
Ret_TI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`Retina Value TI` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

Ret_TI_models <- Ret_TI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

Ret_TI_trend <- left_join(Ret_TI_models, trend_distinct_eyes,
                         by = c("Firstname", "Eye"))

write_csv(Ret_TI_trend,"mac_volume_Ret_TI_trend_MAPS.csv")

Ret_TI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres))  

Ret_TI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                      prog5 = sum(sig_prog5),
                                                                      prog2p5 = sum(sig_prog2p5),
                                                                      prog5_thres = sum(sig_prog5_wThres),
                                                                      prog2p5_thres = sum(sig_prog2p5_wThres)) 

## Ret_I metric
Ret_I_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`Retina Value I` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

Ret_I_models <- Ret_I_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

Ret_I_trend <- left_join(Ret_I_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(Ret_I_trend,"mac_volume_Ret_I_trend_MAPS.csv")

Ret_I_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

Ret_I_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 

## Ret_NI metric
Ret_NI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`Retina Value NI` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

Ret_NI_models <- Ret_NI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

Ret_NI_trend <- left_join(Ret_NI_models, trend_distinct_eyes,
                         by = c("Firstname", "Eye"))

write_csv(Ret_NI_trend,"mac_volume_Ret_NI_trend_MAPS.csv")

Ret_NI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres))  

Ret_NI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                      prog5 = sum(sig_prog5),
                                                                      prog2p5 = sum(sig_prog2p5),
                                                                      prog5_thres = sum(sig_prog5_wThres),
                                                                      prog2p5_thres = sum(sig_prog2p5_wThres)) 


## Ret_TS metric
Ret_TS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`Retina Value TS` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

Ret_TS_models <- Ret_TS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

Ret_TS_trend <- left_join(Ret_TS_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(Ret_TS_trend,"mac_volume_Ret_TS_trend_MAPS.csv")

Ret_TS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

Ret_TS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 

## Ret_S metric
Ret_S_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`Retina Value S` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

Ret_S_models <- Ret_S_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

Ret_S_trend <- left_join(Ret_S_models, trend_distinct_eyes,
                         by = c("Firstname", "Eye"))

write_csv(Ret_S_trend,"mac_volume_Ret_S_trend_MAPS.csv")

Ret_S_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres))  

Ret_S_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                      prog5 = sum(sig_prog5),
                                                                      prog2p5 = sum(sig_prog2p5),
                                                                      prog5_thres = sum(sig_prog5_wThres),
                                                                      prog2p5_thres = sum(sig_prog2p5_wThres)) 

## Ret_NS metric
Ret_NS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`Retina Value NS` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

Ret_NS_models <- Ret_NS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

Ret_NS_trend <- left_join(Ret_NS_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(Ret_NS_trend,"mac_volume_Ret_NS_trend_MAPS.csv")

Ret_NS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

Ret_NS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres))



## RNFL_G metric
RNFL_G_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`RNFL Value G` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

RNFL_G_models <- RNFL_G_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

RNFL_G_trend <- left_join(RNFL_G_models, trend_distinct_eyes,
                         by = c("Firstname", "Eye"))

write_csv(RNFL_G_trend,"mac_volume_RNFL_G_trend_MAPS.csv")

RNFL_G_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres))  

RNFL_G_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                      prog5 = sum(sig_prog5),
                                                                      prog2p5 = sum(sig_prog2p5),
                                                                      prog5_thres = sum(sig_prog5_wThres),
                                                                      prog2p5_thres = sum(sig_prog2p5_wThres)) 


## RNFL_TI metric
RNFL_TI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`RNFL Value TI` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

RNFL_TI_models <- RNFL_TI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

RNFL_TI_trend <- left_join(RNFL_TI_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(RNFL_TI_trend,"mac_volume_RNFL_TI_trend_MAPS.csv")

RNFL_TI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

RNFL_TI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 

## RNFL_I metric
RNFL_I_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`RNFL Value I` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

RNFL_I_models <- RNFL_I_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

RNFL_I_trend <- left_join(RNFL_I_models, trend_distinct_eyes,
                         by = c("Firstname", "Eye"))

write_csv(RNFL_I_trend,"mac_volume_RNFL_I_trend_MAPS.csv")

RNFL_I_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres))  

RNFL_I_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                      prog5 = sum(sig_prog5),
                                                                      prog2p5 = sum(sig_prog2p5),
                                                                      prog5_thres = sum(sig_prog5_wThres),
                                                                      prog2p5_thres = sum(sig_prog2p5_wThres)) 

## RNFL_NI metric
RNFL_NI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`RNFL Value NI` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

RNFL_NI_models <- RNFL_NI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

RNFL_NI_trend <- left_join(RNFL_NI_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(RNFL_NI_trend,"mac_volume_RNFL_NI_trend_MAPS.csv")

RNFL_NI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

RNFL_NI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 


## RNFL_TS metric
RNFL_TS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`RNFL Value TS` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

RNFL_TS_models <- RNFL_TS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

RNFL_TS_trend <- left_join(RNFL_TS_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(RNFL_TS_trend,"mac_volume_RNFL_TS_trend_MAPS.csv")

RNFL_TS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

RNFL_TS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 

## RNFL_S metric
RNFL_S_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`RNFL Value S` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

RNFL_S_models <- RNFL_S_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

RNFL_S_trend <- left_join(RNFL_S_models, trend_distinct_eyes,
                         by = c("Firstname", "Eye"))

write_csv(RNFL_S_trend,"mac_volume_RNFL_S_trend_MAPS.csv")

RNFL_S_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres))  

RNFL_S_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                      prog5 = sum(sig_prog5),
                                                                      prog2p5 = sum(sig_prog2p5),
                                                                      prog5_thres = sum(sig_prog5_wThres),
                                                                      prog2p5_thres = sum(sig_prog2p5_wThres)) 

## RNFL_NS metric
RNFL_NS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`RNFL Value NS` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

RNFL_NS_models <- RNFL_NS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

RNFL_NS_trend <- left_join(RNFL_NS_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(RNFL_NS_trend,"mac_volume_RNFL_NS_trend_MAPS.csv")

RNFL_NS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

RNFL_NS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 


## GCL_G metric
GCL_G_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`GCL Value G` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

GCL_G_models <- GCL_G_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

GCL_G_trend <- left_join(GCL_G_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(GCL_G_trend,"mac_volume_GCL_G_trend_MAPS.csv")

GCL_G_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

GCL_G_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 


## GCL_TI metric
GCL_TI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`GCL Value TI` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

GCL_TI_models <- GCL_TI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

GCL_TI_trend <- left_join(GCL_TI_models, trend_distinct_eyes,
                           by = c("Firstname", "Eye"))

write_csv(GCL_TI_trend,"mac_volume_GCL_TI_trend_MAPS.csv")

GCL_TI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                         prog5 = sum(sig_prog5),
                                                                         prog2p5 = sum(sig_prog2p5),
                                                                         prog5_thres = sum(sig_prog5_wThres),
                                                                         prog2p5_thres = sum(sig_prog2p5_wThres))  

GCL_TI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres)) 

## GCL_I metric
GCL_I_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`GCL Value I` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

GCL_I_models <- GCL_I_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

GCL_I_trend <- left_join(GCL_I_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(GCL_I_trend,"mac_volume_GCL_I_trend_MAPS.csv")

GCL_I_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

GCL_I_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 

## GCL_NI metric
GCL_NI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`GCL Value NI` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

GCL_NI_models <- GCL_NI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

GCL_NI_trend <- left_join(GCL_NI_models, trend_distinct_eyes,
                           by = c("Firstname", "Eye"))

write_csv(GCL_NI_trend,"mac_volume_GCL_NI_trend_MAPS.csv")

GCL_NI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                         prog5 = sum(sig_prog5),
                                                                         prog2p5 = sum(sig_prog2p5),
                                                                         prog5_thres = sum(sig_prog5_wThres),
                                                                         prog2p5_thres = sum(sig_prog2p5_wThres))  

GCL_NI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres)) 


## GCL_TS metric
GCL_TS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`GCL Value TS` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

GCL_TS_models <- GCL_TS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

GCL_TS_trend <- left_join(GCL_TS_models, trend_distinct_eyes,
                           by = c("Firstname", "Eye"))

write_csv(GCL_TS_trend,"mac_volume_GCL_TS_trend_MAPS.csv")

GCL_TS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                         prog5 = sum(sig_prog5),
                                                                         prog2p5 = sum(sig_prog2p5),
                                                                         prog5_thres = sum(sig_prog5_wThres),
                                                                         prog2p5_thres = sum(sig_prog2p5_wThres))  

GCL_TS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres)) 

## GCL_S metric
GCL_S_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`GCL Value S` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

GCL_S_models <- GCL_S_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

GCL_S_trend <- left_join(GCL_S_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(GCL_S_trend,"mac_volume_GCL_S_trend_MAPS.csv")

GCL_S_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

GCL_S_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 

## GCL_NS metric
GCL_NS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`GCL Value NS` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

GCL_NS_models <- GCL_NS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

GCL_NS_trend <- left_join(GCL_NS_models, trend_distinct_eyes,
                           by = c("Firstname", "Eye"))

write_csv(GCL_NS_trend,"mac_volume_GCL_NS_trend_MAPS.csv")

GCL_NS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                         prog5 = sum(sig_prog5),
                                                                         prog2p5 = sum(sig_prog2p5),
                                                                         prog5_thres = sum(sig_prog5_wThres),
                                                                         prog2p5_thres = sum(sig_prog2p5_wThres))  

GCL_NS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres)) 


## IPL_G metric
IPL_G_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`IPL Value G` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

IPL_G_models <- IPL_G_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

IPL_G_trend <- left_join(IPL_G_models, trend_distinct_eyes,
                         by = c("Firstname", "Eye"))

write_csv(IPL_G_trend,"mac_volume_IPL_G_trend_MAPS.csv")

IPL_G_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres))  

IPL_G_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                      prog5 = sum(sig_prog5),
                                                                      prog2p5 = sum(sig_prog2p5),
                                                                      prog5_thres = sum(sig_prog5_wThres),
                                                                      prog2p5_thres = sum(sig_prog2p5_wThres)) 


## IPL_TI metric
IPL_TI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`IPL Value TI` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

IPL_TI_models <- IPL_TI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

IPL_TI_trend <- left_join(IPL_TI_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(IPL_TI_trend,"mac_volume_IPL_TI_trend_MAPS.csv")

IPL_TI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

IPL_TI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 

## IPL_I metric
IPL_I_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`IPL Value I` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

IPL_I_models <- IPL_I_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

IPL_I_trend <- left_join(IPL_I_models, trend_distinct_eyes,
                         by = c("Firstname", "Eye"))

write_csv(IPL_I_trend,"mac_volume_IPL_I_trend_MAPS.csv")

IPL_I_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres))  

IPL_I_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                      prog5 = sum(sig_prog5),
                                                                      prog2p5 = sum(sig_prog2p5),
                                                                      prog5_thres = sum(sig_prog5_wThres),
                                                                      prog2p5_thres = sum(sig_prog2p5_wThres)) 

## IPL_NI metric
IPL_NI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`IPL Value NI` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

IPL_NI_models <- IPL_NI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

IPL_NI_trend <- left_join(IPL_NI_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(IPL_NI_trend,"mac_volume_IPL_NI_trend_MAPS.csv")

IPL_NI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

IPL_NI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 


## IPL_TS metric
IPL_TS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`IPL Value TS` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

IPL_TS_models <- IPL_TS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

IPL_TS_trend <- left_join(IPL_TS_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(IPL_TS_trend,"mac_volume_IPL_TS_trend_MAPS.csv")

IPL_TS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

IPL_TS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 

## IPL_S metric
IPL_S_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`IPL Value S` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

IPL_S_models <- IPL_S_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

IPL_S_trend <- left_join(IPL_S_models, trend_distinct_eyes,
                         by = c("Firstname", "Eye"))

write_csv(IPL_S_trend,"mac_volume_IPL_S_trend_MAPS.csv")

IPL_S_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres))  

IPL_S_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                      prog5 = sum(sig_prog5),
                                                                      prog2p5 = sum(sig_prog2p5),
                                                                      prog5_thres = sum(sig_prog5_wThres),
                                                                      prog2p5_thres = sum(sig_prog2p5_wThres)) 

## IPL_NS metric
IPL_NS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`IPL Value NS` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

IPL_NS_models <- IPL_NS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

IPL_NS_trend <- left_join(IPL_NS_models, trend_distinct_eyes,
                          by = c("Firstname", "Eye"))

write_csv(IPL_NS_trend,"mac_volume_IPL_NS_trend_MAPS.csv")

IPL_NS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                        prog5 = sum(sig_prog5),
                                                                        prog2p5 = sum(sig_prog2p5),
                                                                        prog5_thres = sum(sig_prog5_wThres),
                                                                        prog2p5_thres = sum(sig_prog2p5_wThres))  

IPL_NS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                       prog5 = sum(sig_prog5),
                                                                       prog2p5 = sum(sig_prog2p5),
                                                                       prog5_thres = sum(sig_prog5_wThres),
                                                                       prog2p5_thres = sum(sig_prog2p5_wThres)) 




### BMO Metrics ###
prog_group <- read_csv("BMO_2ormoreScans_MAPS.csv") %>% distinct() %>% drop_na() %>%
  group_by(Firstname, Eye) %>%
  distinct(Diff_date, .keep_all = TRUE) %>%
  ungroup()

prog_trend <- prog_group %>% filter(n_tests_total > 3,
                                    n_tests_long > 0)

maps_eyes <- read_csv("MAPS_diagnosis_progGrades.csv") %>%
  mutate(Study_Eye = if_else(Eye =="OD","R", "L")) %>%
  select(ID, Study_Eye, Dx, MAPS_Group)

prog_trend <- left_join(prog_trend, maps_eyes,
                        by = c("Firstname"="ID","Eye" = "Study_Eye")) %>% drop_na() %>%
  mutate(Diff_date_yr = Diff_date / 365)

trend_distinct_eyes <- prog_trend %>% group_by(Firstname, Eye) %>%
  filter(Diff_date ==0) %>%
  select(Firstname, Eye, DOB,n_tests_total, Dx, MAPS_Group) %>% ungroup()


## G metric
G_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`MRW Global` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

G_models <- G_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

G_trend <- left_join(G_models, trend_distinct_eyes,
                     by = c("Firstname", "Eye"))

write_csv(G_trend,"BMO_G_trend_MAPS.csv")


## T metric
T_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`MRW Tmp` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

T_models <- T_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

T_trend <- left_join(T_models, trend_distinct_eyes,
                     by = c("Firstname", "Eye"))

write_csv(T_trend,"BMO_T_trend_MAPS.csv")

## TI metric
TI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`MRW TI` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

TI_models <- TI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

TI_trend <- left_join(TI_models, trend_distinct_eyes,
                      by = c("Firstname", "Eye"))

write_csv(TI_trend,"BMO_TI_trend_MAPS.csv")


## TS metric
TS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`MRW TS` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

TS_models <- TS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

TS_trend <- left_join(TS_models, trend_distinct_eyes,
                      by = c("Firstname", "Eye"))

write_csv(TS_trend,"BMO_TS_trend_MAPS.csv")

## N metric
N_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`MRW Nas` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

N_models <- N_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

N_trend <- left_join(N_models, trend_distinct_eyes,
                     by = c("Firstname", "Eye"))

write_csv(N_trend,"BMO_N_trend_MAPS.csv")

## NI metric
NI_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`MRW NI` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

NI_models <- NI_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

NI_trend <- left_join(NI_models, trend_distinct_eyes,
                      by = c("Firstname", "Eye"))

write_csv(NI_trend,"BMO_NI_trend_MAPS.csv")

## NS metric
NS_models <- prog_trend %>% ungroup() %>%
  nest_by(Firstname,Eye) %>%
  mutate(model = list(lm(`MRW NS` ~ Diff_date_yr, data = data))) %>%
  summarise(tidy(model))

NS_models <- NS_models %>% filter(term == "Diff_date_yr") %>%
  mutate(p.value_1tail = p.value / 2,
         sig_prog5 = if_else(estimate < 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5 = if_else(estimate < 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5 = if_else(estimate > 0 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5 = if_else(estimate > 0 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_prog5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_prog2p5_wThres = if_else(estimate < -0.2 & p.value_1tail < 0.025, TRUE, FALSE),
         sig_impr5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.05, TRUE, FALSE),
         sig_impr2p5_wThres = if_else(estimate > 0.2 & p.value_1tail < 0.025, TRUE, FALSE))

NS_trend <- left_join(NS_models, trend_distinct_eyes,
                      by = c("Firstname", "Eye"))

write_csv(NS_trend,"BMO_NS_trend_MAPS.csv")


## Analysis

G_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres))  

G_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                  prog5 = sum(sig_prog5),
                                                                  prog2p5 = sum(sig_prog2p5),
                                                                  prog5_thres = sum(sig_prog5_wThres),
                                                                  prog2p5_thres = sum(sig_prog2p5_wThres)) 

T_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres))  

T_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                  prog5 = sum(sig_prog5),
                                                                  prog2p5 = sum(sig_prog2p5),
                                                                  prog5_thres = sum(sig_prog5_wThres),
                                                                  prog2p5_thres = sum(sig_prog2p5_wThres))

TI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                    prog5 = sum(sig_prog5),
                                                                    prog2p5 = sum(sig_prog2p5),
                                                                    prog5_thres = sum(sig_prog5_wThres),
                                                                    prog2p5_thres = sum(sig_prog2p5_wThres))  

TI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres)) 

TS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                    prog5 = sum(sig_prog5),
                                                                    prog2p5 = sum(sig_prog2p5),
                                                                    prog5_thres = sum(sig_prog5_wThres),
                                                                    prog2p5_thres = sum(sig_prog2p5_wThres))  

TS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres)) 


N_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres))  

N_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                  prog5 = sum(sig_prog5),
                                                                  prog2p5 = sum(sig_prog2p5),
                                                                  prog5_thres = sum(sig_prog5_wThres),
                                                                  prog2p5_thres = sum(sig_prog2p5_wThres))

NI_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                    prog5 = sum(sig_prog5),
                                                                    prog2p5 = sum(sig_prog2p5),
                                                                    prog5_thres = sum(sig_prog5_wThres),
                                                                    prog2p5_thres = sum(sig_prog2p5_wThres))  

NI_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres)) 

NS_trend %>% ungroup() %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                                    prog5 = sum(sig_prog5),
                                                                    prog2p5 = sum(sig_prog2p5),
                                                                    prog5_thres = sum(sig_prog5_wThres),
                                                                    prog2p5_thres = sum(sig_prog2p5_wThres))  

NS_trend %>% ungroup() %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                                   prog5 = sum(sig_prog5),
                                                                   prog2p5 = sum(sig_prog2p5),
                                                                   prog5_thres = sum(sig_prog5_wThres),
                                                                   prog2p5_thres = sum(sig_prog2p5_wThres)) 
