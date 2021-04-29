library(tidyverse)

# Fixed an issue where columns were not numeric and NAs were introduced
# prog_group$RNFLMean_G <- as.numeric(prog_group$RNFLMean_G)
# prog_group$RNFLMean_T <- as.numeric(prog_group$RNFLMean_T)
# prog_group$RNFLMean_TI <- as.numeric(prog_group$RNFLMean_TI)
# prog_group$RNFLMean_TS <- as.numeric(prog_group$RNFLMean_TS)
# prog_group$RNFLMean_N <- as.numeric(prog_group$RNFLMean_N)
# prog_group$RNFLMean_NI <- as.numeric(prog_group$RNFLMean_NI)
# prog_group$RNFLMean_NS <- as.numeric(prog_group$RNFLMean_NS)
# 
# prog_group <- prog_group %>% drop_na(RNFLMean_G,RNFLMean_T,
#                                    RNFLMean_TI, RNFLMean_TS,
#                                    RNFLMean_N,
#                                    RNFLMean_NI, RNFLMean_NS)
# 
# write_csv(prog_group, "cRNFL_small_progGroup_MAPS.csv")



# prog_group <- read_csv("cRNFL_small_progGroup_MAPS.csv") %>% distinct() %>% drop_na() %>%
#   group_by(Firstname, Eye) %>%
#   distinct(Diff_date, .keep_all = TRUE) # I am taking one test from each day; add something to choose best quality of the day

prog_group <- read_csv("cRNFL_large_progGroup_MAPS.csv") %>% distinct() %>% drop_na() %>%
  group_by(Firstname, Eye) %>%
  distinct(Diff_date, .keep_all = TRUE) # I am taking one test from each day; add something to choose best quality of the day

# quant_results <- read_csv("quant_results_cRNFL_small_MAPS.csv")
quant_results <- read_csv("quant_results_cRNFL_large_MAPS.csv")

maps_eyes <- read_csv("MAPS_diagnosis_progGrades.csv") %>%
  mutate(Study_Eye = if_else(Eye =="OD","R", "L")) %>%
  select(ID, Study_Eye, Dx, MAPS_Group)

prog_event <- prog_group %>% group_by(Firstname, Eye) %>% distinct(ExamDate, .keep_all = TRUE) %>%
  filter(row_number()==1 | row_number()==n()) %>% ungroup()

prog_event <- left_join(prog_event, maps_eyes,
                        by = c("Firstname"="ID","Eye" = "Study_Eye"))

prog_event <- prog_event %>% drop_na()

prog_event <- prog_event %>%
  mutate(test_time = if_else(Diff_date==0, "Baseline", "Follow-Up"))

prog_event_baseline <- prog_event %>% filter(test_time == "Baseline")

prog_event_baseline <- prog_event_baseline %>% pivot_wider(names_from = test_time, values_from = c(RNFLMean_G, RNFLMean_T,
                                                                                                   RNFLMean_TI, RNFLMean_TS,
                                                                                                   RNFLMean_N, RNFLMean_NI, RNFLMean_NS)) %>%
  select(1:3,7:18)

prog_event_fup <- prog_event %>% filter(test_time == "Follow-Up")

prog_event_fup <- prog_event_fup %>% pivot_wider(names_from = test_time, values_from = c(RNFLMean_G, RNFLMean_T,
                                                                                         RNFLMean_TI, RNFLMean_TS,
                                                                                         RNFLMean_N, RNFLMean_NI, RNFLMean_NS)) %>%
  select(1:3,7:18)

prog_event <- left_join(prog_event_baseline,prog_event_fup,
                        by = c("Firstname", "Eye", "DOB",
                               "n_tests_total", "n_tests_short", "n_tests_long",
                               "Dx", "MAPS_Group"))

prog_event <- prog_event %>% select(1:8, RNFLMean_G_Baseline, `RNFLMean_G_Follow-Up`,
                                    RNFLMean_T_Baseline, `RNFLMean_T_Follow-Up`,
                                    RNFLMean_TI_Baseline, `RNFLMean_TI_Follow-Up`,
                                    RNFLMean_TS_Baseline, `RNFLMean_TS_Follow-Up`,
                                    RNFLMean_N_Baseline, `RNFLMean_N_Follow-Up`,
                                    RNFLMean_NI_Baseline, `RNFLMean_NI_Follow-Up`,
                                    RNFLMean_NS_Baseline, `RNFLMean_NS_Follow-Up`)

## G Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "G",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "G",
                                          tau == 0.025,
                                          term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "G",
                                          tau == 0.05,
                                          term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "G",
                                      tau == 0.05,
                                      term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "G",
                                          tau == 0.975,
                                          term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "G",
                                      tau == 0.975,
                                      term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "G",
                                        tau == 0.95,
                                        term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "G",
                                    tau == 0.95,
                                    term == "baseline") %>% pull(estimate)

G_event <- prog_event %>% select(1:8,RNFLMean_G_Baseline,`RNFLMean_G_Follow-Up`) %>%
  mutate(thres_2p5 = round(RNFLMean_G_Baseline*slope_2p5 + intercept_2p5),
         thres_5 = round(RNFLMean_G_Baseline*slope_5 + intercept_5),
         thres_95 = round(RNFLMean_G_Baseline*slope_95 + intercept_95),
         thres_97p5 = round(RNFLMean_G_Baseline*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFLMean_G_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFLMean_G_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFLMean_G_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFLMean_G_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(G_event, "small_cRNFL_G_event_MAPS.csv")
write_csv(G_event, "large_cRNFL_G_event_MAPS.csv")


## T Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "T",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "T",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "T",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "T",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "T",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "T",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "T",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "T",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

T_event <- prog_event %>% select(1:8,RNFLMean_T_Baseline,`RNFLMean_T_Follow-Up`) %>%
  mutate(thres_2p5 = round(RNFLMean_T_Baseline*slope_2p5 + intercept_2p5),
         thres_5 = round(RNFLMean_T_Baseline*slope_5 + intercept_5),
         thres_95 = round(RNFLMean_T_Baseline*slope_95 + intercept_95),
         thres_97p5 = round(RNFLMean_T_Baseline*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFLMean_T_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFLMean_T_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFLMean_T_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFLMean_T_Follow-Up` > thres_97p5, TRUE, FALSE))
write_csv(T_event, "small_cRNFL_T_event_MAPS.csv")
write_csv(T_event, "large_cRNFL_T_event_MAPS.csv")

## TI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "TI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "TI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "TI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "TI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "TI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "TI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "TI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "TI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

TI_event <- prog_event %>% select(1:8,RNFLMean_TI_Baseline,`RNFLMean_TI_Follow-Up`) %>%
  mutate(thres_2p5 = round(RNFLMean_TI_Baseline*slope_2p5 + intercept_2p5),
         thres_5 = round(RNFLMean_TI_Baseline*slope_5 + intercept_5),
         thres_95 = round(RNFLMean_TI_Baseline*slope_95 + intercept_95),
         thres_97p5 = round(RNFLMean_TI_Baseline*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFLMean_TI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFLMean_TI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFLMean_TI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFLMean_TI_Follow-Up` > thres_97p5, TRUE, FALSE))
write_csv(TI_event, "small_cRNFL_TI_event_MAPS.csv")
write_csv(TI_event, "large_cRNFL_TI_event_MAPS.csv")

## TS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "TS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "TS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "TS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "TS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "TS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "TS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "TS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "TS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

TS_event <- prog_event %>% select(1:8,RNFLMean_TS_Baseline,`RNFLMean_TS_Follow-Up`) %>%
  mutate(thres_2p5 = round(RNFLMean_TS_Baseline*slope_2p5 + intercept_2p5),
         thres_5 = round(RNFLMean_TS_Baseline*slope_5 + intercept_5),
         thres_95 = round(RNFLMean_TS_Baseline*slope_95 + intercept_95),
         thres_97p5 = round(RNFLMean_TS_Baseline*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFLMean_TS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFLMean_TS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFLMean_TS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFLMean_TS_Follow-Up` > thres_97p5, TRUE, FALSE))
write_csv(TS_event, "small_cRNFL_TS_event_MAPS.csv")
write_csv(TS_event, "large_cRNFL_TS_event_MAPS.csv")

## N Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "N",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "N",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "N",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "N",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "N",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "N",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "N",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "N",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

N_event <- prog_event %>% select(1:8,RNFLMean_N_Baseline,`RNFLMean_N_Follow-Up`) %>%
  mutate(thres_2p5 = round(RNFLMean_N_Baseline*slope_2p5 + intercept_2p5),
         thres_5 = round(RNFLMean_N_Baseline*slope_5 + intercept_5),
         thres_95 = round(RNFLMean_N_Baseline*slope_95 + intercept_95),
         thres_97p5 = round(RNFLMean_N_Baseline*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFLMean_N_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFLMean_N_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFLMean_N_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFLMean_N_Follow-Up` > thres_97p5, TRUE, FALSE))
write_csv(N_event, "small_cRNFL_N_event_MAPS.csv")
write_csv(N_event, "large_cRNFL_N_event_MAPS.csv")

## NI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "NI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "NI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "NI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "NI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "NI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "NI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "NI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "NI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

NI_event <- prog_event %>% select(1:8,RNFLMean_NI_Baseline,`RNFLMean_NI_Follow-Up`) %>%
  mutate(thres_2p5 = round(RNFLMean_NI_Baseline*slope_2p5 + intercept_2p5),
         thres_5 = round(RNFLMean_NI_Baseline*slope_5 + intercept_5),
         thres_95 = round(RNFLMean_NI_Baseline*slope_95 + intercept_95),
         thres_97p5 = round(RNFLMean_NI_Baseline*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFLMean_NI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFLMean_NI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFLMean_NI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFLMean_NI_Follow-Up` > thres_97p5, TRUE, FALSE))
write_csv(NI_event, "small_cRNFL_NI_event_MAPS.csv")
write_csv(NI_event, "large_cRNFL_NI_event_MAPS.csv")

## NS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "NS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "NS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "NS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "NS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "NS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "NS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "NS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "NS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

NS_event <- prog_event %>% select(1:8,RNFLMean_NS_Baseline,`RNFLMean_NS_Follow-Up`) %>%
  mutate(thres_2p5 = round(RNFLMean_NS_Baseline*slope_2p5 + intercept_2p5),
         thres_5 = round(RNFLMean_NS_Baseline*slope_5 + intercept_5),
         thres_95 = round(RNFLMean_NS_Baseline*slope_95 + intercept_95),
         thres_97p5 = round(RNFLMean_NS_Baseline*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFLMean_NS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFLMean_NS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFLMean_NS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFLMean_NS_Follow-Up` > thres_97p5, TRUE, FALSE))
write_csv(NS_event, "small_cRNFL_NS_event_MAPS.csv")
write_csv(NS_event, "large_cRNFL_NS_event_MAPS.csv")

## Analysis
G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     spec = (n_eyes - total) / n_eyes)

G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     hit = (total) / n_eyes)

G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     spec = (n_eyes - total) / n_eyes)

G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_2p5),
                                                    hit = (total) / n_eyes)


T_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     spec = (n_eyes - total) / n_eyes)

T_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_5),
                                                    hit = (total) / n_eyes)

T_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     spec = (n_eyes - total) / n_eyes)

T_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_2p5),
                                                    hit = (total) / n_eyes)

TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     spec = (n_eyes - total) / n_eyes)

TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_5),
                                                    hit = (total) / n_eyes)

TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     spec = (n_eyes - total) / n_eyes)

TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_2p5),
                                                    hit = (total) / n_eyes)

TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_5),
                                                      spec = (n_eyes - total) / n_eyes)

TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     hit = (total) / n_eyes)

TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_2p5),
                                                      spec = (n_eyes - total) / n_eyes)

TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     hit = (total) / n_eyes)


N_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     spec = (n_eyes - total) / n_eyes)

N_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_5),
                                                    hit = (total) / n_eyes)

N_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     spec = (n_eyes - total) / n_eyes)

N_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_2p5),
                                                    hit = (total) / n_eyes)

NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_5),
                                                      spec = (n_eyes - total) / n_eyes)

NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     hit = (total) / n_eyes)

NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_2p5),
                                                      spec = (n_eyes - total) / n_eyes)

NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     hit = (total) / n_eyes)

NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_5),
                                                      spec = (n_eyes - total) / n_eyes)

NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     hit = (total) / n_eyes)

NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_2p5),
                                                      spec = (n_eyes - total) / n_eyes)

NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     hit = (total) / n_eyes)







### Macular Volume Metrics ###

prog_group <- read_csv("mac_volume_progGroup_MAPS.csv") %>% distinct() %>% drop_na() %>%
  group_by(Firstname, Eye) %>%
  distinct(Diff_date, .keep_all = TRUE) # I am taking one test from each day; add something to choose best quality of the day

quant_results <- read_csv("quant_results_mac_volume_MAPS.csv")

maps_eyes <- read_csv("MAPS_diagnosis_progGrades.csv") %>%
  # mutate(Study_Eye = if_else(Eye =="OD","R", "L")) %>%
  select(ID, Eye, Dx, MAPS_Group)

prog_event <- prog_group %>% group_by(Firstname, Eye) %>% distinct(ExamDate, .keep_all = TRUE) %>%
  filter(row_number()==1 | row_number()==n()) %>% ungroup()

prog_event <- left_join(prog_event, maps_eyes,
                        by = c("Firstname"="ID","Eye" = "Eye"))

prog_event <- prog_event %>% drop_na()

prog_event <- prog_event %>%
  mutate(test_time = if_else(Diff_date==0, "Baseline", "Follow-Up"))

prog_event_baseline <- prog_event %>% filter(test_time == "Baseline")

prog_event_baseline <- prog_event_baseline %>%
  pivot_wider(names_from = test_time, values_from = c(`Retina Value G`, `Retina Value TS`, `Retina Value S`,`Retina Value NS`, 
                                                      `Retina Value TI`, `Retina Value I`, `Retina Value NI`,
                                                      `RNFL Value G`, `RNFL Value TS`, `RNFL Value S`,`RNFL Value NS`,
                                                      `RNFL Value NI`, `RNFL Value I`, `RNFL Value TI`,
                                                      `IPL Value G`, `IPL Value TS`, `IPL Value S`,`IPL Value NS`,
                                                      `IPL Value NI`, `IPL Value I`, `IPL Value TI`,
                                                      `GCL Value G`, `GCL Value TS`, `GCL Value S`,`GCL Value NS`,
                                                      `GCL Value NI`, `GCL Value I`, `GCL Value TI`)) %>%
  select(1:3,7:39)

prog_event_fup <- prog_event %>% filter(test_time == "Follow-Up")

prog_event_fup <- prog_event_fup %>%
  pivot_wider(names_from = test_time, values_from = c(`Retina Value G`, `Retina Value TS`, `Retina Value S`,`Retina Value NS`, 
                                                      `Retina Value TI`, `Retina Value I`, `Retina Value NI`,
                                                      `RNFL Value G`, `RNFL Value TS`, `RNFL Value S`,`RNFL Value NS`,
                                                      `RNFL Value NI`, `RNFL Value I`, `RNFL Value TI`,
                                                      `IPL Value G`, `IPL Value TS`, `IPL Value S`,`IPL Value NS`,
                                                      `IPL Value NI`, `IPL Value I`, `IPL Value TI`,
                                                      `GCL Value G`, `GCL Value TS`, `GCL Value S`,`GCL Value NS`,
                                                      `GCL Value NI`, `GCL Value I`, `GCL Value TI`)) %>%
  select(1:3,7:39)

prog_event <- left_join(prog_event_baseline,prog_event_fup,
                        by = c("Firstname", "Eye", "DOB",
                               "n_tests_total", "n_tests_short", "n_tests_long",
                               "Dx", "MAPS_Group"))

prog_event <- prog_event %>% select(1:8, `Retina Value G_Baseline`,`Retina Value G_Follow-Up`,
                                    `Retina Value TS_Baseline`,`Retina Value TS_Follow-Up`,
                                    `Retina Value S_Baseline`,`Retina Value S_Follow-Up`,
                                    `Retina Value NS_Baseline`,`Retina Value NS_Follow-Up`,
                                    `Retina Value TI_Baseline`,`Retina Value TI_Follow-Up`,
                                    `Retina Value I_Baseline`,`Retina Value I_Follow-Up`,
                                    `Retina Value NI_Baseline`,`Retina Value NI_Follow-Up`,
                                    `RNFL Value G_Baseline`, `RNFL Value G_Follow-Up`,
                                    `RNFL Value TS_Baseline`,`RNFL Value TS_Follow-Up`,
                                    `RNFL Value S_Baseline`, `RNFL Value S_Follow-Up`,
                                    `RNFL Value NS_Baseline`,`RNFL Value NS_Follow-Up`,
                                    `RNFL Value TI_Baseline`,`RNFL Value TI_Follow-Up`,
                                    `RNFL Value I_Baseline`,`RNFL Value I_Follow-Up`,
                                    `RNFL Value NI_Baseline`,`RNFL Value NI_Follow-Up`,
                                    `GCL Value G_Baseline`, `GCL Value G_Follow-Up`,
                                    `GCL Value TS_Baseline`,`GCL Value TS_Follow-Up`,
                                    `GCL Value S_Baseline`, `GCL Value S_Follow-Up`,
                                    `GCL Value NS_Baseline`,`GCL Value NS_Follow-Up`,
                                    `GCL Value TI_Baseline`,`GCL Value TI_Follow-Up`,
                                    `GCL Value I_Baseline`,`GCL Value I_Follow-Up`,
                                    `GCL Value NI_Baseline`,`GCL Value NI_Follow-Up`,
                                    `IPL Value G_Baseline`, `IPL Value G_Follow-Up`,
                                    `IPL Value TS_Baseline`,`IPL Value TS_Follow-Up`,
                                    `IPL Value S_Baseline`, `IPL Value S_Follow-Up`,
                                    `IPL Value NS_Baseline`,`IPL Value NS_Follow-Up`,
                                    `IPL Value TI_Baseline`,`IPL Value TI_Follow-Up`,
                                    `IPL Value I_Baseline`,`IPL Value I_Follow-Up`,
                                    `IPL Value NI_Baseline`,`IPL Value NI_Follow-Up`,)

## Retina G Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "Ret_G",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "Ret_G",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "Ret_G",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "Ret_G",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "Ret_G",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "Ret_G",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "Ret_G",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "Ret_G",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

Ret_G_event <- prog_event %>% select(1:8, `Retina Value G_Baseline`,`Retina Value G_Follow-Up`) %>%
  mutate(thres_2p5 = round(`Retina Value G_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`Retina Value G_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`Retina Value G_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`Retina Value G_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`Retina Value G_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`Retina Value G_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`Retina Value G_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`Retina Value G_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(Ret_G_event, "mac_volume_Ret_G_event_MAPS.csv")

Ret_G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         spec = (n_eyes - total) / n_eyes)

Ret_G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_5),
                                                        hit = (total) / n_eyes)

Ret_G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         spec = (n_eyes - total) / n_eyes)

Ret_G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_2p5),
                                                        hit = (total) / n_eyes)

## Retina TS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "Ret_TS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "Ret_TS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "Ret_TS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "Ret_TS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "Ret_TS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "Ret_TS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "Ret_TS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "Ret_TS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

Ret_TS_event <- prog_event %>% select(1:8, `Retina Value TS_Baseline`,`Retina Value TS_Follow-Up`) %>%
  mutate(thres_2p5 = round(`Retina Value TS_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`Retina Value TS_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`Retina Value TS_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`Retina Value TS_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`Retina Value TS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`Retina Value TS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`Retina Value TS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`Retina Value TS_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(Ret_TS_event, "mac_volume_Ret_TS_event_MAPS.csv")

Ret_TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         spec = (n_eyes - total) / n_eyes)

Ret_TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_5),
                                                        hit = (total) / n_eyes)

Ret_TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         spec = (n_eyes - total) / n_eyes)

Ret_TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_2p5),
                                                        hit = (total) / n_eyes)

## Retina S Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "Ret_S",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "Ret_S",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "Ret_S",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "Ret_S",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "Ret_S",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "Ret_S",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "Ret_S",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "Ret_S",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

Ret_S_event <- prog_event %>% select(1:8, `Retina Value S_Baseline`,`Retina Value S_Follow-Up`) %>%
  mutate(thres_2p5 = round(`Retina Value S_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`Retina Value S_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`Retina Value S_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`Retina Value S_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`Retina Value S_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`Retina Value S_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`Retina Value S_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`Retina Value S_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(Ret_S_event, "mac_volume_Ret_S_event_MAPS.csv")

Ret_S_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

Ret_S_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

Ret_S_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

Ret_S_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)


## Retina NS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "Ret_NS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "Ret_NS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "Ret_NS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "Ret_NS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "Ret_NS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "Ret_NS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "Ret_NS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "Ret_NS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

Ret_NS_event <- prog_event %>% select(1:8, `Retina Value NS_Baseline`,`Retina Value NS_Follow-Up`) %>%
  mutate(thres_2p5 = round(`Retina Value NS_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`Retina Value NS_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`Retina Value NS_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`Retina Value NS_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`Retina Value NS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`Retina Value NS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`Retina Value NS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`Retina Value NS_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(Ret_NS_event, "mac_volume_Ret_NS_event_MAPS.csv")

Ret_NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         spec = (n_eyes - total) / n_eyes)

Ret_NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_5),
                                                        hit = (total) / n_eyes)

Ret_NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         spec = (n_eyes - total) / n_eyes)

Ret_NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_2p5),
                                                        hit = (total) / n_eyes)


## Retina TI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "Ret_TI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "Ret_TI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "Ret_TI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "Ret_TI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "Ret_TI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "Ret_TI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "Ret_TI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "Ret_TI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

Ret_TI_event <- prog_event %>% select(1:8, `Retina Value TI_Baseline`,`Retina Value TI_Follow-Up`) %>%
  mutate(thres_2p5 = round(`Retina Value TI_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`Retina Value TI_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`Retina Value TI_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`Retina Value TI_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`Retina Value TI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`Retina Value TI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`Retina Value TI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`Retina Value TI_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(Ret_TI_event, "mac_volume_Ret_TI_event_MAPS.csv")

Ret_TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

Ret_TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

Ret_TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

Ret_TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)

## Retina I Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "Ret_I",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "Ret_I",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "Ret_I",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "Ret_I",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "Ret_I",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "Ret_I",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "Ret_I",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "Ret_I",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

Ret_I_event <- prog_event %>% select(1:8, `Retina Value I_Baseline`,`Retina Value I_Follow-Up`) %>%
  mutate(thres_2p5 = round(`Retina Value I_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`Retina Value I_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`Retina Value I_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`Retina Value I_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`Retina Value I_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`Retina Value I_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`Retina Value I_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`Retina Value I_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(Ret_I_event, "mac_volume_Ret_I_event_MAPS.csv")

Ret_I_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         spec = (n_eyes - total) / n_eyes)

Ret_I_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_5),
                                                        hit = (total) / n_eyes)

Ret_I_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         spec = (n_eyes - total) / n_eyes)

Ret_I_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_2p5),
                                                        hit = (total) / n_eyes)


## Retina NI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "Ret_NI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "Ret_NI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "Ret_NI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "Ret_NI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "Ret_NI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "Ret_NI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "Ret_NI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "Ret_NI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

Ret_NI_event <- prog_event %>% select(1:8, `Retina Value NI_Baseline`,`Retina Value NI_Follow-Up`) %>%
  mutate(thres_2p5 = round(`Retina Value NI_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`Retina Value NI_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`Retina Value NI_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`Retina Value NI_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`Retina Value NI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`Retina Value NI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`Retina Value NI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`Retina Value NI_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(Ret_NI_event, "mac_volume_Ret_NI_event_MAPS.csv")

Ret_NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

Ret_NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

Ret_NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

Ret_NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)


## RNFL G Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "RNFL_G",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "RNFL_G",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "RNFL_G",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "RNFL_G",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "RNFL_G",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "RNFL_G",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "RNFL_G",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "RNFL_G",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

RNFL_G_event <- prog_event %>% select(1:8, `RNFL Value G_Baseline`,`RNFL Value G_Follow-Up`) %>%
  mutate(thres_2p5 = round(`RNFL Value G_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`RNFL Value G_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`RNFL Value G_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`RNFL Value G_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFL Value G_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFL Value G_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFL Value G_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFL Value G_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(RNFL_G_event, "mac_volume_RNFL_G_event_MAPS.csv")

RNFL_G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         spec = (n_eyes - total) / n_eyes)

RNFL_G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_5),
                                                        hit = (total) / n_eyes)

RNFL_G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         spec = (n_eyes - total) / n_eyes)

RNFL_G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_2p5),
                                                        hit = (total) / n_eyes)

## RNFL TS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "RNFL_TS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "RNFL_TS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "RNFL_TS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "RNFL_TS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "RNFL_TS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "RNFL_TS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "RNFL_TS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "RNFL_TS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

RNFL_TS_event <- prog_event %>% select(1:8, `RNFL Value TS_Baseline`,`RNFL Value TS_Follow-Up`) %>%
  mutate(thres_2p5 = round(`RNFL Value TS_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`RNFL Value TS_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`RNFL Value TS_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`RNFL Value TS_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFL Value TS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFL Value TS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFL Value TS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFL Value TS_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(RNFL_TS_event, "mac_volume_RNFL_TS_event_MAPS.csv")

RNFL_TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

RNFL_TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

RNFL_TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

RNFL_TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)

## RNFL S Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "RNFL_S",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "RNFL_S",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "RNFL_S",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "RNFL_S",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "RNFL_S",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "RNFL_S",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "RNFL_S",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "RNFL_S",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

RNFL_S_event <- prog_event %>% select(1:8, `RNFL Value S_Baseline`,`RNFL Value S_Follow-Up`) %>%
  mutate(thres_2p5 = round(`RNFL Value S_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`RNFL Value S_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`RNFL Value S_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`RNFL Value S_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFL Value S_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFL Value S_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFL Value S_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFL Value S_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(RNFL_S_event, "mac_volume_RNFL_S_event_MAPS.csv")

RNFL_S_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         spec = (n_eyes - total) / n_eyes)

RNFL_S_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_5),
                                                        hit = (total) / n_eyes)

RNFL_S_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         spec = (n_eyes - total) / n_eyes)

RNFL_S_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_2p5),
                                                        hit = (total) / n_eyes)


## RNFL NS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "RNFL_NS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "RNFL_NS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "RNFL_NS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "RNFL_NS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "RNFL_NS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "RNFL_NS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "RNFL_NS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "RNFL_NS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

RNFL_NS_event <- prog_event %>% select(1:8, `RNFL Value NS_Baseline`,`RNFL Value NS_Follow-Up`) %>%
  mutate(thres_2p5 = round(`RNFL Value NS_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`RNFL Value NS_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`RNFL Value NS_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`RNFL Value NS_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFL Value NS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFL Value NS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFL Value NS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFL Value NS_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(RNFL_NS_event, "mac_volume_RNFL_NS_event_MAPS.csv")

RNFL_NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

RNFL_NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

RNFL_NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

RNFL_NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)


## RNFL TI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "RNFL_TI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "RNFL_TI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "RNFL_TI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "RNFL_TI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "RNFL_TI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "RNFL_TI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "RNFL_TI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "RNFL_TI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

RNFL_TI_event <- prog_event %>% select(1:8, `RNFL Value TI_Baseline`,`RNFL Value TI_Follow-Up`) %>%
  mutate(thres_2p5 = round(`RNFL Value TI_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`RNFL Value TI_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`RNFL Value TI_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`RNFL Value TI_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFL Value TI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFL Value TI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFL Value TI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFL Value TI_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(RNFL_TI_event, "mac_volume_RNFL_TI_event_MAPS.csv")

RNFL_TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

RNFL_TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

RNFL_TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

RNFL_TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)

## RNFL I Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "RNFL_I",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "RNFL_I",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "RNFL_I",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "RNFL_I",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "RNFL_I",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "RNFL_I",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "RNFL_I",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "RNFL_I",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

RNFL_I_event <- prog_event %>% select(1:8, `RNFL Value I_Baseline`,`RNFL Value I_Follow-Up`) %>%
  mutate(thres_2p5 = round(`RNFL Value I_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`RNFL Value I_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`RNFL Value I_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`RNFL Value I_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFL Value I_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFL Value I_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFL Value I_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFL Value I_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(RNFL_I_event, "mac_volume_RNFL_I_event_MAPS.csv")

RNFL_I_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         spec = (n_eyes - total) / n_eyes)

RNFL_I_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_5),
                                                        hit = (total) / n_eyes)

RNFL_I_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         spec = (n_eyes - total) / n_eyes)

RNFL_I_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_2p5),
                                                        hit = (total) / n_eyes)


## RNFL NI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "RNFL_NI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "RNFL_NI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "RNFL_NI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "RNFL_NI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "RNFL_NI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "RNFL_NI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "RNFL_NI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "RNFL_NI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

RNFL_NI_event <- prog_event %>% select(1:8, `RNFL Value NI_Baseline`,`RNFL Value NI_Follow-Up`) %>%
  mutate(thres_2p5 = round(`RNFL Value NI_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`RNFL Value NI_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`RNFL Value NI_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`RNFL Value NI_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`RNFL Value NI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`RNFL Value NI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`RNFL Value NI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`RNFL Value NI_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(RNFL_NI_event, "mac_volume_RNFL_NI_event_MAPS.csv")

RNFL_NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

RNFL_NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

RNFL_NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

RNFL_NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)


## GCL G Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "GCL_G",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "GCL_G",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "GCL_G",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "GCL_G",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "GCL_G",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "GCL_G",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "GCL_G",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "GCL_G",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

GCL_G_event <- prog_event %>% select(1:8, `GCL Value G_Baseline`,`GCL Value G_Follow-Up`) %>%
  mutate(thres_2p5 = round(`GCL Value G_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`GCL Value G_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`GCL Value G_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`GCL Value G_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`GCL Value G_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`GCL Value G_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`GCL Value G_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`GCL Value G_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(GCL_G_event, "mac_volume_GCL_G_event_MAPS.csv")

GCL_G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

GCL_G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

GCL_G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

GCL_G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)

## GCL TS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "GCL_TS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "GCL_TS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "GCL_TS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "GCL_TS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "GCL_TS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "GCL_TS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "GCL_TS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "GCL_TS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

GCL_TS_event <- prog_event %>% select(1:8, `GCL Value TS_Baseline`,`GCL Value TS_Follow-Up`) %>%
  mutate(thres_2p5 = round(`GCL Value TS_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`GCL Value TS_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`GCL Value TS_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`GCL Value TS_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`GCL Value TS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`GCL Value TS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`GCL Value TS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`GCL Value TS_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(GCL_TS_event, "mac_volume_GCL_TS_event_MAPS.csv")

GCL_TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                           total = sum(prog_5),
                                                           spec = (n_eyes - total) / n_eyes)

GCL_TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          hit = (total) / n_eyes)

GCL_TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                           total = sum(prog_2p5),
                                                           spec = (n_eyes - total) / n_eyes)

GCL_TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          hit = (total) / n_eyes)

## GCL S Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "GCL_S",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "GCL_S",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "GCL_S",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "GCL_S",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "GCL_S",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "GCL_S",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "GCL_S",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "GCL_S",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

GCL_S_event <- prog_event %>% select(1:8, `GCL Value S_Baseline`,`GCL Value S_Follow-Up`) %>%
  mutate(thres_2p5 = round(`GCL Value S_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`GCL Value S_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`GCL Value S_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`GCL Value S_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`GCL Value S_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`GCL Value S_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`GCL Value S_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`GCL Value S_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(GCL_S_event, "mac_volume_GCL_S_event_MAPS.csv")

GCL_S_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

GCL_S_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

GCL_S_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

GCL_S_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)


## GCL NS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "GCL_NS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "GCL_NS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "GCL_NS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "GCL_NS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "GCL_NS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "GCL_NS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "GCL_NS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "GCL_NS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

GCL_NS_event <- prog_event %>% select(1:8, `GCL Value NS_Baseline`,`GCL Value NS_Follow-Up`) %>%
  mutate(thres_2p5 = round(`GCL Value NS_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`GCL Value NS_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`GCL Value NS_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`GCL Value NS_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`GCL Value NS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`GCL Value NS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`GCL Value NS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`GCL Value NS_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(GCL_NS_event, "mac_volume_GCL_NS_event_MAPS.csv")

GCL_NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                           total = sum(prog_5),
                                                           spec = (n_eyes - total) / n_eyes)

GCL_NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          hit = (total) / n_eyes)

GCL_NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                           total = sum(prog_2p5),
                                                           spec = (n_eyes - total) / n_eyes)

GCL_NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          hit = (total) / n_eyes)


## GCL TI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "GCL_TI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "GCL_TI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "GCL_TI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "GCL_TI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "GCL_TI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "GCL_TI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "GCL_TI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "GCL_TI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

GCL_TI_event <- prog_event %>% select(1:8, `GCL Value TI_Baseline`,`GCL Value TI_Follow-Up`) %>%
  mutate(thres_2p5 = round(`GCL Value TI_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`GCL Value TI_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`GCL Value TI_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`GCL Value TI_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`GCL Value TI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`GCL Value TI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`GCL Value TI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`GCL Value TI_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(GCL_TI_event, "mac_volume_GCL_TI_event_MAPS.csv")

GCL_TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                           total = sum(prog_5),
                                                           spec = (n_eyes - total) / n_eyes)

GCL_TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          hit = (total) / n_eyes)

GCL_TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                           total = sum(prog_2p5),
                                                           spec = (n_eyes - total) / n_eyes)

GCL_TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          hit = (total) / n_eyes)

## GCL I Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "GCL_I",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "GCL_I",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "GCL_I",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "GCL_I",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "GCL_I",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "GCL_I",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "GCL_I",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "GCL_I",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

GCL_I_event <- prog_event %>% select(1:8, `GCL Value I_Baseline`,`GCL Value I_Follow-Up`) %>%
  mutate(thres_2p5 = round(`GCL Value I_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`GCL Value I_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`GCL Value I_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`GCL Value I_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`GCL Value I_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`GCL Value I_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`GCL Value I_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`GCL Value I_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(GCL_I_event, "mac_volume_GCL_I_event_MAPS.csv")

GCL_I_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

GCL_I_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

GCL_I_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

GCL_I_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)


## GCL NI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "GCL_NI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "GCL_NI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "GCL_NI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "GCL_NI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "GCL_NI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "GCL_NI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "GCL_NI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "GCL_NI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

GCL_NI_event <- prog_event %>% select(1:8, `GCL Value NI_Baseline`,`GCL Value NI_Follow-Up`) %>%
  mutate(thres_2p5 = round(`GCL Value NI_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`GCL Value NI_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`GCL Value NI_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`GCL Value NI_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`GCL Value NI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`GCL Value NI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`GCL Value NI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`GCL Value NI_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(GCL_NI_event, "mac_volume_GCL_NI_event_MAPS.csv")

GCL_NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                           total = sum(prog_5),
                                                           spec = (n_eyes - total) / n_eyes)

GCL_NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          hit = (total) / n_eyes)

GCL_NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                           total = sum(prog_2p5),
                                                           spec = (n_eyes - total) / n_eyes)

GCL_NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          hit = (total) / n_eyes)




## IPL G Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "IPL_G",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "IPL_G",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "IPL_G",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "IPL_G",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "IPL_G",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "IPL_G",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "IPL_G",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "IPL_G",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

IPL_G_event <- prog_event %>% select(1:8, `IPL Value G_Baseline`,`IPL Value G_Follow-Up`) %>%
  mutate(thres_2p5 = round(`IPL Value G_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`IPL Value G_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`IPL Value G_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`IPL Value G_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`IPL Value G_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`IPL Value G_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`IPL Value G_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`IPL Value G_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(IPL_G_event, "mac_volume_IPL_G_event_MAPS.csv")

IPL_G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         spec = (n_eyes - total) / n_eyes)

IPL_G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_5),
                                                        hit = (total) / n_eyes)

IPL_G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         spec = (n_eyes - total) / n_eyes)

IPL_G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_2p5),
                                                        hit = (total) / n_eyes)

## IPL TS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "IPL_TS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "IPL_TS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "IPL_TS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "IPL_TS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "IPL_TS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "IPL_TS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "IPL_TS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "IPL_TS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

IPL_TS_event <- prog_event %>% select(1:8, `IPL Value TS_Baseline`,`IPL Value TS_Follow-Up`) %>%
  mutate(thres_2p5 = round(`IPL Value TS_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`IPL Value TS_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`IPL Value TS_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`IPL Value TS_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`IPL Value TS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`IPL Value TS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`IPL Value TS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`IPL Value TS_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(IPL_TS_event, "mac_volume_IPL_TS_event_MAPS.csv")

IPL_TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

IPL_TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

IPL_TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

IPL_TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)

## IPL S Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "IPL_S",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "IPL_S",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "IPL_S",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "IPL_S",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "IPL_S",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "IPL_S",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "IPL_S",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "IPL_S",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

IPL_S_event <- prog_event %>% select(1:8, `IPL Value S_Baseline`,`IPL Value S_Follow-Up`) %>%
  mutate(thres_2p5 = round(`IPL Value S_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`IPL Value S_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`IPL Value S_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`IPL Value S_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`IPL Value S_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`IPL Value S_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`IPL Value S_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`IPL Value S_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(IPL_S_event, "mac_volume_IPL_S_event_MAPS.csv")

IPL_S_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         spec = (n_eyes - total) / n_eyes)

IPL_S_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_5),
                                                        hit = (total) / n_eyes)

IPL_S_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         spec = (n_eyes - total) / n_eyes)

IPL_S_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_2p5),
                                                        hit = (total) / n_eyes)


## IPL NS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "IPL_NS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "IPL_NS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "IPL_NS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "IPL_NS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "IPL_NS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "IPL_NS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "IPL_NS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "IPL_NS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

IPL_NS_event <- prog_event %>% select(1:8, `IPL Value NS_Baseline`,`IPL Value NS_Follow-Up`) %>%
  mutate(thres_2p5 = round(`IPL Value NS_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`IPL Value NS_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`IPL Value NS_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`IPL Value NS_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`IPL Value NS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`IPL Value NS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`IPL Value NS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`IPL Value NS_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(IPL_NS_event, "mac_volume_IPL_NS_event_MAPS.csv")

IPL_NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

IPL_NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

IPL_NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

IPL_NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)


## IPL TI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "IPL_TI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "IPL_TI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "IPL_TI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "IPL_TI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "IPL_TI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "IPL_TI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "IPL_TI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "IPL_TI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

IPL_TI_event <- prog_event %>% select(1:8, `IPL Value TI_Baseline`,`IPL Value TI_Follow-Up`) %>%
  mutate(thres_2p5 = round(`IPL Value TI_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`IPL Value TI_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`IPL Value TI_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`IPL Value TI_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`IPL Value TI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`IPL Value TI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`IPL Value TI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`IPL Value TI_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(IPL_TI_event, "mac_volume_IPL_TI_event_MAPS.csv")

IPL_TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

IPL_TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

IPL_TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

IPL_TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)

## IPL I Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "IPL_I",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "IPL_I",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "IPL_I",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "IPL_I",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "IPL_I",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "IPL_I",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "IPL_I",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "IPL_I",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

IPL_I_event <- prog_event %>% select(1:8, `IPL Value I_Baseline`,`IPL Value I_Follow-Up`) %>%
  mutate(thres_2p5 = round(`IPL Value I_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`IPL Value I_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`IPL Value I_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`IPL Value I_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`IPL Value I_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`IPL Value I_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`IPL Value I_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`IPL Value I_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(IPL_I_event, "mac_volume_IPL_I_event_MAPS.csv")

IPL_I_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         spec = (n_eyes - total) / n_eyes)

IPL_I_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_5),
                                                        hit = (total) / n_eyes)

IPL_I_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         spec = (n_eyes - total) / n_eyes)

IPL_I_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                        total = sum(prog_2p5),
                                                        hit = (total) / n_eyes)


## IPL NI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "IPL_NI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "IPL_NI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "IPL_NI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "IPL_NI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "IPL_NI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "IPL_NI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "IPL_NI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "IPL_NI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

IPL_NI_event <- prog_event %>% select(1:8, `IPL Value NI_Baseline`,`IPL Value NI_Follow-Up`) %>%
  mutate(thres_2p5 = round(`IPL Value NI_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`IPL Value NI_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`IPL Value NI_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`IPL Value NI_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`IPL Value NI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`IPL Value NI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`IPL Value NI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`IPL Value NI_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(IPL_NI_event, "mac_volume_IPL_NI_event_MAPS.csv")

IPL_NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_5),
                                                          spec = (n_eyes - total) / n_eyes)

IPL_NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_5),
                                                         hit = (total) / n_eyes)

IPL_NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                          total = sum(prog_2p5),
                                                          spec = (n_eyes - total) / n_eyes)

IPL_NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                         total = sum(prog_2p5),
                                                         hit = (total) / n_eyes)





### BMO Metrics ###

prog_group <- read_csv("BMO_progGroup_MAPS.csv") %>% distinct() %>% drop_na() %>%
  group_by(Firstname, Eye) %>%
  distinct(Diff_date, .keep_all = TRUE) # I am taking one test from each day; add something to choose best quality of the day

# quant_results <- read_csv("quant_results_cRNFL_small_MAPS.csv")
quant_results <- read_csv("quant_results_BMO_MAPS.csv")

maps_eyes <- read_csv("MAPS_diagnosis_progGrades.csv") %>%
  mutate(Study_Eye = if_else(Eye =="OD","R", "L")) %>%
  select(ID, Study_Eye, Dx, MAPS_Group)

prog_event <- prog_group %>% group_by(Firstname, Eye) %>% distinct(ExamDate, .keep_all = TRUE) %>%
  filter(row_number()==1 | row_number()==n()) %>% ungroup()

prog_event <- left_join(prog_event, maps_eyes,
                        by = c("Firstname"="ID","Eye" = "Study_Eye"))

prog_event <- prog_event %>% drop_na()

prog_event <- prog_event %>%
  mutate(test_time = if_else(Diff_date==0, "Baseline", "Follow-Up"))

prog_event_baseline <- prog_event %>% filter(test_time == "Baseline")

prog_event_baseline <- prog_event_baseline %>% pivot_wider(names_from = test_time, values_from = c(`MRW Global`, `MRW Tmp`,
                                                                                                   `MRW TI`, `MRW TS`, `MRW Nas`,
                                                                                                   `MRW NI`, `MRW NS`)) 

prog_event_fup <- prog_event %>% filter(test_time == "Follow-Up")

prog_event_fup <- prog_event_fup %>% pivot_wider(names_from = test_time, values_from = c(`MRW Global`, `MRW Tmp`,
                                                                                         `MRW TI`, `MRW TS`, `MRW Nas`,
                                                                                         `MRW NI`, `MRW NS`)) 

prog_event <- left_join(prog_event_baseline,prog_event_fup,
                        by = c("Firstname", "Eye", "DOB",
                               "n_tests_total", "n_tests_short", "n_tests_long",
                               "Dx", "MAPS_Group"))

prog_event <- prog_event %>% select(1:3, 6:10, `MRW Global_Baseline`, `MRW Global_Follow-Up`,
                                    `MRW Tmp_Baseline`, `MRW Tmp_Follow-Up`, `MRW TI_Baseline`, `MRW TI_Follow-Up`,
                                    `MRW TS_Baseline`, `MRW TS_Follow-Up`, `MRW Nas_Baseline`, `MRW Nas_Follow-Up`,
                                    `MRW NI_Baseline`, `MRW NI_Follow-Up`, `MRW NS_Baseline`, `MRW NS_Follow-Up`)

## G Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "G",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "G",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "G",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "G",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "G",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "G",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "G",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "G",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

G_event <- prog_event %>% select(1:8,`MRW Global_Baseline`,`MRW Global_Follow-Up`) %>%
  mutate(thres_2p5 = round(`MRW Global_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`MRW Global_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`MRW Global_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`MRW Global_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`MRW Global_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`MRW Global_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`MRW Global_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`MRW Global_Follow-Up` > thres_97p5, TRUE, FALSE))


write_csv(G_event, "BMO_G_event_MAPS.csv")


## T Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "T",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "T",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "T",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "T",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "T",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "T",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "T",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "T",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

T_event <- prog_event %>% select(1:8,`MRW Tmp_Baseline`,`MRW Tmp_Follow-Up`) %>%
  mutate(thres_2p5 = round(`MRW Tmp_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`MRW Tmp_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`MRW Tmp_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`MRW Tmp_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`MRW Tmp_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`MRW Tmp_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`MRW Tmp_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`MRW Tmp_Follow-Up` > thres_97p5, TRUE, FALSE))

write_csv(T_event, "BMO_T_event_MAPS.csv")

## TI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "TI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "TI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "TI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "TI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "TI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "TI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "TI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "TI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

TI_event <- prog_event %>% select(1:8,`MRW TI_Baseline`,`MRW TI_Follow-Up`) %>%
  mutate(thres_2p5 = round(`MRW TI_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`MRW TI_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`MRW TI_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`MRW TI_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`MRW TI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`MRW TI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`MRW TI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`MRW TI_Follow-Up` > thres_97p5, TRUE, FALSE))

write_csv(TI_event, "BMO_TI_event_MAPS.csv")

## TS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "TS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "TS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "TS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "TS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "TS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "TS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "TS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "TS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

TS_event <- prog_event %>% select(1:8,`MRW TS_Baseline`,`MRW TS_Follow-Up`) %>%
  mutate(thres_2p5 = round(`MRW TS_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`MRW TS_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`MRW TS_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`MRW TS_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`MRW TS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`MRW TS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`MRW TS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`MRW TS_Follow-Up` > thres_97p5, TRUE, FALSE))

write_csv(TS_event, "BMO_TS_event_MAPS.csv")

## N Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "N",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "N",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "N",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "N",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "N",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "N",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "N",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "N",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

N_event <- prog_event %>% select(1:8,`MRW Nas_Baseline`, `MRW Nas_Follow-Up`) %>%
  mutate(thres_2p5 = round(`MRW Nas_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`MRW Nas_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`MRW Nas_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`MRW Nas_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`MRW Nas_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`MRW Nas_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`MRW Nas_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`MRW Nas_Follow-Up` > thres_97p5, TRUE, FALSE))

write_csv(N_event, "BMO_N_event_MAPS.csv")

## NI Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "NI",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "NI",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "NI",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "NI",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "NI",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "NI",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "NI",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "NI",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

NI_event <- prog_event %>% select(1:8, `MRW NI_Baseline`, `MRW NI_Follow-Up`) %>%
  mutate(thres_2p5 = round(`MRW NI_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`MRW NI_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`MRW NI_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`MRW NI_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`MRW NI_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`MRW NI_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`MRW NI_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`MRW NI_Follow-Up` > thres_97p5, TRUE, FALSE))

write_csv(NI_event, "BMO_NI_event_MAPS.csv")

## NS Metric - Event
intercept_2p5 <- quant_results %>% filter(sector == "NS",
                                          tau == 0.025,
                                          term == "(Intercept)") %>% pull(estimate)
slope_2p5 <- quant_results %>% filter(sector == "NS",
                                      tau == 0.025,
                                      term == "baseline") %>% pull(estimate)
intercept_5 <- quant_results %>% filter(sector == "NS",
                                        tau == 0.05,
                                        term == "(Intercept)") %>% pull(estimate)
slope_5 <- quant_results %>% filter(sector == "NS",
                                    tau == 0.05,
                                    term == "baseline") %>% pull(estimate)
intercept_97p5 <- quant_results %>% filter(sector == "NS",
                                           tau == 0.975,
                                           term == "(Intercept)") %>% pull(estimate)
slope_97p5 <- quant_results %>% filter(sector == "NS",
                                       tau == 0.975,
                                       term == "baseline") %>% pull(estimate)
intercept_95 <- quant_results %>% filter(sector == "NS",
                                         tau == 0.95,
                                         term == "(Intercept)") %>% pull(estimate)
slope_95 <- quant_results %>% filter(sector == "NS",
                                     tau == 0.95,
                                     term == "baseline") %>% pull(estimate)

NS_event <- prog_event %>% select(1:8, `MRW NS_Baseline`, `MRW NS_Follow-Up`) %>%
  mutate(thres_2p5 = round(`MRW NS_Baseline`*slope_2p5 + intercept_2p5),
         thres_5 = round(`MRW NS_Baseline`*slope_5 + intercept_5),
         thres_95 = round(`MRW NS_Baseline`*slope_95 + intercept_95),
         thres_97p5 = round(`MRW NS_Baseline`*slope_97p5 + intercept_97p5),
         prog_2p5 = if_else(`MRW NS_Follow-Up` < thres_2p5, TRUE, FALSE),
         prog_5 = if_else(`MRW NS_Follow-Up` < thres_5, TRUE, FALSE),
         impr_95 = if_else(`MRW NS_Follow-Up` > thres_95, TRUE, FALSE),
         impr_97p5= if_else(`MRW NS_Follow-Up` > thres_97p5, TRUE, FALSE))

write_csv(NS_event, "BMO_NS_event_MAPS.csv")

## Analysis
G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     spec = (n_eyes - total) / n_eyes)

G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_5),
                                                    hit = (total) / n_eyes)

G_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     spec = (n_eyes - total) / n_eyes)

G_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_2p5),
                                                    hit = (total) / n_eyes)


T_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     spec = (n_eyes - total) / n_eyes)

T_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_5),
                                                    hit = (total) / n_eyes)

T_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     spec = (n_eyes - total) / n_eyes)

T_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_2p5),
                                                    hit = (total) / n_eyes)

TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_5),
                                                      spec = (n_eyes - total) / n_eyes)

TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     hit = (total) / n_eyes)

TI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_2p5),
                                                      spec = (n_eyes - total) / n_eyes)

TI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     hit = (total) / n_eyes)

TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_5),
                                                      spec = (n_eyes - total) / n_eyes)

TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     hit = (total) / n_eyes)

TS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_2p5),
                                                      spec = (n_eyes - total) / n_eyes)

TS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     hit = (total) / n_eyes)


N_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     spec = (n_eyes - total) / n_eyes)

N_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_5),
                                                    hit = (total) / n_eyes)

N_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     spec = (n_eyes - total) / n_eyes)

N_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                    total = sum(prog_2p5),
                                                    hit = (total) / n_eyes)

NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_5),
                                                      spec = (n_eyes - total) / n_eyes)

NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     hit = (total) / n_eyes)

NI_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_2p5),
                                                      spec = (n_eyes - total) / n_eyes)

NI_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     hit = (total) / n_eyes)

NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_5),
                                                      spec = (n_eyes - total) / n_eyes)

NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_5),
                                                     hit = (total) / n_eyes)

NS_event %>% filter(MAPS_Group == "HC") %>% summarize(n_eyes = n(),
                                                      total = sum(prog_2p5),
                                                      spec = (n_eyes - total) / n_eyes)

NS_event %>% filter(MAPS_Group == "P") %>% summarize(n_eyes = n(),
                                                     total = sum(prog_2p5),
                                                     hit = (total) / n_eyes)
