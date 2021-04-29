library(tidyverse)


## Fixed an issue where columns were not numeric and NAs were introduced
# var_group$RNFLMean_G <- as.numeric(var_group$RNFLMean_G)
# var_group$RNFLMean_T <- as.numeric(var_group$RNFLMean_T)
# var_group$RNFLMean_TI <- as.numeric(var_group$RNFLMean_TI)
# var_group$RNFLMean_TS <- as.numeric(var_group$RNFLMean_TS)
# var_group$RNFLMean_N <- as.numeric(var_group$RNFLMean_N)
# var_group$RNFLMean_NI <- as.numeric(var_group$RNFLMean_NI)
# var_group$RNFLMean_NS <- as.numeric(var_group$RNFLMean_NS)
# 
# var_group <- var_group %>% drop_na(RNFLMean_G,RNFLMean_T,
#                                    RNFLMean_TI, RNFLMean_TS,
#                                    RNFLMean_N,
#                                    RNFLMean_NI, RNFLMean_NS)
# 
# write_csv(var_group, "cRNFL_small_varGroup_MAPS.csv")

# var_group <- read_csv("cRNFL_small_varGroup_MAPS.csv")
var_group <- read_csv("cRNFL_large_varGroup_MAPS.csv")

eligible_eyes <- var_group %>% distinct(Firstname,Eye)


all_G_sectors <- tibble()
all_T_sectors <- tibble()
all_TI_sectors <- tibble()
all_TS_sectors <- tibble()
all_N_sectors <- tibble()
all_NI_sectors <- tibble()
all_NS_sectors <- tibble()


for(i in 1:nrow(eligible_eyes)){

  eye_scans <- var_group %>% filter(Firstname == eligible_eyes$Firstname[i],
                                    Eye == eligible_eyes$Eye[i])
  
  baseline_scan <- eye_scans[1,]
  fup_scans <- eye_scans[-1,]
  
  for(j in 1:nrow(fup_scans)){
    G_sector <- bind_cols(baseline = baseline_scan$RNFLMean_G[1],
                          fup = fup_scans$RNFLMean_G[j])
    
    all_G_sectors <- bind_rows(all_G_sectors,
                               G_sector)
    
    T_sector <- bind_cols(baseline = baseline_scan$RNFLMean_T[1],
                          fup = fup_scans$RNFLMean_T[j])
    
    all_T_sectors <- bind_rows(all_T_sectors,
                               T_sector)
    
    TI_sector <- bind_cols(baseline = baseline_scan$RNFLMean_TI[1],
                          fup = fup_scans$RNFLMean_TI[j])
    
    all_TI_sectors <- bind_rows(all_TI_sectors,
                               TI_sector)
    
    TS_sector <- bind_cols(baseline = baseline_scan$RNFLMean_TS[1],
                           fup = fup_scans$RNFLMean_TS[j])
    
    all_TS_sectors <- bind_rows(all_TS_sectors,
                                TS_sector)
    
    N_sector <- bind_cols(baseline = baseline_scan$RNFLMean_N[1],
                          fup = fup_scans$RNFLMean_N[j])
    
    all_N_sectors <- bind_rows(all_N_sectors,
                               N_sector)
    
    NI_sector <- bind_cols(baseline = baseline_scan$RNFLMean_NI[1],
                           fup = fup_scans$RNFLMean_NI[j])
    
    all_NI_sectors <- bind_rows(all_NI_sectors,
                                NI_sector)
    
    NS_sector <- bind_cols(baseline = baseline_scan$RNFLMean_NS[1],
                           fup = fup_scans$RNFLMean_NS[j])
    
    all_NS_sectors <- bind_rows(all_NS_sectors,
                                NS_sector)
    
    
    
  }
  
}


quantreg_G <- quantreg::rq(fup ~ baseline, data = all_G_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "G")

quantreg_T <- quantreg::rq(fup ~ baseline, data = all_T_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "T")

quantreg_TI <- quantreg::rq(fup ~ baseline, data = all_TI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "TI")

quantreg_TS <- quantreg::rq(fup ~ baseline, data = all_TS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "TS")

quantreg_N <- quantreg::rq(fup ~ baseline, data = all_N_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "N")

quantreg_NI <- quantreg::rq(fup ~ baseline, data = all_NI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "NI")

quantreg_NS <- quantreg::rq(fup ~ baseline, data = all_NS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "NS")

quant_results <- bind_rows(quantreg_G,
                           quantreg_T,
                           quantreg_TI,
                           quantreg_TS,
                           quantreg_N,
                           quantreg_NI,
                           quantreg_NS)

# write_csv(quant_results, "quant_results_cRNFL_small_MAPS.csv")
write_csv(quant_results, "quant_results_cRNFL_large_MAPS.csv")





### Mac Metrics ###

var_group <- read_csv("mac_volume_varGroup_MAPS.csv")

eligible_eyes <- var_group %>% distinct(Firstname,Eye)


all_Ret_G_sectors <- tibble()
all_Ret_TI_sectors <- tibble()
all_Ret_I_sectors <- tibble()
all_Ret_NI_sectors <- tibble()
all_Ret_TS_sectors <- tibble()
all_Ret_S_sectors <- tibble()
all_Ret_NS_sectors <- tibble()

all_RNFL_G_sectors <- tibble()
all_RNFL_TI_sectors <- tibble()
all_RNFL_I_sectors <- tibble()
all_RNFL_NI_sectors <- tibble()
all_RNFL_TS_sectors <- tibble()
all_RNFL_S_sectors <- tibble()
all_RNFL_NS_sectors <- tibble()

all_GCL_G_sectors <- tibble()
all_GCL_TI_sectors <- tibble()
all_GCL_I_sectors <- tibble()
all_GCL_NI_sectors <- tibble()
all_GCL_TS_sectors <- tibble()
all_GCL_S_sectors <- tibble()
all_GCL_NS_sectors <- tibble()

all_IPL_G_sectors <- tibble()
all_IPL_TI_sectors <- tibble()
all_IPL_I_sectors <- tibble()
all_IPL_NI_sectors <- tibble()
all_IPL_TS_sectors <- tibble()
all_IPL_S_sectors <- tibble()
all_IPL_NS_sectors <- tibble()


for(i in 1:nrow(eligible_eyes)){
  
  eye_scans <- var_group %>% filter(Firstname == eligible_eyes$Firstname[i],
                                    Eye == eligible_eyes$Eye[i])
  
  baseline_scan <- eye_scans[1,]
  fup_scans <- eye_scans[-1,]
  
  for(j in 1:nrow(fup_scans)){
    Ret_G_sector <- bind_cols(baseline = baseline_scan$`Retina Value G`[1],
                          fup = fup_scans$`Retina Value G`[j])
    
    all_Ret_G_sectors <- bind_rows(all_Ret_G_sectors,
                                   Ret_G_sector)
    
    Ret_S_sector <- bind_cols(baseline = baseline_scan$`Retina Value S`[1],
                          fup = fup_scans$`Retina Value S`[j])
    
    all_Ret_S_sectors <- bind_rows(all_Ret_S_sectors,
                                   Ret_S_sector)
    
    Ret_TI_sector <- bind_cols(baseline = baseline_scan$`Retina Value TI`[1],
                           fup = fup_scans$`Retina Value TI`[j])
    
    all_Ret_TI_sectors <- bind_rows(all_Ret_TI_sectors,
                                    Ret_TI_sector)
    
    Ret_TS_sector <- bind_cols(baseline = baseline_scan$`Retina Value TS`[1],
                           fup = fup_scans$`Retina Value TS`[j])
    
    all_Ret_TS_sectors <- bind_rows(all_Ret_TS_sectors,
                                    Ret_TS_sector)
    
    Ret_I_sector <- bind_cols(baseline = baseline_scan$`Retina Value I`[1],
                          fup = fup_scans$`Retina Value I`[j])
    
    all_Ret_I_sectors <- bind_rows(all_Ret_I_sectors,
                                   Ret_I_sector)
    
    Ret_NI_sector <- bind_cols(baseline = baseline_scan$`Retina Value NI`[1],
                           fup = fup_scans$`Retina Value NI`[j])
    
    all_Ret_NI_sectors <- bind_rows(all_Ret_NI_sectors,
                                    Ret_NI_sector)
    
    Ret_NS_sector <- bind_cols(baseline = baseline_scan$`Retina Value NS`[1],
                           fup = fup_scans$`Retina Value NS`[j])
    
    all_Ret_NS_sectors <- bind_rows(all_Ret_NS_sectors,
                                    Ret_NS_sector)
    
    ##
    RNFL_G_sector <- bind_cols(baseline = baseline_scan$`RNFL Value G`[1],
                              fup = fup_scans$`RNFL Value G`[j])
    
    all_RNFL_G_sectors <- bind_rows(all_RNFL_G_sectors,
                                    RNFL_G_sector)
    
    RNFL_S_sector <- bind_cols(baseline = baseline_scan$`RNFL Value S`[1],
                              fup = fup_scans$`RNFL Value S`[j])
    
    all_RNFL_S_sectors <- bind_rows(all_RNFL_S_sectors,
                                    RNFL_S_sector)
    
    RNFL_TI_sector <- bind_cols(baseline = baseline_scan$`RNFL Value TI`[1],
                               fup = fup_scans$`RNFL Value TI`[j])
    
    all_RNFL_TI_sectors <- bind_rows(all_RNFL_TI_sectors,
                                     RNFL_TI_sector)
    
    RNFL_TS_sector <- bind_cols(baseline = baseline_scan$`RNFL Value TS`[1],
                               fup = fup_scans$`RNFL Value TS`[j])
    
    all_RNFL_TS_sectors <- bind_rows(all_RNFL_TS_sectors,
                                     RNFL_TS_sector)
    
    RNFL_I_sector <- bind_cols(baseline = baseline_scan$`RNFL Value I`[1],
                              fup = fup_scans$`RNFL Value I`[j])
    
    all_RNFL_I_sectors <- bind_rows(all_RNFL_I_sectors,
                                    RNFL_I_sector)
    
    RNFL_NI_sector <- bind_cols(baseline = baseline_scan$`RNFL Value NI`[1],
                               fup = fup_scans$`RNFL Value NI`[j])
    
    all_RNFL_NI_sectors <- bind_rows(all_RNFL_NI_sectors,
                                     RNFL_NI_sector)
    
    RNFL_NS_sector <- bind_cols(baseline = baseline_scan$`RNFL Value NS`[1],
                               fup = fup_scans$`RNFL Value NS`[j])
    
    all_RNFL_NS_sectors <- bind_rows(all_RNFL_NS_sectors,
                                     RNFL_NS_sector)
    
    ##
    GCL_G_sector <- bind_cols(baseline = baseline_scan$`GCL Value G`[1],
                               fup = fup_scans$`GCL Value G`[j])
    
    all_GCL_G_sectors <- bind_rows(all_GCL_G_sectors,
                                   GCL_G_sector)
    
    GCL_S_sector <- bind_cols(baseline = baseline_scan$`GCL Value S`[1],
                               fup = fup_scans$`GCL Value S`[j])
    
    all_GCL_S_sectors <- bind_rows(all_GCL_S_sectors,
                                   GCL_S_sector)
    
    GCL_TI_sector <- bind_cols(baseline = baseline_scan$`GCL Value TI`[1],
                                fup = fup_scans$`GCL Value TI`[j])
    
    all_GCL_TI_sectors <- bind_rows(all_GCL_TI_sectors,
                                    GCL_TI_sector)
    
    GCL_TS_sector <- bind_cols(baseline = baseline_scan$`GCL Value TS`[1],
                                fup = fup_scans$`GCL Value TS`[j])
    
    all_GCL_TS_sectors <- bind_rows(all_GCL_TS_sectors,
                                    GCL_TS_sector)
    
    GCL_I_sector <- bind_cols(baseline = baseline_scan$`GCL Value I`[1],
                               fup = fup_scans$`GCL Value I`[j])
    
    all_GCL_I_sectors <- bind_rows(all_GCL_I_sectors,
                                   GCL_I_sector)
    
    GCL_NI_sector <- bind_cols(baseline = baseline_scan$`GCL Value NI`[1],
                                fup = fup_scans$`GCL Value NI`[j])
    
    all_GCL_NI_sectors <- bind_rows(all_GCL_NI_sectors,
                                    GCL_NI_sector)
    
    GCL_NS_sector <- bind_cols(baseline = baseline_scan$`GCL Value NS`[1],
                                fup = fup_scans$`GCL Value NS`[j])
    
    all_GCL_NS_sectors <- bind_rows(all_GCL_NS_sectors,
                                    GCL_NS_sector)
    
    
    ##
    IPL_G_sector <- bind_cols(baseline = baseline_scan$`IPL Value G`[1],
                              fup = fup_scans$`IPL Value G`[j])
    
    all_IPL_G_sectors <- bind_rows(all_IPL_G_sectors,
                                   IPL_G_sector)
    
    IPL_S_sector <- bind_cols(baseline = baseline_scan$`IPL Value S`[1],
                              fup = fup_scans$`IPL Value S`[j])
    
    all_IPL_S_sectors <- bind_rows(all_IPL_S_sectors,
                                   IPL_S_sector)
    
    IPL_TI_sector <- bind_cols(baseline = baseline_scan$`IPL Value TI`[1],
                               fup = fup_scans$`IPL Value TI`[j])
    
    all_IPL_TI_sectors <- bind_rows(all_IPL_TI_sectors,
                                    IPL_TI_sector)
    
    IPL_TS_sector <- bind_cols(baseline = baseline_scan$`IPL Value TS`[1],
                               fup = fup_scans$`IPL Value TS`[j])
    
    all_IPL_TS_sectors <- bind_rows(all_IPL_TS_sectors,
                                    IPL_TS_sector)
    
    IPL_I_sector <- bind_cols(baseline = baseline_scan$`IPL Value I`[1],
                              fup = fup_scans$`IPL Value I`[j])
    
    all_IPL_I_sectors <- bind_rows(all_IPL_I_sectors,
                                   IPL_I_sector)
    
    IPL_NI_sector <- bind_cols(baseline = baseline_scan$`IPL Value NI`[1],
                               fup = fup_scans$`IPL Value NI`[j])
    
    all_IPL_NI_sectors <- bind_rows(all_IPL_NI_sectors,
                                    IPL_NI_sector)
    
    IPL_NS_sector <- bind_cols(baseline = baseline_scan$`IPL Value NS`[1],
                               fup = fup_scans$`IPL Value NS`[j])
    
    all_IPL_NS_sectors <- bind_rows(all_IPL_NS_sectors,
                                    IPL_NS_sector)
    
    
    
    
    
  }
  
}


quantreg_Ret_G <- quantreg::rq(fup ~ baseline, data = all_Ret_G_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "Ret_G")

quantreg_Ret_I <- quantreg::rq(fup ~ baseline, data = all_Ret_I_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "Ret_I")

quantreg_Ret_TI <- quantreg::rq(fup ~ baseline, data = all_Ret_TI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "Ret_TI")

quantreg_Ret_TS <- quantreg::rq(fup ~ baseline, data = all_Ret_TS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "Ret_TS")

quantreg_Ret_S <- quantreg::rq(fup ~ baseline, data = all_Ret_S_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "Ret_S")

quantreg_Ret_NI <- quantreg::rq(fup ~ baseline, data = all_Ret_NI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "Ret_NI")

quantreg_Ret_NS <- quantreg::rq(fup ~ baseline, data = all_Ret_NS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "Ret_NS")



quantreg_RNFL_G <- quantreg::rq(fup ~ baseline, data = all_RNFL_G_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "RNFL_G")

quantreg_RNFL_I <- quantreg::rq(fup ~ baseline, data = all_RNFL_I_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "RNFL_I")

quantreg_RNFL_TI <- quantreg::rq(fup ~ baseline, data = all_RNFL_TI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "RNFL_TI")

quantreg_RNFL_TS <- quantreg::rq(fup ~ baseline, data = all_RNFL_TS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "RNFL_TS")

quantreg_RNFL_S <- quantreg::rq(fup ~ baseline, data = all_RNFL_S_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "RNFL_S")

quantreg_RNFL_NI <- quantreg::rq(fup ~ baseline, data = all_RNFL_NI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "RNFL_NI")

quantreg_RNFL_NS <- quantreg::rq(fup ~ baseline, data = all_RNFL_NS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "RNFL_NS")





quantreg_GCL_G <- quantreg::rq(fup ~ baseline, data = all_GCL_G_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "GCL_G")

quantreg_GCL_I <- quantreg::rq(fup ~ baseline, data = all_GCL_I_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "GCL_I")

quantreg_GCL_TI <- quantreg::rq(fup ~ baseline, data = all_GCL_TI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "GCL_TI")

quantreg_GCL_TS <- quantreg::rq(fup ~ baseline, data = all_GCL_TS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "GCL_TS")

quantreg_GCL_S <- quantreg::rq(fup ~ baseline, data = all_GCL_S_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "GCL_S")

quantreg_GCL_NI <- quantreg::rq(fup ~ baseline, data = all_GCL_NI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "GCL_NI")

quantreg_GCL_NS <- quantreg::rq(fup ~ baseline, data = all_GCL_NS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "GCL_NS")



quantreg_IPL_G <- quantreg::rq(fup ~ baseline, data = all_IPL_G_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "IPL_G")

quantreg_IPL_I <- quantreg::rq(fup ~ baseline, data = all_IPL_I_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "IPL_I")

quantreg_IPL_TI <- quantreg::rq(fup ~ baseline, data = all_IPL_TI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "IPL_TI")

quantreg_IPL_TS <- quantreg::rq(fup ~ baseline, data = all_IPL_TS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "IPL_TS")

quantreg_IPL_S <- quantreg::rq(fup ~ baseline, data = all_IPL_S_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "IPL_S")

quantreg_IPL_NI <- quantreg::rq(fup ~ baseline, data = all_IPL_NI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "IPL_NI")

quantreg_IPL_NS <- quantreg::rq(fup ~ baseline, data = all_IPL_NS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "IPL_NS")


quant_results <- bind_rows(quantreg_Ret_G,
                           quantreg_Ret_TI,
                           quantreg_Ret_I,
                           quantreg_Ret_NI,
                           quantreg_Ret_TS,
                           quantreg_Ret_S,
                           quantreg_Ret_NS,
                           quantreg_RNFL_G,
                           quantreg_RNFL_TI,
                           quantreg_RNFL_I,
                           quantreg_RNFL_NI,
                           quantreg_RNFL_TS,
                           quantreg_RNFL_S,
                           quantreg_RNFL_NS,
                           quantreg_GCL_G,
                           quantreg_GCL_TI,
                           quantreg_GCL_I,
                           quantreg_GCL_NI,
                           quantreg_GCL_TS,
                           quantreg_GCL_S,
                           quantreg_GCL_NS,
                           quantreg_IPL_G,
                           quantreg_IPL_TI,
                           quantreg_IPL_I,
                           quantreg_IPL_NI,
                           quantreg_IPL_TS,
                           quantreg_IPL_S,
                           quantreg_IPL_NS)


write_csv(quant_results, "quant_results_mac_volume_MAPS.csv")



### BMO metrics ###

var_group <- read_csv("BMO_varGroup_MAPS.csv")

eligible_eyes <- var_group %>% distinct(Firstname,Eye)


all_G_sectors <- tibble()
all_T_sectors <- tibble()
all_TI_sectors <- tibble()
all_TS_sectors <- tibble()
all_N_sectors <- tibble()
all_NI_sectors <- tibble()
all_NS_sectors <- tibble()


for(i in 1:nrow(eligible_eyes)){
  
  eye_scans <- var_group %>% filter(Firstname == eligible_eyes$Firstname[i],
                                    Eye == eligible_eyes$Eye[i])
  
  baseline_scan <- eye_scans[1,]
  fup_scans <- eye_scans[-1,]
  
  for(j in 1:nrow(fup_scans)){
    G_sector <- bind_cols(baseline = baseline_scan$`MRW Global`[1],
                          fup = fup_scans$`MRW Global`[j])
    
    all_G_sectors <- bind_rows(all_G_sectors,
                               G_sector)
    
    T_sector <- bind_cols(baseline = baseline_scan$`MRW Tmp`[1],
                          fup = fup_scans$`MRW Tmp`[j])
    
    all_T_sectors <- bind_rows(all_T_sectors,
                               T_sector)
    
    TI_sector <- bind_cols(baseline = baseline_scan$`MRW TI`[1],
                           fup = fup_scans$`MRW TI`[j])
    
    all_TI_sectors <- bind_rows(all_TI_sectors,
                                TI_sector)
    
    TS_sector <- bind_cols(baseline = baseline_scan$`MRW TS`[1],
                           fup = fup_scans$`MRW TS`[j])
    
    all_TS_sectors <- bind_rows(all_TS_sectors,
                                TS_sector)
    
    N_sector <- bind_cols(baseline = baseline_scan$`MRW Nas`[1],
                          fup = fup_scans$`MRW Nas`[j])
    
    all_N_sectors <- bind_rows(all_N_sectors,
                               N_sector)
    
    NI_sector <- bind_cols(baseline = baseline_scan$`MRW NI`[1],
                           fup = fup_scans$`MRW NI`[j])
    
    all_NI_sectors <- bind_rows(all_NI_sectors,
                                NI_sector)
    
    NS_sector <- bind_cols(baseline = baseline_scan$`MRW NS`[1],
                           fup = fup_scans$`MRW NS`[j])
    
    all_NS_sectors <- bind_rows(all_NS_sectors,
                                NS_sector)
    
    
    
  }
  
}


quantreg_G <- quantreg::rq(fup ~ baseline, data = all_G_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "G")

quantreg_T <- quantreg::rq(fup ~ baseline, data = all_T_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "T")

quantreg_TI <- quantreg::rq(fup ~ baseline, data = all_TI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "TI")

quantreg_TS <- quantreg::rq(fup ~ baseline, data = all_TS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "TS")

quantreg_N <- quantreg::rq(fup ~ baseline, data = all_N_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "N")

quantreg_NI <- quantreg::rq(fup ~ baseline, data = all_NI_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "NI")

quantreg_NS <- quantreg::rq(fup ~ baseline, data = all_NS_sectors, tau = c(0.025,0.05,0.95,0.975)) %>%
  broom::tidy() %>% mutate(sector = "NS")

quant_results <- bind_rows(quantreg_G,
                           quantreg_T,
                           quantreg_TI,
                           quantreg_TS,
                           quantreg_N,
                           quantreg_NI,
                           quantreg_NS)

# write_csv(quant_results, "quant_results_cRNFL_small_MAPS.csv")
write_csv(quant_results, "quant_results_BMO_MAPS.csv")
