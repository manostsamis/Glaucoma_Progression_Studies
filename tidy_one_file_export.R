library(tidyverse)
library(octr)
library(zeallot)

## From the one-file export

cRNFL_HE_MAPS <- read_csv("MAPS_sector_thickness.csv")

## Only the Firstname column has the IDs in correct format
# 
# cRNFL_HE_MAPS %>% filter(grepl('COL',Firstname)) %>% nrow() == nrow(cRNFL_HE_MAPS)
# 
##

# Keep small circles only and appropriate columns

cRNFL_HE_MAPS$ExamDate <- lubridate::mdy(cRNFL_HE_MAPS$ExamDate)
cRNFL_HE_MAPS$RNFLMean_G <- as.numeric(cRNFL_HE_MAPS$RNFLMean_G)
cRNFL_HE_MAPS$RNFLMean_T <- as.numeric(cRNFL_HE_MAPS$RNFLMean_T)
cRNFL_HE_MAPS$RNFLMean_TI <- as.numeric(cRNFL_HE_MAPS$RNFLMean_TI)
cRNFL_HE_MAPS$RNFLMean_TS <- as.numeric(cRNFL_HE_MAPS$RNFLMean_TS)
cRNFL_HE_MAPS$RNFLMean_N <- as.numeric(cRNFL_HE_MAPS$RNFLMean_N)
cRNFL_HE_MAPS$RNFLMean_NI <- as.numeric(cRNFL_HE_MAPS$RNFLMean_NI)
cRNFL_HE_MAPS$RNFLMean_NS <- as.numeric(cRNFL_HE_MAPS$RNFLMean_NS)

small_circles_all <- cRNFL_HE_MAPS %>% filter(`Scan Type`== "OCT Radial Circle Scan",
                                             `Diameter [mm]` == 4.7) %>% arrange(Firstname, Eye, ExamDate)

small_circles_all <- small_circles_all %>% select(Firstname, Eye, DOB,
                                                  ExamDate, Quality,RNFLMean_G,
                                                  RNFLMean_T, RNFLMean_TI, RNFLMean_TS,
                                                  RNFLMean_N, RNFLMean_NI, RNFLMean_NS) %>%
  drop_na()



# Number of unique tests
small_circles_distinct <- small_circles_all %>% group_by(Firstname,Eye) %>% 
  distinct(ExamDate) %>% summarize(n_tests_total = n())

## There are a few differences between one-file export and our XML export
## (But not too much) - anyway, use one-file export
#
# check_differences <- left_join(small_circles_distinct, unique_onh_per_patient, by = c("Firstname" = "First_Name", "Eye"))
#
##
##


# Add difference in days from baseline
small_circles_all <- small_circles_all %>% group_by(Firstname, Eye) %>%
  mutate(Diff_date = c(0,difftime(tail(ExamDate, -1),
                                  head(ExamDate, 1),units = "days"))) %>%
  dplyr::ungroup()


small_circles_all <- left_join(small_circles_all, small_circles_distinct,
                               by = c("Firstname", "Eye"))


# Add column with number of unique short-term tests (less than 140 days from baseline)
small_circles_2ormore <- small_circles_all %>% filter(n_tests_total>1)

small_circles_distinct_shortTerm <- small_circles_2ormore %>% filter(Diff_date < 140) %>% group_by(Firstname,Eye) %>% 
  distinct(ExamDate, .keep_all = TRUE) %>% summarize(n_tests_short = n())

small_circles_2ormore <- left_join(small_circles_2ormore, small_circles_distinct_shortTerm,
                                   by = c("Firstname", "Eye"))

# Add column with number of unique long-term tests (more than 340 days from baseline)
small_circles_distinct_longTerm <- small_circles_2ormore %>% filter(Diff_date > 340) %>% group_by(Firstname,Eye) %>% 
  distinct(ExamDate, .keep_all = TRUE) %>% summarize(n_tests_long = n())

small_circles_2ormore <- left_join(small_circles_2ormore, small_circles_distinct_longTerm,
                                   by = c("Firstname", "Eye")) 

small_circles_2ormore$n_tests_long <- replace_na(small_circles_2ormore$n_tests_long, 0)

# write_csv(small_circles_2ormore, "cRNFL_small_2ormoreScans_MAPS.csv")
write_csv(small_circles_2ormore, "cRNFL_large_2ormoreScans_MAPS.csv")

var_group <- small_circles_2ormore %>% filter(n_tests_short > 1,
                                              Diff_date < 140)

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


# write_csv(var_group, "cRNFL_small_varGroup_MAPS.csv")
write_csv(var_group, "cRNFL_large_varGroup_MAPS.csv")

prog_group <- small_circles_2ormore %>% filter(n_tests_long > 0,
                                               Diff_date > 340 | Diff_date == 0 )

# prog_group$RNFLMean_G <- as.numeric(prog_group$RNFLMean_G)
# prog_group$RNFLMean_T <- as.numeric(prog_group$RNFLMean_T)
# prog_group$RNFLMean_TI <- as.numeric(prog_group$RNFLMean_TI)
# prog_group$RNFLMean_TS <- as.numeric(prog_group$RNFLMean_TS)
# prog_group$RNFLMean_N <- as.numeric(prog_group$RNFLMean_N)
# prog_group$RNFLMean_NI <- as.numeric(prog_group$RNFLMean_NI)
# prog_group$RNFLMean_NS <- as.numeric(prog_group$RNFLMean_NS)
# 
# prog_group <- prog_group %>% drop_na(RNFLMean_G,RNFLMean_T,
#                                      RNFLMean_TI, RNFLMean_TS,
#                                      RNFLMean_N,
#                                      RNFLMean_NI, RNFLMean_NS)

# write_csv(prog_group, "cRNFL_small_progGroup_MAPS.csv")
write_csv(prog_group, "cRNFL_large_progGroup_MAPS.csv")







### Macular Metrics ###

## From the one-file export

mac_HE_MAPS <- read_csv("DeviationMapsExport.csv")

## Only the Firstname column has the IDs in correct format
# 
#mac_HE_MAPS %>% filter(grepl('COL',FirstName)) %>% nrow() == nrow(mac_HE_MAPS)
# 
##

# Keep appropriate columns

mac_HE_MAPS <- mac_HE_MAPS %>% rename(Firstname = "FirstName") %>%
  select(Firstname, Eye, DOB,
         ExamDate, Quality,
         `Retina Value G`, `Retina Value TS`, `Retina Value S`,`Retina Value NS`, 
         `Retina Value TI`, `Retina Value I`, `Retina Value NI`,
         `RNFL Value G`, `RNFL Value TS`, `RNFL Value S`,`RNFL Value NS`,
         `RNFL Value NI`, `RNFL Value I`, `RNFL Value TI`,
         `IPL Value G`, `IPL Value TS`, `IPL Value S`,`IPL Value NS`,
         `IPL Value NI`, `IPL Value I`, `IPL Value TI`,
         `GCL Value G`, `GCL Value TS`, `GCL Value S`,`GCL Value NS`,
         `GCL Value NI`, `GCL Value I`, `GCL Value TI`)

mac_HE_MAPS$ExamDate <- lubridate::mdy(mac_HE_MAPS$ExamDate)
# mac_HE_MAPS$DOB <- lubridate::mdy(mac_HE_MAPS$DOB) ## issue wit showing the correct century

mac_HE_MAPS <- mac_HE_MAPS  %>% arrange(Firstname, Eye, ExamDate) %>%
  drop_na()




# Number of unique tests
mac_volume_distinct <- mac_HE_MAPS %>% group_by(Firstname,Eye) %>% 
  distinct(ExamDate) %>% summarize(n_tests_total = n())



# Add difference in days from baseline
mac_HE_MAPS <- mac_HE_MAPS %>% group_by(Firstname, Eye) %>%
  mutate(Diff_date = c(0,difftime(tail(ExamDate, -1),
                                  head(ExamDate, 1),units = "days"))) %>%
  ungroup()


mac_HE_MAPS <- left_join(mac_HE_MAPS, mac_volume_distinct,
                         by = c("Firstname", "Eye"))


# Add column with number of unique short-term tests (less than 140 days from baseline)
mac_volume_2ormore <- mac_HE_MAPS %>% filter(n_tests_total>1)

mac_volume_shortTerm <- mac_volume_2ormore %>% filter(Diff_date < 140) %>% group_by(Firstname,Eye) %>% 
  distinct(ExamDate, .keep_all = TRUE) %>% summarize(n_tests_short = n()) 

mac_volume_2ormore <- left_join(mac_volume_2ormore, mac_volume_shortTerm,
                                by = c("Firstname", "Eye"))

# Add column with number of unique long-term tests (more than 340 days from baseline)
mac_volume_distinct_longTerm <- mac_volume_2ormore %>% filter(Diff_date > 340) %>% group_by(Firstname,Eye) %>% 
  distinct(ExamDate, .keep_all = TRUE) %>% summarize(n_tests_long = n())

mac_volume_2ormore <- left_join(mac_volume_2ormore, mac_volume_distinct_longTerm,
                                by = c("Firstname", "Eye")) 

mac_volume_2ormore$n_tests_long <- replace_na(mac_volume_2ormore$n_tests_long, 0)

write_csv(mac_volume_2ormore, "mac_volume_2ormoreScans_MAPS.csv")

var_group <- mac_volume_2ormore %>% filter(n_tests_short > 1,
                                           Diff_date < 140)

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


# write_csv(var_group, "cRNFL_small_varGroup_MAPS.csv")
write_csv(var_group, "mac_volume_varGroup_MAPS.csv")

prog_group <- mac_volume_2ormore %>% filter(n_tests_long > 0,
                                            Diff_date > 340 | Diff_date == 0 )

# prog_group$RNFLMean_G <- as.numeric(prog_group$RNFLMean_G)
# prog_group$RNFLMean_T <- as.numeric(prog_group$RNFLMean_T)
# prog_group$RNFLMean_TI <- as.numeric(prog_group$RNFLMean_TI)
# prog_group$RNFLMean_TS <- as.numeric(prog_group$RNFLMean_TS)
# prog_group$RNFLMean_N <- as.numeric(prog_group$RNFLMean_N)
# prog_group$RNFLMean_NI <- as.numeric(prog_group$RNFLMean_NI)
# prog_group$RNFLMean_NS <- as.numeric(prog_group$RNFLMean_NS)
# 
# prog_group <- prog_group %>% drop_na(RNFLMean_G,RNFLMean_T,
#                                      RNFLMean_TI, RNFLMean_TS,
#                                      RNFLMean_N,
#                                      RNFLMean_NI, RNFLMean_NS)

# write_csv(prog_group, "cRNFL_small_progGroup_MAPS.csv")
write_csv(prog_group, "mac_volume_progGroup_MAPS.csv")



### BMO Metrics ###

## From the one-file export

BMO_HE_MAPS <- read_csv("BMO_metrics_HE.csv")

## Only the Firstname column has the IDs in correct format
# 
#BMO_HE_MAPS %>% filter(grepl('COL',Firstname)) %>% nrow() == nrow(BMO_HE_MAPS)
# 
##

# Keep appropriate columns

BMO_HE_MAPS <- BMO_HE_MAPS %>%
  select(Firstname, Eye, DOB,
         ExamDate, `MRW Global`,
         `MRW Tmp`, `MRW TS`, `MRW TI`,
         `MRW Nas`, `MRW NS`, `MRW NI`)

BMO_HE_MAPS$ExamDate <- lubridate::ymd(BMO_HE_MAPS$ExamDate)
BMO_HE_MAPS$DOB <- lubridate::ymd(BMO_HE_MAPS$DOB)

BMO_HE_MAPS <- BMO_HE_MAPS  %>% arrange(Firstname, Eye, ExamDate) %>%
  drop_na()




# Number of unique tests
BMO_distinct <- BMO_HE_MAPS %>% group_by(Firstname,Eye) %>% 
  distinct(ExamDate) %>% summarize(n_tests_total = n())



# Add difference in days from baseline
BMO_HE_MAPS <- BMO_HE_MAPS %>% group_by(Firstname, Eye) %>%
  mutate(Diff_date = c(0,difftime(tail(ExamDate, -1),
                                  head(ExamDate, 1),units = "days"))) %>%
  ungroup()


BMO_HE_MAPS <- left_join(BMO_HE_MAPS, BMO_distinct,
                         by = c("Firstname", "Eye"))


# Add column with number of unique short-term tests (less than 140 days from baseline)
BMO_2ormore <- BMO_HE_MAPS %>% filter(n_tests_total>1)

BMO_shortTerm <- BMO_2ormore %>% filter(Diff_date < 140) %>% group_by(Firstname,Eye) %>% 
  distinct(ExamDate, .keep_all = TRUE) %>% summarize(n_tests_short = n()) 

BMO_2ormore <- left_join(BMO_2ormore, BMO_shortTerm,
                                by = c("Firstname", "Eye"))

# Add column with number of unique long-term tests (more than 340 days from baseline)
BMO_distinct_longTerm <- BMO_2ormore %>% filter(Diff_date > 340) %>% group_by(Firstname,Eye) %>% 
  distinct(ExamDate, .keep_all = TRUE) %>% summarize(n_tests_long = n())

BMO_2ormore <- left_join(BMO_2ormore, BMO_distinct_longTerm,
                                by = c("Firstname", "Eye")) 

BMO_2ormore$n_tests_long <- replace_na(BMO_2ormore$n_tests_long, 0)

BMO_2ormore$`MRW Global` <- as.numeric(BMO_2ormore$`MRW Global`)
BMO_2ormore$`MRW Tmp` <- as.numeric(BMO_2ormore$`MRW Tmp`)
BMO_2ormore$`MRW TS` <- as.numeric(BMO_2ormore$`MRW TS`)
BMO_2ormore$`MRW TI` <- as.numeric(BMO_2ormore$`MRW TI`)
BMO_2ormore$`MRW Nas` <- as.numeric(BMO_2ormore$`MRW Nas`)
BMO_2ormore$`MRW NS` <- as.numeric(BMO_2ormore$`MRW NS`)
BMO_2ormore$`MRW NI` <- as.numeric(BMO_2ormore$`MRW NI`)

BMO_2ormore <- BMO_2ormore %>% drop_na()

write_csv(BMO_2ormore, "BMO_2ormoreScans_MAPS.csv")

var_group <- BMO_2ormore %>% filter(n_tests_short > 1,
                                           Diff_date < 140)


write_csv(var_group, "BMO_varGroup_MAPS.csv")

prog_group <- BMO_2ormore %>% filter(n_tests_long > 0,
                                            Diff_date > 340 | Diff_date == 0 )

write_csv(prog_group, "BMO_progGroup_MAPS.csv")


