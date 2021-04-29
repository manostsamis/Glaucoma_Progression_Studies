library(tidyverse)

all_onh <- readr::read_csv("all_xml_exports_w_correct_segment_010521.csv") %>%
  dplyr::filter(OCT_Type == "ONH" & Num_Images == 28)

all_onh$Filename <- stringr::str_replace(all_onh$Filename,"/Volumes/z/","/Volumes/hoodorchard.psych.columbia.edu/")

all_ppScans <- readr::read_csv("all_xml_exports_w_correct_segment_010521.csv") %>%
  dplyr::filter(OCT_Type == "Retina" & Num_Images == 62)

all_ppScans$Filename <- stringr::str_replace(all_ppScans$Filename,"/Volumes/z/","/Volumes/hoodorchard.psych.columbia.edu/")

onh_tidy <- all_onh %>% dplyr::select(1:8)

onh_tidy <- onh_tidy %>% dplyr::arrange(First_Name, Eye, DoT) %>%
  dplyr::group_by(First_Name, Eye) %>%
  dplyr::mutate(Diff_date = c(0,difftime(tail(DoT, -1),
                                         head(DoT, 1),units = "days"))) %>%
  dplyr::ungroup()

onh_tidy <- onh_tidy %>% filter(Diff_date < 100)

onh_unique_tests <- onh_tidy %>%
  dplyr::group_by(First_Name, Eye) %>%
  dplyr::distinct(First_Name, Eye, DoT, .keep_all = TRUE) %>%
  dplyr::mutate(n_tests = n()) %>%
  dplyr::ungroup()

unique_onh_per_patient <- onh_unique_tests %>%
  dplyr::group_by(First_Name, Eye) %>%
  dplyr::distinct(First_Name, Eye, DoT, .keep_all = TRUE) %>%
  dplyr::summarize(n_tests = n()) %>%
  dplyr::ungroup()

onh_tidy <- dplyr::left_join(onh_tidy, unique_onh_per_patient,
                             by = c("First_Name","Eye"))

onh_tidy <- onh_tidy %>% filter(n_tests >1)

baseline_tests <- onh_tidy %>% group_by(First_Name, Eye) %>% slice(1,2)

all_ppScans <- readr::read_csv("all_xml_exports_w_correct_segment_010521.csv") %>%
  dplyr::filter(OCT_Type == "Retina" & Num_Images == 62)

all_ppScans$Filename <- stringr::str_replace(all_ppScans$Filename,"/Volumes/z/","/Volumes/hoodorchard.psych.columbia.edu/")

ppScans_tidy <- all_ppScans %>% select(1:8)

ppScans_tidy <- ppScans_tidy %>% arrange(First_Name, Eye, DoT) %>%
  group_by(First_Name, Eye) %>%
  mutate(Diff_date = c(0,difftime(tail(DoT, -1),
                                  head(DoT, 1),units = "days"))) %>%
  ungroup()

ppScans_tidy <- ppScans_tidy %>% filter(Diff_date < 100)

ppScans_unique_tests <- ppScans_tidy %>%
  group_by(First_Name, Eye) %>%
  distinct(First_Name, Eye, DoT, .keep_all = TRUE) %>%
  mutate(n_tests = n()) %>%
  ungroup()

unique_ppScans_per_patient <- ppScans_unique_tests %>%
  group_by(First_Name, Eye) %>%
  distinct(First_Name, Eye, DoT, .keep_all = TRUE) %>%
  summarize(n_tests = n()) %>%
  ungroup()

ppScans_tidy <- dplyr::left_join(ppScans_tidy, unique_ppScans_per_patient,
                             by = c("First_Name","Eye"))

ppScans_tidy <- ppScans_tidy %>% filter(n_tests >1)

baseline_tests_ppScans <- ppScans_tidy %>% group_by(First_Name, Eye) %>% slice(1,2)


joined_tests <- left_join(baseline_tests,baseline_tests_ppScans,
                          by = c("ID","Last_Name","First_Name",
                                 "DoB","Gender","Eye","DoT"),
                          suffix = c("_circle","_ppole"))

maps_eyes <- read_csv("MAPS_diagnosis_progGrades.csv") %>% select(ID, Eye) %>%
  mutate(Study_Eye = if_else(Eye =="OD","R", "L"))

joined_tests <- inner_join(joined_tests,maps_eyes,
                          by = c("First_Name"="ID",
                                 "Eye" = "Study_Eye")) %>%
  select(!"Eye.y")


joined_tests <- joined_tests %>% drop_na()

write_csv(joined_tests, "baseline_tests.csv")


onh_tidy <- onh_tidy %>% distinct(First_Name, Eye, .keep_all = TRUE)

non_study_eyes <- anti_join(onh_tidy,maps_eyes,
                            by = c("First_Name"="ID",
                                   "Eye" = "Study_Eye"))
write_csv(non_study_eyes, 
          "non_study_maps.csv")



