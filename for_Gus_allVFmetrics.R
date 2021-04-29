library(tidyverse)
library(lubridate)
library(octr)
library(zeallot)

all_VF_metrics <- read_csv("MAPS_Progression_Group_VF_metrics.csv")

all_VF_metrics <- all_VF_metrics %>% select(Study_ID,Eye,Test_Type, DoT, MD, PSD)

all_VF_metrics$Eye[71] = "OD"
all_VF_metrics$DoT[71] = "Jul 11, 2019"
all_VF_metrics$Test_Type[71] = "10-2"
all_VF_metrics$Eye[73] = "OD"
all_VF_metrics$DoT[73] = "Jul 11, 2019"
all_VF_metrics$Eye[376] = "OS"
all_VF_metrics$DoT[376] = "Feb 11, 2016"
all_VF_metrics$Eye[905] = "OD"
all_VF_metrics$DoT[905] = "Jul 13, 2017"
all_VF_metrics$Eye[1027] = "OD"
all_VF_metrics$DoT[1027] = "Feb 11, 2020"
all_VF_metrics$Test_Type[1027] = "10-2"
all_VF_metrics$Eye[1028] = "OD"
all_VF_metrics$DoT[1028] = "Feb 11, 2020"
all_VF_metrics$Test_Type[1028] = "24-2"
all_VF_metrics$Eye[1401] = "OD"
all_VF_metrics$DoT[1401] = "Feb 11, 2020"
all_VF_metrics$Test_Type[1401] = "10-2"
all_VF_metrics$Eye[1404] = "OD"
all_VF_metrics$DoT[1404] = "Feb 11, 2020"
all_VF_metrics$Test_Type[1404] = "24-2"


all_VF_metrics$DoT <- mdy(all_VF_metrics$DoT)

all_VF_metrics_final <- all_VF_metrics %>% distinct() %>%pivot_wider(names_from = Test_Type, values_from = c(MD,PSD))

all_VF_metrics_final <- all_VF_metrics_final %>% mutate(Eye = if_else(Eye == "OD", "R", "L")) %>% arrange(Study_ID, Eye,DoT) %>%
  group_by(Study_ID,Eye) %>% mutate(NTest = row_number()) 

all_onh <- readr::read_csv("all_xml_exports_w_correct_segment_010521.csv") %>%
  dplyr::filter(OCT_Type == "ONH" & Num_Images == 28)

all_onh$Filename <- stringr::str_replace(all_onh$Filename,"/Volumes/z/","/Volumes/hoodorchard.psych.columbia.edu/")

onh_tidy <- all_onh %>% dplyr::select(1:8)

onh_tidy <- onh_tidy %>% dplyr::arrange(First_Name, Eye, DoT) %>%
  dplyr::group_by(First_Name, Eye) %>%
  dplyr::mutate(Diff_date = c(0,difftime(tail(DoT, -1),
                                         head(DoT, 1),units = "days"))) %>%
  dplyr::ungroup()


onh_tidy <- onh_tidy[-c(773:774,777,1493, 1498:1499, 1506,1794),]


all_scan_sectors <- tibble()

for(i in 1:nrow(onh_tidy)){

  crnfl_small %<-% octr::extract_HE_crnfl(onh_tidy$Filename[i])

  crnfl_sectors %<-% octr::calculate_HE_crnfl_sectors(crnfl_small$rnfl)$all_sectors

  scan_sectors <- bind_cols(onh_tidy[i,],
                            crnfl_sectors)

  all_scan_sectors <- bind_rows(all_scan_sectors,
                                scan_sectors)


}


all_scan_sectors <- all_scan_sectors %>% select(-c(1,2)) %>% arrange(First_Name, Eye, DoT) %>%
  group_by(First_Name, Eye) %>% mutate(NTest = row_number())

joined_VF_cRNFL <- left_join(all_VF_metrics_final,all_scan_sectors,
                             by = c("Study_ID" = "First_Name",
                                    "Eye" = "Eye",
                                    "NTest" = "NTest"),
                             suffix = c("_VF","_cRNFL"))


mac_metrics <- read_csv("all_mac_metrics.csv")

mac_metrics <- mac_metrics %>% arrange(First_Name, Eye, DoT.x) %>%
  group_by(First_Name, Eye) %>% mutate(NTest = row_number())


joined_all <- left_join(joined_VF_cRNFL,mac_metrics,
                        by = c("Study_ID" = "First_Name",
                               "Eye" = "Eye",
                               "NTest" = "NTest"),
                        suffix = c("_VF_cRNFL","_mac"))

write_csv(joined_all, "combined_VF_OCT_metrics_for_Gus.csv")
