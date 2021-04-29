library(octr)
library(zeallot)

an_example <- all_onh[9,]

crnfl_small %<-% octr::extract_HE_crnfl(an_example$Filename[1])
small_circle <- crnfl_small$rnfl

degrees <- seq(from = 0 , to= 360, length.out = length(small_circle))
small_circle <- tibble(degrees = round(degrees, 1) ,
                       thickness = round(small_circle))

check <- small_circle %>% mutate(FtD_adj_degrees = degrees - -5.3) %>%
  mutate(FtD_adj_degrees = case_when(FtD_adj_degrees < 0 ~ 360 + FtD_adj_degrees,
                                     FtD_adj_degrees > 360 ~ FtD_adj_degrees - 360,
                                     FtD_adj_degrees > 0 & FtD_adj_degrees < 360 ~ FtD_adj_degrees)) %>%
  arrange(FtD_adj_degrees)


global = round(mean(check$thickness))  
temporal_inferior <- check %>% filter(FtD_adj_degrees >= 41,
                                      FtD_adj_degrees <= 80) %>% summarise(Mean_TI = round(mean(thickness)))
