library("dplyr")
library("readr")
dir()

points<-read_delim("data/raw/em export/2021-05_Abrolhos_stereo-BRUVS_Points.txt")
snapper<-points %>%
  dplyr::filter(Species %in% "auratus")%>%
  distinct(OpCode)
