library("dplyr")
library("readr")
dir()

points<-read_delim("R/2024-10_SwC_stereo-BRUVs_Points.txt")
snapper<-points %>%
  dplyr::filter(Species %in% "auratus")%>%
  distinct(OpCode)
