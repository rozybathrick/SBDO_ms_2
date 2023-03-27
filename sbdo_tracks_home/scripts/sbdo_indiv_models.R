### fitting models to each track indepenently

library(aniMotum)
library(tidyverse)
library(TMB)

## make dataframes for each bird

B_181624 <- filter(sbdo_22, id == "181624_Beluga")
B_181625 <- filter(sbdo_22, id == "181625_Beluga")
B_181626 <- filter(sbdo_22, id == "181626_Beluga")
B_233811 <- filter(sbdo_22, id == "233811_Beluga")
B_233812 <- filter(sbdo_22, id == "233812_Beluga")
B_233813 <- filter(sbdo_22, id == "233813_Beluga")
KS_233815 <- filter(sbdo_22, id == "233815_King_Salmon")
KS_233816 <- filter(sbdo_22, id == "233816_King_Salmon")
KS_233817 <- filter(sbdo_22, id == "233817_King_Salmon")
KS_233818 <- filter(sbdo_22, id == "233818_King_Salmon")
KS_233819 <- filter(sbdo_22, id == "233819_King_Salmon")
KS_233820 <- filter(sbdo_22, id == "233820_King_Salmon")
KS_233821 <- filter(sbdo_22, id == "233821_King_Salmon")
KS_233823 <- filter(sbdo_22, id == "233823_King_Salmon")
