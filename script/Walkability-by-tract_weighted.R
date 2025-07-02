library(readr)
walkability_harris <- read_csv("~/pharis_data/geography/localised/walkability_harris.csv")

library(tidyverse)

walkability_harris <- walkability_harris %>% mutate(NatWalkInd = NatWalkInd - (D4A_Ranked / 3))

walkability_harris <- walkability_harris %>% mutate(TRACTCE = paste("48201", TRACTCE, sep=""))

walkability_harris_tractPop <- walkability_harris %>%
  select(TRACTCE, TotPop) %>%
  group_by(TRACTCE) %>%
  summarise(tract_pop = sum(TotPop))


walkability_harris <- walkability_harris %>%
  left_join(walkability_harris_tractPop) %>%
  mutate(NatWalkInd_weighted = NatWalkInd*(TotPop/tract_pop))

walkability_harris_tract <- walkability_harris %>%
  select(TRACTCE, NatWalkInd_weighted) %>%
  group_by(TRACTCE) %>%
  summarise(NatWalkInd_weighted = sum(NatWalkInd_weighted))

write.csv(walkability_harris_tract, file = "walkability_harris_tract.csv")