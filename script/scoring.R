library(tidyverse)
library(readr)
library(sf)
library(RColorBrewer)

foodaccess_harris <- read_csv("~/pharis_data/geography/localised/foodaccess_harris.csv")
censustracts_19 <- readRDS("~/pharis_data/censustracts_19.RDS")
walkability_harris_tract <- read_csv("~/pharis_data/geography/localised/walkability_harris_tract.csv")

foodaccess_harris <- foodaccess_harris %>% mutate(Name = paste("<at><openparen>", foodaccess_harris$CensusTract, "<closeparen>", sep=""))
walkability_harris_tract <- walkability_harris_tract %>% mutate(Name = paste("<at><openparen>", walkability_harris_tract$TRACTCE, "<closeparen>", sep=""))
harris <- st_read("~/pharis_data/map/harris.kml")
censustracts_19 = censustracts_19 %>% mutate(Name = paste("<at><openparen>", censustracts_19$STATEFP, censustracts_19$COUNTYFP, censustracts_19$TRACTCE, "<closeparen>", sep = ""))
censustracts_19$geometry <- NULL

#Calculate the Income Score
medianIncome <- median(foodaccess_harris$MedianFamilyIncome)
foodaccess_harris <- foodaccess_harris %>% mutate(IncomeScore = 100 * (( 2 / (1 + exp(-(foodaccess_harris$MedianFamilyIncome)/medianIncome)) ) - 1) )

#Calculate the Poverty Score
medianPoverty <- median(foodaccess_harris$PovertyRate)
foodaccess_harris <- foodaccess_harris %>% mutate(PovertyScore = 100 / (1 + (foodaccess_harris$PovertyRate / medianPoverty)) )

#Calculate the Wealth Score, 
foodaccess_harris <- foodaccess_harris %>% mutate(wealthScore = (foodaccess_harris$IncomeScore * 0.5) + (foodaccess_harris$PovertyScore * 0.5))

funmap = merge(harris, censustracts_19, by.x = "Name")
foodaccess_harris <- merge(funmap, foodaccess_harris, by.x = "Name")

foodaccess_harris <- foodaccess_harris %>% mutate(carlessMetres = (foodaccess_harris$TractHUNV / foodaccess_harris$OHU2010) * foodaccess_harris$tract_avg_bus)
medianCarless <- median(foodaccess_harris$carlessMetres, na.rm = TRUE)
foodaccess_harris <- foodaccess_harris %>% mutate(TransportScore = 100 / ( 1 + (foodaccess_harris$carlessMetres / medianCarless) ) )

foodaccess_harris <- foodaccess_harris %>% mutate(PopDensity = 100000 * foodaccess_harris$tract_pop_total / foodaccess_harris$ALAND)
foodaccess_harris <- foodaccess_harris %>% mutate(marketDist = (pmin(foodaccess_harris$tract_avg_super_market, foodaccess_harris$tract_avg_shop_center)))
medianMarketDist <- median(foodaccess_harris$marketDist, na.rm = TRUE)
foodaccess_harris <- foodaccess_harris %>% mutate(DistanceScore = 100 / (1 + (foodaccess_harris$marketDist / (medianMarketDist) )))

walkability_harris_tract$TRACTCE <- NULL
foodaccess_harris <- merge(foodaccess_harris, walkability_harris_tract, by.x = "Name")

foodaccess_harris <- foodaccess_harris %>% mutate(scaledWalkInd = 100 * foodaccess_harris$NatWalkInd_weighted / ( 40/3 ) )
foodaccess_harris <- foodaccess_harris %>% mutate(appliedDist = case_when(foodaccess_harris$marketDist <= 2414.02 ~ 0.6 * foodaccess_harris$DistanceScore + 0.4 * foodaccess_harris$scaledWalkInd, TRUE ~ foodaccess_harris$DistanceScore) )

foodaccess_harris <- foodaccess_harris %>% mutate(FoodAccessScore = (0.5 * foodaccess_harris$appliedDist) + (0.3 * foodaccess_harris$TransportScore) + (0.2 * foodaccess_harris$wealthScore))

breaks <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
pal <- brewer.pal(10, "RdBu")
class(pal)

plot(foodaccess_harris["FoodAccessScore"], main = "Food Access Score", breaks = breaks, nbreaks = 11, pal=pal)

relationWealth <- lm(foodaccess_harris$FoodAccessScore~foodaccess_harris$wealthScore)
relationDistance <- lm(foodaccess_harris$FoodAccessScore~foodaccess_harris$appliedDist)
relationTransport <- lm(foodaccess_harris$FoodAccessScore~foodaccess_harris$TransportScore)

print(summary(relationWealth))
print(summary(relationDistance))
print(summary(relationTransport))

foodaccess_harris <- foodaccess_harris %>% mutate(bareminimum = case_when( ((foodaccess_harris$LILATracts_Vehicle > 0) & (foodaccess_harris$LILATracts_1And10 > 0) & (foodaccess_harris$LILATracts_1And20 > 0) & (foodaccess_harris$LILATracts_halfAnd10 > 0)) ~ "Y", TRUE ~ "N"))
foodaccess_harris <- foodaccess_harris %>% mutate(maximum = case_when( ((foodaccess_harris$LILATracts_Vehicle > 0) | (foodaccess_harris$LILATracts_1And10 > 0) | (foodaccess_harris$LILATracts_1And20 > 0) | (foodaccess_harris$LILATracts_halfAnd10 > 0)) ~ "Y", TRUE ~ "N"))
foodaccess_harris <- foodaccess_harris %>% mutate(fortyBelow = case_when(foodaccess_harris$FoodAccessScore <= 40 ~ "Y", TRUE ~ "N"))
foodaccess_harris <- foodaccess_harris %>% mutate(disagreement = case_when((foodaccess_harris$fortyBelow == "Y" & foodaccess_harris$maximum == "N")~"Y",TRUE~"N"))
foodaccess_harris <- foodaccess_harris %>% mutate(disagreement2 = case_when((foodaccess_harris$fortyBelow == "N" & foodaccess_harris$maximum == "Y")~"Y",TRUE~"N"))
foodaccess_harris <- foodaccess_harris %>% mutate(agreement = case_when((foodaccess_harris$fortyBelow == "Y" & foodaccess_harris$maximum == "Y")~"Y",TRUE~"N"))
foodaccess_harris <- foodaccess_harris %>% mutate(maximum_numeric = case_when(foodaccess_harris$maximum=="Y"~1,TRUE~0))

logistic_model <- glm(foodaccess_harris$maximum_numeric~foodaccess_harris$FoodAccessScore, family=binomial)
print(summary(logistic_model))


