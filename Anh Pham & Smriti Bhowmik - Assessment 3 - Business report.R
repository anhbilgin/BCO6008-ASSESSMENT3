title: "Assignment3"
author: "Group 7 - Anh and Bhowmik"
date: "2023-06-09"
output: html_document



library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)


captured_vs_farmed<-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')
consumption<-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')
production<-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')


data<-left_join(captured_vs_farmed, consumption, by=c("Year","Code","Entity"))
data<-left_join(data,production,by=c("Year","Code","Entity"))


data%>%skim()
data<-data%>%clean_names()
data<-data[, !colnames(data) %in% "code"]
data<-data%>%na.omit()


data <-data %>%
  rename(country=entity,
          farmed_fish_production=aquaculture_production_metric_tons,
          capture_fish_production = capture_fisheries_production_metric_tons,
         consumption_per_capita=fish_seafood_food_supply_quantity_kg_capita_yr_fao_2020,
         pelagic=commodity_balances_livestock_and_fish_primary_equivalent_pelagic_fish_2763_production_5510_tonnes,
         crustaceans=commodity_balances_livestock_and_fish_primary_equivalent_crustaceans_2765_production_5510_tonnes,
         cephalopods=commodity_balances_livestock_and_fish_primary_equivalent_cephalopods_2766_production_5510_tonnes,
         demersal=commodity_balances_livestock_and_fish_primary_equivalent_demersal_fish_2762_production_5510_tonnes,
         freshwater=commodity_balances_livestock_and_fish_primary_equivalent_freshwater_fish_2761_production_5510_tonnes,
         molluscs=commodity_balances_livestock_and_fish_primary_equivalent_molluscs_other_2767_production_5510_tonnes,
         marine=commodity_balances_livestock_and_fish_primary_equivalent_marine_fish_other_2764_production_5510_tonnes)


#Top 10 countries having highest consumption per capita

seafood<-data%>%
  filter(!is.na(country))%>%
  group_by(country)%>%
  mutate(average_by_country=mean(consumption_per_capita))%>%
  ungroup()


seafood%>%count(country,average_by_country,sort=TRUE)%>%arrange(-average_by_country)

seafood<-seafood%>% filter(country %in% c("Iceland", "Kiribati", "Japan", "Hong Kong", "Portugal",
                                          "Norway","Malaysia", "South Korea","Solomon Islands", " Spain")
)


seafood %>%
  count(country, average_by_country) %>%
  mutate(country = fct_reorder(country, average_by_country)) %>%
  ggplot(aes(x = average_by_country, y = country, fill = country)) +
  geom_col() +
   labs(fill = NULL, x = "Consumption per capita (Kgs)", y = "Country", 
       title = "Top 10 countries having highest consumption per capita")




#The comparison between Aquaculture & Capture Production within top 10 per capita consuming countries.

##Aquaculture production

seafood%>%group_by(country) %>%
  summarise(farmed_fish_production = mean(farmed_fish_production)) %>%
  mutate(country = fct_reorder(country, -farmed_fish_production)) %>%
  ggplot(aes(x = country, y = farmed_fish_production, fill = country)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Average Aquaculture Production among top 10",
       x = "Country",
       y = "Aquaculture Production") +
  theme_minimal() +
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")


##Capture production

seafood%>%group_by(country) %>%
  summarise(capture_fish_production = mean(capture_fish_production)) %>%
  mutate(country = fct_reorder(country, -capture_fish_production)) %>%
  ungroup()%>%
  ggplot(aes(x = country, y = capture_fish_production, fill = country)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Average capture Production among top 10",
       x = "Country",
       y = "Capture Production") +
  theme_minimal() +
  scale_y_continuous(labels = comma)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")




#Mapping on World map.

library(rworldmap)
library(dplyr)

datamap <- data %>% group_by(country) %>%
  mutate(average_by_country=mean(consumption_per_capita))%>%
  mutate(countriesAndTerritories=gsub("_", " ", country))   
datamap.map <- joinCountryData2Map(datamap, joinCode = "NAME", nameJoinColumn = "countriesAndTerritories")

par(mar=c(0,0,1,0))
mapCountryData(datamap.map, nameColumnToPlot="average_by_country")


#Insight of seafood production in 4 Asian countries.


##Japan
seafood_japan<-seafood%>%filter(country=="Japan")

seafood_japan%>%
  summarize(pelagi = mean(pelagic, na.rm = TRUE),
            crustaceans = mean(crustaceans, na.rm = TRUE),
            cephalopods = mean(cephalopods, na.rm = TRUE),
            demersal = mean(demersal, na.rm = TRUE),
            freshwater = mean(freshwater, na.rm = TRUE),
            molluscs = mean(molluscs, na.rm = TRUE),
            marine = mean(marine, na.rm = TRUE))%>%
            pivot_longer(everything(), names_to = "Variable", values_to = "Mean")%>%
            mutate(Variable = fct_reorder(Variable, Mean, .desc=TRUE))%>%
            ggplot(aes(x = Variable, y = Mean)) +
            geom_bar(stat = "identity", fill = "blue") +
            labs(x = "Seafood Types", y = "Quantity in Tonnes", 
            title = "The average of each Seafood type's production in Japan") +
            scale_y_continuous(labels = comma)


##Hongkong
seafood_hongkong<-seafood%>%filter(country=="Hong Kong")

seafood_hongkong%>%
  summarize(pelagi = mean(pelagic, na.rm = TRUE),
            crustaceans = mean(crustaceans, na.rm = TRUE),
            cephalopods = mean(cephalopods, na.rm = TRUE),
            demersal = mean(demersal, na.rm = TRUE),
            freshwater = mean(freshwater, na.rm = TRUE),
            molluscs = mean(molluscs, na.rm = TRUE),
            marine = mean(marine, na.rm = TRUE))%>%
            pivot_longer(everything(), names_to = "Variable", values_to = "Mean")%>%
            mutate(Variable = fct_reorder(Variable, Mean, .desc=TRUE))%>%
            ggplot(aes(x = Variable, y = Mean)) +
            geom_bar(stat = "identity", fill = "blue") +
            labs(x = "Seafood Types", y = "Quantity in Tonnes", 
            title = "The average of each Seafood type's production in Hongkong") +
            scale_y_continuous(labels = comma)


##Malay

seafood_malay<-seafood%>%filter(country=="Malaysia")

seafood_malay%>%
  summarize(pelagi = mean(pelagic, na.rm = TRUE),
            crustaceans = mean(crustaceans, na.rm = TRUE),
            cephalopods = mean(cephalopods, na.rm = TRUE),
            demersal = mean(demersal, na.rm = TRUE),
            freshwater = mean(freshwater, na.rm = TRUE),
            molluscs = mean(molluscs, na.rm = TRUE),
            marine = mean(marine, na.rm = TRUE))%>%
            pivot_longer(everything(), names_to = "Variable", values_to = "Mean")%>%
            mutate(Variable = fct_reorder(Variable, Mean, .desc=TRUE))%>%
            ggplot(aes(x = Variable, y = Mean)) +
            geom_bar(stat = "identity", fill = "blue") +
            labs(x = "Seafood Types", y = "Quantity in Tonnes", 
            title = "The average of each Seafood type's production in Malaysia") +
            scale_y_continuous(labels = comma)



##Korea

seafood_korea<-seafood%>%filter(country=="South Korea")

seafood_korea%>%
  summarize(pelagi = mean(pelagic, na.rm = TRUE),
            crustaceans = mean(crustaceans, na.rm = TRUE),
            cephalopods = mean(cephalopods, na.rm = TRUE),
            demersal = mean(demersal, na.rm = TRUE),
            freshwater = mean(freshwater, na.rm = TRUE),
            molluscs = mean(molluscs, na.rm = TRUE),
            marine = mean(marine, na.rm = TRUE))%>%
            pivot_longer(everything(), names_to = "Variable", values_to = "Mean")%>%
            mutate(Variable = fct_reorder(Variable, Mean, .desc=TRUE))%>%
            ggplot(aes(x = Variable, y = Mean)) +
            geom_bar(stat = "identity", fill = "blue") +
            labs(x = "Seafood Types", y = "Quantity in Tonnes", 
            title = "The average of each Seafood type's production in South Korea") +
            scale_y_continuous(labels = comma)



#Statistical Analysis: Consumption with Country.


seafood %>%
  ggplot(aes(x=consumption_per_capita, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20)+
  scale_x_log10()+
  labs(fill = NULL, x = "Consumption per capita (Kgs)", y="Count of occurances", 
       title="Consumption per capita (Kgs) accross countries")



#Farmed fish (Aquaculture) and Captured fish production with consumption.

##Farmed fish (Aquaculture) with consumption.

seafood%>%group_by(country)%>%
  ggplot(aes(x=consumption_per_capita,y=farmed_fish_production, color = country)) +
  geom_point()+
  scale_y_continuous(labels = comma)+
  labs(fill = NULL, x = "Consumption per cappita (Kgs)", y="Farmed fish production by country", 
       title="")

##Captured fish production with consumption.

seafood%>%group_by(country)%>%
  ggplot(aes(x=consumption_per_cappita,y=Capture_fish_production, color = country)) +
  geom_point()+
  scale_y_continuous(labels = comma)+
  labs(fill = NULL, x = "Consumption per cappita (Kgs)", y="Average Captured fish production by country", 
       title="")



#Different Types of seafood production quantity with consumption.

##Pelagic
seafood%>%group_by(country)%>%
  ggplot(aes(consumption_per_capita,pelagic,color=country))+
  geom_point()+
  scale_y_continuous(labels = comma)+
  labs(x = "Average consumption per capita in Kg", y="Seafood type: Pelagic ", 
       title="Pelagic production and Average Consumption per Capita (Kgs)")

##Marine
seafood%>%group_by(country)%>%
  ggplot(aes(consumption_per_capita,marine,color=country))+
  geom_point()+
  scale_y_continuous(labels = comma)+
  labs(x = "Average consumption per capita in Kg", y="Seafood type: Marine", 
       title="Marine production and Average Consumption per Capita (Kgs)")

##freshwater
seafood%>%group_by(country)%>%
  ggplot(aes(consumption_per_capita,freshwater,color=country))+
  geom_point()+
  scale_y_continuous(labels = comma)+
  labs(x = "Average consumption per capita in Kg", y="Seafood type: Freshwater", 
       title="Freshwater production and Average Consumption per Capita (Kgs)")



#Set Modelling

set.seed(123)
data_split <- seafood%>%
  mutate(consumption_per_capita = log(consumption_per_capita + 1))%>%
  initial_split(strata = consumption_per_capita)

data_train <- training(data_split)
data_test <- testing(data_split)

#Creating subdatasets for cross validation
set.seed(123)
data_folds <- vfold_cv(seafood, v = 5, strata = consumption_per_capita)
data_folds


data_recipe<-recipe(
  consumption_per_capita~country+farmed_fish_production+capture_fish_production+
    pelagic+crustaceans+ demersal+
    cephalopods+
    freshwater+
    molluscs+
    marine,
  data=seafood)%>%
  step_nzv(all_numeric())%>%
  step_zv(all_predictors())%>%
   step_dummy(all_nominal())%>%
  step_naomit(all_predictors())
  

##Model 1: linear model
model1<-linear_reg()%>%
  set_mode("regression")%>%
  set_engine("lm")

##Model 2: Random Forest
model2<-rand_forest()%>%
  set_engine("ranger")%>%
  set_mode("regression")


wf1<-workflow()%>%
  add_recipe(data_recipe)%>%
  add_model(model1)

wf2<-workflow()%>%
  add_recipe(data_recipe)%>%
  add_model(model2)

fit1<-fit(wf1,data=data_train)
fit2<-fit(wf2,data=data_train)


fit1_test<-augment(fit1,new_data = data_train) 
fit1_test1<-augment(fit1,new_data = data_test) 

fit2_test<-augment(fit2,new_data = data_train) 
fit2_test1<-augment(fit2,new_data = data_test)


#Evaluate the model:
doParallel::registerDoParallel()

##Check the R-squared value and Root Mean Squared Error (RMSE).
set.seed(123)

bag_rs1 <- fit_resamples(wf1, data_folds)
collect_metrics(bag_rs1)
bag_rs2 <- fit_resamples(wf2, data_folds)
collect_metrics(bag_rs2)


##Model visualisation
library(scales)

###Model 1:
fit1_test %>%
  ggplot(aes(exp(consumption_per_capita), exp(.pred),color=country)) +
  geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
  geom_point(alpha = 0.2)+
  labs(x = "Consumption per capita (Kgs)", y = "Predicted consumption per capita (Kgs)", 
       title = "Linear model predicting consumption per capita on the traning dataset")

fit1_test1 %>%
  ggplot(aes(exp(consumption_per_capita), exp(.pred),color=country)) +
  geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
  geom_point(alpha = 0.2)+
  labs(x = "Consumption per capita (Kgs)", y = "Predicted consumption per capita (Kgs)", 
       title = "Linear model predicting consumption per capita on the test dataset")

###Model 2:
fit2_test %>%
  ggplot(aes(exp(consumption_per_capita), exp(.pred),color=country)) +
  geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
  geom_point(alpha = 0.2)+
  labs(x = "Consumption per capita (Kgs)", y = "Predicted consumption per capita (Kgs)", 
       title = "Random forest model predicting consumption per capita on the traning dataset")

fit2_test1 %>%
  ggplot(aes(exp(consumption_per_capita), exp(.pred),color=country)) +
  geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
  geom_point(alpha = 0.2)+
  labs(x = "Consumption per capita (Kgs)", y = "Predicted consumption per capita (Kgs)", 
       title = "Random forest model predicting consumption per capita on the tesing dataset")
