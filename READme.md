# Analysis on the factors that affect COVID-19 death rate

20S project

## Project objectives

Based on the datasets on health_indicators, age, and death_rate of COVID-19, we wanted to conduct an analysis about the relationship between the factors and the death rate of COVID-19. Therefore we set our objective as identifying the factors that affect the death rate among the ones given in the raw data. Then, we decided to use them to build a relatively accurate model to forecast the death rate given the statistics by using decision tree.

## Data sets

1. Country Health Indicators[1]

> This dataset is a collection of data about several indicators related to the health of a country. There are a total of 180 countries, and 70 variables about health. The variables can be divided into the following areas : Country Region(col 1), COVID-19 facts(2~8, 67~70), Death Causes statistics(9~18), Other fatalities(19,20), Food Sources(21~41), Health care systems(43~48), School closures(49, 63~66), BCG data(50, 51), CIA factbook statistics(People/Society facts)(52~62). 
> The data used in this study is Death Causes statistics, Food sources, Health care systems, and factbook statistics.

2. COVID-19 cases: worldwide[2]

> This dataset is a collection of data about the number of COVID-19 cases and deaths worldwide. We used this dataset to calculate the death rate of COVID-19. Because the number of confirmed cases increases everyday, we fixed the date on May 31.

3, Population ages 65 and above(%)[3]

> The dataset is about the percentage of the elderly population in each country, over 65. We used the data of 2018, and tried to analyze the death rate of COVID-19 according to the proportion of elderly in the population.

## Methods

1. Pre-processing

> min-max normalization
> change numerical value into categorical value
> oversampling by SMOTE algorithm

2. Infer the relationship with features that we don't generally know.

> ANOVA test
> scheffe test for post-hoc analysis

3. Make a prediction model

> decision tree
> 5-fold cross validation
> ensemble methods(bagging & randomForest)

## Results & Discussion

Through this study, we concluded that cancer, median age, and over 65 population is related to the COVID-19 death rate. Influence of two food sources, animal fats and vegetable oils are also included in above three variables. By observing these variables, we noticed that all these variables are related to countries' wealth. Especially in Europe, the consumption level of animal fat and vegetable oils are high, DALYs of cancer is higher than other diseases since other diseases are easy to treat. Also the over 65 population and median age of countries in Europe are high. This comes to the conclusion that the factors we’ve selected are factors targeting Europe, which is suffering heavily from COVID-19. We wanted to infer the factors related to COVID-19 death rate, but actually the factors we specified were only the characteristics of countries which are suffering from COVID-19.

While thinking about the cause of this situation, we concluded that the problematic part was to underestimate the relation of COVID-19 confirmed cases and death cases. We first thought that when some people got COVID-19, the variables that determine whether a person survives are related to health data, and have a small relation with the confirmed rate. However, while considering the total percentage of death, it should be related somehow to the confirmed rate. So geographical features, number of cross-country movements and a massive number of variables should be included while calculating the COVID-19 death rate. The data we used had about 40 variables, and it was not sufficient enough.

Moreover, during this study, we noticed that sampling and pre-processing steps are crucial. Depending on the type of data, we should choose appropriate methods for processing, and analysis. Also the lack of sample numbers was the biggest problem in our study, so we learned that preparing a sufficient number of samples is significant during data processing. The conclusion we made was insufficient, but we could test various methods we’ve learned using R such as anova, decision tree, and randomforest, which was a meaningful experience.


## References

[1] https://www.kaggle.com/nxpnsv/country-health-indicators 
[2] https://ourworldindata.org/coronavirus 
[3] https://data.worldbank.org/indicator/SP.POP.65UP.TO.ZS?end=2018&start=2018&view=map
