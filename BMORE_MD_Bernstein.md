---
title: "Baltimore Arrests Analysis"
author: "Geoff Bernstein"
date: 19 Sept 2018
output:
  pdf_document:
    toc: true
  md_document:
    variant: markdown_github
  word_document: 
    toc: true
  html_document:
    toc: true
    toc_depth: 10
    number_sections: true
---

<!--output options: md_, word_ pdf_ or html_document
            beamer_, ioslides, or slidy_presentation-->



##Overview
The Baltimore City Arrests Database is contains booking and intake data of the city since 1 Jan, 2017, and is freely avaiable on the City's Open Baltimore data repository[^1].  It contains the data of nearly 37,000 arrests described by 17 features, such as age, race, sex of the detainee, as well as the date and decription of incident.  In this analysis, I seek to describe the breakdown of crime in Baltimore by geography, time, and demography.

## Research Questions
1. Does the data follow a seasonal, weekly, daily, or hourly pattern?              Decision Tree/Random Forest 
1. Is there any predicting who will receive a ChargeDescriton == "Unknown Charge"? ggplot2 visualization
1. Does the data follow a seasonal, weekly, daily, or hourly pattern?              ggplot2 visualization
1. Do Federal Holidays affect specific crimes?  -->                                ggplot2 visualization
1. Do NA values follow a trend (can they be predicted?)?  Do specific district/post precincts or hours tend input bad data?
1. Are black males more likely to be charged without an **IncidentOffense** of "unknown offense"?          t-test

### To Do
1. Group by violent/non-violent crime
1. What's the best way to mark extrapolated data in R?  Add a dummy column of 0/1?
1. Best way to group concurrent events.... DTG + Location? 
1. How do I want to deal with Day of Week, Weekday identification?  If I'm just plotting it, can I pull it out w/ ggplot?
    - Lubridate functions **wday()**, **month()**, etc, within a ggplot call
1. Create function: purrr map/reduce to lookup arrests with the same date, same location, which assigns an incremented number to a group. 
1. Create fuction: stringr to clean up **ChargeDescripion**

### What can we tell from the Data, what are the limits of the data, and what assumptions do we need to make explicit in order to continue?
1. It only contains arrest data, so I can't extrapolate guilty/non-guilty findings 
1. Missing data is not deliberate....which I will seek to prove/disprove.
1. This *isn't* an arrest database, it's a database of individuals *charged* with crimes. 

### Thinking through the data
1. I don't like to delete data, so If there's a way to extrapolate missing values, I will.
1. I apply functional programming, so use (and write) functions in R from beginning to end, iteratively chopping away at the solution.


[^1]: https://data.baltimorecity.gov/Public-Safety/BPD-Arrests/3i3v-ibrt 


## Libraries Used
```{r message=FALSE, warning=FALSE, results=FALSE}
library(tidyverse)
library(broom)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(corrplot)
library(graphics)
library(tree)
```
  
## Import and Clean Data
```{r message=FALSE, warning=FALSE}
data <- read_csv("BPD_Arrests.csv") 

data$Sex[data$Sex == "M"] <- "Male"
data$Sex[data$Sex == "F"] <- "Female"
data$Race[data$Race == "A"] <- "Asian"
data$Race[data$Race == "W"] <- "White"
data$Race[data$Race == "B"] <- "Black"
data$Race[data$Race == "I"] <- "Indigenous"
data$Race[data$Race == "U"] <- "Unknown"

data$District[is.null(data$District)] <- NA

data <- data %>%
  unite(col = ArrestDateTime,  ArrestDate, ArrestTime, sep = " ") %>%
  mutate(ArrestDateTime = mdy_hms(ArrestDateTime, tz = "GMT")) %>% 
  mutate_at(vars(Sex, Race, IncidentOffense, ChargeDescription, District, Post, Neighborhood), as.factor) %>%
  rename(LatLong = `Location 1`) %>%
  arrange(Arrest)


data$Race <- factor(data$Race, levels = c("Asian","White","Unknown","Indigenous","Black"))

glimpse(data)
```
  
  
### Write a **TimeOfDay** factor for a rough analysis of time catagories 
```{r}

data$TimeOfDay <- NA
  
data$TimeOfDay <- as.factor(with(data, ifelse(
  hour(data$ArrestDateTime) >= 4  & hour(data$ArrestDateTime) < 12, 'morning (4am to 12pm)', ifelse(
  hour(data$ArrestDateTime) >= 12 & hour(data$ArrestDateTime) < 20, 'evening (12pm to 8pm)', ifelse(
  hour(data$ArrestDateTime) >= 20 & hour(data$ArrestDateTime) <= 24, 'night (8pm to 4am)', ifelse(
  hour(data$ArrestDateTime) >= 0 & hour(data$ArrestDateTime) < 4, 'night (8pm to 4am)', NA))))))

sum(is.na(data$TimeOfDay))    # test to find NA Values

```

  

###Create **AgeGroup** factor for a rough analysis of ages of arrestees  
```{r}

data$AgeGroup <- NA
  
data$AgeGroup <- as.factor(with(data, ifelse(
  data$Age >= 12  & data$Age <= 17, '12 – 17', ifelse(
  data$Age >= 18  & data$Age <= 24, '18 – 24', ifelse(
  data$Age >= 25  & data$Age <= 34, '25 – 34', ifelse(
  data$Age >= 35  & data$Age <= 44, '35 – 44', ifelse(
  data$Age >= 45  & data$Age <= 54, '45 – 54', ifelse(
  data$Age >= 55  & data$Age <= 64, '55 – 64', ifelse(
  data$Age >= 65, '65+', NA)))))))))

sum(is.na(data$AgeGroup))  
``` 
  > 5 NA values will have to be cleaned  
    

### Creating Group Arrests
```{r}

data <- data %>% 
  arrange(ArrestDateTime)

data$GroupArrest <- NA

for (i in 1:nrow(data)) {
      data$GroupArrest[i] <- with(data,ifelse(
      (data$ArrestDateTime[i] == data$ArrestDateTime[i+1]) & + (data$ArrestLocation[i] == data$ArrestLocation[i+1]),  TRUE, ifelse(
      (data$ArrestDateTime[i] == data$ArrestDateTime[i-1]) & + (data$ArrestLocation[i] == data$ArrestLocation[i-1]),  TRUE, FALSE)))
}

data$GroupArrest[is.na(data$GroupArrest)] <- FALSE

```
  
**IncidentOffense** is a mandatory, standardized input during booking, so unlike other variables it has consistent naming and a lack of *NA* values.  Many empty rows will only contain **Age**, **Sex**, and **IncidentOffense**.  Prior to dropping *NA* values, I want to capture that information by counting the charge types.
  


```{r message=FALSE, warning=FALSE}
top.charges <- data %>% 
  group_by(IncidentOffense) %>%
  summarise( IncidentChargeCount = n()) %>% 
  arrange(desc(IncidentChargeCount)) %>% top_n(30) 

```
  
```{r message=FALSE, warning=FALSE}
data %>%
  filter(is.na(ArrestLocation)) %>%
  filter(is.na(IncidentLocation)) %>%
  filter(is.na(District)) %>%
  filter(is.na(Post)) %>%
  filter(is.na(Neighborhood)) %>%
  filter(is.na(Longitude)) %>%
  filter(is.na(Latitude)) %>%
  summarise(n()) / count(data)

data <- data %>%
  drop_na(Arrest, ArrestLocation, Age, AgeGroup, District, Post)

write_excel_csv(data, "cleaned_Bmore.csv", na = "NA", append = FALSE, col_names = TRUE)  # export CSV for Tableau

summarise(data, "Total Observations in Clean Data" = n())
```
  
> Less than half of the orignal data remains after cleaning
  
## Exploratory Visualizations
```{r warning=FALSE}
ggplot(data= data, aes(x = week(ArrestDateTime), fill = District)) + geom_histogram(
  binwidth = 1, color = "grey") + scale_x_continuous(
    breaks = seq(15,75,10)) + ggtitle("Weekly Count of Arrests by District")
```
  
> Not a particularly useful graph, but we do see a drop in arrests around week 30.  I expect this is due to input error rather than an underlying change in the data
  
    
```{r warning=FALSE}
ggplot(data= data, aes(x = week(ArrestDateTime), fill = AgeGroup)) + 
  geom_histogram(binwidth = 1, color = "grey") + 
  scale_x_continuous(breaks = seq(1,52,13)) + 
  facet_wrap(~ Race, ncol = 3) +
  ggtitle("Weekly Count of Arrests by Age and Race")
```
  
> Baltimore is a majority black city, and the data doesn't capture police patrol patterns or any possible profiling of suspects.  This graph only gives a rough magnitude of arrrests by race, and shouldn't be interpreted any further.
  
  
  
```{r warning=FALSE}
ggplot(data= data, aes(x = week(ArrestDateTime), fill = GroupArrest)) + 
  geom_histogram(binwidth = 1, color = "grey") + 
  scale_x_continuous(breaks = seq(1,52,13)) +
  facet_wrap(~ AgeGroup, ncol = 3) +
  ggtitle("Count of Group Arrests by Age Group")
```
  
> I would expect group arrests go up with lower age groups, but the data doesn't bear that out
  
    
## Correlation Plot
```{r}
data.matrix <- data %>% 
  mutate(GroupArrest = as.numeric(GroupArrest)) %>%
  select(-Arrest, -Age, -ArrestLocation, -Neighborhood, -ArrestDateTime, -IncidentLocation, -Charge, -Latitude, -Longitude, -LatLong, -ChargeDescription, -Post) %>%
  drop_na() %>%
  data.matrix()
cor.data <- cor(data.matrix)
corrplot(cor.data, method = "color", order = "hclust", number.cex = .5)


```
    
Using only the numeric elemnts of our dataset, we see correlation implies only weak relationships between numeric variables.  Interpretatin is difficult given than most of these are factors, but we see some correlation between 
- **IncidentOffense** and **TimeOfDay**  
- **IncidentOffense** and **GroupArrest**  
- **Race** and **District**  
- **Race** and **Sex**  
- **Race** and **Age**  
- **Age** and **GroupArrest**  
  
   
## Mosaic Plot of Chi-Square Independence Tests
The *mosaicplot()* function runs chi-Square tests of independence along factor variables. Red tiles indicate significant negative residuals, where the frequency is less than expected. Blue tiles indicate significant positive residuals, indicating overrepresentation in the sample. The intensity of the color represents the magnitude of the residual.
    
```{r message=FALSE, warning=FALSE, paged.print=TRUE}
data.mosaic <- data %>% 
  mutate(GroupArrest = as.numeric(GroupArrest)) %>%
  select(-Arrest, -AgeGroup, -ArrestLocation, -Neighborhood, -ArrestDateTime, -IncidentLocation, -Charge, -Latitude, -Longitude, -LatLong, -ChargeDescription, -Post) %>%
  drop_na() %>%
  data.matrix()

```

```{r message=FALSE, warning=FALSE}
tableRaceArrestGroup <- table(data$TimeOfDay, data$AgeGroup)
  mosaicplot(tableRaceArrestGroup, shade = TRUE, las=4,
           main = "Group Arrests by Race")
```  
    
> Overrepresentation of arrests among young adults in the morning, implying a positive relationship.  In the afternoon, 35-54 year olds are more likely to be arrested.  
  
```{r message=FALSE, warning=FALSE}
tableTimeOfDayDistrict <- table(data$TimeOfDay, data$District)
  mosaicplot(tableTimeOfDayDistrict, shade = TRUE, las=4,
           main = "Arrests by District by Time of Day")
```
    
> Here we see overrepresentation of events in the Western district in the morning and the Eastern and Southern Districts during the day.
  
```{r message=FALSE, warning=FALSE}
tableRaceSex <- table(data$Race, data$Sex)
  mosaicplot(tableRaceSex, shade = TRUE, las=4,
           main = "Arrests by Race and Sex")
```
> Overrepresentation of white female and black male arrests
  

## Veiwing the Data Geographically
```{r message=FALSE, warning=FALSE}
#baltimore <- get_map(location = c(-76.7673, 39.2221, -76.48, 39.3770), source = “google”)         

#ggmap(baltimore) + geom_point(alpha = .15, data = data, aes(x = Longitude, y = Latitude, color = Race)) +
#labs(x = "Longitude", y = "Latitude", title = "Map of Arrests by Race") 
```
    
    

## Further Analysis
1. Correct for demography: African-Americans are overreperesented in the data, but without deeper analysis correcting
but arguemnts against historical disenfranchisement.
1. Time series analysis would provide deeper understanding of trends rather than graphing.
1. For predictive analysis, Decision Trees would make good use of our factor variables.









