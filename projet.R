NYPD_Complaint_Data_Historic <- read.csv("C:/Users/sasoy/Downloads/NYPD_Complaint_Data_Historic.csv")
##crime<-head(NYPD_Complaint_Data_Historic,1000)
##summary(crime)
##crime


install.packages("dplyr")
library(dplyr)
library(tidyverse)
install.packages("mlbench", dependencies=TRUE)
library(mlbench)
install.packages("tidyverse", dependencies=TRUE)

library(tidyverse)  # data manipulation and visualization
library(gridExtra)  # plot arrangement
library(magrittr)

crime1<- NYPD_Complaint_Data_Historic 

crime1<-crime1 %>% select(c(BORO_NM, OFNS_DESC))   
#View(crime1)
##row_names<-row.names(crime1 %>% select(c(BORO_NM)) )
##row_names



crime1<-crime1 %>% 
  group_by(BORO_NM,OFNS_DESC)%>% summarise(Count = n())  
#View(crime1)

crime1[is.na(crime1)] =0
#View(crime1)

crime1$Count <- as.numeric(crime1$Count)
#View(crime1)

##apply(crime1, 1, var)

##yest<-t(crime1)

##filter

crime1<-subset(crime1, crime1$BORO_NM != "")   ## Remove unknown location
crime1<-subset(crime1, crime1$OFNS_DESC != "")   ## Remove unknown location




View(crime1)

crime1 <-
union(
union(
union(
union(
  crime1 %>%
    filter(BORO_NM =='BRONX') %>%
  select(BORO_NM,OFNS_DESC,Count)%>% 
  mutate( Count=(Count/2648403)*1000 ), ## Population 2648403,

  crime1 %>%
    filter(BORO_NM =='BROOKLYN') %>%
    select(BORO_NM,OFNS_DESC,Count)%>% 
    mutate( Count=(Count/2589970)*1000))  ,

crime1 %>%
  filter(BORO_NM =='MANHATTAN') %>%
  select(BORO_NM,OFNS_DESC,Count)%>% 
  mutate( Count=(Count/1631990)*1000 )),

crime1 %>%
  filter(BORO_NM =='QUEENS') %>%
  select(BORO_NM,OFNS_DESC,Count)%>% 
  mutate( Count=(Count/2287390)*1000)),

crime1 %>%
  filter(BORO_NM =='MANHATTAN') %>%
  select(BORO_NM,OFNS_DESC,Count)%>% 
  mutate( Count=(Count/1631990)*1000))

##Manhattan Population 2021 1,631,990
##QUEENS 2021 2,287,390
#BRONX 2021 1,435,070
## BROOKLYN 2021 2,589,970
## STATEN ISLAND 2021 474,893

View(crime1)



#1/2648403


crime2<-crime1 %>%
  spread(OFNS_DESC,Count)

View(crime2)

str(crime2)
crime2[is.na(crime2)] =0

##dems <- crime2[,crime2$``]
##dems

##crime2<-crime2[is.null(crime2)] <- 0
##crime2<-crime2[is.na(crime2)] <- 0
##crime2
View(crime2)

crime2<-crime2[,-1]
crime2[is.na(crime2)] =0

View(crime2)

apply(crime2, 1, var)


pca_result <- prcomp(crime2, scale = TRUE)
names(crime2)

shapiro.test(crime2)


str(crime2)
View(crime2)

arrests.cov <- cov(crime2)
arrests.cov
arrests.eigen <- eigen(arrests.cov)
arrests.eigen 

USArrests<-crime2
View(USArrests)
head(USArrests, 10)

pca_result <- prcomp(USArrests, scale = TRUE)
names(pca_result)

pca_result$center
pca_result$scale

pca_result$rotation


pca_result$rotation <- -pca_result$rotation
pca_result$rotation

pca_result$x <- - pca_result$x
head(pca_result$x)

biplot(pca_result, scale = 0)




