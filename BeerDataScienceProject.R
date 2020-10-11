#Assumptions for this Analysis
#1. Each observation is being treated as unique since I will be analysing the reviews which are unique
#To treat the missing values, assigning the mean value to the NAs, there are close to 20000 missing values, and removing them will cause Bias


library(dplyr)

data<-read.csv(file.choose(),header = TRUE)
View(data)
length(data$beer_beerId)
unique(data$beer_style)
#Checking for missing values

str(data)
summary(data)  #Mean of beer_ABV is 7.017
data<-data.frame(data)
sum(is.na(data))
apply(is.na(data),2,which)
sum(is.na(data$beer_ABV))

View(data)
#Only beer_ABV has missing values
#Replace missing values with mean in beer_ABV Columns
for(i in 1:length(data$beer_ABV))
{
  if(is.na(data$beer_ABV[i])==TRUE)
  {
    data$beer_ABV[i]=7.017
  }
}
sum(is.na(data$beer_ABV))
View(data)
#Question-1 Top three breweries which produce the strongest beer

#The Alcohol by volume ratio will help us in determining the strongest beer and the respective brewery that brews it

data_sort<- data[with(data, order(-beer_ABV)), ]
View(data_sort)
head(unique(data_sort$beer_brewerId),3)  #This will give the brewery ID top 3 breweries with the strongest beer
head(unique(data_sort$beer_name),3) #Name of top 3 strongest beer


#Question-2 
#In what year reviews were the highest
#Considering only overall reviews

data_sort1<- data[with(data, order(-review_overall)), ]
data_sort1$Date<-substring(data_sort1$Date,7,10)
head(unique(data_sort1$Date),3)
#Beers enjoyed the highest rating in the years 2010,2011,2009


#Question-3
data1<-data.frame(data$review_appearance,data$review_palette,data$review_overall,data$review_taste,data$review_aroma)
cor(data1)
plot(cor(data1))
heatmap(cor(data1))
cor.test(data$review_overall,data$review_appearance)
cor.test(data$review_overall,data$review_palette)
cor.test(data$review_overall,data$review_taste)
cor.test(data$review_overall,data$review_aroma)
plot(data$review_taste,data$review_appearance)
#The overall review has the highest correlation with aroma review. This implies the higher the reviews are for aroma, the higher it is for overall rating
#Aroma is the most important factor, then taste, followed by pallette, Appearance shares a low correlation with the overall rating

#Question 4
#If I were to recommend three beers to my friends, the primary factor I will take into consideration is the taste of the beer
#To recommend anything to a customer, personal preference along with historical data should be taken into account
#Since I am the best judge of my friends preferences, I will recommend the one with the highest Taste review

data<-data_sort1<- data[with(data, order(-review_taste)), ]
View(data)
head(unique(data$beer_name),3)
#The three Beers I will suggest my friends are Caldera Pale Ale, Pilot Rock Porter and Old Growth Imperial Stout

#Question 5

