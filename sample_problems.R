rm(list=ls())

library(ggplot)
library(dplyr)
library(gapminder)

df <- gapminder

#How many rows and columns does the data have?
dim(df)
#Does it have any missing values?
table(is.na(df))
#What are the variable names?
names(df)
#What is the format of the country variable? (character, numeric, factor, etc.)
class(df$country)
#How many countries are in the dataset?
length(unique(df$country))
#Show me three efficient ways to find the life expectancy in Vietnam in 1972
df %>% filter(country == "Vietnam", year==1972) %>% dplyr :: select(lifeExp)
df %>% filter(country == "Vietnam", year==1972) %>% summarise(lifeExp)
subset(df, country=="Vietnam")
#What is the mean population in Tanzania for the entire time period in the dataset?
df %>% filter(country=="Tanzania") %>% summarise(mean(pop))
#Which countries have the longest and shortest mean life expectancies?
df %>% group_by(country) %>% summarise(lifeexp = mean(lifeExp)) %>% arrange(lifeexp)
#which continent has the most countries in the dataset? The fewest?
df %>% group_by(continent) %>% summarise(length(unique(country)))
#Challenge: which country had the largest change in GDP per capita between 1952 and 2007? 
out <- split(df, df$country)
storage <- rep(NA, length(out))
for(i in 1:length(out)){
  storage[i] <- out[[i]]$gdpPercap[12] - out[[i]]$gdpPercap[1] 
}
out2 <- data.frame(unique(df$country), storage)
out2 %>% arrange(desc(storage)) %>% head




#EXERCISE LOOPS

#Write a loop that takes 1000 samples of 50 values from a standard normal distribution 
#(rnorm()), calculates the mean, and stores that mean in a vector. Then plot the 
#distribution of those means, calculate the mean and 95% confidence interval.

container <- rep(NA, 1000)
for(i in 1:1000){
  draw <- rnorm(50)
  mean.samp <- mean(draw)
  container[i] <- mean.samp
}
plot(density(container))
mean(container)
quantile(container, c(0.025, 0.975))
