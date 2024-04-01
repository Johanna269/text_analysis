#Install necessary libraries
library(dplyr)
library(tm)
library(sentimentr)
library(ggplot2)
library(PerformanceAnalytics)

#Load required dataframes "reviews" and "movie descriptives" containing the reviews and stats such as "revenue"
load("/Users/johannaschoellhorn/Desktop/Master/Analysing U.D/Assignments/Data_movie_reviews.RData")


#How many characters does a review have and does it vary with each movie 
reviews$nchar <- nchar(reviews$review_text)
summary(reviews$nchar)
hist(reviews$nchar)
boxplot(nchar~movie_id, data=reviews, main = "Characters per movie review", xlab = "Movie ID", ylab = "Number of character")

#Checking for correlation between revenue and max theaters
correlations <- moviedescriptives[,3:4]
chart.Correlation(correlations, histogram = TRUE, pch = 19)
rm(correlations) #remove objects again

#Convert reviews for further analysis
reviews$review_title <- as.character(reviews$review_title)
reviews$review_text <- as.character(reviews$review_text)
str(reviews)

# Assigning a new variable to the reviews dataframe. The avg_length is grouped by movie_id.
moviedescriptives <- reviews %>%
  group_by(movie_id) %>%
  summarise(avg_length = mean(nchar(review_text))) %>%
  right_join(moviedescriptives, by = "movie_id")
head(moviedescriptives)

# Check correlation between length of reviews, revenue and max theater
correlations <- moviedescriptives[, c(2,5:6)]
chart.Correlation(correlations, histogram = TRUE, pch = 19)
rm(correlations)

# Full data processing with removing: Punctuation, Numbers, capitalized letters, stop words, white spaces and punctuation as well as simplification of words 
reviews$review_processed <- reviews$review_text %>%
  removePunctuation() %>%
  removeNumbers() %>%
  tolower() %>%
  removeWords(stopwords("en")) %>%
  stripWhitespace()

reviews$review_precessed <- gsub("[[:punct:]]", "", reviews$review_processed) #remove punctution and replace with empty string

# Calculate sentiment polarity
reviews$polarity <- sentiment(reviews$review_processed)$sentiment

#Plot polarity scores: 
summary(reviews$polarity)
hist(reviews$polarity)
boxplot(polarity~movie_id, data = reviews, main = "Polarity score per movie review", xlab = "Movie IdD", ylab = "Polarity score")

# Calculate movie aggregated polarity
agg_sentiment <- reviews %>%
  group_by(movie_id) %>%
  summarise(mean_sentiment = mean(polarity),
            sd_sentiment = sd(polarity))
head(agg_sentiment)


# Select relevant columns for the linear model
lm_data <- agg_sentiment %>% 
  left_join(moviedescriptives, "movie_id") %>%
  select(movie_id, Gross, mean_sentiment, sd_sentiment)
head(lm_data)

#Build linear model
lm_gross_sentiment <- lm(Gross ~ mean_sentiment, lm_data)
summary(lm_gross_sentiment)

lm_gross_sentiment <- lm(Gross/1000000 ~ mean_sentiment + sd_sentiment, lm_data) 
summary(lm_gross_sentiment)






