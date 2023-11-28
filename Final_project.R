#setwd("C:/Users/12068/OneDrive - UW/Documents/桌面/UW Course/INFO201/Final Project")
library(dplyr)
library(stringr)
library(ggplot2)
library(testthat)

anime_df <- read.csv("anime.csv") #DO NOT CHANGE!
anime2_df <- read.csv("animes_2.csv") #DO NOT CHANGE!

profile_df <- read.csv("profiles.csv")


df <- merge(x=anime2_df, y=anime_df, by.x = "uid", by.y = "anime_id", all.x=TRUE)



df <- unique(df)
df$episodes.x[is.na(df$episodes.x)] <- 0  
df$genre.y <- NULL

# Define a function to extract the year from the 'aired' column
extract_year <- function(aired_str) {
  # Split the string by ' to ' and take the last part if it exists, otherwise take the whole string
  date_str <- ifelse(grepl(" to ", aired_str), sub(".* to ", "", aired_str), aired_str)
  # Extract the year from the date string
  year <- sub(".*\\s", "", date_str)
  # Return the year
  as.numeric(year)
}

# Apply the function to the 'aired' column to create a new 'year' column
df$year <- sapply(df$aired, extract_year)





# create new category based on score
df$new_category <- ifelse(df$score > 6, "High", "Low")


grouped_df <- group_by(df, type)
summarized_df <- summarise(grouped_df, 
                           avg_score = mean(score, na.rm = TRUE),  # mean score
                           max_score = max(score, na.rm = TRUE),   # highest score
                           min_score = min(score, na.rm = TRUE)    # lowest score
)


write.csv(df, "merged_anime_data.csv", row.names = FALSE)
#setwd("C:/Users/12068/OneDrive - UW/Documents/桌面/UW Course/INFO201/Final Project")
library(dplyr)
library(stringr)
library(ggplot2)
library(testthat)

anime_df <- read.csv("anime.csv") #DO NOT CHANGE!
anime2_df <- read.csv("animes_2.csv") #DO NOT CHANGE!

profile_df <- read.csv("profiles.csv")


df <- merge(x=anime2_df, y=anime_df, by.x = "uid", by.y = "anime_id", all.x=TRUE)



df <- unique(df)
df$episodes.x[is.na(df$episodes.x)] <- 0  
df$genre.y <- NULL

# Define a function to extract the year from the 'aired' column
extract_year <- function(aired_str) {
  # Split the string by ' to ' and take the last part if it exists, otherwise take the whole string
  date_str <- ifelse(grepl(" to ", aired_str), sub(".* to ", "", aired_str), aired_str)
  # Extract the year from the date string
  year <- sub(".*\\s", "", date_str)
  # Return the year
  as.numeric(year)
}

# Apply the function to the 'aired' column to create a new 'year' column
df$year <- sapply(df$aired, extract_year)





# create new category based on score
df$new_category <- ifelse(df$score > 6, "High", "Low")


grouped_df <- group_by(df, type)
summarized_df <- summarise(grouped_df, 
                           avg_score = mean(score, na.rm = TRUE),  # mean score
                           max_score = max(score, na.rm = TRUE),   # highest score
                           min_score = min(score, na.rm = TRUE)    # lowest score
)


#write.csv(df, "merged_anime_data.csv", row.names = FALSE)
