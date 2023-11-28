#setwd("C:/Users/12068/OneDrive - UW/Documents/桌面/UW Course/INFO201/Final Project")
library(dplyr)
library(stringr)
library(ggplot2)
library(testthat)

anime_df <- read.csv("anime.csv") #DO NOT CHANGE!
anime2_df <- read.csv("animes_2.csv") #DO NOT CHANGE!

profile_df <- read.csv("profiles.csv")


df <- merge(x=anime2_df, y=anime_df, by.x = "uid", by.y = "anime_id", all.x=TRUE)