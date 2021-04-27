library("devtools")
library(roxygen2)
library(ggplot2)
library(tidyverse)
library(wordcloud2)


data <- read.csv("C:\\Users\\Victo\\Downloads\\daylio_export_2021_04_26.csv")


#'Function to clean the original csv data set
#'
#' @param data
#'
#' @return Cleaned data. Full Date is converted to date. Activities column is lowercase.
#'
#' @export
#'
#' @examples
#' clean_data <- clean(data)
clean <- function(data) {

  #Rename first col to full_date
  names(data)[1] <- "full_date"

  #Changes full_date and
  data$full_date <- as.Date(data$full_date, format= "%m/%d/%Y")

  #Lower case activities
  data$activities <- tolower(data$activities)

  return(data)
}

#'Function to create a frequency table of activities
#'
#' @param data
#'
#' @return Frequency table of activities
#' @export
#'
#' @examples
#' activity_cnt <- act_cnt(data)
act_cnt <- function(data){

  new_data <- data %>%
    mutate(activities=strsplit(activities, "\\|")) %>%
    unnest(activities)

  new_data$activities <- trimws(new_data$activities)

  #Lower case activities
  new_data$activities <- tolower(new_data$activities)

  activity_cnt <- as.data.frame(table(new_data$activities))

  names(activity_cnt)[1] <- "activities"
  names(activity_cnt)[2] <- "freq"

  return(activity_cnt)
}

#'Function to create a frequency table of activities by mood
#'
#' @param data
#'
#' @return Frequency table of activities by mood
#' @export
#'
#' @examples
#' activity_cnt_m <- act_cnt_m(data)
act_cnt_m <- function(data){

  new_data <- data %>%
    mutate(activities=strsplit(activities, "\\|")) %>%
    unnest(activities)

  new_data$activities <- trimws(new_data$activities)

  #Lower case activities
  new_data$activities <- tolower(new_data$activities)

  activity_cnt <- as.data.frame(table(new_data$activities, new_data$mood))

  names(activity_cnt)[1] <- "activities"
  names(activity_cnt)[2] <- "mood"
  names(activity_cnt)[3] <- "freq"

  return(activity_cnt)
}

#'Function to create a frequency table of activities by day
#'
#' @param data
#'
#' @return Frequency table of activities by day
#' @export
#'
#' @examples
#' activity_cnt_d <- act_cnt_d(data)
act_cnt_d <- function(data){

  new_data <- data %>%
    mutate(activities=strsplit(activities, "\\|")) %>%
    unnest(activities)

  new_data$activities <- trimws(new_data$activities)

  #Lower case activities
  new_data$activities <- tolower(new_data$activities)

  activity_cnt <- as.data.frame(table(new_data$activities, new_data$weekday))

  names(activity_cnt)[1] <- "activities"
  names(activity_cnt)[2] <- "weekday"
  names(activity_cnt)[3] <- "freq"

  return(activity_cnt)
}

#'Function to create a frequency table of mood by day
#'
#' @param data
#'
#' @return Frequency table of mood by day
#' @export
#'
#' @examples
#' mood_cnt_d <- mod_cnt_d(data)
mod_cnt_d <- function(data){

  new_data <- data %>%
    mutate(activities=strsplit(activities, "\\|")) %>%
    unnest(activities)

  new_data$activities <- trimws(new_data$activities)

  #Lower case activities
  new_data$activities <- tolower(new_data$activities)

  activity_cnt <- as.data.frame(table(new_data$mood, new_data$weekday))

  names(activity_cnt)[1] <- "mood"
  names(activity_cnt)[2] <- "weekday"
  names(activity_cnt)[3] <- "freq"

  return(activity_cnt)
}

#'Function to create a frequency table of mood by month
#'
#' @param data
#'
#' @return Frequency table of mood by month
#' @export
#'
#' @examples
#' mood_cnt_n <- mod_cnt_n(data)
mod_cnt_n <- function(data){

    #Rename first col to full_date
    names(data)[1] <- "full_date"

    #Changes full_date
    data$full_date <- as.Date(data$full_date, format= "%m/%d/%Y")

    #Lower case activities
    data$activities <- tolower(data$activities)

    #Trim white space from activity column
    data$activities <- trimws(data$activities)

    #Mutate data
    new_data <- data %>%
    mutate(activities=strsplit(activities, "\\|")) %>%
    unnest(activities)

    #Form count
    #activity_cnt <- as.data.frame(table(new_data$mood, format(new_data$full_date,"%B")))
    activity_cnt <- as.data.frame(table(new_data$mood, new_data$full_date))

    names(activity_cnt)[1] <- "mood"
    names(activity_cnt)[2] <- "month"
    names(activity_cnt)[3] <- "freq"

    return(activity_cnt)
}


#'Visualize mood by day
#'
#' @param data
#'
#' @return Basic heatmap of days and mood
#' @export
#'
#' @examples
#' day_mood(mood_cnt_d)
day_mood <- function(data){

  data$mood = factor(data$mood, levels = c("bad", "meh", "good", "rad"))

  ggplot(data, aes(x=weekday, y=mood))+
    geom_tile(aes(fill=freq))+
    scale_fill_gradient(
      low = "darkslategray3",
      high = "deeppink3"
    )
}


#' Visualize activities
#'
#' @param data
#' @param int
#'
#' @return Basic barchart of top <int>
#' @export
#'
#' @examples
#' top_activity(activity_cnt, 20)
top_activity <- function(data, int){

  b_data <- subset(data, data$freq >= int)

  bar <- ggplot(b_data, aes(x=activities, y=freq, fill=activities))+
    geom_bar(stat="identity")+
    coord_flip()

  return(bar)
}



