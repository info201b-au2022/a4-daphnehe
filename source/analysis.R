library(tidyverse)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
avg_black_incarcerated_2018 <- function(){
  avg_black <- data %>% 
    filter(year == 2018) %>% 
    select(black_pop_15to64) %>% 
    summarise(avg = mean(black_pop_15to64, na.rm = TRUE)) %>% 
    pull(avg)
  return(prettyNum(round(avg_black, 0),big.mark=",",scientific=FALSE))
  # return(round(avg_black,0))
}

avg_black_incarcerated_2018()

avg_black_incarcerated_2000 <- function(){
  avg_black <- data %>% 
    filter(year == 2000) %>% 
    select(black_pop_15to64) %>% 
    summarise(avg = mean(black_pop_15to64, na.rm = TRUE)) %>% 
    pull(avg)
  return(prettyNum(round(avg_black, 0),big.mark=",",scientific=FALSE))
  # return(round(avg_black))
}

avg_black_incarcerated_2000()


avg_white_incarcerated_2018 <- function(){
  avg_white <- data %>% 
    filter(year == 2018) %>% 
    select(white_pop_15to64) %>% 
    summarise(avg = mean(white_pop_15to64, na.rm = TRUE)) %>% 
    pull(avg)
  return(prettyNum(round(avg_white, 0),big.mark=",",scientific=FALSE))
  # return(round(avg_white))
}

avg_white_incarcerated_2018()

black_max_incarcerated <- function(){
  max_total <- data %>% 
    select(black_pop_15to64, county_name) %>% 
    filter(black_pop_15to64 == max(black_pop_15to64, na.rm = TRUE)) %>% 
    pull(county_name)
  return(max_total)
}

black_max_incarcerated()

change_over_time <- function(){
  change <- data %>% 
    group_by(year) %>% 
    select(year, total_pop) %>% 
    summarise (avg_total_pop = mean(total_pop, na.rm = TRUE)) %>% 
    pull(avg_total_pop)
  return(prettyNum(round(change, 0),big.mark=",",scientific=FALSE))
  # return(round(change,0))
}

change_over_time()

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
    tot_jail_pop <- data %>% 
      group_by(year) %>% 
      summarise (total_pop = mean(total_pop, na.rm = TRUE)) %>% 
      select(year, total_pop)
    return(tot_jail_pop)
}
get_year_jail_pop()
View(get_year_jail_pop())

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  bar_chart <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_pop)) +
    labs(x = "Year", y = "Total prison populaton")
  return(bar_chart)
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_jail_pop_by_states <- function(states) {
  pop_by_states <- data %>% 
    filter(state %in% states) %>% 
    group_by(state, year) %>% 
    mutate(avg = mean(total_pop, na.rm = TRUE)) %>% 
    select(year, state, avg)
  return(pop_by_states)
}

get_jail_pop_by_states(c("CA", "AL", "WA"))

plot_jail_pop_by_states <- function(states) {
  plotted_chart <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = avg, color = state)) +
    labs(x = "Year", y = "Average prison population") 
  return(plotted_chart)
}

plot_jail_pop_by_states(c("WA", "OR", "CA"))
 

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


