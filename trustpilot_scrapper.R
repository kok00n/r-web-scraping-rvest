#####
# Packages
#####

library(tidyverse)
library(rvest)
library(stringr)
library(rebus)
library(lubridate)

#####
# Main loop
#####

url <- 'http://www.trustpilot.com/review/www.amazon.com'
max_page <- 1
min_page <- 1

itr <- min_page
repeat{
  page_data <- read_html(paste0(url, '?page=', itr)) 
  
  max_page <- page_data %>% rvest::html_nodes(., '.pagination-page') %>% 
    html_text(.) %>% as.numeric(.) %>% max(.)
  
  print(paste('max_page:', max_page))
  print(paste('itr:', itr))
  
  if(max_page == itr){
    break
  } else{
    itr = itr + 1  
  }
}

#####
# Helper functions
#####

get_title <- function(data, class_name = '.review-info__body__title'){
  data %>% html_nodes(class_name) %>% 
    html_text(.) %>% str_trim(.)
}

get_body <- function(data, class_name = '.review-info__body__text'){
  data %>% html_nodes(class_name) %>% 
    html_text(.) %>% str_trim(.)
}

get_stars <- function(data, class_name = c('.star-rating', '.star-rating--medium')){
  data %>% html_nodes(class_name[2]) %>% 
    html_attrs(.) %>% map(., 
                          function(x){
                            str_match(x, pattern = {class_name[1] %>% gsub('.', '', .)} %R% capture(DIGIT)) %>% 
                              .[2]
                            }
                          ) %>% unlist(.)
}

get_author <- function(data){
  
}

