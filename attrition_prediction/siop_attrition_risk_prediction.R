#rm(list = ls())
options(java.parameters = Inf)

##Load packages
library(tictoc)
library(readxl)
library(tidyverse)
library(skimr)
library(timetk)
library(googledrive)
library(googlesheets4)



####load data
dat_location<-"C:/Users/bcostell/Dropbox/SIOP MAS/Data/"

load_siop_data<-function(year, dat_location){
  dat<-read_csv(
    paste0(
      dat_location,
      year,
      "/Cleaned/R/",
      "Membership Data ",
      year, " R.csv"
      )
    )%>%janitor::clean_names()%>%
    mutate(
      year_started_in_i_o_field= year_started_in_i_o_field%>%
        str_split(pattern = "\\, ")%>%
        sapply("[", 1)%>%as.numeric(),

      zip_code= zip_code%>%
        str_split(pattern = "\\-| ")%>%
        sapply("[", 1)%>%as.integer(),

      siop_year= year
      )
}

years<-2017:2021

siop_dat<-bind_rows(lapply(years, load_siop_data, dat_location))






####get models for aggregation dynamically
##get most recent model date
date_cleaned<-
  gsub("-","_",
       max(
         as.Date(
           sapply(
             str_split(
               list.dirs(path=paste(getwd(),"model_train_output",sep="/")),
               pattern="/"),
             "[",
             4),
           "%Y_%m_%d"),
         na.rm=T)
  )
##get the correct folder
date_of_interest<-
  paste0(
    date_cleaned,"_",
    min(as.numeric(#change to max/min depending on desired model
      sapply(
        str_split(
          sapply(
            str_split(
              grep(
                date_cleaned,
                list.dirs(path=paste(getwd(),"model_train_output",sep="/")),
                value=TRUE
              ),
              pattern="/"
            ),
            "[",4
          ),
          pattern="_"
        ),
        "[",4
      )
    ))
  )

