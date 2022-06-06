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

years<-2017:2021


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



siop_dat<-bind_rows(lapply(years, load_siop_data, dat_location))


siop_dat%>%select(which(sapply(.,class)=="character"))%>%
  sapply(function(x)head(unique(x)))

siop_dat%>%select(which(sapply(.,class)=="character"))%>%
  sapply(function(x)head(table(x)/sum(table(x))))

###########################
####Feature Engineering####
###########################
####get yearly attrition info####
get_attrition_info<-function(years, dat){

  years_to_calc<-min(years):(max(years)-1)

  calc_yearly_attrition<-function(year_of_intrest){

    dat_begin<-dat%>%
      select(sid, siop_year)%>%filter(siop_year==year_of_intrest)

    dat_end<-dat%>%
      select(sid, siop_year)%>%filter(siop_year==year_of_intrest+1)
    ids_end<-dat_end$sid

    output_dat<-dat_begin%>%left_join(
      dat_begin%>%filter(!sid %in% ids_end)%>%
        mutate(attrition=1),
      by = c("sid", "siop_year")
    )%>%
      mutate(attrition=ifelse(is.na(attrition), 0, attrition))

    return(output_dat)
  }

  final_output<-bind_rows(
    lapply(years_to_calc, calc_yearly_attrition)
  )
  return(final_output)
}

siop_dat1<-siop_dat%>%
  left_join(
    get_attrition_info(years, siop_dat),
    by = c("sid", "siop_year")
  )


####get attrition rates by year
siop_dat1%>%group_by(siop_year)%>%
  summarise(
    unique_members=length(unique(sid)),
    attrition_count=sum(attrition),
    attrition_pct=attrition_count/unique_members
  )


siop_dat1%>%group_by(siop_year, membership_dues)%>%
  summarise(
    unique_members=length(unique(sid)),
    attrition_count=sum(attrition),
    attrition_pct=attrition_count/unique_members
  )

siop_dat1%>%group_by(siop_year, membership_dues, highest_degree)%>%
  summarise(
    unique_members=length(unique(sid)),
    attrition_count=sum(attrition),
    attrition_pct=attrition_count/unique_members
  )%>%View()



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

