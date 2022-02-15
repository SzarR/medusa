####Dynamically clean interest area field####
##Load packages
library(tidyverse)
library(readxl)

##Load data
df<-read_excel("file_path/Demo 7-7-21.xlsx")

##Custom string split function to not remove delimeter
#https://www.r-bloggers.com/2018/04/strsplit-but-keeping-the-delimiter/
strsplit2 <- function(x, split, type = "remove", perl = FALSE, ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}


####Clean data so that it is unique by SID with dynamicaly added interest area columns corresponding to the max number of aoi
##get interest area fields
df_interest<-df%>%select(SID, starts_with("Interest Area"))%>%

  ##pivot long across all interest area fields
  pivot_longer(-SID, values_to = "interest_area")%>%select(-name)%>%

  ##extract all interest areas by using ", capital letter" as delimmter
  mutate(interest_area=strsplit2(interest_area, "\\, [A-Z]", type = "before"))%>%

  ##unnest data
  unnest(cols = c(interest_area))%>%

  ##remove duplicate SID/interest_area combos
  distinct(SID, interest_area, .keep_all = T)%>%

  ##get interest area count
  group_by(SID)%>%
  mutate(
    interest_area_count=paste("interest_area", 1:n(), sep = "_"),
    interest_area=gsub("^\\, ","", interest_area)
  )%>%ungroup()%>%

  ##pivot wider
  pivot_wider(names_from = interest_area_count, values_from = interest_area)


