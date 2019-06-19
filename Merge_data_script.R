#Must be correct directory!!!!

library(tidyverse)

file_list <- list.files()

rm(dataset)
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset<- read_csv(file)
    s = unlist(strsplit(names(dataset)[2],""))
    names(dataset) = c("Date",paste0(s[1],s[2]))
  }else{
    new_data <- read_csv(file)
    s = unlist(strsplit(names(new_data)[2],""))
    names(new_data) = c("Date",paste0(s[1],s[2]))
    dataset = full_join(dataset,new_data, by = "Date")
  }
  
}



tidy_pop = dataset %>%
  gather("State","population",-Date)

year_only = function(long_date){
  
  return(as.numeric(substring(long_date,1,4)))
}

tidy_pop_year_only = mutate(tidy_pop,year = year_only(Date))%>%
  select(-Date)









