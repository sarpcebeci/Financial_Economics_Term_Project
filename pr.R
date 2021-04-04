library(rvest)
library(rlist)
library(reshape2)
library(tidyverse)

country <- c("US","US","US","US","China","Japan","Japan","Japan","Japan","Brazil",
             "Turkey", "Germany", "UK", "India", "Nigeria", "SouthKorea","SouthKorea", 
             "SouthKorea","France", "Indonesia","Indonesia","Israel","Poland","Spain")
  
tbl_list <- list()  
for (i in c(1:24)) {
  tbl_list[[i]] <- read_csv(paste0(i, "p.csv")) %>% 
    mutate(Country = country[i])
}

profile_tbl <- list.rbind(tbl_list)

profile_tbl1 <- profile_tbl %>% 
  rename(.,
         Firm_Name = V1,
         Industry = V2,
         Sector = V3, 
         Emp_Number = V4, 
         Equity_Type = V5) %>% 
  mutate(Industry = substring(Industry, 9) %>% as.factor(),
         Sector = substring(Sector, 7) %>% as.factor(),
         Emp_Number = substring(Emp_Number, 10) %>% as.numeric(),
         Equity_Type = substring(Equity_Type, 12) %>% as.factor(),
         Firm_Name = as.factor(Firm_Name))

write_csv(profile_tbl1, "profiless.csv")

mega_df <- read_csv("mega_df.csv")


mega_df1 <- mega_df %>% 
  select(-X6, -X7) %>% 
  rename(Ratios = X1,
         Company = X2,
         Industry = X3) %>% 
  pivot_longer(cols = c(Company,Industry)) %>% 
  filter( !str_detect(value, "Profitability"),
          !str_detect(value, "Management")) %>% 
  mutate(value = case_when(
    str_detect(value, "%") ~ value %>% 
      str_replace("%", "") %>% 
      as.numeric() / 100,
    str_detect(value, "K") ~ value %>% 
      str_replace("K", "") %>% 
      as.numeric() * 1000,
    str_detect(value, "B") ~ value %>% 
      str_replace("B", "") %>% 
      as.numeric() * 1000000000,
    str_detect(value, "M") ~ value %>% 
      str_replace("M", "") %>% 
      as.numeric() * 1000000,
    TRUE ~ as.numeric(value)
  )) %>% 
  filter(Ratios != "hatali", 
         !is.na(Firm_Name)) %>% 
  left_join(., profile_tbl1) 




write_csv(mega_df1, "analysis_tbl.csv")


mega_df2 <- read_csv("analysis_tbl.csv")

mega_df2 %>% 
  left_join(., countries, by = c("Country" = "country")) %>% 
  write_csv(., "analysis_tbl2.csv")

## get other parts from the previous attempt

setwd("~/Documents/İTÜ/Semesters/6th semester/Financial/Project/datasets")
create_list <- function(char){ 
  
  get_datasets <- function(v){  
    path_file <- paste0(v,char)
    tbl <- read_csv(path_file)
    return(tbl)
  }
  r <- lapply(countries$country[1:14], get_datasets)
  
  names(r) <- countries$country[1:14]
  
  tidy_profile <- function(tbl){
    tbl %>% 
      rename(.,
             Firm_Name = V1,
             Industry = V2,
             Sector = V3, 
             Emp_Number = V4, 
             Equity_Type = V5) %>% 
      mutate(Industry = substring(Industry, 9) %>% as.factor(),
             Sector = substring(Sector, 7) %>% as.factor(),
             Emp_Number = substring(Emp_Number, 10) %>% as.numeric(),
             Equity_Type = substring(Equity_Type, 12) %>% as.factor(),
             Firm_Name = as.factor(Firm_Name)) %>% 
      head(n = -35) %>% 
      return()
  }
  
  r <- lapply(r, tidy_profile)
  
  for (i in c(1:14)) {
    r[[i]] = r[[i]] %>% mutate(Country = countries$country[i])
  }
  return(r)
}

list_pr <- create_list("_pr.csv")

list_pr[[15]] <- read_csv("US_pr.csv") %>% 
  mutate(Country = "US") %>% 
  rename(Firm_Name = firm, 
         Emp_Number = empl,
         Equity_Type = equity,
         Industry = industry,
         Sector = sector)

profiles <- list.rbind(list_pr) 

profiles <- profiles %>% 
  left_join(., countries, by = c("Country" = "country"))

create_list_raw <- function(char){
  
  get_datasets <- function(v){  
    path_file <- paste0(v,char)
    tbl <- read_csv(path_file)
    return(tbl)
  }
  
  r <- lapply(countries$country, get_datasets)
  
  names(r) <- countries$country
  return(r)
}

list_rt <- create_list_raw("_rt.csv")

list_rt[[15]] <- list_rt[[15]] %>% 
  rename(Firm_Name = Firm)

ratios1 <- list.rbind(list_rt)

ratios_merge <- profiles %>% 
  left_join(., ratios1) %>% 
  mutate(value = case_when(
    str_detect(value, "%") ~ value %>% 
      str_replace("%", "") %>% 
      as.numeric() / 100,
    str_detect(value, "K") ~ value %>% 
      str_replace("K", "") %>% 
      as.numeric() * 1000,
    str_detect(value, "B") ~ value %>% 
      str_replace("B", "") %>% 
      as.numeric() * 1000000000,
    str_detect(value, "M") ~ value %>% 
      str_replace("M", "") %>% 
      as.numeric() * 1000000,
    TRUE ~ as.numeric(value)),
    Emp_Number = as.numeric(Emp_Number),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude))
  
  

tbl_firms <- tbl %>% 
  pull(Firm_Name) %>% 
  unique()

rm_firms <- ratios_merge %>% 
  pull(Firm_Name) %>% 
  unique()

diff <- setdiff(rm_firms,tbl_firms)


tbl1 <- tbl %>% 
  bind_rows(., ratios_merge %>% 
  filter(Firm_Name %in% diff))


tbl1 %>% write_csv(., "analysis_tbl3.csv")




































