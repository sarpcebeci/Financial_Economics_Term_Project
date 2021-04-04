library(tidyverse)
library(rvest)
library(reshape2)


cf_all <- function(i){  
  url3 <- countries$links[i]
  html_firm_names <- read_html(url3)
  
  links <- html_firm_names %>%  
    html_nodes("tbody") %>% 
    html_nodes("tr") %>% 
    html_nodes("td") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  links2 <- links[startsWith(links, "/equities")]
  
  firm_names <- str_remove(links2, "/equities/") %>% 
    str_replace_all("-", "_")
  
  urls_is <- paste0("https://www.investing.com", links2, "-cash-flow")
  is_list_brazil <- list()
  
  
  is_lapply <- function(ll){  
    
    firm_name <- str_remove(ll, "/equities/") %>% 
      str_replace_all("-", "_")
    
    firm_link <- paste0("https://www.investing.com", ll, "-cash-flow")
    
    inv_list <- firm_link %>% 
      read_html() %>% 
      html_table(fill = T)
    
    df <- inv_list[[2]] %>% 
      as.data.frame() 
    
    df1 <- df[-c(2,13,24,32,40),1:5]
    
    colnames(df1) <- c("Statement_Elements", "Q4_2020",
                       "Q3_2020","Q2_2020","Q1_2020")
    
    df1$Firm_Name <- firm_name
    
    df2 <- df1 %>% melt(id = c("Statement_Elements", "Firm_Name"))
    
    return(df2)
  }
  
  brazil_list <- links2 %>% 
    as.list() %>% 
    lapply(., is_lapply)
  
  brazil_is_all <- rbind(brazil_list[[1]])
  for (i in c(2:length(brazil_list))) {
    brazil_is_all <- rbind(brazil_is_all, brazil_list[[i]]) 
  }
  
  
  setwd("~/Documents/İTÜ/Semesters/6th semester/Financial/Project")
  path_ch <- paste0(i,"_cf.csv")
  write_csv(brazil_is_all, path = path_ch)
  return()
}


for (i in c(1:15)) {
  cf_all(i) 
  print(i)
}