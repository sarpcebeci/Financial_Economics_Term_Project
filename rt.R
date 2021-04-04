library(rvest)
library(rlist)
library(reshape2)
library(tidyverse)

#links
market_links <- c(
  "https://www.investing.com/indices/nyse-composite-components",
  "https://www.investing.com/indices/nyse-composite-components/2",
  "https://www.investing.com/indices/nyse-composite-components/3",
  "https://www.investing.com/indices/nyse-composite-components/4",
  "https://www.investing.com/indices/szse-component-components",
  "https://www.investing.com/indices/topix-components",
  "https://www.investing.com/indices/topix-components/2",
  "https://www.investing.com/indices/topix-components/3",
  "https://www.investing.com/indices/topix-components/4",
  "https://www.investing.com/indices/tag-along-index-components",
  "https://www.investing.com/indices/ise-all-shares-components",
  "https://www.investing.com/indices/prime-all-share-components",
  "https://www.investing.com/indices/ftse-350-components",
  "https://www.investing.com/indices/s-p-cnx-500-components",
  "https://www.investing.com/indices/nse-all-share-components",
  "https://www.investing.com/indices/kosdaq-components",
  "https://www.investing.com/indices/kosdaq-components/2",
  "https://www.investing.com/indices/kosdaq-components/3",
  "https://www.investing.com/indices/cac-allshares-components",
  "https://www.investing.com/indices/idx-composite-components",
  "https://www.investing.com/indices/idx-composite-components/2",
  "https://www.investing.com/indices/ta-composite-components",
  "https://www.investing.com/indices/wig-components",
  "https://www.investing.com/indices/general-madrid-components"
)


create_ratio_list <- function(firm_link){
  name <-  substring(firm_link, 36) %>% 
    str_replace_all("-", "_")
  
  Sys.sleep(5)
  
  inv_list <- firm_link %>% 
    read_html() %>% 
    html_table(fill = T)
  
  list_bind <- list(inv_list[[3]],inv_list[[4]],inv_list[[5]],inv_list[[6]],
                    inv_list[[7]],inv_list[[8]],inv_list[[9]],inv_list[[10]])
  
  
  return(list_bind)
}


url_generator <- function(market_link){
  firms_v <- market_link %>% 
    read_html() %>%  
    html_nodes("tbody") %>% 
    html_nodes("tr") %>% 
    html_nodes("td") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  firms_v2 <- firms_v[startsWith(firms_v, "/equities")]
  firms_v3 <- head(firms_v2, -35)
  firm_names <- str_remove(firms_v3, "/equities/") %>% 
    str_replace_all("-", "_")
  
  firms_ratio_url <- paste0("https://www.investing.com", firms_v3, "-ratios")
  tbl = tibble(Firms_Name = firm_names,
               Firms_Links = firms_ratio_url,
               From = market_link)
  return(tbl)
}

market_links_df <- market_links %>% 
  as.list() %>% 
  lapply(., url_generator)
 
market_links_tbl <- bind_rows(market_links_df)
write_csv(market_links_tbl, "market_links_tbl.csv")

ratio_mega_list1 <- market_links_tbl$Firms_Links[1:1000] %>% 
  as.list() %>% 
  lapply(., create_ratio_list)

ratio_mega_tbl1 <- bind_rows(ratio_mega_list1)

ratio_mega_list2 <- market_links_tbl$Firms_Links[1001:2001] %>% 
  as.list() %>% 
  lapply(., create_ratio_list)

ratio_mega_list3 <- market_links_tbl$Firms_Links[2002:3002] %>% 
  as.list() %>% 
  lapply(., create_ratio_list)

ratio_mega_list4 <- market_links_tbl$Firms_Links[3003:4003] %>% 
  as.list() %>% 
  lapply(., create_ratio_list)

ratio_mega_list5 <- market_links_tbl$Firms_Links[4004:5005] %>% 
  as.list() %>% 
  lapply(., create_ratio_list)

##
market_links_tbl$Firms_Links[9001:9814] %>% 
  map(. %>% 
        read_html() %>% 
        html_table(fill = T)   
    ) -> try_ee

##

ratio_mega_list6 <- market_links_tbl$Firms_Links[5006:6006] %>% 
  as.list() %>% 
  lapply(., create_ratio_list)

ratio_mega_list6 <- try_ee

ratio_mega_list7 <- market_links_tbl$Firms_Links[6007:7007] %>% 
  as.list() %>% 
  lapply(., create_ratio_list)

ratio_mega_list8 <- market_links_tbl$Firms_Links[8008:9000] %>% 
  as.list() %>% 
  lapply(., create_ratio_list)

ratio_mega_list8 <- try_ee

ratio_mega_list9 <- market_links_tbl$Firms_Links[9001:9814] %>% 
  as.list() %>% 
  lapply(., create_ratio_list)

ratio_mega_list9 <- try_ee

ratio_mega_list10 <- market_links_tbl$Firms_Links[7008:8007] %>% 
  as.list() %>% 
  lapply(., create_ratio_list)

ratio_mega_list10 <- try_ee


mega_list <- list(ratio_mega_list1,ratio_mega_list2,ratio_mega_list3,
                  ratio_mega_list4,ratio_mega_list5,ratio_mega_list6,
                  ratio_mega_list7,ratio_mega_list8,
                  ratio_mega_list9,ratio_mega_list10)

mega_list %>% str

# a firm
mega_list[[1]][[1]]


# tidy a tbl
mega_list[[1]][[1]][4] %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  mutate(X3 = as.character(X3),
         X2 = as.character(X2),
         X1 = as.character(X1))

mega_list1 <- mega_list

# detect the ones get with map
# 10, 6, 8, 9

#mega_list1[[6]][[1]][c(3:10)] 

get_correct_list <- function(list_item){
  list <- list_item
  list[c(3:10)] %>% return()
}

mega_list1[[6]] <- mega_list1[[6]] %>% 
  lapply(., get_correct_list)

mega_list1[[8]] <- mega_list1[[8]] %>% 
  lapply(., get_correct_list)

mega_list1[[9]] <- mega_list1[[9]] %>% 
  lapply(., get_correct_list)

mega_list1[[10]] <- mega_list1[[10]] %>% 
  lapply(., get_correct_list)


mega_list1[[6]][[1]][1] %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  mutate(X3 = as.character(X3),
         X2 = as.character(X2),
         X1 = as.character(X1))

mega_list1[[6]][[1]] %>%
  class()

correct_tbl <- function(elm){
  elm %>% 
    as.data.frame() %>% 
    return()
}

mega_list2 <- mega_list1


for (i in c(1:length(mega_list2))) {
  for (j in c(1:length(mega_list2[[i]]))) {
    mega_list2[[i]][[j]] <- mega_list2[[i]][[j]] %>% 
      lapply(., correct_tbl)
  }
}

mega_list3 <- mega_list2


for (i in c(1:10)) {
for (j in c(1:length(mega_list3[[i]]))) {
for (k in c(1:length(mega_list3[[i]][[j]]))) {
  if (ncol(mega_list3[[i]][[j]][[k]]) == 3){ 
mega_list3[[i]][[j]][[k]] <- mega_list3[[i]][[j]][[k]] %>%
  as_tibble() %>%
  mutate_all(as.character) } else {
    mega_list3[[i]][[j]][[k]] = tibble(X1 = "hatali", X2 = "hatali", X3 = "hatali")
  }
}
}
}

mega_list3.5 <- mega_list3

mega_list3[[1]][[1]] %>% 
  bind_rows() %>% 
  mutate(Firm_Name = market_links_tbl$Firms_Name[1])


for (i in c(1:1000)) {
  mega_list3[[1]][[i]] <- mega_list3[[1]][[i]] %>% 
    bind_rows() %>%
    mutate(Firm_Name = market_links_tbl$Firms_Name[i])
}

k = 1
for (i in c(1001:2001)) {
  mega_list3[[2]][[k]] <- mega_list3[[2]][[i]] %>% 
    bind_rows() %>% 
    mutate(Firm_Name = market_links_tbl$Firms_Name[i])
  k = k + 1
}

k = 1
for (i in c(2002:3002)) {
  mega_list3[[3]][[k]] <- mega_list3[[3]][[k]] %>% 
    bind_rows() %>% 
    mutate(Firm_Name = market_links_tbl$Firms_Name[i])
  k = k + 1
}

k = 1
for (i in c(3003:4003)) {
  mega_list3[[4]][[k]] <- mega_list3[[4]][[k]] %>% 
    bind_rows() %>% 
    mutate(Firm_Name = market_links_tbl$Firms_Name[i])
  k = k + 1
}

k = 1
for (i in c(4004:5005)) {
  mega_list3[[5]][[k]] <- mega_list3[[5]][[k]] %>% 
    bind_rows() %>% 
    mutate(Firm_Name = market_links_tbl$Firms_Name[i])
  k = k + 1
}

k = 1
for (i in c(5006:6006)) {
  mega_list3[[6]][[k]] <- mega_list3[[6]][[k]] %>% 
    bind_rows() %>% 
    mutate(Firm_Name = market_links_tbl$Firms_Name[i])
  k = k + 1
}

k = 1
for (i in c(6007:7007)) {
  mega_list3[[7]][[k]] <- mega_list3[[7]][[k]] %>% 
    bind_rows() %>% 
    mutate(Firm_Name = market_links_tbl$Firms_Name[i])
  k = k + 1
}

k = 1
for (i in c(8008:9000)) {
  mega_list3[[8]][[k]] <- mega_list3[[8]][[k]] %>% 
    bind_rows() %>% 
    mutate(Firm_Name = market_links_tbl$Firms_Name[i])
  k = k + 1
}

k = 1
for (i in c(9001:9814)) {
  mega_list3[[9]][[i]] <- mega_list3[[9]][[k]] %>% 
    bind_rows() %>% 
    mutate(Firm_Name = market_links_tbl$Firms_Name[i])
  k = k + 1
}

k = 1
for (i in c(7008:8007)) {
  mega_list3[[10]][[i]] <- mega_list3[[10]][[k]] %>% 
    bind_rows() %>% 
    mutate(Firm_Name = market_links_tbl$Firms_Name[i])
  k = k + 1
}

mega_list4 <- mega_list3

dff = bind_rows(mega_list4[[1]])

mega_df = mega_list4 %>% 
  lapply(., bind_rows) %>% 
  bind_rows()

write_csv(mega_df, "mega_df.csv")











