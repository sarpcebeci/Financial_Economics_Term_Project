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
  
  firms_ratio_url <- paste0("https://www.investing.com", firms_v3, "-company-profile")
  tbl = tibble(Firms_Name = firm_names,
               Firms_Links = firms_ratio_url,
               From = market_link)
  return(tbl)
}

