library(stringr)
library(rvest)
library(lubridate)

remove <- "\\<[^()]+\\>"
months <- c("January", "February", "March", "April", "May",
            "June", "July", "August", "September", "October",
            "November", "December")


all.sotu.dates <- 
  html("http://stateoftheunion.onetwothree.net/texts/index.html") %>%
  html_nodes("#text") %>%
  html_text() %>%
  as.character() %>%
  str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "\t", replacement = "")

month.starts <- str_locate_all(all.sotu.dates, pattern = months[1])[[1]][, 1]
month.ends <- str_locate_all(all.sotu.dates, pattern = months[1])[[1]][, 2]

dates <- 
  str_sub(all.sotu.dates, 
          start = month.starts, end = 
          month.ends + 10)

for(i in 2:12){
  
  month.starts <- 
    str_locate_all(all.sotu.dates, pattern = months[i])[[1]][, 1]
  
  month.ends <- 
    str_locate_all(all.sotu.dates, pattern = months[i])[[1]][, 2]
  
  dates.add <- str_sub(all.sotu.dates, 
                       start = month.starts, end = 
                         month.ends + 10)
  
  dates <- c(dates, dates.add)
  
}
  
dates <-
  dates%>%
  mdy() %>%
  as.character() %>%
  str_replace_all(pattern = "-", replacement = "")

html.address <- paste("http://stateoftheunion.onetwothree.net/texts/", 
                      dates[1], ".html", sep = "")

sotu.address <-
  html(html.address) %>%
  html_nodes("#text") %>%
  html_text() %>%
  as.character() %>%
  str_replace_all(pattern = "\n", replacement = " ") %>%
  str_replace_all(pattern = "\t", replacement = "") %>%
  str_replace(pattern = remove, replacement = "") %>%
  str_replace(pattern = "State of the Union Address", replacement = "") %>%
  str_split(" ")

sotu.address <- 
  sotu.address[[1]][-((length(sotu.address[[1]]) - 5):
                        length(sotu.address[[1]]))]

sotu.address <- sotu.address[which(sotu.address != "")]


sotu.address.all.data <-
  sotu.address



for(i in 2:length(dates)){
  
  html.address <- paste("http://stateoftheunion.onetwothree.net/texts/", 
                        dates[i], ".html", sep = "")
  
  sotu.address <-
    html(html.address) %>%
    html_nodes("#text") %>%
    html_text() %>%
    as.character() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "\t", replacement = "") %>%
    str_replace(pattern = remove, replacement = "") %>%
    str_replace(pattern = "State of the Union Address", replacement = "") %>%
    str_split(" ")
  
  sotu.address <- 
    sotu.address[[1]][-((length(sotu.address[[1]]) - 5):
                          length(sotu.address[[1]]))]
  
  sotu.address <- sotu.address[which(sotu.address != "")]
  
  
  sotu.address.all.data <-
    c(sotu.address.all.data, sotu.address)
  
  
  
}



