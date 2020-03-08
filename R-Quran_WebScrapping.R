setwd("~/Dropbox/NLP_Alquran")
#https://quanteda.io/reference/dfm.html#arguments

library(curl)
library(rvest)
library(dplyr)
library(stringr)
library(RSQLite)
library(DataExplorer)
library(tm)
library(wordcloud)
library(quanteda)

conn <- dbConnect(RSQLite::SQLite(),  paste0(getwd(),"/q_nlp.db"))
dbListTables(conn)
main_link <- "http://www.surah.my"
main_surah <- html(main_link)

surah <- dbReadTable(conn,"q_surah")
summary(surah)
DataExplorer::introduce(surah)
DataExplorer::plot_density(surah)
DataExplorer::plot_histogram(surah)
DataExplorer::plot_qq(surah)
DataExplorer::plot_scatterplot(surah,by = "Ayat")
DataExplorer::plot_str(surah)

q_content <- dbReadTable(conn,"q_content")

for (i in 1:27)
{
  surah <- main_surah %>% html_nodes(paste0("body > div > article > div:nth-child(4) > ul > li:nth-child(",i,") > a"))  %>% html_attr("href") 
  #body > div > article > div:nth-child(4) > ul > li:nth-child(27) > a
  surah_link <- paste0(main_link,surah)
  print(paste0(main_link,surah))
  #bacaan_div > div > p
  surah_link_content <- html(surah_link)
  
  nama_surah <- surah_link_content %>% html_nodes(xpath = paste0("//*[@id=\"bacaan_div\"]/h2"))  %>% html_text()
  print(nama_surah)
  
  for (x in 1:300)
  {
  content <- surah_link_content %>% html_nodes(xpath = paste0("//*[@id=\"",x,"\"]/p/text()"))  %>% html_text()
  
  result <- tryCatch({
  if (str_length(content) > 0)
  {
  print(paste0("Surah ==> " ,nama_surah," | Ayat ==> ",x, " | Kandungan ==> ",content))
  
  #surah_id <- as.data.frame(i)
  #surah_name <- as.data.frame(nama_surah)
  #surah_content <- as.data.frame(content)
  tb_surah <-  as.data.frame(cbind(juz=(i+87),ayat=x,surah=nama_surah,surah_desc=content))
  RSQLite::dbWriteTable(conn,"q_content",tb_surah,append =T)  
  RSQLite::dbCommit(conn)  
  #print(content)
  }
  }, error = function(e) {
    #print(paste0("Error ---> ",e))
  })
  
  #surah_link_content %>% html_nodes(xpath ="//*[@id=\"2\"]/p/text()")  %>% html_text()
  #surah_link_content %>% html_nodes(xpath ="//*[@id=\"3\"]/p/text()")  %>% html_text()
  #Sys.sleep(2)
  }
}

RSQLite::dbDisconnect(conn)
