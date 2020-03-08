setwd("~/Dropbox/NLP_Alquran")
#https://www.citypopulation.de/en/malaysia/admin/
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

in_Range <- function(values, min.value, max.value)
{
  (values >= min.value) & (values <= max.value)
}

conn <- dbConnect(RSQLite::SQLite(),  paste0(getwd(),"/q_nlp.db"))
dbListTables(conn)

# unlink("age_dist.csv")
# unlink("ethnic_group.csv")
# unlink("gender.csv")
# unlink("nationality.csv")
# unlink("age_group.csv")
main_link <- "http://www.surah.my"
main_surah <- html(main_link)

#juz <- read.csv("juz.csv")
#dbWriteTable(conn, "juz", juz)

#surah <- read.csv("surah.csv")
#dbWriteTable(conn, "surah", surah)

#surah <- dbReadTable(conn,"surah")
#surah$Nama_Surah <- trimws(str_replace(surah$Nama_Surah,paste0(surah$Surah,"\\)"),""),which = "both")
#RSQLite::dbWriteTable(conn,"surah",surah,overwrite=T)

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


#=====================================================================================

conn <- dbConnect(RSQLite::SQLite(),  paste0(getwd(),"/q_nlp.db"))
dbListTables(conn)
surah <- dbReadTable(conn,"q_surah")
summary(surah)
DataExplorer::introduce(surah)
DataExplorer::plot_density(surah)
DataExplorer::plot_histogram(surah)
DataExplorer::plot_qq(surah)
DataExplorer::plot_scatterplot(surah,by = "Ayat")
DataExplorer::plot_str(surah)

q_content <- dbReadTable(conn,"q_content")
DataExplorer::introduce(q_content)
DataExplorer::plot_density(q_content)
DataExplorer::plot_histogram(q_content)
DataExplorer::plot_qq(q_content)
DataExplorer::plot_boxplot(q_content,by = "juz")
DataExplorer::plot_boxplot(q_content,by = "ayat")
DataExplorer::plot_intro(q_content)

df_summary_q <- q_content %>% group_by(juzuk=juz) %>% summarise(total_ayat = n())
print(df_summary_q)
DataExplorer::plot_boxplot(df_summary_q , by = "juzuk")


mining <- q_content %>% filter(juz == 7) 
surah <- unique(mining$surah)

q_juz_summary <- dbReadTable(conn,"q_juz_summary")
q_juz_summary <- q_juz_summary %>% select (juz,cluster) %>% filter(juz == unique(mining$juz))
cluster_asign <- q_juz_summary$cluster

df_review <- mining
df_review <- as.data.frame(tolower(df_review$surah_desc))
df_review <- as.data.frame(sapply(df_review,gsub,pattern="[[:digit:]]",replacement=" "))
df_review <- as.data.frame(sapply(df_review,gsub,pattern="[[:punct:]]",replacement=" "))
df_review <- as.data.frame(sapply(df_review,gsub,pattern="@\\w+",replacement=" ")) 
df_review <- as.data.frame(sapply(df_review,gsub,pattern="^\\s+|\\s+$",replacement=" "))
df_review <- as.data.frame(sapply(df_review,gsub,pattern="[ \t]{2,}",replacement=" "))
df_review <- as.data.frame(sapply(df_review,gsub,pattern="[^[:alnum:]]",replacement=" "))
names(df_review) = "review"
match_corpus = Corpus(VectorSource(df_review$review))

stopwords_malay = c("ini","dan","yang","buat","mcm","kat","apa","mmg",
                    "dah","saya","lagi","dgn","ada","bin","aku","pastu","untuk","perlu")
junk_words = c("tetap", "sebagai","lalu","ia","lain", "ke", "lah", "kelak","kemudian","kali", "di", "masing", "sekali" , "di", "pula","sahaja","iaitu", "setelah", "yang", "dan", "nescaya", "sedang", "tiap", "antara", "ialah","bagi", "dalam", "oleh", "pada", "daripada", "kerana", "dapat", "adalah","kami","sesungguhnya","tidak","itu","kepada","dari","maka","dengan","akan","telah","atau","juga","pula","jika","serta","supaya","mereka","orang","kamu")

#1 NGRAM
# tdm = TermDocumentMatrix(
#   match_corpus,
#   control = list(
#     removePunctuation = TRUE
#    # stopwords = c(stopwords_malay,stopwords("english"),junk_words),
#   #  removeNumbers = TRUE, tolower = TRUE
#   )
# )

# mach_corpus = VCorpus(VectorSource(df_review$review))
# BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}
# tdm = TermDocumentMatrix(mach_corpus,
#                          control = list(removePunctuation = TRUE, tokenize = BigramTokenizer,
#                                         stopwords = c("yang", "net","lowyat","https","www",stopwords("english"),
#                                                       junk_words,
#                                                       stopwords_malay),removeNumbers = TRUE, tolower = TRUE))

#library(devtools)
#install_version("statnet.common", version = "3.3.0", repos = "http://cran.r-project.org")
#install_version("ISOcodes", version = "2018.06.29", repos = "http://cran.r-project.org")


toks1 <- tokens(as.character(df_review$review), remove_punct = TRUE , split_hyphens = TRUE)
toks2 <- tokens_remove(toks1, stopwords_malay )
toks3 <- tokens_remove(toks2, junk_words )
#toks4 <- tokens_ngrams(toks3, n = c(1,2), concatenator = "_")
if (cluster_asign == 1) {
  toks4 <- tokens_ngrams(toks3, n = 1 , concatenator = "_")
} else if (cluster_asign == 2) {
  toks4 <- tokens_ngrams(toks3, n = c(1, 2) , concatenator = "_")
} else if (cluster_asign == 3) {
  toks4 <- tokens_ngrams(toks3, n = c(1, 2, 3) , concatenator = "_")
}
featnames(dfm(toks4))

#as.data.frame(as.character(toks3$text3))

mach_corpus = VCorpus(VectorSource(toks4))

tdm = TermDocumentMatrix( mach_corpus, control = list(
     removePunctuation = F, removeNumbers = TRUE, tolower = TRUE
 ))

 s <- seq(0.01,0.99, by = 0.01)
 for(i in s) {
   tdm2 <-  removeSparseTerms(tdm, sparse = i)
   print(nrow(head(as.matrix(tdm2),100)))
   print(i)
   if ( in_Range(nrow(head(as.matrix(tdm2),100)),17,20) )
   {
     print("OK")
     print(i)
     m <- as.matrix(tdm2)
     DataExplorer::plot_correlation(t(m),title = surah)
     break()
   } else if ( nrow(head(as.matrix(tdm2),100)) > 20 )
   {
     print("OK")
     print(i)
     m <- as.matrix(tdm2)
     DataExplorer::plot_correlation(t(m),title = surah)
     break()
   }
}

v <- sort(rowSums(m), decreasing = TRUE)
 #wordcloud
word_freqs = sort(rowSums(m), decreasing=TRUE) 
dm = data.frame(word=names(word_freqs), freq=word_freqs)
dm$freq <- as.numeric(dm$freq)
colors=rev(colorRampPalette(brewer.pal(9,"Dark2"))(32)[seq(8,32,6)])
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, paste0("WordCloud ",surah))
suppressWarnings(suppressMessages(wordcloud(dm$word, dm$freq , random.order=F, colors = colors, min.freq=1, max.words=200 ,  rot.per=0.55, 
                                             scale = c(1,1))))

assoc <- as.data.frame(head(findAssocs(tdm, terms = toString("allah"),corlimit = 0.00001),100000))
assoc$watchword <- rownames(assoc)
rownames(assoc) <- c()
names(assoc)[1] = "correlation"
assoc$watchword <- toupper(assoc$watchword)
max(assoc$correlation)




#all inside al-quran
#==================================================================================

conn <- dbConnect(RSQLite::SQLite(),  paste0(getwd(),"/q_nlp.db"))
dbListTables(conn)
surah <- dbReadTable(conn,"q_surah")
summary(surah)
q_content <- dbReadTable(conn,"q_content")
df_summary_q <- q_content %>% group_by(juzuk=juz) %>% summarise(total_ayat = n())
print(df_summary_q)
DataExplorer::plot_boxplot(df_summary_q , by = "juzuk")

mining <- q_content %>% filter(juz != 10000) 
surah <- unique(mining$surah)

df_review <- mining
df_review <- as.data.frame(tolower(df_review$surah_desc))
df_review <- as.data.frame(sapply(df_review,gsub,pattern="[[:digit:]]",replacement=" "))
df_review <- as.data.frame(sapply(df_review,gsub,pattern="[[:punct:]]",replacement=" "))
df_review <- as.data.frame(sapply(df_review,gsub,pattern="@\\w+",replacement=" ")) 
df_review <- as.data.frame(sapply(df_review,gsub,pattern="^\\s+|\\s+$",replacement=" "))
df_review <- as.data.frame(sapply(df_review,gsub,pattern="[ \t]{2,}",replacement=" "))
df_review <- as.data.frame(sapply(df_review,gsub,pattern="[^[:alnum:]]",replacement=" "))
names(df_review) = "review"
match_corpus = Corpus(VectorSource(df_review$review))

stopwords_malay = c("ini","dan","yang","buat","mcm","kat","apa","mmg",
                    "dah","saya","lagi","dgn","ada","bin","aku","pastu","untuk","perlu")
junk_words = c("apabila","bahawa","wahai", "dia","tetap", "sebagai","lalu","ia","lain", "ke", "lah", "kelak","kemudian","kali", "di", "masing", "sekali" , "di", "pula","sahaja","iaitu", "setelah", "yang", "dan", "nescaya", "sedang", "tiap", "antara", "ialah","bagi", "dalam", "oleh", "pada", "daripada", "kerana", "dapat", "adalah","kami","sesungguhnya","tidak","itu","kepada","dari","maka","dengan","akan","telah","atau","juga","pula","jika","serta","supaya","mereka","orang","kamu")


toks1 <- tokens(as.character(df_review$review), remove_punct = TRUE , split_hyphens = TRUE)
toks2 <- tokens_remove(toks1, stopwords_malay )
toks3 <- tokens_remove(toks2, junk_words )
toks4 <- tokens_ngrams(toks3, n = c(1,2), concatenator = "_")
#toks4 <- tokens_ngrams(toks3, n = 2 , concatenator = "_")
featnames(dfm(toks4))


#df1 <- as.matrix(tdm)

mining$surah

#as.data.frame(as.character(toks3$text3))

mach_corpus = VCorpus(VectorSource(toks4))

tdm = TermDocumentMatrix( mach_corpus, control = list(
  removePunctuation = F, removeNumbers = TRUE, tolower = TRUE
))

tdm$dimnames

s <- seq(0.01,0.99, by = 0.01)
for(i in s) {
  tdm2 <-  removeSparseTerms(tdm, sparse = i)
  print(nrow(head(as.matrix(tdm2),100)))
  print(i)
  if ( in_Range(nrow(head(as.matrix(tdm2),100)),18,20) )
  {
    print("OK")
    print(i)
    m <- as.matrix(tdm2)
    DataExplorer::plot_correlation(t(m),title = surah)
    break()
  } else if ( nrow(head(as.matrix(tdm2),100)) > 20 )
  {
    print("OK")
    print(i)
    m <- as.matrix(tdm2)
    DataExplorer::plot_correlation(t(m),title = surah)
    break()
  }
}

v <- sort(rowSums(m), decreasing = TRUE)
#wordcloud
word_freqs = sort(rowSums(m), decreasing=TRUE) 
dm = data.frame(word=names(word_freqs), freq=word_freqs)
dm$freq <- as.numeric(dm$freq)
colors=rev(colorRampPalette(brewer.pal(9,"Set1"))(32)[seq(8,32,6)])
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, paste0("WordCloud Al-Quran"))
suppressWarnings(suppressMessages(wordcloud(dm$word, dm$freq , random.order=F, colors = colors, min.freq=3, max.words=500 ,  rot.per=0.55)))

dfUnit <- as.data.frame(head(dm,20))
dfUnit$word <- rownames(dfUnit)
rownames(dfUnit) <- c()
dfUnit$word <- toupper(dfUnit$word)
barplot(dm[1:20,]$freq, las = 2, names.arg = toupper(dm[1:20,]$word),
        col = topo.colors(20), main = paste0("Top 20 Most Frequent Words Al_Quran"),
        ylab = "Keyword" , cex.axis=0.6, cex.names=0.6)

top_words <- head(dm,20)

for(i in 1:nrow(top_words))
{
  assoc <- as.data.frame(head(findAssocs(tdm, terms = toString(top_words$word[i]),corlimit = 0.00001),100000))
  assoc$watchword <- rownames(assoc)
  rownames(assoc) <- c()
  names(assoc)[1] = "correlation"
  assoc$watchword <- toupper(assoc$watchword)
  max(assoc$correlation)
  for(x in 1:nrow(assoc))
  {
    assoc$correlation[x] <- assoc$correlation[x] + runif(n=1, min = 0, max = (1-assoc$correlation[x]))/100
  }
  
  assoc <- assoc[order(-assoc$correlation),]
  barplot(assoc[1:20,]$correlation, las = 2, names.arg = assoc[1:20,]$watchword,
          col = terrain.colors(20), main =paste("Rank No ",i,". Correlation of Keyword '",toupper(top_words$word[i]),"'.",sep = ""),
          ylab = "Correlation" ,  cex.axis=0.6, cex.names=0.6)
  
}

assoc <- as.data.frame(head(findAssocs(tdm, terms = toString("bosan"),corlimit = 0.00001),100000))
assoc$watchword <- rownames(assoc)
rownames(assoc) <- c()
names(assoc)[1] = "correlation"
assoc$watchword <- toupper(assoc$watchword)
max(assoc$correlation)



























# myDfm <- dfm(toks3, remove_numbers = TRUE, remove_punct = TRUE)
# myDfm2 <- dfm_remove(myDfm,pattern = c(paste0("^", stopwords_malay, "_"),paste0("_", stopwords_malay, "$")),valuetype = "regex")
# head(featnames(myDfm2))



# VSource2  <- paste(unlist(t(tdm$dimnames$Terms)), collapse=",")
# mach_corpus = VCorpus(VectorSource(VSource2))
# BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
# tdm = TermDocumentMatrix(mach_corpus,
#                          control = list(removePunctuation = TRUE, tokenize = BigramTokenizer,
#                                         stopwords = c("yang", "net","lowyat","https","www",stopwords("english"),
#                                                       junk_words,
#                                                       stopwords_malay),removeNumbers = TRUE, tolower = TRUE))
# 
# 
# #plot(tdm, terms = findFreqTerms(tdm, lowfreq = 2)[1:50], corThreshold = 0.5)
# #kk <-  as.data.frame(cbind(tdm$i,tdm$j,tdm$v,tdm$dimnames$Terms,tdm$dimnames$Docs))
# 
# tdm2 <- removeSparseTerms(tdm, sparse = 0.75)
# m <- as.matrix(tdm2)
# DataExplorer::plot_correlation(t(m))
# #DataExplorer::plot_prcomp(t(m))





dfUnit <- as.data.frame(head(dm,20))
dfUnit$word <- rownames(dfUnit)
rownames(dfUnit) <- c()
dfUnit$word <- toupper(dfUnit$word)
barplot(dm[1:20,]$freq, las = 2, names.arg = toupper(dm[1:20,]$word),
        col = topo.colors(20), main = paste0("Top 20 Most Frequent Words - ",surah ),
        ylab = "Keyword" , cex.axis=0.6, cex.names=0.6)

top_words <- head(dm,20)

for(i in 1:nrow(top_words))
{
  assoc <- as.data.frame(head(findAssocs(tdm, terms = toString(top_words$word[i]),corlimit = 0.00001),100000))
  assoc$watchword <- rownames(assoc)
  rownames(assoc) <- c()
  names(assoc)[1] = "correlation"
  assoc$watchword <- toupper(assoc$watchword)
  max(assoc$correlation)
  for(x in 1:nrow(assoc))
  {
    assoc$correlation[x] <- assoc$correlation[x] + runif(n=1, min = 0, max = (1-assoc$correlation[x]))/100
  }
  
  assoc <- assoc[order(-assoc$correlation),]
  barplot(assoc[1:20,]$correlation, las = 2, names.arg = assoc[1:20,]$watchword,
          col = terrain.colors(20), main =paste("Rank No ",i,". Correlation of Keyword '",toupper(top_words$word[i]),"'.",sep = ""),
          ylab = "Correlation" ,  cex.axis=0.6, cex.names=0.6)

}


#panderOptions('table.alignment.default',
#     function(df) ifelse(sapply(df, is.numeric), 'right', 'left'))
#pander(head(j,45))
#print(head(j,45))
tdm2 <- removeSparseTerms(tdm, sparse = 0.94)
hc <- hclust(d = dist(tdm2, method = "canberra"), method = "complete")
plot(hc)
plot(hc, xlab="xlab", ylab="ylab", main="main", sub="")

# reduced label size
par(cex=0.4, mar=c(5, 8, 4, 0.1))
plot(hc, xlab="Word List", ylab="Height", main="Happiness Survey Cluster Canberra Dendogram", sub="", axes=TRUE)
par(cex=1)
#title(xlab="xlab", ylab="ylab", main="Cluster Dendogram")
#axis(2)

