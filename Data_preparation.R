setwd('/Users/vinu/Documents/DalarnaUnivercity/Thesis/Data/')

library(dplyr)
library(tidytext)
library(readr)
library(tidyverse)
library(stopwords)
library(quanteda)
library(stm)
library(lda)
library(slam)
library(stringr)
library(tm)

debates_dataset <- read.csv('final_dataset.csv', encoding="latin1")

#unique(debates_dataset$text_dok_rm)
debates_dataset_2 <- debates_dataset[!duplicated(debates_dataset[,c('text_talare')]),]
write.csv(debates_dataset_2,'talare.csv')
mystopwords <- tibble(lemma1 = c("ung", "fru", "oro", "herr", "bero", "del", "peng", "sak", "tio","m?ste"))

debates_dataset <- anti_join(debates_dataset, mystopwords, by = "lemma1")

debates_dataset$id_and_activity <- paste(debates_dataset$text_dok_id,"_",debates_dataset$text_kammaraktivitet)

#Get Aktuell data
get_docs_aktuell <- subset(debates_dataset,text_kammaraktivitet == "aktuell debatt")

head(get_docs_aktuell)
tf_idf_aktuell <- get_docs_aktuell %>% 
  count(id_and_activity, lemma1,text_talare,text_parti ) %>%
  bind_tf_idf(lemma1, id_and_activity,n)


summary(tf_idf_aktuell$tf_idf)
nrow(tf_idf_aktuell)

tf_idf_aktuell <- tf_idf_aktuell[tf_idf_aktuell$tf_idf >= 0.00032,]

summary(tf_idf_aktuell$tf_idf)
View(tf_idf_aktuell)
nrow(tf_idf_aktuell)

##Arende debate
unique(debates_dataset$text_kammaraktivitet)
get_docs_arende <- subset(debates_dataset,text_kammaraktivitet == "ärendedebatt")

tf_idf_arende <- get_docs_arende %>% 
  count(id_and_activity, lemma1,text_talare,text_parti ) %>%
  bind_tf_idf(lemma1, id_and_activity,n)

summary(tf_idf_arende$tf_idf)
nrow(tf_idf_arende)

tf_idf_arende <- tf_idf_arende[tf_idf_arende$tf_idf >= 0.00011,]

summary(tf_idf_arende$tf_idf)

nrow(tf_idf_arende)

## Budget

get_docs_budget <- subset(debates_dataset,text_kammaraktivitet == "budgetdebatt")

nrow(get_docs_budget)


tf_idf_budget <- get_docs_budget %>% 
  count(id_and_activity, lemma1,text_talare,text_parti ) %>%
  bind_tf_idf(lemma1, id_and_activity,n)

nrow(tf_idf_budget)
summary(tf_idf_budget$tf_idf)

tf_idf_budget <- tf_idf_budget[tf_idf_budget$tf_idf >= 0.00022,]

summary(tf_idf_budget$tf_idf)


nrow(tf_idf_budget)

# Interpellations

get_docs_interpellations <- subset(debates_dataset,text_kammaraktivitet == "interpellationsdebatt")

nrow(get_docs_interpellations)

tf_idf_interpellations <- get_docs_interpellations %>% 
  count(id_and_activity, lemma1,text_talare,text_parti ) %>%
  bind_tf_idf(lemma1, id_and_activity,n)

nrow(tf_idf_interpellations)
summary(tf_idf_interpellations$tf_idf)

tf_idf_interpellations <- tf_idf_interpellations[tf_idf_interpellations$tf_idf >= 0.00018,]

summary(tf_idf_interpellations$tf_idf)

nrow(tf_idf_interpellations)

########Foriegn Policy
get_docs_utripolitisk <- subset(debates_dataset,text_kammaraktivitet == "utrikespolitisk debatt")

nrow(get_docs_utripolitisk)

tf_idf_utripolitisk <- get_docs_utripolitisk %>% 
  count(id_and_activity, lemma1,text_talare,text_parti ) %>%
  bind_tf_idf(lemma1, id_and_activity,n)

nrow(tf_idf_utripolitisk)
summary(tf_idf_utripolitisk$tf_idf)

tf_idf_utripolitisk <- tf_idf_utripolitisk[tf_idf_utripolitisk$tf_idf >= 0.0000337,]
summary(bind_cleaned_dataset$tf_idf)
summary(tf_idf_utripolitisk$tf_idf)

nrow(tf_idf_utripolitisk)

# Combines the debate text
bind_cleaned_dataset <- bind_rows(tf_idf_aktuell,tf_idf_arende,tf_idf_budget,tf_idf_interpellations,tf_idf_utripolitisk)
 
bind_cleaned_dataset <- bind_cleaned_dataset %>% 
  count(id_and_activity, lemma1,text_talare,text_parti ) %>%
  bind_tf_idf(lemma1, id_and_activity,n)


bind_cleaned_dataset <- bind_cleaned_dataset[bind_cleaned_dataset$tf_idf >= 0.0035333,]

summary(bind_cleaned_dataset$tf_idf)

write.csv(bind_cleaned_dataset,'cleaned_stm_dataset.csv')


df2 <- unique(bind_cleaned_dataset$text_talare)
df2


ministor
priministor
member of the parliment
king
speaker


bind_cleaned_dataset$position <- ifelse(bind_cleaned_dataset$text_talare=="Statsrådet Ardalan Shekarabi (S)","Statsråd",NA)

bind_cleaned_dataset[is.na(bind_cleaned_dataset$position)]

bind_cleaned_dataset$position <- bind_cleaned_dataset$text_talare

bind_cleaned_dataset[text_talare=="Statsrådet Ardalan Shekarabi (S)"]$position <-"Statsråd" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Europaparlamentariker" 
bind_cleaned_dataset[text_talare=="Jabar Amin (MP)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Westlund (S)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Jennie Nilsson (S)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Mattias Jonsson (S)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Hillevi Engström (M)"]$position <-"Statsråd" 
bind_cleaned_dataset[text_talare=="Helene Petersson (S)"]$position <-"Politiker" 
bind_cleaned_dataset[text_talare=="Pia Nilsson (S)"]$position <-"Politiker" 
bind_cleaned_dataset[text_talare=="Statsrådet Anna Johansson (S)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 
bind_cleaned_dataset[text_talare=="Tomas Tobé (M)"]$position <-"Riksdagsledamot" 







data <- subset(data, select = -c(text_kammaraktivitet,`%PDF-1.4`))
data$lemma <- gsub("Herr talman ! ","",as.character(data$lemma))
data$lemma <- gsub("Fru talman !","",as.character(data$lemma))
data$lemma <- gsub("!","",as.character(data$lemma))
data$lemma <- gsub("``","",as.character(data$lemma))
data$lemma <- gsub(", , ","",as.character(data$lemma))
data$lemma <- gsub("ung","",as.character(data$lemma))
data$lemma <- gsub("del","",as.character(data$lemma))
data$lemma <- gsub("peng","",as.character(data$lemma))
data$lemma <- gsub("sak","",as.character(data$lemma))
data$lemma <- gsub("oro","",as.character(data$lemma))
data$lemma <- gsub("fru","",as.character(data$lemma))
data$lemma <- gsub("herr","",as.character(data$lemma))
data$lemma <- gsub("bero","",as.character(data$lemma))
data$lemma <- gsub("ska","",as.character(data$lemma))
data$lemma <- gsub("fler","",as.character(data$lemma))
data$lemma <- gsub("också","",as.character(data$lemma))
data$lemma <- gsub("finns","",as.character(data$lemma))
data$lemma <- gsub("kommer","",as.character(data$lemma))
data$lemma <- gsub("vill","",as.character(data$lemma))
data$lemma <- gsub("eu","",as.character(data$lemma))
data$lemma <- gsub("även","",as.character(data$lemma))
data$lemma <- gsub("fler","",as.character(data$lemma))
data$lemma <- gsub("mer","",as.character(data$lemma))
data$lemma <- gsub("får","",as.character(data$lemma))
data$lemma <- gsub("andra","",as.character(data$lemma))
data$lemma <- gsub("få","",as.character(data$lemma))
data$lemma <- gsub("måste","",as.character(data$lemma))
data$lemma <- gsub("gäller","",as.character(data$lemma))
data$lemma <- gsub("därför","",as.character(data$lemma))
data$lemma <- gsub("fråga","",as.character(data$lemma))
data$lemma <- gsub("frågan","",as.character(data$lemma))
data$lemma <- gsub("göra","",as.character(data$lemma))
data$lemma <- gsub("r","",as.character(data$lemma))
data$lemma <- gsub("eu","",as.character(data$lemma))
data$lemma <- gsub("fram","",as.character(data$lemma))
data$lemma <- gsub("förslag","",as.character(data$lemma))
data$lemma <- gsub("åtgärder","",as.character(data$lemma))
data$lemma <- gsub("tycker","",as.character(data$lemma))
data$lemma <- gsub("sven","",as.character(data$lemma))

# ----------------------------------------
# PREPARE AND PRE-PROCESS DATA
# ----------------------------------------



data %>% filter_all(any_vars(!is.na(.)))

# Stemming, stopword removal, etc. using textProcessor() function
# processed <- textProcessor(data$lemma, metadata = data) 

stopwords1 <- c("ban*", "egeingen" ,"se","ta","a","kunna","eu","lände","tyck","människo","se","n","gå"
                ,"ba","gö","to","väldigt","fam","annat","kvinno","många","abet","behöv","viktigt","säge"
                ,"lite","baa","in","sätt","kansk","säga","genom","olika","beslut","handla","säga","debatten","fågo"
                ,"stoa","komma","s","ge","vet","hela","job","stå")

nytcorpus <- corpus(data$lemma)
dfm = dfm(nytcorpus,
          stem = TRUE,
          remove_numbers = TRUE, 
          remove_punct = TRUE,
          remove_symbols = TRUE,
          verbose  = T)%>%  dfm_remove(c(stopwords("swedish")))%>%  dfm_remove(stopwords1)

cdfm <- dfm_trim(dfm, min_docfreq = 20)

data$text_parti <- ifelse(is.na(data$text_parti),str_extract_all(data$text_talare, "(?<=\\().*(?=\\))"),data$text_parti)

text_talare <- data$text_talare
text_parti <- data$text_parti

unique(text_parti)



meta <- data.frame(talare=text_talare, parti=text_parti)

stm <- stm(documents=cdfm, K=5,max.em.its = 10,
           data=meta, seed=123456)

stm <- stm(documents=dfm, K=5,content =text_talare ,  prevalence= ~text_talare,
           data=meta, seed=123456)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)





Completing Iteration 5 (approx. per word bound = -8.267, relative change = 1.651e-03) 
Topic 1: föslag, statsådet, tidiga, nä, helt 
Topic 2: exempel, både, nya, helt, nä 
Topic 3: jobb, politik, abetsmaknaden, föetag, peson 
Topic 4: möjlighet, skolan, peson, föslag, elev 
Topic 5: föslag, ministen, helt, nä, bygga 















