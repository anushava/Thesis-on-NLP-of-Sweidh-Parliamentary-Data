# Clear theenvironment
rm(list=ls())

# Set working directory
setwd("OneDrive\ -\ Högskolan\ Dalarna/Thesis/LDA\ Implementation/")

#Data wrangling
library(tidyverse)

#Text Processing
library(tm)
library(tidytext)
library(textmineR)


#Topic modeling Packages
require(quanteda)
library(topicmodels)

#Visualization
library(reshape2)


##Reading the Data



#Reading data

#aktuell_data_2014 <- read.csv("aktuell debatt_2014.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
aktuell_data1 <- read.csv("aktuell debatt_2014_15.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
aktuell_data2 <- read.csv("aktuell debatt_2015_16.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
aktuell_data3 <- read.csv("aktuell debatt_2016_17.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
aktuell_data4 <- read.csv("aktuell debatt_2017_18.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)


aktuell_data <- bind_rows(aktuell_data1,aktuell_data2,aktuell_data3,aktuell_data4)
#View(aktuell_data)
aktuell_data$lemma1 <- 
    unlist(lapply(strsplit(as.character(aktuell_data$lemma),split="|",fixed=TRUE),`[[`,2)) 
    
aktuell_data$lemma1 <-gsub(".*-|\\:+[0-9]*", "", aktuell_data$lemma1)
  
#check whether data is cleaned or not
# sort1 <- aktuell_data[order(aktuell_data$lemma1),]
# d<- sort1 %>% select(lemma1) %>% head(2000)
# View(d)

# Cleaning lemma
# Text preprocessing

aktuell_data$lemma1 <- removeWords(aktuell_data$lemma1,stopwords('sv'))
aktuell_data <- aktuell_data %>% filter(lemma1!="") 
#aktuell_data %>% select(match,pos,msd,lemma,lemma1) %>% head(10)

aktuell_cleaned <- aktuell_data %>% filter(pos!='PP' & pos!='PN'& pos!='IE' & pos!='KN' & pos!='SN' 
                                & pos!='DT' & pos!='HA'& pos!='HP' & msd!='AB' & 
                                  (nchar(lemma1)>2))
aktuell_cleaned$id_and_activity <- paste(aktuell_cleaned$text_dok_id,"_",aktuell_cleaned$text_kammaraktivitet)

#View(aktuell_cleaned)
#unique(aktuell_cleaned$id_and_activity)

#table(aktuell_cleaned$id_and_activity)


##########################################

aktuell_count <- aktuell_cleaned %>%
  count(id_and_activity,lemma1,sort = TRUE)

aktuell_dfm <- aktuell_count %>% cast_dfm(id_and_activity,lemma1,n)
#inspect(aktuell_dtm)

aktuell_dtm <- quanteda::convert(aktuell_dfm,to="topicmodels")

word_freq <- findFreqTerms(aktuell_dtm,
              lowfreq = 5,
              highfreq = nrow((aktuell_dtm)*0.9))

aktuell_dtm <- aktuell_dtm[,word_freq]
#inspect(aktuell_dtm)





#frequency_aktuell <- aktuell_cleaned %>%  count(lemma1) 
#hist(frequency_aktuell$n)

# include only words that occur at least 50 times
#aktuell_dataset <- aktuell_cleaned %>%
#  group_by(lemma1) %>%
#  mutate(word_total = n()) %>%
#  ungroup() %>%
#  filter(word_total >= quantile(frequency_aktuell$n,0.99))

#View(aktuell_dataset)

#aktuell_dataset %>%  count(lemma1, sort = TRUE)


##?rendedebatt

?rende_data1 <- read.csv("?rendedebatt_2014_15.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
?rende_data2 <- read.csv("?rendedebatt_2015_16.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
?rende_data3 <- read.csv("?rendedebatt_2016_17.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
?rende_data4 <- read.csv("?rendedebatt_2017_18.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)

?rende_data <- bind_rows(?rende_data1,?rende_data2,?rende_data3,?rende_data4)


# Cleaning lemma
?rende_data$lemma1 <- 
  unlist(lapply(strsplit(as.character(?rende_data$lemma),split="|",fixed=TRUE),`[[`,2)) 

?rende_data$lemma1 <- gsub(".*-|\\:+[0-9]*", "", ?rende_data$lemma1)

#check whether data is cleaned or not
# sort2 <- ?rende_data[order(?rende_data$lemma1),]
# d2<- sort2 %>% select(lemma1) %>% head(2000)
# View(d2)

# Text preprocessing
?rende_data$lemma1 <- removeWords(?rende_data$lemma1,stopwords('sv'))
?rende_data <- ?rende_data %>% filter(lemma1!="") 
#?rende_data %>% select(match,pos,msd,lemma,lemma1) %>% head(10)

arende_cleaned <- ?rende_data %>% filter(pos!='PP' & pos!='PN'& pos!='IE' & pos!='KN' & pos!='SN' 
                                           & pos!='DT' & pos!='HA'& pos!='HP' & msd!='AB' & 
                                             (nchar(lemma1)>2))

arende_cleaned$id_and_activity <- paste(arende_cleaned$text_dok_id,"_",arende_cleaned$text_kammaraktivitet)

#View(arende_cleaned)
#unique(arende_cleaned$id_and_activity)

#sort(unique(table(arende_cleaned$id_and_activity)))

arende_count <- arende_cleaned %>%
  count(id_and_activity,lemma1,sort = TRUE)

arende_dfm <- arende_count %>% cast_dfm(id_and_activity,lemma1,n)

arende_dtm <- quanteda::convert(arende_dfm,to="topicmodels")

#inspect(arende_dtm)

word_freq_arende <- findFreqTerms(arende_dtm,
                           lowfreq = 5,
                           highfreq = nrow((arende_dtm)*0.9))

arende_dtm <- arende_dtm[,word_freq_arende]
#inspect(arende_dtm)

#View(arende_cleaned)

#frequency_arende <- arende_cleaned %>%  count(lemma1) 
#hist(frequency_arende$n)
#quantile(frequency_arende$n,0.998)
# include only words that occur at least 50 times
# arende_dataset <- arende_cleaned %>%
#   group_by(lemma1) %>%
#   mutate(word_total = n()) %>%
#   ungroup() %>%
#   filter(word_total >= quantile(frequency_arende$n,0.998))
# 
# View(arende_dataset)

# interpellationsdebatt
# interpellationsdebatt_2014
# interpellationsdebatt_2014_15
# interpellationsdebatt_2015_16
# interpellationsdebatt_2016_17
# interpellationsdebatt_2017_18

#interpellations_data_2014 <- read.csv("interpellationsdebatt_2014.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
interpellations_data1 <- read.csv("interpellationsdebatt_2014_15.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
interpellations_data2 <- read.csv("interpellationsdebatt_2015_16.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
interpellations_data3 <- read.csv("interpellationsdebatt_2016_17.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
interpellations_data4 <- read.csv("interpellationsdebatt_2017_18.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)

interpellations_data <- bind_rows(interpellations_data1,interpellations_data2,interpellations_data3,interpellations_data4)



# Cleaning lemma
interpellations_data$lemma1 <- 
  unlist(lapply(strsplit(as.character(interpellations_data$lemma),split="|",fixed=TRUE),`[[`,2)) 

interpellations_data$lemma1 <- gsub(".*-|\\:+[0-9]*", "", interpellations_data$lemma1)

#check whether data is cleaned or not
# sort3 <- interpellations_data[order(interpellations_data$lemma1),]
# d3<- sort3 %>% select(lemma1) %>% head(2000)
# View(d3)

# Text preprocessing
interpellations_data$lemma1 <- removeWords(interpellations_data$lemma1,stopwords('sv'))
interpellations_data <- interpellations_data %>% filter(lemma1!="") 
#interpellations_data %>% select(match,pos,msd,lemma,lemma1) %>% head(10)

interpellations_cleaned <- interpellations_data %>% filter(pos!='PP' & pos!='PN'& pos!='IE' & pos!='KN' & pos!='SN' 
                                         & pos!='DT' & pos!='HA'& pos!='HP' & msd!='AB' & 
                                           (nchar(lemma1)>2))

interpellations_cleaned$id_and_activity <- paste(interpellations_cleaned$text_dok_id,"_",interpellations_cleaned$text_kammaraktivitet)

#View(interpellations_cleaned)
#unique(interpellations_cleaned$id_and_activity)

#sort(unique(table(interpellations_cleaned$id_and_activity)))

interpellations_count <- interpellations_cleaned %>%
  count(id_and_activity,lemma1,sort = TRUE)

interpellations_dfm <- interpellations_count %>% cast_dfm(id_and_activity,lemma1,n)

interpellations_dtm <- quanteda::convert(interpellations_dfm,to="topicmodels")

#inspect(interpellations_dtm)

word_freq_interpellations <- findFreqTerms(interpellations_dtm,
                                  lowfreq = 5,
                                  highfreq = nrow((interpellations_dtm)*0.9))

interpellations_dtm <- interpellations_dtm[,word_freq_interpellations]
#inspect(interpellations_dtm)

#View(interpellations_cleaned)
#frequency_interpellations <- interpellations_cleaned %>%  count(lemma1)
#quantile(frequency_interpellations$n,0.99)

# include only words that occur at least 50 times
# interpellations_dataset <- interpellations_cleaned %>%
#   group_by(lemma1) %>%
#   mutate(word_total = n()) %>%
#   ungroup() %>%
#   filter(word_total >= quantile(frequency_interpellations$n,0.998))

#View(interpellations_dataset)
#Utrikespolitisk
#Utrikespolitisk_data_2014 <- read.csv("Utrikespolitisk debatt_2014.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
Utrikespolitisk_data1 <- read.csv("Utrikespolitisk debatt_2014_15.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
Utrikespolitisk_data2 <- read.csv("Utrikespolitisk debatt_2015_16.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
Utrikespolitisk_data3 <- read.csv("Utrikespolitisk debatt_2016_17.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
Utrikespolitisk_data4 <- read.csv("Utrikespolitisk debatt_2017_18.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)

Utrikespolitisk_data <- bind_rows(Utrikespolitisk_data1,Utrikespolitisk_data2,Utrikespolitisk_data3,Utrikespolitisk_data4)
#dim(Utrikespolitisk_data)

#sum(is.na(Utrikespolitisk_data$text_kammaraktivitet))
#View(Utrikespolitisk_data)

Utrikespolitisk_data$text_kammaraktivitet <-"utrikespolitisk debatt"

# Cleaning lemma
Utrikespolitisk_data$lemma1 <- 
  unlist(lapply(strsplit(as.character(Utrikespolitisk_data$lemma),split="|",fixed=TRUE),`[[`,2)) 
Utrikespolitisk_data$lemma1 <-gsub(".*-|\\:+[0-9]*","", Utrikespolitisk_data$lemma1)

Utrikespolitisk_data$lemma1 <- removeWords(Utrikespolitisk_data$lemma1,stopwords('sv'))
Utrikespolitisk_data <- Utrikespolitisk_data %>% filter(lemma1!="") 

Utrikespolitisk_cleaned <- Utrikespolitisk_data %>% filter(pos!='PP' & pos!='PN'& pos!='IE' & pos!='KN' & pos!='SN' 
                                         & pos!='DT' & pos!='HA'& pos!='HP' & msd!='AB' & 
                                           (nchar(lemma1)>2))

Utrikespolitisk_cleaned$id_and_activity <- paste(Utrikespolitisk_cleaned$text_dok_id,"_",Utrikespolitisk_cleaned$text_kammaraktivitet)



View(Utrikespolitisk_cleaned)
#unique(Utrikespolitisk_cleaned$id_and_activity)

#sort(unique(table(Utrikespolitisk_cleaned$id_and_activity)))

Utrikespolitisk_count <- Utrikespolitisk_cleaned %>%
  count(id_and_activity,lemma1,sort = TRUE)

Utrikespolitisk_dfm <- Utrikespolitisk_count %>% cast_dfm(id_and_activity,lemma1,n)
Utrikespolitisk_dtm <- quanteda::convert(Utrikespolitisk_dfm,to="topicmodels")

#inspect(Utrikespolitisk_dtm)


word_freq_Utrikespolitisk <- findFreqTerms(Utrikespolitisk_dtm,
                                           lowfreq = 2,
                                           highfreq = nrow((Utrikespolitisk_dtm)*0.9))

Utrikespolitisk_dtm <- Utrikespolitisk_dtm[,word_freq_Utrikespolitisk]
#View(Utrikespolitisk_dtm)
#inspect(Utrikespolitisk_dtm)

#dim(Utrikespolitisk_cleaned)
#Utrikespolitiskfrequency_table <- Utrikespolitisk_cleaned %>%  count(lemma1)
#quantile(Utrikespolitiskfrequency_table$n,0.99)
#hist(Utrikespolitiskfrequency_table$n)

# include only words that occur at least 50 times
# Utrikespolitisk_dataset <- Utrikespolitisk_cleaned %>%
#   group_by(lemma1) %>%
#   mutate(word_total = n()) %>%
#   ungroup() %>%
#   filter(word_total >= quantile(Utrikespolitiskfrequency_table$n,0.99))

#dim(Utrikespolitisk_dataset)
#budgetdebatt

#budgetdebatt_2014
#budgetdebatt_2014_15
#budget_2015_16
#budget_2016_17
#budget_2017_18



#budget_data_2014 <- read.csv("budgetdebatt_2014.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
budget_data1 <- read.csv("budget_2014_15.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
budget_data2 <- read.csv("budget_2015_16.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
budget_data3 <- read.csv("budget_2016_17.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
budget_data4 <- read.csv("budget_2017_18.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)

budget_data <- bind_rows(budget_data1,budget_data2,budget_data3,budget_data4)

#sum(is.na(budget_data$text_kammaraktivitet))
#View(budget_data)

budget_data["text_kammaraktivitet"] <- "budgetdebatt"
# Cleaning lemma
budget_data$lemma1 <- 
  unlist(lapply(strsplit(as.character(budget_data$lemma),split="|",fixed=TRUE),`[[`,2)) 
#View(budget_data)

budget_data$lemma1 <- gsub(".*-|\\:+[0-9]*", "", budget_data$lemma1)

#check whether data is cleaned or not
#sort5 <- budget_data[order(budget_data$lemma1),]
#d5<- sort5 %>% select(lemma1) %>% head(2000)
#View(d5)

# Text preprocessing
#library(tm)

budget_data$lemma1 <- removeWords(budget_data$lemma1,stopwords('sv'))
budget_data <- budget_data %>% filter(lemma1!="") 
#budget_data %>% select(match,pos,msd,lemma,lemma1) %>% head(10)

budget_cleaned <- budget_data %>% filter(pos!='PP' & pos!='PN'& pos!='IE' & pos!='KN' & pos!='SN' 
                                                           & pos!='DT' & pos!='HA'& pos!='HP' & msd!='AB' & 
                                                             (nchar(lemma1)>2))

budget_cleaned$id_and_activity <- paste(budget_cleaned$text_dok_id,"_",budget_cleaned$text_kammaraktivitet)

View(budget_cleaned)
#unique(budget_cleaned$text_dok_id)

#sort(unique(table(budget_cleaned$id_and_activity)))

budget_count <- budget_cleaned %>%
  count(id_and_activity,lemma1,sort = TRUE)

budget_dfm <- budget_count %>% cast_dfm(id_and_activity,lemma1,n)

budget_dtm <- quanteda::convert(budget_dfm,to="topicmodels")

#inspect(budget_dtm)


word_freq_budget <- findFreqTerms(budget_dtm,
                                           lowfreq = 5,
                                           highfreq = nrow((budget_dtm)*0.9))

budget_dtm <- budget_dtm[,word_freq_budget]
#inspect(budget_dtm)

combined_dtm <- c(aktuell_dtm,arende_dtm,interpellations_dtm,Utrikespolitisk_dtm,budget_dtm)
#View(combined_dtm)

library(topicmodels)

# Fit LDA

m = topicmodels::LDA(combined_dtm, method = "Gibbs", k = 5,   control=list(iter = 5000, seed = 18061987,alpha = 0.1))

# Plotting the results
m %>%
  tidy() %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# Plot the distribution of topics
m %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("id", "chamber_activity"), sep = "_") %>%
  mutate(chamber_activity = reorder(chamber_activity, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ chamber_activity) +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")

terms(m,5)


library(LDAvis)

dtm <- combined_dtm
dtm = dtm[slam::row_sums(dtm) > 0, ]

phi = as.matrix(posterior(m)$terms)
theta <- as.matrix(posterior(m)$topics)

vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
                   doc.length = doc.length, term.frequency = term.freq,encoding = 'UTF-8')
serVis(json)
