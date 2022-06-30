rm(list=ls())
# Set working directory
setwd("D:/Thesis/Data/latest CSV files")

# install required packages
#install.packages("tm")
#nstall.packages("topicmodels")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("wordcloud")
#install.packages("pals")
#install.packages("quanteda.textstats")


# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# load packages
library(knitr) 
library(kableExtra) 
library(DT)
library(tm)
library(topicmodels)
library(quanteda)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(flextable)
library(quanteda.textstats)


debates_dataset <- read.csv("final_dataset.csv",header=TRUE,stringsAsFactors = FALSE)
View(debates_dataset)
nrow(debates_dataset)

mystopwords <- tibble(lemma1 = c("ung", "fru", "oro", "herr", "bero", "del", "peng", "sak", "tio","m?ste"))

debates_dataset <- anti_join(debates_dataset, mystopwords, by = "lemma1")
nrow(debates_dataset)

# Combined and chamber activity columns
debates_dataset$id_and_activity <- paste(debates_dataset$text_dok_id,"_",debates_dataset$text_kammaraktivitet)



#Get Aktuell data
get_docs_aktuell <- subset(debates_dataset,text_kammaraktivitet == "aktuell debatt")

tf_idf_aktuell <- get_docs_aktuell %>% 
  count(id_and_activity, lemma1,Role,text_parti, sort = TRUE) %>%
  bind_tf_idf(lemma1, id_and_activity,n)

summary(tf_idf_aktuell$tf_idf)
nrow(tf_idf_aktuell)

tf_idf_aktuell <- tf_idf_aktuell[tf_idf_aktuell$tf_idf >= 0.00032,]

summary(tf_idf_aktuell$tf_idf)
View(tf_idf_aktuell)
nrow(tf_idf_aktuell)

##Arende debate

get_docs_arende <- subset(debates_dataset,text_kammaraktivitet == "ärendedebatt")

tf_idf_arende <- get_docs_arende %>% 
  count(id_and_activity, lemma1,Role,text_parti, sort = TRUE) %>%
  bind_tf_idf(lemma1, id_and_activity,n)

summary(tf_idf_arende$tf_idf)
nrow(tf_idf_arende)

tf_idf_arende <- tf_idf_arende[tf_idf_arende$tf_idf >= 0.00011,]

summary(tf_idf_arende$tf_idf)

nrow(tf_idf_arende)

## Budget

unique(debates_dataset$text_kammaraktivitet)
get_docs_budget <- subset(debates_dataset,text_kammaraktivitet == "Budgetdebatt")

nrow(get_docs_budget)


tf_idf_budget <- get_docs_budget %>% 
  count(id_and_activity, lemma1,Role,text_parti, sort = TRUE) %>%
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
  count(id_and_activity, lemma1,Role,text_parti, sort = TRUE) %>%
  bind_tf_idf(lemma1, id_and_activity,n)

unique(debates_dataset$id_and_activity)

nrow(tf_idf_interpellations)
summary(tf_idf_interpellations$tf_idf)

tf_idf_interpellations <- tf_idf_interpellations[tf_idf_interpellations$tf_idf >= 0.00018,]

summary(tf_idf_interpellations$tf_idf)

nrow(tf_idf_interpellations)

########Foriegn Policy
get_docs_utripolitisk <- subset(debates_dataset,text_kammaraktivitet == "utrikespolitisk debatt")

nrow(get_docs_utripolitisk)

tf_idf_utripolitisk <- get_docs_utripolitisk %>%   
  count(id_and_activity, lemma1,Role,text_parti, sort = TRUE) %>%
  bind_tf_idf(lemma1, id_and_activity,n)

nrow(tf_idf_utripolitisk)
summary(tf_idf_utripolitisk$tf_idf)

tf_idf_utripolitisk <- tf_idf_utripolitisk[tf_idf_utripolitisk$tf_idf >= 0.0000337,]

summary(tf_idf_utripolitisk$tf_idf)

nrow(tf_idf_utripolitisk)

# Combines the debate text
bind_cleaned_dataset <- bind_rows(tf_idf_aktuell,tf_idf_arende,tf_idf_budget,tf_idf_interpellations,tf_idf_utripolitisk)
View(bind_cleaned_dataset)


# Frequency table
word_counts <- debates_dataset %>%
  count(lemma1,id_and_activity,sort = TRUE) 

corpus_data <- word_counts %>% 
    bind_tf_idf(lemma1, id_and_activity,n)

summary(corpus_data$tf_idf)

corpus_data <- corpus_data[corpus_data$tf_idf >= 0.00119,]
View(corpus_data)

nrow(corpus_data)

corpus_data %>% count(lemma1)
#####Create document frequency 

filter_corpus <- corpus_data %>% subset(n>=5)
View(filter_corpus)

dfm <- filter_corpus %>%
  cast_dfm(id_and_activity, lemma1, n)

dfm.trim <- dfm %>% 
  dfm_trim(min_docfreq = 0.01, docfreq_type = "prop")

dtm = convert(dfm, to = "topicmodels") 
View(dtm)


# Fit LDA
set.seed(18061987)

m = LDA(dtm, method = "Gibbs", k = 5,  control=list(iter = 5000, seed = 18061987,alpha = 0.1))
m

terms(m,20)


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
       y = "Percentage of tokens where this was the highest % topic")


# see alpha from previous model
attr(m, "alpha") 



###################Subset the debates

# #Get Aktuell, Interpellations and legislative data
# ###docs_aktuell_interpellations_legislative <- subset(debates_dataset,
#                                                    text_kammaraktivitet == ("aktuell debatt" 
#                                                                             "?rendedebatt" | 
#                                                                               "interpellationsdebatt"))

# Combines the debate text
bind_aai_dataset <- bind_rows(tf_idf_aktuell,tf_idf_arende,tf_idf_interpellations)
View(bind_aai_dataset)
nrow(bind_aai_dataset)
nrow(bind_cleaned_dataset)

# Frequency table
word_counts <- bind_aai_dataset %>%
  count(lemma1,sort = TRUE) 

corpus_data_aai <- bind_aai_dataset %>% 
  bind_tf_idf(lemma1, id_and_activity,n)

summary(corpus_data_aai$tf_idf)
summary(corpus_data_aai$n)
corpus_data_aai <- corpus_data_aai[corpus_data_aai$tf_idf >= 0.0012,]
View(corpus_data_aai)

nrow(corpus_data_aai)

filter_corpus_aai %>% count(lemma1)
#####Create document frequency 

filter_corpus_aai <- corpus_data_aai %>% subset(n>5)
View(filter_corpus_aai)

dfm.aai <- filter_corpus_aai %>%
  cast_dfm(id_and_activity, lemma1, n)

dtm.aai = convert(dfm.aai, to = "topicmodels") 
View(dtm.aai)


# Fit LDA

m.aai = LDA(dtm.aai, method = "Gibbs", k = 3,  control=list(iter = 5000, seed = 18061987,alpha = 0.1))
m.aai

terms(m.aai,20)


# Plotting the results
m.aai %>%
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
m.aai %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("id", "chamber_activity"), sep = "_") %>%
  mutate(chamber_activity = reorder(chamber_activity, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ chamber_activity) +
  labs(x = "Topic",
       y = "Percentage of tokens where this was the highest % topic")

# see alpha from previous model
attr(m.aai, "alpha") 




debates_corpus <- corpus(debates_dataset, text_field = "lemma1",docid_field = "text_dok_id")
View(debates_corpus)

debates_tokens <- tokens(debates_corpus)
View(debates_tokens)

# Create DTM, but remove terms which occur in less than 1% of all documents
DTM <- debates_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 0.99, docfreq_type = "prop")

# have a look at the number of documents and terms in the matrix
dim(DTM)



########


corpus_DFM <- dfm(debates_corpus, tolower = TRUE, stem = FALSE)

docvars(corpus_DFM, "text_dok_id")
debates_dataset$text_dok_id

#This enables you to easily recombine your dfm by user
  
dfm_group(corpus_DFM, groups = "text_dok_id")

##########













frequency <- debates_dataset %>% 
             group_by(text_talare) %>% 
             count(lemma1, sort = TRUE) %>% 
             left_join(debates_dataset %>% 
                       group_by(text_talare) %>% 
                       summarise(total = n())) %>%
             mutate(freq = n/total)


frequency


library(stringr)

debates_dataset %>% 
  filter(str_detect(lemma1, "interpellant")) %>% 
  select(lemma1)


# create a frequency table year wise
word_counts <- debates_dataset %>%
  count(text_dok_rm,lemma1,sort = TRUE) 

library(tidyr)

year_term_counts <- word_counts %>%
  complete(text_dok_rm, lemma1, fill = list(n = 0)) %>%
  group_by(text_dok_rm) %>%
  mutate(year_total = sum(n))

View(year_term_counts)

library(ggplot2)
ylims=labels = scales::percent_format()
year_term_counts %>%
  filter(lemma1 %in% c("bifall", "skola", "v?rd", "land")) %>%
  ggplot(aes(text_dok_rm, n / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ lemma1, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "% frequency of words" )


library(ggalt)



word_counts %>%
  complete(text_dok_rm, lemma1, fill = list(n = 0)) %>%
  group_by(text_dok_rm) %>%
  mutate(year_total = sum(n)) %>%
  filter(lemma1 %in% c("bifall", "skola", "v?rd", "land")) %>%
  ggplot(aes(x = text_dok_rm, y = n / year_total)) + 
  geom_point() +
  geom_smooth() +
  facet_wrap(~ lemma1, scales = "free_y") +
  geom_xspline(aes(y = expected), size = 0.8,
               spline_shape = -.15, colour = 'red')+
  labs(y = "",
       title = "% frequency of words")



#########################################

mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm",
                               "ab", "_k", "_k_", "_x"))

physics_words <- anti_join(physics_words, mystopwords, 
                           by = "word")




#budgetfrequency_table <- budget_cleaned %>%  count(lemma1)
#dim(budget_cleaned)

#quantile(budgetfrequency_table$n,0.99)
#hist(budgetfrequency_table$n)

# include only words that occur at least 50 times
# budget_dataset <- budget_cleaned %>%
#   group_by(lemma1) %>%
#   mutate(word_total = n()) %>%
#   ungroup() %>%
#  filter(word_total >= quantile(budgetfrequency_table$n,0.99))

#dim(budget_dataset)

final_dataset <- bind_rows(budget_cleaned,Utrikespolitisk_cleaned,interpellations_cleaned,arende_cleaned,aktuell_cleaned)

View(final_dataset)

final_dataset <- final_dataset %>% select(3,4,6,7,8,10,15)

#dim(final_dataset)
write.csv(final_dataset,"final_dataset.csv",row.names=F)
# Preprocess with qunteda

# Using tm

myCorpus <- Corpus(VectorSource(final_dataset$lemma1))
dtm = DocumentTermMatrix(myCorpus) 
View(dtm)

dfm = dfm(myCorpus)
dtm = convert(dtm, to = "topicmodels") 
set.seed(1)
m = LDA(dtm, method = "Gibbs", k = 10,  control = list(alpha = 0.1))
m


corpus_reshape()

notSparse = removeSparseTerms(dtm, 0.99)
View(notSparse)
finalWords=as.data.frame(as.matrix(notSparse))
View(finalWords)
corpus<- tokens(corp)
dfm<- dfm(corp)





##create DTM
dtm <- CreateDtm(partyleader_data$lemma1,
                 doc_names = partyleader_data$text_dok_id)

#explore the basic frequency
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
View(original_tf)
rownames(original_tf) <- 1:nrow(original_tf)

# Partiledardebatt


Partiledardebatt_2014_15
Partiledardebatt_2015_16
Partiledardebatt_2016_17
Partiledardebatt_2017_18

#Partiledar_data_2014 <- read.csv("Partiledardebatt_2014.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
Partiledar_data1 <- read.csv("Partiledardebatt_2014_15.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
Partiledar_data2 <- read.csv("Partiledardebatt_2015_16.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
Partiledar_data3 <- read.csv("Partiledardebatt_2016_17.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)
Partiledar_data4 <- read.csv("Partiledardebatt_2017_18.csv",encoding = 'UTF-8',header=TRUE,stringsAsFactors = FALSE)

Partiledar_data <- bind_rows(Partiledar_data1,Partiledar_data2,Partiledar_data3,Partiledar_data4)

# Cleaning lemma
Partiledar_data$lemma1 <- 
  unlist(lapply(strsplit(as.character(Partiledar_data$lemma),split="|",fixed=TRUE),`[[`,2)) 
View(Partiledar_data)

Partiledar_data$lemma1 <- sub("^\\-*|\\:+[0-9]*", "", Partiledar_data$lemma1)

#check whether data is cleaned or not
sort4 <- Partiledar_data[order(Partiledar_data$lemma1),]
d4<- sort4 %>% select(lemma1) %>% head(2000)
View(d4)

# Text preprocessing


Partiledar_data$lemma1 <- removeWords(Partiledar_data$lemma1,stopwords('sv'))
Partiledar_data <- Partiledar_data %>% filter(lemma1!="") 
Partiledar_data %>% select(match,pos,msd,lemma,lemma1) %>% head(10)

Partiledar_cleaned <- Partiledar_data %>% filter(pos!='PP' & pos!='PN'& pos!='IE' & pos!='KN' & pos!='SN' 
                                                 & pos!='DT' & pos!='HA'& pos!='HP' & msd!='AB' & 
                                                   (nchar(lemma1)>2))

View(Partiledar_cleaned)

# looking at top 5000 rows
partyleader_data <- Partiledar_cleaned %>% select(lemma1,text_dok_id) 
View(partyleader_data)

library(textmineR)
library(lda)

##create DTM
dtm <- CreateDtm(partyleader_data$lemma1,
                 doc_names = partyleader_data$text_dok_id)

#explore the basic frequency
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
View(original_tf)
rownames(original_tf) <- 1:nrow(original_tf)

# Eliminate words appearing less than 2 times or in more than half of the
# documents
hist(tf$doc_freq)
quantile(tf$doc_freq,0.99)
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm


View(vocabulary)



k_list <- seq(1, 5, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines
#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
library(ggplot2)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")

write.csv(Partiledar_cleaned,"Partiledar_debate.csv")

Partiledarfrequency_table <- Partiledar_cleaned %>%  count(lemma1, sort = TRUE)
write.csv(Partiledarfrequency_table,"Partiledarfrequency_table.csv")
View(Partiledarfrequency_table)


Partiledar_cleaned %>%  count(text_dok_id, sort = TRUE)








