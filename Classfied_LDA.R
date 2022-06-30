rm(list=ls())
# Set working directory
setwd("D:/Thesis/Data/latest CSV files")


#Data wrangling
library(tidyverse)
library(stringi)

#Text Processing
library(tm)
library(tidytext)
library(tidyr)
library(textmineR)
library(stringr)
library(dplyr)
library(openxlsx)

#Topic modeling Packages
require(quanteda)
library(topicmodels)

#Visualization
library(reshape2)
library(ggplot2)

# Read dataset
debates_dataset <- read.csv("Classified_dataset.csv",header=TRUE,stringsAsFactors = FALSE)
nrow(debates_dataset)
#View(debates_dataset)
debates_count <- debates_dataset %>%
  count(id_and_activity,lemma1,sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(lemma1, id_and_activity,n)

summary(debates_count$tf_idf)

tf_idf_Debates <- debates_count[debates_count$tf_idf >= 0.0005396,]


debates_dfm <- tf_idf_Debates %>% select(1,2,3) %>% cast_dfm(id_and_activity,lemma1,n)

debates_dtm <- quanteda::convert(debates_dfm,to="topicmodels")

inspect(debates_dtm)


word_freq_debates <- findFreqTerms(debates_dtm,
                                  lowfreq = 5,
                                  highfreq = nrow((debates_dtm)*0.9))

debates_dtm <- debates_dtm[,word_freq_debates]



# Fit LDA
model_lda = topicmodels::LDA(debates_dtm, method = "Gibbs", k = 16,   control=list(iter = 300, seed = 18061987,alpha = 0.1))


save(model_lda, file="/Users/vinu/Documents/DalarnaUnivercity/Thesis/Data//backup/lda_model_output.Rdata")

load("/Users/vinu/Documents/DalarnaUnivercity/Thesis/Data//backup/lda_model_output.Rdata")

phi_lda = as.matrix(posterior(model_lda)$terms)
theta_lda <- as.matrix(posterior(model_lda)$topics)
vocab_lda <- colnames(phi)

typeof(debates_dtm)
typeof(phi_lda)
typeof(theta_lda)


ll_lda <- CalcLikelihood2(dtm = debates_dtm, 
                     phi=phi_lda,
                     theta = theta_lda)

ll_lda

names(theta_lda)

colnames(debates_dtm)
colnames(theta_lda)
colnames(phi_lda)

ncol(debates_dtm)
ncol(theta_lda)
ncol(phi_lda)

nrow(debates_dtm)
nrow(theta_lda)
nrow(phi_lda)

gamma.topics.lda <- tidy(model_lda,"gamma") %>%
  separate(document, c("id", "chamber_activity"), sep = "_") %>%
  group_by(topic)

#view(gamma.topics) 

# To plot the probability of activities per topic
gamma_activity_topics_lda <- gamma.topics.lda %>% 
  group_by(chamber_activity,topic) %>%
  summarise(m = mean(gamma))

View(gamma_activity_topics)

ggplot(gamma_activity_topics_lda, aes(x =topic, y = m, colour = topic)) + geom_line() + 
  geom_point() + scale_x_continuous(breaks = c(1:16)) +
  xlab("Topics of LDA")+ylab("Mean Gamma Probabilities of Documents")+
  facet_wrap(~ chamber_activity, ncol = 2)



gamma_activity_combain <- data.frame(topic =gamma_activity_topics$topic,
                                     chamber_activity=gamma_activity_topics_lda$chamber_activity,
                                     m_lda= gamma_activity_topics_lda$m,
                                     m_stm = gamma_activity_topics$m,
                                     colour=rep(c("STN","LDA"), each = 2))




ggplot(gamma_activity_combain, aes(x=topic,y = m_lda, colour = colour)) + 
  geom_line(aes(y = m_lda,colour='LDA'),lwd=1.5) + 
  geom_point(aes(y = m_lda,),color='black')+
  scale_x_continuous(breaks = c(1:16)) +
  geom_line(aes(y = m_stm,colour='STM'),lwd=1.5) +
  geom_point(aes(y = m_stm),color='black')+
  facet_wrap(~ chamber_activity, ncol = 2)+
  labs(x = "Topics",
       y = "Mean Gamma Probabilities of Documents",
       color = "Model",)



gfg_plot <- ggplot(gamma_activity_combain,            
                   aes(x = m_lda,
                       y = value,
                       color = variable)) +  geom_line()+
scale_x_continuous(breaks = c(1:16)) +
  xlab("Topics of LDA and STM")+ylab("Mean Gamma Probabilities of Documents")+
  facet_wrap(~ chamber_activity, ncol = 2)




ggplot(gamma_activity_topics_lda, aes(x =topic, y = m, colour = topic)) + geom_line() + 
  geom_point() + scale_x_continuous(breaks = c(1:16)) +
  xlab("Topics of LDA")+ylab("Mean Gamma Probabilities of Documents")+
  facet_wrap(~ chamber_activity, ncol = 2)


ggplot(economics, aes(x=date)) + 
  geom_line(aes(y = psavert), color = "darkred") + 
  geom_line(aes(y = uempmed), color="steelblue", linetype="twodash") 




plot( x, y1, type="l", col="red" )
par(new=TRUE)
plot( x, y2, type="l", col="green" )

# Get the higest mean of documents probabilities

activities_mean <- gamma_activity_topics %>%
  group_by(chamber_activity) %>%
  slice_max(m,n=1)
View(activities_mean)


# Plotting the results
model_lda %>%
  tidy() %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# MInistry    Whole words_Classification        Top words classification
#1 Education              0.55                            0.68
#5 Foreign - 0.62
# 6 Climate - 0.42 0.78   6-Rural Affairs 0.60 0.8
#7 Energy - 0.42 7-Enterprise and Innovation - 0.35
#8 Defense - 0.55
#9 Justice - 0.30
# 10 Finance                                             0.95
# 12 Infrastructure 0.55       0.85
#11-Health and social 0.57
# 13 -Culture -0.50 0.65
#14 Employment - 62.5
# 15=Housing and Dev - 0.45 0.65
#16-Gender Equality - 32.5
#

# Plot the distribution of topics whole terms
m %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("id", "chamber_activity"), sep = "_") %>%
  mutate(chamber_activity = reorder(chamber_activity, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ chamber_activity) +
  labs(x = "Topic Numbers",
       y = "# of messages where this was the highest % topic")

# PLot the top terms
m %>%
  tidy(matrix = "gamma") %>%
  group_by(topic) %>%
  slice_max(gamma,n = 25) %>%
  separate(document, c("id", "chamber_activity"), sep = "_") %>%
  mutate(chamber_activity = reorder(chamber_activity, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ chamber_activity) +
  labs(x = "Topic Numbers",
       y = "# of messages where this was the highest % topic")

llis.terms <- as.data.frame(topicmodels::terms(m, 30), stringsAsFactors = FALSE)



gamma_activity_topics <- gamma.topics %>% 
  group_by(chamber_activity,topic) %>%
  summarise(m = mean(gamma))

View(gamma_activity_topics)

ggplot(gamma_activity_topics, aes(x =topic, y = m, colour = topic)) + geom_line() + 
  geom_point() + scale_x_continuous(breaks = c(1:16)) +
  xlab("Topics of LDA")+ylab("Mean Gamma Probabilities of Documents")+
  facet_wrap(~ chamber_activity, ncol = 2)











toLDAvis(m,debates_dtm)


library(LDAvis)

dtm <- debates_dtm
dtm = dtm[slam::row_sums(dtm) > 0, ]

str_enc

phi = as.matrix(posterior(m)$terms)
theta <- as.matrix(posterior(m)$topics)

Encoding(colnames(phi))<-"latin1"
vocab <- colnames(phi)

doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)
