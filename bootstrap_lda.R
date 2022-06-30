rm(list=ls())
# Set working directory
setwd('/Users/vinu/Documents/DalarnaUnivercity/Thesis/Data/')

# Set seed to replicate results across several analysis of methods
set.seed(19870611)

#Data wrangling
library(tidyverse)

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

# Sampling
library(boot)

# Reading the dataset
#debates_dataset <- read.csv("",header=TRUE,stringsAsFactors = FALSE,encoding = 'latin1')
#d<- debates_dataset %>% select(lemma1,id_and_activity)
#write.csv(d,"d.csv",row.names=F)Classified_dataset.csv
debates_dataset <- read.csv("Classified_dataset.csv",header=TRUE,stringsAsFactors = FALSE,encoding = 'latin1')
#glimpse(debates_dataset)
#head(debates_dataset,10)
debates_dataset_boot <- debates_dataset[,c('id_and_activity','lemma1')]
# Function to caluclate Stistics for boot strap sample

LoopMeanBootstrap <- function(dataset) {
  
  # Number of observations to sample
  N <- length(unique(dataset$id_and_activity))
  print(N)
  
  # Number of Bootstrap samples
  B <- 200
  # Variables to store outcome
  #out.prevalence <- rep(0, B)
  #sample_coherence <- rep(0,B)
  out.dataframe <- list()
  # Repeat sampling with number of given number of boot strap samples
  for (i in 1:B) {
    #idx <- sample(N, replace = TRUE)
    idx <- dataset %>% nest(-id_and_activity) %>% slice_sample(n=N,replace = TRUE) %>% unnest()
    
    # To calcuclate tf-idf measure
    debates_count <- idx %>%
                    count(id_and_activity,lemma1,sort = TRUE) %>%
                    ungroup() %>%
                    bind_tf_idf(lemma1, id_and_activity,n)
    
    # Based on median of tf-idf remove unnecessary words
    tf_idf_debates <- debates_count[debates_count$tf_idf >= median(debates_count$tf_idf),]
    
    # Prepare dtm
    debates_dtm <- tf_idf_debates %>% select(1,2,3) %>% cast_dtm(id_and_activity,lemma1,n)
    
    # Trim the dtm due to huge dataset
    word_freq_debates <- findFreqTerms(debates_dtm,
                                       lowfreq = 5,
                                       highfreq = nrow((debates_dtm)*0.9))
    
    # Subset dtm 
    debates_dtm <- debates_dtm[,word_freq_debates]
    
    m300 = topicmodels::LDA(debates_dtm, method = "Gibbs", k = 16,   control=list(iter = 300, seed = 18061987,alpha = 0.1))
    
    # Selected topic proportions using gamma probability
    gamma.topics <- tidy(m300,"gamma") %>%
      separate(document, c("id", "chamber_activity"), sep = "_") %>%
      group_by(topic)
    
    
    #view(gamma.topics) 
    
    # To plot the probability of activities per topic
    gamma_activity_topics <- gamma.topics %>% 
      group_by(chamber_activity,topic) %>%
      summarise(m = mean(gamma))
    
    activities_mean <- gamma_activity_topics %>%
      group_by(chamber_activity) %>%
      slice_max(m,n=1)
      
    print(activities_mean)
    print('-----------------------------------')
    print(out.dataframe)
    print('-----------------------------------')
    print(length(out.dataframe)==0)
    if(length(out.dataframe)==0){
      print('Hereeeeeeeeeee')
      out.dataframe <- activities_mean
    }
    else{
    out.dataframe <- bind_rows(out.dataframe,activities_mean)
    }
    print(out.dataframe)
  }
  #list(out,sample_coherence)
  out.dataframe }

# Calling the function
gamma_mean_activities <- LoopMeanBootstrap( debates_dataset_boot)

write.csv(gamma_mean_activities,'gamma_mean_activities_2.csv')
ibrary(DescTools)

gamma_mean_activities %>% 
  nest(data = - "chamber_activity") %>% 
  mutate(ci = map(data, ~ MeanCI(.x$m,conf.level=0.99, na.rm=TRUE))) %>% 
  unnest_wider(ci)

length(out.dataframe)==0


# bootstrapping with 100 replications
#results <- boot(data=d, LoopMeanBootstrap, R = 100)





