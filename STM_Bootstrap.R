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
library(stm)
# Sampling
library(boot)

# packages for cluster standard errors
library(lmtest)
library(lfe)
# use multicore functions
library(snow)
library(parallel)
library(MASS)
numCores <- detectCores()


# Reading the dataset
debates_dataset <- read.csv("Classified_dataset.csv",header=TRUE,stringsAsFactors = FALSE,encoding = 'latin1')

talare_position <- read.csv('talare_position_2.csv',sep = ';')[,c("text_talare","Role")]

debates_dataset <- left_join(debates_dataset, unique(talare_position), 
                             by = c("text_talare" = "text_talare"))

debates_dataset$id_and_activity_role <- paste(debates_dataset$id_and_activity,"_",debates_dataset$Role)

debates_dataset_boot <- debates_dataset[,c('id_and_activity','lemma1','id_and_activity_role')]

LoopMeanBootstrap <- function(dataset) {
  # open time stamp
  t1 <- Sys.time()
  # Number of observations to sample
  N <- length(unique(dataset$id_and_activity))
  print(N)
  
  # Number of Bootstrap samples
  B <- 100
  # Variables to store outcome
  #out.prevalence <- rep(0, B)
  #sample_coherence <- rep(0,B)
  out.dataframe <- list()
  # Repeat sampling with number of given number of boot strap samples
  for (i in 1:B) {
    print(i)
    #idx <- sample(N, replace = TRUE)
    idx <- dataset %>% nest(-id_and_activity) %>% slice_sample(n=N,replace = TRUE) %>% unnest()
    
    dfm.un.meta <- idx %>% 
      count(id_and_activity_role, lemma1, sort = TRUE) %>%
      cast_dfm(id_and_activity_role,lemma1,n)
    
    dfm.un.trim1.meta <- dfm_trim(dfm.un.meta, min_termfreq = 5, max_docfreq = 0.99, docfreq_type = "quantile") # min 7.5% / max 90%
    
    head(dfm.un.trim1.meta)
    
    dfm.un.trim1.meta <- dfm_remove(dfm.un.trim1.meta, c("bra", "mer", "stå", "del", "brev", "ort", 
                                                         "maste", "regering", "tur", "fort", "igång"
                                                         , "fördel", "medföra", "fråga", "sätt", "annan", "grann",
                                                         "namn","par","namn","namn","namn","namn"))
    
    
    untidy <- tidy(dfm.un.trim1.meta)
    tf_idf_Debates_sep <- untidy %>%
      separate(document, c( "id","chamber_activity","role"), 
               sep = "_", convert = TRUE)
    
    covariates <- tf_idf_Debates_sep %>%
      sample_frac() %>%
      distinct(id,chamber_activity, role)
    
    print
    debates_dtm_meta <- quanteda::convert(dfm.un.trim1.meta,to="stm")
    
    
    
    out <- prepDocuments(debates_dtm_meta$documents, debates_dtm_meta$vocab, covariates)
    
    n.topics <- 16
    model_stm <- stm(
      documents =out$documents,
      vocab =out$vocab,
      data = out$meta,
      prevalence = ~role,
      K = n.topics
    )
    
    # Selected topic proportions using gamma probability
    gamma.topics <- tidy(model_stm,"gamma",document_names = rownames(dfm.un.trim1.meta)) %>%
      separate(document, c("id","chamber_activity"), sep = "_") %>%
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
    write.csv(out.dataframe,'boot/gamma_mean_activities_STM_100_temp.csv')
  }
  #list(out,sample_coherence)
  t2 <- Sys.time()
  t2 - t1 

  out.dataframe

  }

# Calling the function
gamma_mean_activities_STM <- LoopMeanBootstrap(debates_dataset_boot)

write.csv(gamma_mean_activities_STM,'boot/gamma_mean_activities_STM_50_1.csv')

debates_bootstrap_STM <- read.csv("boot/gamma_mean_activities_STM_200_final.csv",header=TRUE,stringsAsFactors = FALSE,encoding = 'latin1')

debates_bootstrap_LDA <- read.csv("boot/gamma_mean_activities_LDA.csv",header=TRUE,stringsAsFactors = FALSE,encoding = 'latin1')


debates_bootstrap

debates_bootstrap_STM %>% 
  nest(data = - "chamber_activity") %>% 
  mutate(ci = map(data, ~ MeanCI(.x$m,conf.level=0.99, na.rm=TRUE))) %>% 
  unnest_wider(ci)

library(DescTools)

ggplot(gamma_activity_topics, aes(x =topic, y = m, colour = topic)) + geom_line() + 
  geom_point() + scale_x_continuous(breaks = c(1:16)) +
  xlab("Topics of  STM")+ylab("Mean Gamma Probabilities of Documents")+
  facet_wrap(~ chamber_activity, ncol = 2)











