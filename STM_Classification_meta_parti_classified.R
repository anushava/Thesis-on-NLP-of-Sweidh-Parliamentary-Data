rm(list=ls())
# Set working directory
setwd('/Users/vinu/Documents/DalarnaUnivercity/Thesis/Data/')

#Text Processing
library(tidyverse)
library(tm)
library(tidytext)
library(tidyr)
library(textmineR)
library(stringr)
library(dplyr)


#Topic modeling Packages
require(quanteda)
library(topicmodels)
library(quanteda)
#Visualization
library(reshape2)
library(ggplot2)
library(stm)
library(ggwordcloud)
library(lda)

debates_dataset <- read.csv("Classified_dataset.csv",header=TRUE,stringsAsFactors = FALSE,encoding = 'latin1')
talare_position <- read.csv('talare_position_2.csv',sep = ';')[,c("text_talare","Role")]

debates_dataset <- left_join(debates_dataset, unique(talare_position), 
                             by = c("text_talare" = "text_talare"))

debates_dataset <-  debates_dataset %>%
  mutate(parti_classified = case_when(
    text_parti == "MP" ~ "Far Left",
    text_parti == "V" ~ "Far Left",
    text_parti== "SD" ~ "Far right",
    text_parti== "KD" ~ "Far right",
    TRUE ~ "Others"
  ))

unique(debates_dataset$parti_classified)

debates_dataset$id_and_activity_parti_clas <- paste(debates_dataset$id_and_activity,"_",debates_dataset$parti_classified)

dfm_debates_parti_clas <- debates_dataset %>% 
  count(id_and_activity_parti_clas, lemma1, sort = TRUE) %>%
  cast_dfm(id_and_activity_parti_clas,lemma1,n)

dfm_debates_parti_clas_trim <- dfm_trim(dfm_debates_parti_clas, min_termfreq = 5, max_docfreq = 0.99, docfreq_type = "quantile") # min 7.5% / max 90%

head(dfm_debates_parti_clas_trim)

dfm_debates_parti_clas_trim <- dfm_remove(dfm_debates_parti_clas_trim, c("bra", "mer", "stå", "del", "brev", "ort", 
                                                               "maste", "regering", "tur", "fort", "igång"
                                                               , "fördel", "medföra", "fråga", "sätt", "annan", "grann",
                                                               "namn","par","namn","namn","namn","namn"))

nrow(dfm_debates_parti_clas_trim)
dfm_untidy_party_clas <- tidy(dfm_debates_parti_clas_trim)
tf_idf_debates_sep_parti_clas <- dfm_untidy_party_clas %>%
  separate(document, c( "id","chamber_activity","parti"), 
           sep = "_", convert = TRUE)

covariates_parti_clas <- tf_idf_debates_sep_parti_clas %>%
  sample_frac() %>%
  distinct(id,chamber_activity, parti)


debates_dtm_meta_pati_clas <- quanteda::convert(dfm_debates_parti_clas_trim,to="stm")


out_parti_clas <- prepDocuments(debates_dtm_meta_pati_clas$documents, debates_dtm_meta_pati_clas$vocab, covariates_parti_clas, 
                           lower.thresh = 0,verbose = TRUE)

n.topics <- 16
model_stm_parti_clas <- stm(
  documents =out_parti_clas$documents,
  vocab =out_parti_clas$vocab,
  data = out_parti_clas$meta,
  prevalence = ~parti,
  K = n.topics,
  seed = 300
)

save(model_stm_parti_clas, file="/Users/vinu/Documents/DalarnaUnivercity/Thesis/Data//backup/model_stm_parti_clas_output.Rdata")

load("/Users/vinu/Documents/DalarnaUnivercity/Thesis/Data//backup/model_stm_parti_clas_output.Rdata")

model_stm_parti_clas %>%
  tidy()%>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

model_stm_parti_clas %>%
  tidy() %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  theme(text = element_text(size=10))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

model_stm_parti_clas %>%
  tidy(matrix = "theta", document_names = rownames(dfm_debates_parti_trim)) %>%
  group_by(topic) %>%
  slice_max(gamma,n = 20) %>%
  separate(document, c("id", "chamber_activity"), sep = "_") %>%
  mutate(chamber_activity = reorder(chamber_activity, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ chamber_activity) +
  labs(x = "Topic Numbers",
       y = "# of messages where this was the highest % topic")

gamma25_parti_clas <- tidy(model_stm_parti_clas,"gamma",document_names = rownames(dfm_debates_parti_trim)) %>%
  separate(document, c("id", "chamber_activity"), sep = "_") %>%
  group_by(topic) %>%
  slice_max(gamma,n=408)

gamma25_activity_parti_clas <- gamma25 %>% 
  group_by(chamber_activity,topic) %>%
  summarise(m = sum(gamma))

ggplot(gamma25_activity, aes(x =topic, y = m, colour = topic)) + geom_line() + 
  geom_point() + scale_x_continuous(breaks = c(1:16)) +
  xlab("Topics of STM")+ylab("Mean Gamma Probabilities of Documents")+
  facet_wrap(~ chamber_activity, ncol = 2)

plot(model_stm_parti_clas$convergence$bound, type="l", ylab="Approximate Objective", 
     main="Convergence")

toLDAvis(model_stm_parti_clas,out_parti_clas$documents)

#Model comparisons and determination of K

many_stm_models <- tibble(K = c(6,16,26, 36)) %>% 
  mutate(model = map(K, ~ stm(  documents =out$documents,
                                vocab =out$vocab,
                                data = out$meta,
                                prevalence = ~parti,
                                K = .,
                                seed = 300)))

model_stm_scores <- many_stm_models %>% 
  mutate(exclusivity = map(model, exclusivity),
         semantic_coherence = map(model, semanticCoherence, out$documents)) %>% 
  select(K, exclusivity, semantic_coherence)

model_stm_scores %>% 
  unnest(c(exclusivity, semantic_coherence)) %>% 
  ggplot(aes(x = semantic_coherence, y = exclusivity, color = as.factor(K))) +
  geom_point() +
  theme_bw()

model_stm_scores %>% 
  unnest(c(exclusivity, semantic_coherence)) %>% 
  group_by(K) %>% 
  summarize(exclusivity = mean(exclusivity),
            semantic_coherence = mean(semantic_coherence)) %>% 
  ggplot(aes(x = semantic_coherence, y = exclusivity, color = as.factor(K))) +
  geom_point() +
  theme_bw()

model_dtm_parti <- convert(dfm_debates_parti_trim, to = "tm")
model_stm$theta + 
  newdf <- rbind(model_stm$theta, )

ll <- CalcLikelihood(dtm = model_dtm_parti, 
                     phi = out_parti$vocab, 
                     theta = model_stm$theta )

heldout_parti <- make.heldout(out_parti$documents,out_parti$vocab)
eval_heldout_parti <-  eval.heldout(model_stm_parti,heldout_parti$missing)




