rm(list=ls())
gc()
# Set working directory
setwd('/Users/vinu/Documents/DalarnaUnivercity/Thesis/Data/')
library(tidyverse)
library(gutenbergr)
library(LSAfun)

#Text Processing
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

#Geting 
debates_dataset <- read.csv("Classified_dataset.csv",header=TRUE,stringsAsFactors = FALSE,encoding = 'latin1')
talare_position <- read.csv('talare_position.csv',sep = ';')[,c("text_talare","Role")]

debates_dataset <- left_join(debates_dataset, unique(talare_position), 
                             by = c("text_talare" = "text_talare"))

debates_dataset$id_and_activity_role <- paste(debates_dataset$id_and_activity,"_",debates_dataset$Role)
unique(debates_dataset$Role)

colSums(is.na(debates_dataset))
debates_dataset %>% filter(Role == "")

dfm_meta_role <- debates_dataset %>% 
  count(id_and_activity_role, lemma1, sort = TRUE) %>%
  cast_dfm(id_and_activity_role,lemma1,n)

dfm_trim_meta_role <- dfm_trim(dfm_meta_role, min_termfreq = 5, max_docfreq = 0.99, docfreq_type = "quantile") # min 7.5% / max 90%

head(dfm_trim_meta_role)

dfm_trim_meta_role <- dfm_remove(dfm_trim_meta_role, c("bra", "mer", "stå", "del", "brev", "ort", 
                                                     "maste", "regering", "tur", "fort", "igång"
                                                     , "fördel", "medföra", "fråga", "sätt", "annan", "grann",
                                                     "namn","par","namn","namn","namn","namn"))

untidy <- tidy(dfm_trim_meta_role)
tf_idf_Debates_sep <- untidy %>%
  separate(document, c( "id","chamber_activity","role"), 
           sep = "_", convert = TRUE)

covariates <- tf_idf_Debates_sep %>%
  sample_frac() %>%
  distinct(id,chamber_activity, role)

debates_dtm_meta <- quanteda::convert(dfm_trim_meta_role,to="stm")

out_role <- prepDocuments(debates_dtm_meta$documents, debates_dtm_meta$vocab, covariates,
                     lower.thresh = 0,verbose = TRUE)

n.topics <- 16
model_stm_role <- stm(
  documents =out_role$documents,
  vocab =out_role$vocab,
  data = out_role$meta,
  prevalence = ~role,
  K = n.topics,
  seed = 300
)

save(model_stm_role, file="/Users/vinu/Documents/DalarnaUnivercity/Thesis/Data//backup/stm_model_role_output.Rdata")

load("/Users/vinu/Documents/DalarnaUnivercity/Thesis/Data//backup/stm_model_role_output.Rdata")

model_stm %>%
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

model_stm %>%
  tidy(matrix = "theta", document_names = rownames(dfm.un.trim1.meta)) %>%
  group_by(topic) %>%
  slice_max(gamma,n = 20) %>%
  separate(document, c("id", "chamber_activity"), sep = "_") %>%
  mutate(chamber_activity = reorder(chamber_activity, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ chamber_activity) +
  labs(x = "Topic Numbers",
       y = "# of messages where this was the highest % topic")

gamma.topics <- tidy(model_stm,"gamma",document_names = rownames(dfm_trim_meta_role)) %>%
  separate(document, c("id", "chamber_activity"), sep = "_") %>%
  group_by(topic)

# To plot the probability of activities per topic
gamma_activity_topics <- gamma.topics %>% 
  group_by(chamber_activity,topic) %>%
  summarise(m = mean(gamma))

activities_mean <- gamma_activity_topics %>%
  group_by(chamber_activity) %>%
  slice_max(m,n=1)

ggplot(gamma_activity_topics, aes(x =topic, y = m, colour = topic)) + geom_line() + 
  geom_point() + scale_x_continuous(breaks = c(1:16)) +
  xlab("Topics of STM")+ylab("Mean Gamma Probabilities of Documents")+
  facet_wrap(~ chamber_activity, ncol = 2)

gamma_activity_combain <- data.frame(topic =gamma_activity_topics$topic,
                                     chamber_activity=gamma_activity_topics$chamber_activity,
                                     m_parti_clas= gamma25_activity_parti_clas$m,
                                     m_stm = gamma_activity_topics$m,
                                     colour=rep(c("PARTI","STM"), each = 2))

ggplot(gamma_activity_combain, aes(x=topic,y = m_lda, colour = colour)) + 
  geom_line(aes(y = m_lda,colour='LDA')) + 
  geom_point(aes(y = m_lda,),color='black')+
  scale_x_continuous(breaks = c(1:16)) +
  geom_line(aes(y = m_stm,colour='STM'),) +
  geom_point(aes(y = m_stm),color='black')+
  facet_wrap(~ chamber_activity, ncol = 2)+
  labs(x = "Topics",
       y = "Mean Gamma Probabilities of Documents",
       color = "Model",)


LDA_liklyhood <- read.csv('LDA_loglikelihood.csv',sep = ',')
STM_liklyhood <- head(read.csv('convergence2.csv',sep = ';'),50)

# define 3 data sets
xdata <- c(1:50)
y_lda <- LDA_liklyhood$log_likelihood
y_stm <- STM_liklyhood$Role_bound
y_stm_cls <- STM_liklyhood$Parti_clas_bound

# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, y_stm, col="dark blue",ylim=c(min(y_lda),max(y_stm_cls)),ylab="Log-likelihood",xlab="Iterations", type="o",lwd = 1)
lines(xdata, y_stm_cls, col="dark green", lty=1, type="o",)
lines(xdata, y_lda, col="dark red",lty=1, type="o",)
legend(38,mean(y_stm_cls),legend=c("STM Role","STM Parti","LDA"), col=c("dark blue","dark green","dark red"),
     lty=c(1,1,1), ncol=1,density = TRUE)

toLDAvis(model_stm,out_role$documents)

labelTopics(model_stm, n=20)

tibble(
  topic = 1:16,
  exclusivity = exclusivity(model_stm),
  semantic_coherence = semanticCoherence(model_stm, out$documents)
) %>% 
  ggplot(aes(semantic_coherence, exclusivity, label = topic)) +
  geom_point() +
  geom_text(nudge_y = .01) +
  theme_bw()

#Model comparisons and determination of K

many_stm_models <- tibble(K = c(6,16,26, 36)) %>% 
  mutate(model = map(K, ~ stm(  documents =out$documents,
                                vocab =out$vocab,
                                data = out$meta,
                                prevalence = ~role,
                                K = .,
                                )))

heldout <- make.heldout(out$documents,out$vocab)

model_stm_scores <- many_stm_models %>% 
  mutate(exclusivity = map(model, exclusivity),
         semantic_coherence = map(model, semanticCoherence, out$documents),
         eval_heldout = map(model, eval.heldout, heldout$missing),
         residual = map(model, checkResiduals, out$documents),
         bound =  map_dbl(model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(model, function(x) length(x$convergence$bound))) %>% 
  select(K, exclusivity, semantic_coherence,eval_heldout,residual,bound,lfact,lbound,iterations)

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


model_stm_scores %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around 16")


store_data1<- as.data.frame()
Topic <- c("Topic1","Topic2","Topic3","Topic4","Topic5","Topic6","Topic7",
           "Topic8","Topic9","Topic10","Topic11","Topic12","Topic13","Topic14","Topic15","Topic16" )

df <- data.frame( Topic = character(),
                  Prevalence = integer(),
                  stringsAsFactors=FALSE) 

Prevalence <- colSums(model_stm$theta) / sum(model_stm$theta) * 100
store_data <- data.frame(Topic,Prevalence)

store_data %>% pivot_longer(cols = c(Prevalence)) %>%
  ggplot(aes(x = factor(Topic,levels = unique(Topic)), y = value, group = 1)) +
  geom_point() + geom_line() +
  facet_wrap(~name,scales = "free_y",nrow = 2) +
  theme_minimal() +
  labs(
       x = "Topics", y = "Values of Prevalance")

cormat <- topicCorr(model_stm)
plot(cormat)





phi_lda = as.matrix(posterior(model_lda)$terms)
theta_lda <- as.matrix(posterior(model_lda)$topics)

view(phi)
view(theta)

typeof(phi_lda)

unique(debates_dataset$id_and_activity)

ll_lda <- CalcLikelihood(dtm = debates_dtm, 
                         phi=phi,
                         theta = theta)

rownames(model_dtm)
rownames(theta)




#------------------------- calculate Log Likelihood ------------------------

rowTotals <- apply(dfm.un.trim1.meta , 1, sum) 
dtm.new   <- dfm.un.trim1.meta[rowTotals> 0, ]          
model_dtm <- convert(dtm.new, to = "tm")

phi.stm <-model_stm$beta
phi.stm.df <- data.frame(phi.stm)
colnames(phi.stm.df) = model_stm$vocab
phi_stm <- data.matrix(phi.stm.df)
typeof(phi_stm)
rownames(phi_stm) <-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")

typeof(phi.stm.df )
view(model_stm$beta$logbeta)

theta.stm <- model_stm$theta
typeof(theta.stm)
rownames(theta.stm) <-rownames(model_dtm)
colnames(theta.stm) = list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")

ll_stm <- CalcLikelihood2(dtm = model_dtm, 
                     phi = phi_stm, 
                     theta =  theta.stm)

heldout_role <- make.heldout(out_role$documents,out_role$vocab)
eval_heldout_role <-  eval.heldout(model_stm,heldout_role$missing)

sum(eval_heldout_role$doc.heldout)







