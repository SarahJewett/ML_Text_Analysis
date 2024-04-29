# Code here is either my own, or taken from quanteda tutorials and previous courses 
# I have taught at the LSE, in which I have made efforts to customize the code for this context. 

library(quanteda)
library("quanteda.textmodels")
library("quanteda.textstats")
library("quanteda.textplots")
library(readtext)

# Loading data, creating corpora, & tokenizing ------------------------------------------------------

# First, run all the code from the Plavsic_Analysis.Rmd before continuing.
# We will be suing corp_biljana and corpseg data corpus objects

# Now let's reload the statements of guilt data
guilty <- readtext(paste0("/Users/sarahjewett/Library/Mobile Documents/com~apple~CloudDocs/UPENN/ML_Text_Analysis/Statements_Guilt/*.pdf"),
                   docvarsfrom = "filenames", 
                   dvsep = " ",
                   docvarnames = c("first", "last"),
                   ignore_missing_files = TRUE)


# Create guilty corpus

guilty_corpus <- corpus(guilty)

# Tokenize 

guilty_toks <- tokens(guilty_corpus, remove_punct = TRUE)

biljana_toks <- tokens(corp_biljana, remove_punct = TRUE)

corpseg_toks <- tokens(corpseg, remove_punct = TRUE) 
corpseg_toks <- tokens_select(corpseg_toks, pattern = stopwords("en"), selection = "remove")


# Summary of Objects ------------------------------------------------------

summary(guilty_corpus)
summary(corp_biljana)
summary(corpseg)   

summary(guilty_toks)
summary(biljana_toks)
docvars(corpseg_toks)   


# Who spoke the most? The least? ------------------------------------------

par(mar = c(5, 6, .5, 1))
table(docvars(corpseg, "speaker")) %>%
  sort() %>%
  barplot(horiz = TRUE, las = 1, xlab = "Total Times Speaking")

# Frequency Analysis ------------------------------------------------------

# Let's now get a sense of who is using which term and how frequently

# Create a dfm
dfmcorpseg <- dfm(corpseg_toks) 
topfeatures(dfmcorpseg, 20)

tstat_freq <- textstat_frequency(dfmcorpseg, n = 5, groups = speaker)
head(tstat_freq, 20)
# you can also look at the whole thing b/c it is a data.frame 


# Collocation ---------------------------------------------
# Here we are going to see collocations -- in this case, what words are paired together
# often following the pattern of a capital letter word
toks_coll <- tokens(corpseg, remove_punct = TRUE)
tstat_col_caps <- tokens_select(toks_coll, pattern = "^[A-Z]", 
                                valuetype = "regex", 
                                case_insensitive = FALSE, 
                                padding = TRUE) %>% 
                  textstat_collocations(min_count = 75)
head(tstat_col_caps, 20)


# Dictionary --------------------------------------------------------------

remorse_dict <- dictionary(list(
  remorse = c("remorse*"),
  regret = c("regret*"),
  guilt = c("guilt*"),
  shame = c("shame*", "asham**"),
  apology = c("apolog*", "forgiv", "sorrow*", "sorry" )))

print(remorse_dict)

# You can use kwic() with dictionary lookup

kwic_guilty <- kwic(guilty_toks, pattern =remorse_dict, window = 7)
View(kwic_guilty)


# Related words to dictionary words ----------------------------------------
# code lifted from quanteda tutorials: https://tutorials.quanteda.io/advanced-operations/target-word-collocations/
toks_inside <- tokens_keep(guilty_toks, pattern = remorse_dict, window = 10)
toks_inside <- tokens_remove(toks_inside, pattern = remorse_dict) # remove the keywords
toks_outside <- tokens_remove(guilty_toks, pattern = remorse_dict, window = 10)

dfmat_inside <- dfm(toks_inside)
dfmat_outside <- dfm(toks_outside)

tstat_key_inside <- textstat_keyness(rbind(dfmat_inside, dfmat_outside), 
                                     target = seq_len(ndoc(dfmat_inside)))
head(tstat_key_inside, 50)


# LDA ---------------------------------------------------------
#install.packages("topicmodels")
#install.packages("LDAvis")
#install.packages("stm")
#install.packages("knitr")
#install.packages("servr")

library(topicmodels)
library(stm)
#install.packages(seeded.lda)
library(seededlda)
# With topic modelling we are basically doing bag of words analysis, so we will use
# the dfm object for the guilty statements, will make a new one b/c we will be trimming
# features using dfm_trim() to deal with features that are common but not too common

set.seed(22)
dfmat_guilty_trimmed <- dfm(guilty_toks) %>% 
  # keep top 25% of most frequent features (min_termfreq = 0.75)
  dfm_trim(min_termfreq = 0.75, termfreq_type = "quantile",
 # that appear in less than 15% of all documents (max_docfreq = 0.15) 
           max_docfreq = 0.15, docfreq_type = "prop")
# how min_termfreq and max_docfreq are interpreted withinin dfm_trim() 
# full argument options: "count", "prop", "rank", "quantile" 

# You could also do something like this:
# dfm_trim(dfmat, max_termfreq = 10, max_docfreq = 0.75)
# keep only words occurring <= 10 times and in at most 3/4 of the documents

# This might be more intuitive but you need to have a better sense of the data
# than specifying something like "top 25%" 

guilty_lda <- textmodel_lda(dfmat_guilty_trimmed, k = 10)
terms(guilty_lda, 10)

# Topics 5 and 15 both have 'pay' so maybe too many topics if getting repetition

# Play around with k:
set.seed(22)
guilty_lda <- textmodel_lda(dfmat_guilty_trimmed, k = 5)
terms(guilty_lda, 10)

# One issue here is that this is a small corpus! Not a dealbreaker, read:


# LDA pt. 2 ---------------------------------------
  
  # ORRRRR
  set.seed(100)
  
  dfmat_tm <- convert(dfmat_guilty_trimmed, to = "topicmodels")
  
  str(dfmat_tm)
  
  guilty_tm <- LDA(dfmat_tm,
              # k - number of topics
              k = 8, 
              # method - method for fitting
              method = "Gibbs",
              # control - sets control parameters for estimation
              control = list(alpha = 0.02, delta = 0.02))
  # alpha (topic proportions) & 
  # delta (term distribution over topics)

  get_terms(guilty_tm, 10)
  get_topics(guilty_tm, 5)


# Seeded LDA --------------------------------------------------------------
# Here we can use a dictionary as "seed words" to pre-define topics. These seed
# words may appear a topic words as well
guilty_tm_seeded <- textmodel_seededlda(dfmat_guilty_trimmed, dictionary = remorse_dict)

terms(guilty_tm_seeded, 20)

head(topics(guilty_tm_seeded), 20)

# Sentiment Analysis ------------------------------------------------------
# Let's keep going with the dictionaries!
# Both the quanteda packages and tidytext package have sentiment dictionaries built in

# Lexicoder Sentiment Dictionary (part of quanteda)
data_dictionary_LSD2015
lengths(data_dictionary_LSD2015)

# Check out the usage of this dictionary in quanteda tutorials 
# https://tutorials.quanteda.io/advanced-operations/targeted-dictionary-analysis/

# You can also import dictionaries or create your own, as you know!
# Check out this vignette on how to import external dictionaries:
# https://rdrr.io/github/quanteda/quanteda.sentiment/f/vignettes/sentiment_analysis.Rmd

# Hu, M. & Liu, B. 2004. Mining and Summarizing Customer Reviews. Proceedings of the ACM SIGKDD International Conference on Knowledge Discovery and Data Mining. Aug 22–25 2004. Seattle, WA: Association for Computing Machinery. 168-177. DOI: 10.1145/1014052.1014073.
load(url("https://quanteda.org/data/data_dictionary_HuLiu.rda"))
print(data_dictionary_HuLiu, max_nval = 10)

seg_toks_sentiment <- corpseg_toks %>%
  tokens_lookup(dictionary = data_dictionary_HuLiu)

# This has essentially just changed the words to a positive or negative rating
# Compare before and after:

corpseg_toks[6]
seg_toks_sentiment[6]

# You will see it dropped anything it couldn't classify as + or -

# Now we can look at the total positive and negative words, grouping by speaker
dfm_speaker_sentiment <- seg_toks_sentiment %>%
  dfm() %>%
  dfm_group(groups = speaker)

print(dfm_speaker_sentiment)

library(dplyr)

# we can combine the scores into a single sentiment score using
# log odds (logit). We take the logarithm of the ratio of positive to negative -- any differences in document frequencies do not affect this 
# We also are doing some smoothing by adding .5 to avoid division by zero/taking the log of zero
DF_speaker_sentiment <- dfm_speaker_sentiment %>%
  convert(to = "data.frame") %>%
  mutate(logsent = log((positive + 0.5) / (negative + 0.5))) %>%
  rename(speaker = doc_id)

DF_speaker_sentiment

# Visualize
library(ggplot2)
ggplot(DF_speaker_sentiment, aes(x = speaker, y = logsent)) +
  geom_point(size = 3) +
  theme(legend.position = "none") +
  coord_flip() +
  ylab("Log Sentiment")

# This is really interesting that the Interpreter has the most negative sentiment:

corp_interpreter <- corpus_subset(corpseg, speaker == "THE INTERPRETER:")

View(corp_interpreter)

# Now let's try all/some of this out on different data -- either your data from 
# Wednesday or from the following:
data_char_ukimmig2010 
data_corpus_moviereviews # esp good to apply a topic model!
