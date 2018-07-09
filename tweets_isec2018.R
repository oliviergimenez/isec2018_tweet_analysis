# load several useful packages
library(rtweet)
library(tidyverse)
library(proustr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
library(tm)
library(widyr)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(wordcloud)
library(topicmodels)
library(SnowballC)

random_viridis <- function(x){
  sample(viridisLite::viridis(100), x)
}

#---------------------------------------------------
#---- 1. Get the tweets with hashtag #isec2018 ----#
#---------------------------------------------------

# I had some help from James Grecian as I was struggling 
# with the authentification steps; James followed the instructions 
# from http://rtweet.info/ and used functions create_token
# and search_tweets from package rtweet. For convenience, 
# I've made the tweets and retweets available from GitHub

load('tweets_isec2018.RData')
ls()

# tweets_and_rt contains tweets and retweets, while 
# tweets contains the tweets without retweets

#-------------------------------
#---- 2. Descriptive stats ----#
#-------------------------------

# I used scripts from two main sources
# http://breizhdataclub.org/breizhdataday-revue-de-tweets/
# https://brennonborbon.wordpress.com/2018/01/23/analyse-trend-twitter-data-with-rtweet-package-from-r/

#------ distribution

tweets_and_rt_July7 <- tweets_and_rt %>% filter(created_at < "2018-07-08 00:00:00", created_at > "2018-07-06 23:59:59")
ggplot(tweets_and_rt_July7) +
  aes(created_at) +
  geom_histogram(bins = 50, fill = random_viridis(1)) +
  labs(title = "July 7, #isec2018",
       x = "hour",
       y = "volume") +
  theme_minimal()
ggsave("distribution7.png")

tweets_and_rt_July8 <- tweets_and_rt %>% filter(created_at < "2018-07-09 00:00:00", created_at > "2018-07-07 23:59:59")
ggplot(tweets_and_rt_July8) +
  aes(created_at) +
  geom_histogram(bins = 50, fill = random_viridis(1)) +
  labs(title = "July 8, #isec2018",
       x = "hour",
       y = "volume") +
  theme_minimal()
ggsave("distribution8.png")

#------ Tweets and retweets during the day
ggplot(tweets_and_rt_July7) +
  aes(created_at, fill = is_retweet) +
  geom_density() +
  scale_fill_viridis_d() +
  labs(title = "Tweets and retweets during July 7",
       x = "", y = "Density of tweets") +
  facet_grid(is_retweet ~ .) +
  theme_minimal()
ggsave("density_rt_tweets7.png")

#------ Most active accounts
tweets_and_rt %>%
  count(screen_name) %>%
  top_n(20) %>%
  ggplot() +
  aes(reorder(screen_name, n), n) +
  geom_col(fill = random_viridis(1)) +
  labs(title = "Most active accounts during ISEC 2018",
       x = "",
       y = "Number of tweets") +
  coord_flip() +
  theme_minimal()
ggsave("mostactive.png")

#------ sources of the tweets

tweets %>% 
group_by(source) %>% 
summarise(Total=n()) %>% 
arrange(desc(Total)) %>% 
head(10) %>% 
ggplot(aes(reorder(source, Total), Total, fill = source)) + geom_bar(stat="identity") + coord_flip() + labs(title="Top Tweet Sources for #isec2018", x="", subtitle="")
ggsave('source.png')
 
#------ top words

# remove http elements manually
tweets$stripped_text <- gsub("http.*","", tweets$text)
tweets$stripped_text <- gsub("https.*","", tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
tweets_clean <- tweets %>%
 dplyr::select(stripped_text) %>%
 unnest_tokens(word, stripped_text)

# remove stop words from your list of words
tweet_words <- tweets_clean %>%
 anti_join(stop_words)

# plot the top 15 words
tweet_words %>%
 count(word, sort = TRUE) %>%
 top_n(15) %>%
 mutate(word = reorder(word, n)) %>%
 ggplot(aes(x = word, y = n, fill = word)) +
 geom_col() +
 xlab(NULL) +
 coord_flip() +
 labs(y = "Count",
 x = "Unique words",
 title = "Most Common words found in #isec2018 tweets",
 subtitle = '')
ggsave('topwords.png')

#------ network
 
# remove punctuation, convert to lowercase, add identity for each tweet!
tweets_paired_words <- tweets %>%
 dplyr::select(stripped_text) %>%
 unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

tweets_paired_words %>%
 count(paired_words, sort = TRUE)

tweets_separated_words <- tweets_paired_words %>%
 separate(paired_words, c("word1", "word2"), sep = " ")

tweets_filtered <- tweets_separated_words %>%
 filter(!word1 %in% stop_words$word) %>%
 filter(!word2 %in% stop_words$word)

# new bigram counts:
words_counts <- tweets_filtered %>%
 count(word1, word2, sort = TRUE)

# plot #isec2018 word network
words_counts %>%
 filter(n >= 5) %>%
 graph_from_data_frame() %>%
 ggraph(layout = "fr") +
 geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
 geom_node_point(color = "darkslategray4", size = 3) +
 geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
 labs(title = "Word Network: Tweets using the hashtag - #isec2018",
 subtitle = "Text mining twitter data ",
 x = "", y = "") 
ggsave('network_words.png')

#-------------------------------
#---- 3. Topic modelling ----#
#-------------------------------

# Convert to tm corpus and use its API for some additional fun
corpus <- Corpus(VectorSource(tweet_words))  # Create corpus object
corpus <- tm_map(corpus, removeWords, c("isec2018", "amp","isecstatecol"))
corpus <- tm_map(corpus, stemDocument, language = "english")

#------ wordcloud 
pal <- viridisLite::viridis(8)
png("wordclound.png",width = 4, height = 4, units = 'in', res = 300)
wordcloud(corpus, min.freq=2, max.words = 150, random.order = TRUE, col = pal)
dev.off()

#------ topic modeling

# source: https://gist.github.com/bryangoodrich/7b5ef683ce8db592669e

# Get the lengths and make sure we only create a DTM for tweets with some actual content
doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
dtm <- DocumentTermMatrix(corpus[doc.lengths > 0])

# Now for some topics
SEED <- sample(1:1000000, 1)  # Pick a random seed for replication
k <- 5  # Let's start with 5 topics

# Fit model
models <- list(Gibbs = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 5000,
                                                                 thin = 100,    iter = 10000)))

# Top 5 terms of each topic for each model
# Do you see any themes you can label to these "topics" (lists of words)?
lapply(models, terms, 10)

#      Topic 1        Topic 2    Topic 3        Topic 4     Topic 5         
# [1,] "bes"          "models"   "andrews"      "modelling" "britishecolsoc"
# [2,] "david"        "data"     "population"   "analysis"  "rstats"        
# [3,] "people"       "talk"     "biodiversity" "hmms"      "ellner"        
# [4,] "nice"         "movement" "hierarchical" "study"     "distance"      
# [5,] "density"      "model"    "selection"    "theoni"    "forward"       
# [6,] "sig"          "ecology"  "based"        "tomorrow"  "hmm"           
# [7,] "abundance"    "session"  "cool"         "marine"    "inference"     
# [8,] "hoeting"      "species"  "free"         "stephen"   "package"       
# [9,] "johnston"     "week"     "fun"          "science"   "statistics"    
# [10,] "conservation" "day"      "lucaborger"   "stats"     "joint"         



# Topic modelling also possible in the 
# tidy universe https://cran.r-project.org/web/packages/tidytext/vignettes/topic_modeling.html