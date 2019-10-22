#' ---
#' title: "Homework 2"
#' author: "Team 4"
#' date: "September 29th, 2019"
#' ---
library(tidyverse)
library(tidytext)
library(textdata)

#----------------------------------------------------------------
#' PART I: Working with the Tidy Sentiment Lexicons
#----------------------------------------------------------------
# Using the sensesensibility dataset from the janeaustenr package:
library(janeaustenr)

#  1. Using the AFINN lexicon, list the 10 most commonly used words with
# positive sentiment in the book.
tokens <- tibble(text = sensesensibility) %>% 
  unnest_tokens(output = word, input = text)

tokens %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>% 
  summarise(sentiment = sum(value)) %>% 
  arrange(desc(sentiment)) %>% 
  top_n(10, sentiment)

#  2. With the use of a stacked bar chart, show the ratio of negative to positive
# words in each chapter of the book. Use the Bing lexicon for this.
tokens2 <- tibble(text = sensesensibility) %>% 
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(
    text, regex("^chapter [\\divxlc]", ignore_case = TRUE)
  ))) %>% 
  unnest_tokens(output = word, input = text)

tokens2 %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(sentiment %in% c("positive","negative")) %>%
  count(word, chapter, sentiment) %>% 
  ggplot(mapping = aes(x = chapter, y = sentiment, fill = sentiment)) +
  geom_col(show.legend = TRUE) +
  theme_minimal()

#  3. Create a series of smoothed line charts that show a trendline of how often
# " trust ", " anticipation ", " joy ", " disgust ", " surprise " and " anger "
# sentiment words are used in each chapter of the book.
tokens2 %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment == "trust" | sentiment == "anticipation" | sentiment == "joy" | sentiment == "disgust"
         | sentiment == "surprise" | sentiment == "anger") %>% 
  count(chapter, sentiment) %>% 
  ggplot(mapping = aes(x = chapter, y = n, color = sentiment)) +
  geom_smooth(se = FALSE, show.legend = TRUE) +
  theme_minimal()

#----------------------------------------------------------------
#' PART II: Analyzing Yelp Restaurant Review Sentiment
#----------------------------------------------------------------
#In the previous assignment, you were required to import all of the Yelp customer
#reviews for a restaurant of your choosing. Using that same dataset, perform the
#following tasks:
library(rvest)
library(jsonlite)
library(sentimentr)

baseURL <- "https://www.yelp.com/biz/the-kitchen-chicago-3?osq=the+kitchen"
#1. Using the word-level sentiment analysis approach, create a visualization that shows the
#relationship between the rating that a customer assigned to your restaurant
#and the emotional valence of the review they provided.
#Use the Bing lexicon for this.
#First let bring our yelp resturant review data over.
tkHTML <- read_html(baseURL) %>%
  html_nodes("script")
indexJSON <- which(str_detect(tkHTML, "application/ld\\+json"))
the_Kitchen_Reviews <- fromJSON(html_text(tkHTML[indexJSON]), flatten = TRUE)
the_Kitchen_Reviews <-
  tibble(
    reviewDate = unlist(the_Kitchen_Reviews$review["datePublished"]),
    rating = unlist(the_Kitchen_Reviews$review["reviewRating.ratingValue"]),
    review = unlist(the_Kitchen_Reviews$review["description"]),
    author = unlist(the_Kitchen_Reviews$review["author"]))
the_Kitchen_Reviews <- map_dfr(1:30, function(x) {
  cat(".")
  if(x > 1){
    reviewURL <- str_c(baseURL, "?start=", (x-1) * 20)
  }
  else {
    reviewURL <- baseURL
  }
  reviewHTML <- read_html(reviewURL) %>% html_nodes("script")
  indexJSON <- which(str_detect(reviewHTML, "application/ld\\+json"))
  reviews <- fromJSON(html_text(reviewHTML[indexJSON]), flatten = TRUE)
  reviews <-
    tibble(
      reviewDate = unlist(reviews$review["datePublished"]),
      rating = unlist(reviews$review["reviewRating.ratingValue"]),
      review = unlist(reviews$review["description"]),
      author = unlist(reviews$review["author"])
    )
  return(reviews)
})
the_Kitchen_Reviews$rating <- as.factor(the_Kitchen_Reviews$rating)

#Visualize relationship between sentiment and review.
# word-level sentiment analysis
kitchen_tibble <-tibble(customer=1:600, text = the_Kitchen_Reviews$review, rating = the_Kitchen_Reviews$rating)

kitchen_tibble %>%
  group_by(rating) %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"), by = 'word') %>%
  count(customer, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(mapping = aes(x = rating, y = sentiment, fill = sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  labs(title = "The Kitchen Yelp Reviews Word-level Sentiment Analysis") +
  theme_minimal()

#2 Get sentence level sentiment by customer and rating.
kitchen_tibble_sentence <- kitchen_tibble %>%
  group_by(rating) %>%
  get_sentences() %>%
  sentiment_by(by = c("rating", "customer"))

# Create a chart of sentence-level sentiment by customer.
kitchen_tibble_sentence  %>%
  ggplot(mapping = aes(x = rating, y = ave_sentiment, fill =ave_sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  labs(title = "The Kitchen Yelp Reviews Sentence-level Sentiment Analysis") +
  theme_minimal()

#3 There is a differece between the two charts - the word level sentiment analysis shows that only rating 1 has negative sentiment, 
#while the sentence level sentiment shows that rating 2 has a breakdown of both negative and positive sentiment. 
#----------------------------------------------------------------
#' PART III: Labeling Previously Unlabeled News Articles
#----------------------------------------------------------------
# You are presented with 31 news articles which were scraped from a popular
# news site over a one month period. The data can be found here:
#   "https://s3.amazonaws.com/notredame.analytics.data/newsarticles.csv"
# Unfortunately, the intern who scraped this data failed to capture the appropriate
# topical labels for each article. Your task is to rely on your analytic expertise to
# assign reasonable labels to these articles.
# 1. Download and preview the data. You will notice that something is off
# about the encoding of the file and that the file is full of HTML tags.
# a. To convert the file encoding from " latin1 " to " ASCII ", you
# will need to use the iconv() base R function. Make sure to set
# sub = "" to handle any non-convertible bytes in the input.
# b. To remove the HTML tags in the data, you will need to make use
# of the read_html() function from the xml2 package to
# convert the articles into an HTML document object. Then, you
# can use the html_text() function from the rvest package to
# strip the articles of the HTML tags.
# Hint: Note that the three functions referenced above expect text
# vectors as input, not a table or table column. So, you will need to
# loop through each article in the data set and apply the functions
# to each one of them.
library(xml2)
library(stringr)
library(topicmodels)

news <- read_csv("https://s3.amazonaws.com/notredame.analytics.data/newsarticles.csv")
for (row in 1:nrow(news)) {
  news1 <- iconv(news[row,"body"], from = 'latin1', to = 'ASCII', sub = "")
}

news1 <- read_html(news1) %>% html_text()

# 2. To further prepare the data for analysis, remove numbers, any references
# to time (such as 7.53pm GMT) and stop words.
news2 <- tibble(line = 1:length(news1), text = news1, stringsAsFactors = 
                  FALSE)
custom_stop_words <-
  bind_rows(tibble(
    word = c("gmt","am","pm"),
    lexicon = c("custom")
  ), stop_words)
news2 <- news2 %>%
  mutate(text = gsub(x = text, pattern = "[0-9]+|[[:punct:]].", replacement = "")) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  anti_join(custom_stop_words, by = "word")

# 3. Using the class exercises as a guide, with alpha set to 0.5 and gamma set
# to 0.1, train an LDA topic model (based on Gibbs sampling) that assigns
# each of the articles to one of two topic categories.
dtm <- news2 %>%
  count(line, word) %>%
  ungroup() %>%
  cast_dtm(line, word, n)
as.matrix(dtm)
lda <- dtm %>%
  LDA(k = 2, method = "Gibbs", control = list(alpha = 0.5, delta = 0.1, seed = 1234))

# 4. List the top 5 terms within each topic and assign a label to each topic.
# Both topics seem to be about attacks. Topic 1 - Terrorism in France and Topic 2 - Police Brutality in Paris.
terms(lda, k = 5) 

posterior(lda)$topics


# 5. For each of the documents in the dataset, list the consensus topic as
# assigned by the model. Consensus Topic is Topic 1 at .504.
news3 <- lda %>%
  tidy(matrix = "gamma")

news3 %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(as.integer(document))
