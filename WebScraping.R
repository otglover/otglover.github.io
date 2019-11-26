#' ---
#' title: "Webscraping"
#' author: "OGlover"
#' date: "September 15th, 2019"
#' ---

library(tidyverse)
library(stringr)
library(tidytext)

#----------------------------------------------------------------
# PART I: WORKING WITH REGULAR EXPRESSIONS 
#----------------------------------------------------------------

#1. Using the words dataset in the stringr package, create regular expressions 
# in R that find all the words that:

#a. Start with the letter "w". For example: "who", "weigh", etc.
pattern <- "^w"
str_subset(words, pattern)

#b. End with the letter "t". For example: "commit", "fast", etc.
pattern <- "t$"
str_subset(words, pattern)

#c. Have a capital letter. For example: "Christ", "Christmas", etc.
pattern <- "[[:upper:]]"
str_subset(words, pattern)

#d. Start with the letter "w" and end with the letter "t". For examples: "want", "west", etc.
pattern <- "^w.*t$"
str_subset(words, pattern)

#e. Are exactly three letters long. For example: "kid", "son", etc .
pattern <- "^...$"
str_subset(words, pattern)

#f. Start and end with the same letter. For example: "dead", "knock", etc.
pattern <- "^(.).*\\1$"
str_subset(words, pattern)

#2. Using the fruit dataset, which is also from the stringr package, create regular 
# expressions in R that find all fruits with names that:

#a. Have exactly two occurrences of the letter "p" next to each other.
# For example: "apple", "pineapple", etc.
pattern <- "p{2}"
str_subset(fruit, pattern)

#b. Have exactly two occurrences of the letter "p" anywhere in the name. 
# For example, "papaya", "pineapple", etc.
pattern <- "^(?:[^p]*p){2}[^p]*$"
str_subset(fruit, pattern)

#c. Start with three consonants. 
# For example: "lychee", "physalis", etc.
pattern <- "^[^aeiou]{3}"
str_subset(fruit,pattern)

#----------------------------------------------------------------
# PART II: WORKING WITH DATA FROM PROJECT GUTENBERG
#----------------------------------------------------------------
# Only using functions provided by the gutenbergr package and other string and data manipulation functions we are familiar with in R:

library(gutenbergr)

#1. List all of the books written by Oscar Wilde (sort by title).
# There are 31 of them in the Project Gutenberg library:
gutenberg_works() %>%
  filter(author == "Wilde, Oscar") %>%
  arrange(title) %>% 
  select(title) %>% 
  print(n=31)

#2. Sir Arthur Conan Doyle is well known for his book "The Adventures of Sherlock Holmes" . 
# A little known fact about him is that he was also a physician who loved poetry. A few of his published poems are part of the
# project gutenberg archives. Find and list them (sort by title).

# First we find all books by Sir Arthur Conan Doyle and review them
doyle <- gutenberg_works(author == "Doyle, Arthur Conan")
view(gutenberg_works())

# Next we look for any book with either the word "Poetry" or "Poem" in the title, that were written by him.
poetry_books <- filter(gutenberg_works(), grepl("Poe", title, fixed = FALSE))
doyle_poems <- poetry_books %>%
  filter(author == "Doyle, Arthur Conan") %>% 
  arrange(title)

# Looks like there is just one book that contains his poems:
# Book ID #38071
print(doyle_poems)

# Once we have the book id (title), we can now download it by using gutenberg_download().
# We find that the titles of his poems are listed in the table of contents
# on rows 82-98 of the book text:
doyle_poems <- gutenberg_download(38071)
print(doyle_poems, n=98)

# To isolate these titles, we first add new features for line number, book and chapter.
doyle_poems <- doyle_poems %>%
  mutate(
    linenumber = row_number()
  ) 
doyle_poems
print(doyle_poems, n=98)

# Firnally, we select out just the lines that contain the titles of his poems, and organize them alphabetically.
doyle_poems <- doyle_poems[82:98,] %>%
  select(text) %>% 
  arrange(text)
print(doyle_poems)

#----------------------------------------------------------------
# PART III: WORKING WITH YELP RESTAURANT REVIEW DATA
#----------------------------------------------------------------

library(rvest)
library(jsonlite)

# Using the class exercises as a guide, find your favorite restaurant (or one that
# you've been to recently) on Yelp, import the customer reviews (all of them).
# Using the tidytext approach, tokenize by words and remove stop words.
# Based on the data you know have:

#1. Create a visualization of the 20 most frequently occurring words.

# We first start by defining base url - our restaurant is The Kitchen in Chicago:
baseURL <- "https://www.yelp.com/biz/the-kitchen-chicago-3?osq=the+kitchen"

# Next, we retrieve the scripts within the html.
tkHTML <- read_html(baseURL) %>%
  html_nodes("script")
tkHTML

# Using which() and str_detect(), we find the index of the row we need.
indexJSON <- which(str_detect(tkHTML, "application/ld\\+json"))
indexJSON

# With the index, we can now get the JSON.
# Using fromJSON() we convert from JSON to data frame and flatten the data.
the_Kitchen_Reviews <- fromJSON(html_text(tkHTML[indexJSON]), flatten = TRUE)
the_Kitchen_Reviews

# To make the data easier to work with, we convert it to a tibble with only the data we want.
the_Kitchen_Reviews <-
  tibble(
    reviewDate = unlist(the_Kitchen_Reviews$review["datePublished"]),
    rating = unlist(the_Kitchen_Reviews$review["reviewRating.ratingValue"]),
    review = unlist(the_Kitchen_Reviews$review["description"]),
    author = unlist(the_Kitchen_Reviews$review["author"])
  )
the_Kitchen_Reviews

# Now that we've retrieved the reviews on the first page, we next do the same for all 30 pages of reviews available:
the_Kitchen_Reviews <- map_dfr(1:30, function(x) {
  
  # A simple but effective progress indicator. :)
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

# We take a look at some of the reviews we've extracted, to make sure our code ran properly:
the_Kitchen_Reviews

# Next, we change rating to factor, so we can later group and sort by rating
the_Kitchen_Reviews_mod <- as.factor(the_Kitchen_Reviews$rating) 

# Next we tokenize our text data by using the unnest_tokens() function.
# First we import the stop words dictionary into environment:
data("stop_words")

# Next we tidy the data, remove stop words, and get the most frequently occurring words.
the_Kitchen_Reviews %>%
  unnest_tokens(output = word, input = review, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

# In order to create a simple word cloud, we load the wordcloud2 package.
library(wordcloud2)

# For this visualization, we create a word cloud of the 20 most frequently occurring words in the reviews we extracted. 
the_Kitchen_Reviews %>%
  unnest_tokens(output = word, input = review, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>% 
  wordcloud2(size = .5, shape = "circle")

#2. Create another visualization of the top 20 words based on tf-idf.
# To get a better measure of word importance, we look at tf-idf.
# We do this by using the bind_tf_idf() function from the tidytext package.
# Here we also group by rating, as this will help us organize the types of words people use by the rating they give the restaurant.

the_Kitchen_Reviews %>%
  unnest_tokens(output = word, input = review, token = "words") %>%
  anti_join(stop_words, by = "word") %>% 
  count(rating, word, sort = TRUE) %>%
  bind_tf_idf(word, rating, n) %>%
  top_n(20, tf_idf) %>%
  mutate(word = reorder(word, tf_idf)) %>%
  arrange(desc(tf_idf)) %>% 
  ggplot(mapping = aes(x = word, y = tf_idf)) +
  geom_col() +
  labs(x = "word", y = "tf_idf") +
  ggtitle("Top 20 Most Frequently-used Words in Reviews \nof The Kitchen in Chicago \nCalculation: (tf-idf)") +
  coord_flip() + 
  geom_bar(width=0.7, stat = "identity", fill = "blue") +
  theme_minimal()

#3. Is there a difference between the words on both lists? If so, why do you think that is?
# Answer: Yes, there is a difference between the two lists. 
#This is likely because tf-idf uses a weighting approach by choosing a weighting scheme. 
#The tf-idf approach will look for words with more discriminative power rather than just summarizing by its frequency, 
#whereas the first list looks purely at frequency of occurrence of each of the words across the reviews without weighting them.
