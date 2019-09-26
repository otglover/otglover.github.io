#' ---
#' title: "Assignment 1 - MSBA 70450"
#' author: "Othiel Glover"
#' date: "September 16th, 2019"
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

library(gutenbergr)

doyle <- gutenberg_works(author == "Doyle, Arthur Conan")
view(gutenberg_works())
view(gutenberg_works$gutenberg_bookshelf)

#1. List all of the books written by Oscar Wilde (sort by title).

gutenberg_works() %>%
  filter(author == "Wilde, Oscar") %>%
  arrange(title) %>% 
  select(title) %>% 
  print(n=31)

#2. Sir Arthur Conan Doyle is well known for his book "The Adventures of Sherlock Holmes" . 
# A little known fact about him is that he was also a physician who loved poetry. A few of his published poems are part of the
# project gutenberg archives. Find and list them (sort by title).

poetry_books <- filter(gutenberg_works(), grepl("Poe", title, fixed = FALSE))
doyle_poems <- poetry_books %>%
  filter(author == "Doyle, Arthur Conan") %>% 
  arrange(title)

print(doyle_poems)

#---Alternative:
gutenberg_works() %>%
  filter(author == "Doyle, Arthur Conan", is.na(gutenberg_bookshelf)) %>% 
  arrange(title) %>% 
  select(title) %>% 
  print(n=36)

# Once we have the book id (title), we can now download it by using gutenberg_download().
doyle_poems <- gutenberg_download(38071)
print(doyle_poems, n=98)
?print

# Let's add new features for line number, book and chapter.
doyle_poems <- doyle_poems %>%
  mutate(
    linenumber = row_number()
      ) 
doyle_poems


doyle_poems <- doyle_poems[82:98,] %>%
  select(text) %>%
  arrange(text)
print(doyle_poems)

# We can also download more than one book at a time.
# Note our use of the "title" metafield.
books <- gutenberg_download(c(768, 1260), meta_fields = "title")
books

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

# Start by defining base url:
baseURL <- "https://www.yelp.com/biz/the-kitchen-chicago-3?osq=the+kitchen"

# Next, retrieve the scripts within the html.
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

# To make the data easier to work with, let's convert it to a tibble with only the data we want.
the_Kitchen_Reviews <-
  tibble(
    reviewDate = unlist(the_Kitchen_Reviews$review["datePublished"]),
    rating = unlist(the_Kitchen_Reviews$review["reviewRating.ratingValue"]),
    review = unlist(the_Kitchen_Reviews$review["description"]),
    author = unlist(the_Kitchen_Reviews$review["author"])
  )
the_Kitchen_Reviews

# Note that all we've retrieved so far, are simply the reviews on the first page.
# To get all the reviews for this restaurant, we need to recurse through the pages.
# The map_dfr() function from the purr package row-binds the results from each iteration.
# Note: This takes a bit of time to run. Not a bad time to go get some coffee.

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

# What do we now have?
the_Kitchen_Reviews

# change rating to factor
the_Kitchen_Reviews_mod <- as.factor(the_Kitchen_Reviews$rating) 
the_Kitchen_Reviews_mod

# Now we are ready to tokenize our text data.
# This is done using the unnest_tokens() function.
# Import the stop words dictionary into environment:
data("stop_words")

# Let's tidy it, remove stop words and get the most frequently occurring words.
tkreviews %>%
  unnest_tokens(output = word, input = review, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

# We can create a simple word cloud using the wordcloud2 package.
library(wordcloud2)

tkreviews %>%
  unnest_tokens(output = word, input = review, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>% 
  wordcloud2(size = .5, shape = "square")


#2. Create another visualization of the top 20 words based on tf-idf.

# To get a better measure of word importance, let's look at tf-idf.
# We do this by using the bind_tf_idf() function from the tidytext package.

summary(the_Kitchen_Reviews)

tkreviews %>%
  unnest_tokens(output = word, input = review, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)


the_Kitchen_Reviews %>%
  unnest_tokens(output = word, input = review, token = "words") %>%
  anti_join(stop_words, by = "word") %>% 
  count(rating, word, sort = TRUE) %>%
  bind_tf_idf(word, rating, n) %>%
  arrange(desc(tf))

tf_df_the_Kitchen_Reviews <- the_Kitchen_Reviews %>%
  unnest_tokens(output = word, input = review, token = "words") %>%
  anti_join(stop_words, by = "word") %>% 
  group_by(rating) %>%
  mutate(total = n()) %>%
  group_by(rating, word, total) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(tf = count/total) %>%
  arrange(desc(tf))

tf_df_the_Kitchen_Reviews

sub_tf_df_the_Kitchen_Reviews <- select(tf_df_the_Kitchen_Reviews,word,count)

sub_tf_df_the_Kitchen_Reviews

sub_tf_df_the_Kitchen_Reviews %>%
  top_n(20) %>%
  wordcloud2(size = .5, shape = "circle")

#3. Is there a difference between the words on both lists? If so, why do you think that is?
#Yes, there is a difference between the two lists. This is likely because
