#' ---
#' title: "Homework 3"
#' author: "Team 4"
#' date: "October 13th, 2019"
#' ---
library(tidyverse)
library(wordcloud2)
library(SnowballC)
library(e1071)
library(kernlab)
library(tesseract)
library(magick)
library(dplyr)
library(tidytext)
library(reshape2)
library(tidyr)
library(Matrix)
library(digest)
library(devtools)
#----------------------------------------------------------------
#' PART I: Import the Text Message Data
#----------------------------------------------------------------
# For this assignment, you will be training a spam filter using a dataset of 5,559
# sms text messages which were previously labeled as spam or ham . The dataset
# is available at: " https://s3.amazonaws.com/notredame.analytics.data/sms.csv "
# 1. Import the dataset and call it sms .
sms <- read_csv("https://s3.amazonaws.com/notredame.analytics.data/sms.csv") 
# 2. Rename the class as message_label and convert it to a factor
names(sms)[1] <- "message_label"
sms <- mutate(sms, message_label = as.factor(message_label))
# 3. Add a document number for each message call it message_id 
sms <- tibble(message_id=1:5559, message_label = sms$message_label, text = sms$text)
#----------------------------------------------------------------
#' PART II: Explore and Prepare the Text Message Data
#----------------------------------------------------------------
# 1. Remove numbers and all special characters (except " ' ") from the text
# messages.
sms1 <- sms %>%
  mutate(text= str_replace_all(text,"[^[a-zA-z]\'\\s]",""))
# 2. Create separate word clouds of the top 100 most frequently occurring
# words for  
devtools::install_github('Lchiffon/wordcloud2', force = TRUE)
sms1 %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  top_n(100) %>% 
  group_by(word) %>% 
  wordcloud2(size = .9, shape = "circle")
sms1 %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  filter(message_label == 'ham') %>%
  count(word, sort = TRUE) %>%
  top_n(100) %>% 
  group_by(word) %>% 
  wordcloud2(size = .5, shape = "circle")
sms1 %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  filter(message_label == 'spam') %>%
  count(word, sort = TRUE) %>%
  top_n(100) %>% 
  group_by(word) %>% 
  wordcloud2(size = .9, shape = "circle")
#3. How do the word clouds differ from each other?
# Answer: The biggest difference between the ham and spam word clouds is the prevalence of the token "I" as by far the most
# prevalent token in the ham word list, with 2351 occurrences in the ham word cloud. In the spam list, the token "I" comes in
# at number 50 with just 59 occurrences. The overall email word cloud reflects the ham token distribution more closely than
# the spam word list. The top three tokens in both the overall and the ham list are "I", "to", and "you" (not necessarily in that order).
# Whereas the top three tokens in the spam list are "to", "a", and "call." This makes sense, as there are more ham messages in the overall
# set than spam messages. Also, the difference in the top words lists between the overall and ham messages and the spam messages makes sense
# because in "real" or ham messages, people are much more likely to talk about themselves,whereas spam messages are typically targeted
# at the recipient and are much more likely direct the user to take some action as opposed to talking about the sender in the first person. only 15 times.
#   4. Create a new dataset of the unique list of words (only) from the word
# clouds you created for 2(b) and 2(c) above. Call this dataset
# top_words .
# Hint: Look into the union() function from the dplyr package.
spam <- sms1 %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  filter(message_label=='spam') %>%
  count(word, sort = TRUE) %>%
  top_n(-100, row_number()) 
ham <- sms1 %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  anti_join(stop_words, by = "word") %>%
  filter(message_label=='ham') %>%
  count(word, sort = TRUE) %>%
  top_n(-100, row_number()) 
top_words <- union(spam, ham)
top_words <- top_words %>% distinct(word)
# 5. Using the tidytext approach, tokenize the sms dataset by words, remove
# stop words, limit the words in the dataset to only those in the
# top_words dataset and stem the words.
sms2 <- sms1 %>% 
  unnest_tokens(output = word, input = text, token = "words") %>%
  anti_join(stop_words, by = "word")
top_words_stem <- merge(top_words, sms2, by ="word", all.x=TRUE) %>% 
  mutate(word = wordStem(word))
# 6. Using the resulting dataset from the previous step, create a sparse matrix
# of the sms messages. Call the sparse matrix sms_wide . The
# sms_wide dataset should conform to the binary vector space model
# representation format.
sms_wide <- dcast(top_words_stem, message_id + message_label ~ word, fill = 0)
# 7. Split the sms_wide dataset into training and test sets using a 95:5 split
# ratio. 
set.seed(1234)
sample_set <- sample(nrow(sms_wide), round(nrow(sms_wide)*.95), replace = FALSE)
sms_train <- sms_wide[sample_set, ]
sms_test <- sms_wide[-sample_set, ]
#----------------------------------------------------------------
#' PART III: Train and Evaluate the Spam Filter Model
#----------------------------------------------------------------
# 1. Using the naiveBayes() function from the e1071 package, train a
# spam filter based off the training data you created in the previous section.
sms_mod <- naiveBayes(message_label ~ .-message_id, data = sms_train, laplace = 1)
# 2. Evaluate the performance of the model against the test data, by creating a
# confusion matrix and calculating the model's predictive accuracy. 
sms_pred <- predict(sms_mod, sms_test, type = "class")
sms_pred_table <- table(sms_test$message_label, sms_pred)
sms_pred_accuracy <- sum(diag(sms_pred_table)) / nrow(sms_test)
sms_pred_table
sms_pred_accuracy