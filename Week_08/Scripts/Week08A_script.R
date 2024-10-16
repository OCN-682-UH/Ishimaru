# MBIO 612: Week 8A
# Created by: Shelbie Ishimaru
# Created on: 2024-10-15
################################################################################
# Load libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(janeaustenr)

# Intro to strings -------------------------------------------------------------
#string and character are the same thing
words <-"This is a string" # a single string
words

words_vector<-c("Apples", "Bananas","Oranges") # a vector of strings
words_vector

# Intro to stringr -------------------------------------------------------------
#Manipulation
paste("High temp", "Low pH") #pastes words together in one string
paste("High temp", "Low pH", sep = "-") #add dash between words
paste0("High temp", "Low pH") #remove space between words

shapes <- c("Square", "Circle", "Triangle") #working with vectors
paste("My favorite shape is a", shapes) #make "sentences" using the vector: at the end of the phrase

two_cities <- c("best", "worst") #working with vectors 
paste("It was the", two_cities, "of times.") #make "sentences" using the vector: in the middle of the phrase

#Manipulation: individual characters
shapes #vector of shapes
str_length(shapes) #how many letters are in each word within the vector?

seq_data<-c("ATCCCGTC") #how to extract, seq data ex
str_sub(seq_data, start = 2, end = 4) # extract the 2nd to 4th AA

#Manipulation: modifying strings
str_sub(seq_data, start = 3, end = 3) <- "A" #add an A in the 3rd position
seq_data

str_dup(seq_data, times = c(2, 3)) #times is the number of times to duplicate each string

#Whitespace
badtreatments<-c("High", " High", "High ", "Low", "Low")
badtreatments
str_trim(badtreatments) # this removes both (right and left) side spaces
str_trim(badtreatments, side = "left") # this removes left side spaces

str_pad(badtreatments, 5, side = "right") #add a white space to the right side after the 5th character
str_pad(badtreatments, 5, side = "right", pad = "1") #add a 1 to the right side after the 5th character

#Locale Sensitive
x<-"I love R!"
str_to_upper(x) #make everything capital!
str_to_lower(x) #make everything lower case!
str_to_title(x) #capitalize the first letter of each word

#Pattern Matching
data<-c("AAA", "TATA", "CTAG", "GCTT")
str_view(data, pattern = "A") #find all the strings with an A

str_detect(data, pattern = "A") #detect a specific pattern
str_detect(data, pattern = "AT") #detect a specific pattern

str_locate(data, pattern = "AT") #locate a specific pattern

# Intro to regex: Regular Expressions ------------------------------------------
# Metacharacters
# The metacharacters: . \ | ( ) [ { $ * + ?
# Use //. to escape the true meaning of these metacharacters!
vals<-c("a.b", "b.c","c.d") #we need to use //. to escape the meaning of "."
str_replace(vals, "\\.", " ") #replace the "." with a " " 

vals<-c("a.b.c", "b.c.d","c.d.e")
#string, pattern, replace
str_replace(vals, "\\.", " ") #this will only find the first . from each string
str_replace_all(vals, "\\.", " ") #this will find the all . from each string within the vector

#Sequences
#as the name suggests refers to the sequences of characters which can match. We have shorthand versions (or anchors) for commonly used sequences in R
val2<-c("test 123", "test 456", "test")
str_subset(val2, "\\d") #subset the vector to only keep strings with digits (removes "test")

#Character Class
#character set is a list of characters enclosed by square brackets
str_count(val2, "[aeiou]") #counts the number of lowercase vowels in each string
str_count(val2, "[0-9]") #count any digit

#Quantifiers
#find the phone number ex
strings<-c("550-153-7578",
           "banana",
           "435.114.7586",
           "home: 672-442-6739")
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})" #regex expression to find a real phone number, best way to do it!
#[2-9]= find any digit between 2-9 once (can't start with 1)
#[0-9]{2}= find any 2 digit
#[- .]= find a dash or a dot
#[0-9]{3}= find any digit of 3
#[- .]= find a dash or dot
#[0-9]{4}= find any digit of 4
str_detect(strings, phone) #which strings contain phone numbers?
test<-str_subset(strings, phone) #subset only the strings with phone numbers, gets rid of banana instance
test #look at updated vector

#think pair share
clean_phone <- test %>% 
  str_replace_all("\\.", "-") %>%
  str_replace_all(pattern = "[a-zA-Z]|\\:", replacement = "") %>% #remove all the things we don't want
  str_trim() #trim the white space

# tidytext ---------------------------------------------------------------------
head(austen_books()) #explore it from the top
tail(austen_books()) #explore it from the bottom

#Let's clean it up and add a column for line and chapter
original_books <- austen_books() %>% #get all of Jane Austen's books
  group_by(book) %>%
  mutate(line = row_number(), #find every line
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", #count the chapters (starts with the word chapter followed by a digit or roman numeral)
                                                 ignore_case = TRUE)))) %>% #ignore lower or uppercase
  ungroup() #ungroup it so we have a dataframe again

head(original_books) #don't try to view the entire thing... its >73000 lines...


tidy_books <- original_books %>% #Because we are interest in text mining, we will want to clean this so that there is only one word per row
  unnest_tokens(output = word, input = text) # add a column named word, with the input as the text column
head(tidy_books) # there are now >725,000 rows. Don't view the entire thing!

head(get_stopwords()) #see an example of all the stopwords

cleaned_books <- tidy_books %>%
  anti_join(get_stopwords()) # dataframe without the stopwords

head(cleaned_books)

cleaned_books %>%
  count(word, sort = TRUE) #count the most common words across all her books

# Sentiment analysis
sent_word_counts <- tidy_books %>%
  inner_join(get_sentiments()) %>% # only keep pos or negative words
  count(word, sentiment, sort = TRUE) # count them
head(sent_word_counts)[1:3,]

# Plot!
sent_word_counts %>%
  filter(n > 150) %>% # take only if there are over 150 instances of it
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>% # add a column where if the word is negative make the count negative
  mutate(word = reorder(word, n)) %>% # sort it so it goes from largest to smallest
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")

#Make a wordcloud!
words<-cleaned_books %>%
  count(word) %>% # count all the words
  arrange(desc(n))%>% # sort the words
  slice(1:100) #take the top 100
wordcloud2(words, shape = 'triangle', size=0.3) # make a wordcloud out of the top 100 words
#takes any word, any shape, and creates your wordcloud into your desired size