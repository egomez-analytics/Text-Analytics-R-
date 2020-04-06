library(textdata)
library(tidytext)
library(tidyverse)
library(tidyr)
library(dplyr) # piping
library(janeaustenr)
library(gutenbergr)
library(RColorBrewer) # wordclouds
library(wordcloud) # wordclouds
library(textdata)
library(reshape2)
library(textreadr)
library(scales)
library(stringr)
library(plotly) # plots
library(igraph) # bigrams
library(ggraph)
library(tm)
#########################################
# question 1: Top 5 word frequency counts
#########################################
my_txt <- c("Text analytics is the most amazing class ever", 
            "I wish that all the text was tokenized ahead of time.",
            "text analytics and amazing NLP is what is going to give me a job")
mydf1 <- tibble(line=1:3, text=my_txt)
print(mydf1)
frequencies_tokens <- mydf1 %>%
  unnest_tokens(word, text) %>%
  count(word, sort=TRUE)
print(frequencies_tokens)

#########################################
# question 2: number of tokens and correlation between 2&3 and 1&3
#########################################
library(gutenbergr)
data("stop_words")

Group1 <- gutenberg_download(c(35,36))
Group2 <- gutenberg_download(c(37,38))
Group3 <- gutenberg_download(c(39,41))

#Group1 tokenization
tidy_group1 <- Group1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_group1)

tidy_group1 %>% #counting frequencies for tokens
  count(word, sort=TRUE)

#Group2 tokenization
tidy_group2 <- Group2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_group2)

tidy_group2 %>% #counting frequencies for tokens
  count(word, sort=TRUE)

#Group3 tokenization
tidy_group3 <- Group3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_group3)

tidy_group3 %>% #counting frequencies for tokens
  count(word, sort=TRUE)

#bind groups
frequency <- bind_rows(mutate(tidy_group1, author = "group1" ),
                       mutate(tidy_group2, author= "group2"),
                       mutate(tidy_group3, author="group3")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `group1`, `group2`)
frequency

#correlations
cor.test(data=frequency[frequency$author == "group1",],
         ~proportion + `group3`)
cor.test(data=frequency[frequency$author == "group2",],
         ~proportion + `group3`)


#########################################
# question 3: tf_idf for given words in the Emma book after removing stop words
#########################################
library(janeaustenr)

jane_book <- austen_books() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(book, word, sort=TRUE) %>%
  ungroup()

jane_words <- jane_book %>%
  bind_tf_idf(word, book, n)

emma_words <- jane_words %>%
  filter(book == 'Emma') %>%
  arrange(desc(tf_idf))
emma_words

#########################################
# question 4: # of terms in dtm and frequency counts for given words in Sense and Sensibility
#########################################
original_books <- austen_books()
View(original_books[1:40,])
##############################

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(lienumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE))))%>%
  ungroup()

# cast to dtm
austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)
austen_dtm

# frequency counts
freq_count <- austen_books() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(book, word, sort=TRUE) %>%
  ungroup()

# filter for Sense & Sensibility
ss_count <- freq_count %>%
  filter(book == 'Sense & Sensibility') %>%
  arrange(desc(n)) # open from global env. and sort by word
