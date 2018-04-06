library(tidytext)
library(tidyverse)
library(glue)
library(stringr)
library(ggplot2)

#get txt file
LBJ <- data_frame(name="LBJ", text = read_lines("LBJ.txt"))
Ford <- data_frame(name="Ford", text = read_lines("Ford.txt"))

#tokenize data
tokenized1 <- LBJ %>% unnest_tokens(word, text)
tokenized2 <- Ford %>% unnest_tokens(word, text)
tidy
  
nrc_anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")
nrc_anticipation

nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")
nrc_anger

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
nrc_joy

joy <- tokenized1 %>%
  filter(name == "LBJ") %>% inner_join(nrc_joy) %>% count(word, sort = TRUE)
joy

anticipation <- tokenized1 %>%
  filter(name == "LBJ") %>% inner_join(nrc_anticipation) %>% count(word, sort = TRUE)
anticipation
  
anger <- tokenized1 %>%
  filter(name == "LBJ") %>% inner_join(nrc_anger) %>% count(word, sort = TRUE)

# get the sentiment from the LBJ: 
sentiment <- tokenized1 %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) 
sentiment
# get the sentiment from the Ford: 
sentiment2 <- tokenized2 %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds
sentiment2

# sentiments for every word in file
tidy_pres <- tokenized1 %>%
  inner_join(get_sentiments("nrc"))
tidy_pres

tidy_pres2 <- tokenized2 %>%
  inner_join(get_sentiments("nrc"))
tidy_pres2


#barplot for LBJ
nrc_plot <- tidy_pres %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%

  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 300)) + #Hard code the axis limit
  ggtitle("LBJ Sentiment") +
  coord_flip()
nrc_plot

#barplot for Ford
nrc_plot2 <- tidy_pres2 %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment2 = reorder(sentiment, word_count)) %>%
  
  ggplot(aes(sentiment2, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 300)) + #Hard code the axis limit
  ggtitle("Ford Sentiment") +
  coord_flip()
nrc_plot2



