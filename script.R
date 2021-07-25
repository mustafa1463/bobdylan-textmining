#Importing the libraries

library(pdftools)
library(dplyr)
library(tidytext)
library(readtext)
library(tm)
library(ggplot2)
library(tidyr)
library(reshape2)
library(wordcloud2)
data(stop_words)

#Inspect the book 

book <- pdftools::pdf_text("dylanlyrics.pdf")
dylan_words <- tibble(line = 1:length(book), text = book)

lyrics <- dylan_words %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

lyrics

#Further delete the words like "he's", "i'm", "you're":

lyrics <- lyrics[-c(1,2, 3, 4, 5, 7, 9, 12, 13, 14, 16, 24), ] 
lyrics

#Graph the words

lyrics_graph <-  lyrics %>% 
  filter(n > 110) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Frequency", y = "Word", title = "Frequently Used Words in Songs by Bob Dylan") +
  theme(panel.background = element_rect(color = "black", fill = "black"),
        plot.background = element_rect(fill = 'black', colour = 'black'),
        plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0), colour = "white"), 
        axis.title.y = element_text(margin = margin(r = 15), size = 20, colour = "white"),
        axis.title.x = element_text(margin = margin(t = 15), size = 20, colour = "white"),
        plot.caption = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = "white", size = 12, face="bold"),
        axis.text.y = element_text(colour = "white", size = 12, face="bold" ),
        axis.line = element_line(color="white", size = 1.5),
        axis.ticks = element_blank())

lyrics_graph


#Sentiment Analysis

lyrics_sentiment <- lyrics %>%
  inner_join(get_sentiments("afinn")) %>% 
  inner_join(get_sentiments("bing"))

lyrics_sentiment

#Note that some words who are not sentimental are removed here automatically. These words are words like "eyes", "time", etc.  
#Make another column called the sent, which is equal to n * value

sum(lyrics_sentiment$value)
lyrics_sentiment$sent <- lyrics_sentiment$n * lyrics_sentiment$value
sum(lyrics_sentiment$sent)

#Bob Dylan songs sound very pessimistic! 

lyrics_sentiment_graph <- lyrics_sentiment %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Frequency", y = "Word", title = "A Sentiment Analysis of Bob Dylan Songs") + 
  theme(panel.background = element_rect(color = "black", fill = "black"),
        plot.background = element_rect(fill = 'black', colour = 'black'),
        plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0), colour = "white"), 
        axis.title.y = element_text(margin = margin(r = 15), size = 20, colour = "white"),
        axis.title.x = element_text(margin = margin(t = 15), size = 20, colour = "white"),
        plot.caption = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = "white", size = 12, face="bold"),
        axis.text.y = element_text(colour = "white", size = 12, face="bold" ),
        axis.line = element_line(color="white", size = 1.5),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.text.y = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color = "black", fill = "black"),
        axis.ticks = element_blank()) +
        scale_fill_manual(values= c("#f0009c","#f0009c"))

lyrics_sentiment_graph

#Another approach (By the weight of the occurences times their negativity and positivity values)

lyrics_sentiment <- lyrics_sentiment[order(lyrics_sentiment$sent),]
negative_outliers <- lyrics_sentiment[1:10,]
lyrics_sentiment <- lyrics_sentiment[order(-lyrics_sentiment$sent),]
positive_outliers <- lyrics_sentiment[1:10,]

outliers <- rbind(negative_outliers, positive_outliers)
outliers$sent <- abs(outliers$sent)

outliers_graph <- outliers %>% 
  group_by(sentiment) %>% 
  mutate(word = reorder(word, sent)) %>% 
  ggplot(aes(sent, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Weighted Frequency",
       y = "Words") + 
  theme(panel.background = element_rect(color = "black", fill = "black"),
        plot.background = element_rect(fill = 'black', colour = 'black'),
        plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0), colour = "white"), 
        axis.title.y = element_text(margin = margin(r = 15), size = 20, colour = "white"),
        axis.title.x = element_text(margin = margin(t = 15), size = 20, colour = "white"),
        plot.caption = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = "white", size = 12, face="bold"),
        axis.text.y = element_text(colour = "white", size = 12, face="bold" ),
        axis.line = element_line(color="white", size = 1.5),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.text.y = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color = "black", fill = "black"),
        axis.ticks = element_blank()) +
  scale_fill_manual(values= c("#f0009c","#f0009c"))
  
outliers_graph

#Seems like dead's weight was the most, and love was the most positive word!
#make wordclouds from positive and negative words


wordcloud2(lyrics[1:25,], color = "random-light", backgroundColor = "black", fontWeight = "bold", rotateRatio = 0.7,
           shape = "star", ellipticity = 1)


##Creating bigrams

lyrics_biagram <- dylan_words %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(bigram, word1, word2, sep = " ")

lyrics_biagram

bigrams_graph <- lyrics_biagram[1:20,] %>% 
  mutate(bigram = reorder(bigram, n)) %>% 
  ggplot(aes(n, bigram, group = 1)) +
  geom_col(show.legend = FALSE, fill = '#f0009c') +
  labs(x = "Frequency", y = "Bigrams", title = "Biagrams in Bob Dylan's Songs") + 
  theme(panel.background = element_rect(color = "black", fill = "black"),
        plot.background = element_rect(fill = 'black', colour = 'black'),
        plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0), colour = "white"), 
        axis.title.y = element_text(margin = margin(r = 15), size = 20, colour = "white"),
        axis.title.x = element_text(margin = margin(t = 15), size = 20, colour = "white"),
        plot.caption = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = "white", size = 12, face="bold"),
        axis.text.y = element_text(colour = "white", size = 12, face="bold" ),
        axis.line = element_line(color="white", size = 1.5),
        strip.text.x = element_text(size = 12, color = "white", face = "bold"),
        strip.text.y = element_text(size = 12, color = "white", face = "bold"),
        strip.background = element_rect(color = "black", fill = "black"),
        axis.ticks = element_blank())

bigrams_graph  


#The analysis suggests that the love is one of the main themes in Bob Dylan's songs, and that these songs are most likely to be pessimistic, since 
#our analysis also suggests that the words "dead", "die", "trouble", and the bigrams like "stop crying", "trouble trouble", "hard times" occur a lot. 
#"Sarah Jane" is also one of the most used bigrams in Bob Dylan's songs. We know that she was the first wife of Bob Dylan, later to be called "Sarah Dylan".

  


