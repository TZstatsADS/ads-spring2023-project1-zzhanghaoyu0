#load the necessary packages
library(dplyr)
library(qdap)
library(magrittr)
library(tm)
library(tidytext)
library(textdata)
library(tidyr)
library(ggplot2)
library(tidyverse)



philosophy_data <- read.csv("D:/source/Stats 5243/philosophy_data.csv")

head(philosophy_data)

colnames(philosophy_data)

#examine how many different schools does this data set contain
levels(factor(philosophy_data$school))

#examine how many different titles does this dataset contain
levels(factor(philosophy_data$title))

#which school carrys the most positive and negative energy, what are they discussing about,does their emotions vary over time

#focus on all schools, select school and tokenized text for more detailed analysis
#the sentence is kept for reading purpose only
all_school<- philosophy_data %>% 
  select(school,sentence_lowered,tokenized_txt) %>% 
  group_by(school)

#the schools included
levels(factor(all_school$school))

#keep only the school and tokenized text to tidy up the data
all_school_tidy <- all_school %>% select(school,tokenized_txt)
all_school_tidy$text <- all_school_tidy$tokenized_txt %>% 
  removePunctuation() 
all_school_tidy <- all_school_tidy %>% select(school,text)

head(all_school_tidy)

#create one word per row data frame
all_school_tidy <- all_school_tidy %>% unnest_tokens(tokenized,text)

head(all_school_tidy)

#remove stopwords
all_school_tidy <- all_school_tidy %>% anti_join(stop_words,by=c("tokenized"='word'))

head(all_school_tidy)


# stacked bar plot to compare who has more positive sentiment
#use bing because categorized to positive and negative only
all_school_pos <- all_school_tidy %>%
  inner_join(get_sentiments("bing"), by = c("tokenized" = "word")) %>% 
  filter(grepl("positive|negative", sentiment)) %>% 
  count(school, sentiment) %>%
  group_by(school) %>% 
  mutate(percent_positive = 100 * n / sum(n))

# Plot
ggplot(all_school_pos, aes(school, percent_positive, fill = sentiment)) +  
  # Add a col layer
  geom_col()+
  coord_flip()
#from the plot, continental has the most negative energy,capitalism has the most positive


#count the words
all_wordCount <- all_school_tidy %>% 
  count(tokenized,school) %>% 
  group_by(school) %>% 
  top_n(10,n) %>%
  ungroup() %>% 
  mutate(word2 = fct_reorder(tokenized,n))


#plot the top words
ggplot(all_wordCount,aes(x=word2,y=n,fill=school))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~school,scales = "free")+
  coord_flip()+
  ggtitle("Freqently mentioned word counts")


#what is their most frequently mentioned sentimental words
#pair positive/negative with each word
sentiment_all_tidy <- all_school_tidy %>% inner_join(get_sentiments("bing"),by=c("tokenized"="word"))

#count sentimental words
sentiment_all_wordCount <- sentiment_all_tidy %>% 
  count(tokenized,school,sentiment) %>% 
  group_by(school,sentiment) %>% 
  top_n(5,n) %>%
  ungroup() %>% 
  mutate(word2 = fct_reorder(tokenized,n))

#plot
ggplot(sentiment_all_wordCount,aes(x=word2,y=n,fill=sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~school,scales = "free")+
  coord_flip()+
  ggtitle("Freqently mentioned sentimental word counts")

#re-select data to include time
time_count <- philosophy_data %>% 
  select(school,original_publication_date) %>% 
  group_by(school) %>% 
  summarise(num_time = length(levels(factor(original_publication_date)))) %>% 
  top_n(5,num_time)

time_count
#we will focus on the top 3 since they have more years available

#select top 3 schools
time_tidy <- philosophy_data %>% 
  select(school,original_publication_date,tokenized_txt) %>% 
  filter(school %in% c("analytic","german_idealism","continental"))

time_tidy$text <- time_tidy$tokenized_txt %>% 
  removePunctuation() 
time_tidy <- time_tidy %>% select(school,text,original_publication_date)

#create one word one row tokenized data
time_tidy <- time_tidy %>% unnest_tokens(tokenized,text)

head(time_tidy)

#remove stop words
time_tidy <- time_tidy %>% anti_join(stop_words,by=c("tokenized"='word'))

#calculate polarity and make plot against time
time_polarity <- time_tidy %>%
  inner_join(get_sentiments("bing"), by = c("tokenized" = "word")) %>%
  count(sentiment, school,original_publication_date) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    polarity = positive - negative
  )

# Plot polarity vs. line_number
ggplot(time_polarity, aes(x=original_publication_date,y=polarity,fill=school)) + 
  geom_smooth(show.legend = FALSE,se=FALSE) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~school,scales = "free_x")+
  ggtitle("Chronological Polarity") 

#continental philosophy has a sharp positive polarity around 1965
#further observe the sentimental words to make sure they are indeed positive 
continental <- time_tidy %>% 
  filter(school=="continental")

levels(factor(continental$original_publication_date))

#the positive section happened in 1963 to 1967
continental_63to67 <- continental %>% 
  filter(original_publication_date %in% c(1963,1966,1967))

#count the sentimental words used
continental_63to67_sentiment <- continental_63to67 %>% inner_join(get_sentiments("bing"),by=c("tokenized"="word"))

continental_63to67_sentiment <- continental_63to67_sentiment %>% 
  count(tokenized,school,sentiment) %>% 
  group_by(school,sentiment) %>% 
  top_n(5,n) %>%
  ungroup() %>% 
  mutate(word2 = fct_reorder(tokenized,n))

#plot
ggplot(continental_63to67_sentiment,aes(x=word2,y=n,fill=sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~school,scales = "free_y")+
  coord_flip()+
  ggtitle("Freqently mentioned sentimental word counts")

#the word pure are ambiguous like pure evil 
#so look for sentences contain these two words
continental_sentences <- philosophy_data %>% 
  filter(school == "continental",
         original_publication_date %in% c(1963,1966,1967)) %>% 
  select(sentence_lowered)

head(continental_sentences)

#look at sentences that contain pure
head(continental_sentences[grep("pure",continental_sentences$sentence_lowered),])


#try add pure as a stop word and re plot
custom_stop_words <- tribble(
  ~word,~lexicon,
  "pure","CUSTOM"
)
stop_words_new <- stop_words %>% bind_rows(custom_stop_words)
time_tidy_new <- time_tidy %>% anti_join(stop_words_new,by=c("tokenized"='word'))

#calculate polarity after 'pure' is removed
time_polarity_new <- time_tidy_new %>%
  inner_join(get_sentiments("bing"), by = c("tokenized" = "word")) %>%
  count(sentiment, school,original_publication_date) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    polarity = positive - negative
  )

# Plot polarity vs. line_number
ggplot(time_polarity_new, aes(x=original_publication_date,y=polarity,fill=school)) + 
  geom_smooth(show.legend = FALSE,se=FALSE) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~school,scales = "free_x")+
  ggtitle("Chronological Polarity") 
#pure alone does not contribute much