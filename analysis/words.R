setwd("C:/Users/Kelsey/Desktop/Gender-Tracker/data")

install.packages("tidytext")
install.packages("stringr")
install.packages("tidyverse")
install.packages("lubridate")
library(tidytext)
library(tidyverse)
library(stringr)
library(lubridate)

# Read in Data
#-------------------------------------------
log <- read.csv("log.csv")
log <- log[c(0:1160),c(0:6)]
head(log)
tail(log)

# Create Categories
#-------------------------------------------

#can I get a mean of scale, changeto1, and changeto2?

log$gender <- ifelse((log$scale >= -5 & log$scale <= -2), "Masculine", 
                     (ifelse((log$scale > -2 & log$scale < 2), "Neutral", "Feminine")))

log$color <- ifelse((log$scale >= -5 & log$scale <= -2), "lightblue", 
                     (ifelse((log$scale > -2 & log$scale < 2), "lightyellow1", "lightpink")))

log[,c(2,7,8)]

##---------------------------------------------------------------------
## Basic Word Counts
##---------------------------------------------------------------------

# Customize Stopword List
my_stop_words <- tibble( word = c("im"))
not_stopwords <- c("high", "good", "ok", "okay", "want")

all_stop_words <- stop_words %>%
  bind_rows(my_stop_words) %>%
  filter(!word %in% not_stopwords)

# Clean up Text (lightly)
log2 <- log %>%
  mutate(date2 = as.Date(date, "%m/%d/%Y")) %>%
  mutate(text = str_to_lower(thoughts, locale = "en")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% all_stop_words$word,str_detect(word, "[a-z]"))

# Word Counts by Gender
word_counts <- log2 %>%
  count(word, gender) 

# Pull in Exclustion Terms
excl.txt <- read.table("excl.txt", header=F)
exclwords <- excl.txt[['V1']]
class(exclwords)
exclwords

# Word Cloud for Quick Understanding?
# From https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
library(wordcloud)
library(reshape2)

par(bg=NA)
cloud <- log2 %>%
  count(word, gender, sort = TRUE) %>%
  filter(!word %in% exclwords) %>%
  filter(!gender == "Neutral") %>%
  acast(word ~ gender, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#f9687d", "#49abcc"),
                   max.words = 100)
cloud

#c("#FFB6C1", "#ADD8E6") - dash colors
#c("#F8766D", "#00BFC4") - darker
#c("lightpink", "lightblue") - matches
#c("#f9b87d", "4ca3bf")

dev.copy(png,'cloud.png')
dev.off()

##---------------------------------------------------------------------
## Significant Words
##---------------------------------------------------------------------
# From https://www.tidytextmining.com/twitter.html#comparing-word-usage

# Overall Log Odds Ratio of Words (Masc & Fem)
word_ratios <- log2 %>%
  count(word, gender) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(gender, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(Masculine / Feminine)) %>%
  arrange(desc(logratio))

# Overall Top Plot
word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (Masculine/Feminine)") +
  scale_fill_manual("", values=c("#ADD8E6", "#FFB6C1"))
  
#scale_fill_discrete(name = "", labels = c("Masculine", "Feminine"))


# Pre Post aha!
#-------------------------------------------

log2_preaha <- log2 %>%
  filter(date2 < as.Date("2016-04-15"))

log2_postaha <- log2 %>%
  filter(date2 >= as.Date("2016-04-15"))

# Pre Log Odds Ratio of Words (Masc & Fem)
word_ratios_pre <- log2_preaha %>%
  count(word, gender) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(gender, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(Masculine / Feminine)) %>%
  arrange(desc(logratio))

# Pre Top Plot
word_ratios_pre %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (Masculine/Feminine)") +
  scale_fill_discrete(name = "", labels = c("Masculine", "Feminine"))

# Post Log Odds Ratio of Words (Masc & Fem)
word_ratios_post <- log2_postaha %>%
  count(word, gender) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(gender, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(Masculine / Feminine)) %>%
  arrange(desc(logratio))

# Post Top Plot
word_ratios_post %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (Masculine/Feminine)") +
  scale_fill_discrete(name = "", labels = c("Masculine", "Feminine"))

# Merge to see Difference by Word?
ratios_prepost <- full_join(word_ratios_pre, word_ratios_post, by = c("word")) %>%
  mutate(diff = logratio.x - logratio.y)

# Not sure if enough data to do the pre/post justice. 
# Maybe with select words
# Prob just show Overall or Pre and show post changes another way

##---------------------------------------------------------------------
## Significant Words Over Time
##---------------------------------------------------------------------

words_by_time_sep <- log2 %>%
  mutate(time_floor = floor_date(date2, unit = "3 month")) %>%
  count(time_floor, gender, word) %>%
  ungroup() %>%
  group_by(gender, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n)

words_by_time_sep %>%
  filter(word %in% c( "frustrated", "good")) %>%
  filter(gender == "Masculine") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

words_by_time_sep %>%
  filter(word %in% c("confident", "comfortable", "happy")) %>%
  filter(gender == "Feminine") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

#Overall

words_by_time <- log2 %>%
  mutate(time_floor = floor_date(date2, unit = "6 month")) %>%
  count(time_floor, word) %>%
  ungroup() %>%
  group_by(time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n)

words_by_time

words_by_time %>%
  filter(word %in% c("control", "acceptance", "accept")) %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

# Some interesting words, but i suspect most changes are due to just shifts in my language...
# Check if need to trim date ends

##---------------------------------------------------------------------
## Bigrams
##---------------------------------------------------------------------

# Clean up Text (lightly)
logbi <- log %>%
  mutate(date2 = as.Date(date, "%m/%d/%Y")) %>%
  mutate(text = str_to_lower(thoughts, locale = "en")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 

# Bigram Counts by Gender
bi_counts <- logbi %>%
  count(bigram, gender) 

# Remove bigrams where one is a stopword
bigrams_separated <- bi_counts %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% all_stop_words$word) %>%
  filter(!word2 %in% all_stop_words$word)

# Not super interesting, words clouds with ex's prob tell story best
