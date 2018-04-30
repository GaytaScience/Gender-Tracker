setwd("C:/Users/Kelsey/Desktop/Gender-Tracker/data")


install.packages("tidytext")
install.packages("stringr")
install.packages("tidyverse")
library(tidytext)
library(tidyverse)
library(stringr)

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
## Text Analysis
##---------------------------------------------------------------------
                   
# Customize Stopword List
my_stop_words <- tibble( word = c("im"))
not_stopwords <- tibble( word = c("high"))

all_stop_words <- stop_words %>%
  bind_rows(my_stop_words) %>%
  filter(!word %in% not_stopwords)

# Clean up Text (lightly)
log2 <- log %>%
  mutate(date2 = as.Date(date, "%m/%d/%Y")) %>%
  mutate(text = str_to_lower(thoughts, locale = "en")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% all_stop_words$word,str_detect(word, "[a-z]"))

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
  scale_fill_discrete(name = "", labels = c("Masculine", "Feminine"))


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
#Prob just show Overall or Pre and show post changes another way
