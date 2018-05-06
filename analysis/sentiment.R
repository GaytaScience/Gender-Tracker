setwd("C:/Users/Kelsey/Desktop/Gender-Tracker/data")

install.packages("tidytext")
install.packages("stringr")
install.packages("tidyverse")
install.packages("libridate")
install.packages("ggjoy")
install.packages("radarchart")
library(tidytext)
library(tidyverse)
library(stringr)
library(libridate)
library(ggjoy)
library(radarchart)

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
## Length over Time
##---------------------------------------------------------------------

# Should I remove stopwords first? I guess it shouldnt affect anything?
# This will remove days I didnt write anything, which should be good for plotting by day

# Clean up Text (lightly)
log2 <- log %>%
  mutate(date2 = as.Date(date, "%m/%d/%Y")) %>%
  mutate(text = str_to_lower(thoughts, locale = "en")) %>%
  unnest_tokens(word, text)

# Word Counts by Gender
log3 <- log2 %>%
  group_by(date2) %>%
  count(date, scale, thoughts, gender) 

# Plot Length over time
ggplot(log3, aes(date2, n)) + geom_line()

# By day a little busy
# Full merge to get dates with no thoughts
logfull <- merge(log, log3, by = c("date", "thoughts", "gender", "scale"), sort = TRUE, all = TRUE)

# Assign no thoughts days to 0 words
logfull$n[is.na(logfull$n)] <- 0

# Replace date
logfuller <- logfull %>%
  mutate(date2 = as.Date(date, "%m/%d/%Y"))

# Group by week - Average words/day
avgwdsperwk <- logfull %>%
  mutate(date2 = as.Date(date, "%m/%d/%Y")) %>%
  mutate(time_floor = floor_date(date2, unit = "1 week")) %>%
  group_by(time_floor) %>%
  mutate(totwords = sum(n)) %>%
  count(time_floor, totwords) %>%
  mutate(avg = totwords / nn)

# Plot avg words/day by week
ggplot(avgwdsperwk, aes(time_floor, avg)) + geom_line()

##---------------------------------------------------------------------
## Length by Category
##---------------------------------------------------------------------

# Avg length by Gender
avgwdsbygender <- logfull %>%
  mutate(date2 = as.Date(date, "%m/%d/%Y")) %>%
  group_by(gender) %>%
  mutate(totwords = sum(n)) %>%
  count(gender, totwords) %>%
  mutate(avg = totwords / nn)

ggplot(avgwdsbygender, aes(gender, avg)) + geom_bar(stat = "identity")

# Avg length by Scale (Distribution)
avgwdsbyscale <- logfull %>%
  mutate(date2 = as.Date(date, "%m/%d/%Y")) %>%
  group_by(scale) %>%
  mutate(totwords = sum(n)) %>%
  count(scale, totwords) %>%
  mutate(avg = totwords / nn)

ggplot(avgwdsbyscale, aes(scale, avg)) + geom_bar(stat = "identity") #interesting by cut -5 and 5 (too little data)

##---------------------------------------------------------------------
## Lexical Diversity over Time
##---------------------------------------------------------------------

# I want to look at this, but doubtful it will be interesting enough to show, so will hold off

##---------------------------------------------------------------------
## Sentiment - AFINN
##---------------------------------------------------------------------

afinn <- get_sentiments("afinn") # -5 to 5 neg to pos

# Take out some words I know are skewing things
dont_incl <- c("like", "pretty", "kind", "super")

afinn_me <- afinn %>%
  filter(!word %in% dont_incl)

# Scored words dataset
sent_words <- log2 %>%
  left_join(afinn_me, by = "word")

# Grouping by mean for date 
mean_sentiment <- sent_words %>%
  group_by(date2, thoughts, gender, scale) %>%
  summarize(mean_sentiment = mean(score, na.rm = TRUE))

# Merge with word counts by day
sentall <- merge(mean_sentiment, logfuller, by = c("date2", "thoughts", "gender", "scale"), sort = TRUE)

# Plot length and sentiment
ggplot(sentall, aes(n, mean_sentiment, color=gender)) + geom_point() + scale_x_continuous(limits = c(0, 250))

# Plot gender and sentiment
ggplot(sentall, aes(scale, mean_sentiment, color=gender)) + geom_point()

# Plot sentiment over time *
colors = c("Feminine" = "#F8766D", "Neutral" = "wheat", "Masculine" = "#00BFC4")
ggplot(sentall, aes(date2, mean_sentiment, fill = gender)) + geom_bar(stat = "identity") +
scale_fill_manual(values = colors)
#c("#F8766D", "#00BFC4") darker

# Maybe go back and address negating, amplifyers, intensifiers, etc.... Might be skewing things

# Most common words pos and neg

afinn_word_counts <- sent_words %>%
  count(word, score, sort = TRUE) %>%
  filter(!is.na(score)) %>%
  ungroup()

countssent <- afinn_word_counts %>%
  group_by(score) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))
# The swearing kills me cause it is all ranked neg?

# Dists of sentiment overall and by gender *
ggplot(sentall, aes(x=mean_sentiment)) + geom_histogram(binwidth=.5)

ggplot(sentall, aes(x=mean_sentiment, colour = gender)) + geom_histogram(binwidth=.5)

#find mean
cdat <- sentall %>%
  group_by(gender) %>%
  summarize(mean_sentiment.mean = mean(mean_sentiment, na.rm = TRUE))
cdat

ggplot(sentall, aes(x=mean_sentiment)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(gender ~ .) +
  geom_vline(data=cdat, aes(xintercept=mean_sentiment.mean),
             linetype="dashed", size=1, colour="red")

# Dists of sentiment pre/post aha!
sentall$aha <- ifelse(sentall$date2 < as.Date("2016-04-15"), "Pre", "Post")

cdat2 <- sentall %>%
  group_by(aha) %>%
  summarize(mean_sentiment.mean = mean(mean_sentiment, na.rm = TRUE))
cdat2

ggplot(sentall, aes(x=mean_sentiment)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(aha ~ .) +
  geom_vline(data=cdat2, aes(xintercept=mean_sentiment.mean),
             linetype="dashed", size=1, colour="red")

##---------------------------------------------------------------------
## Sentiment - Faceted
##---------------------------------------------------------------------

nrc <- get_sentiments("nrc")

nrc_me <- nrc %>%
  filter(!word %in% dont_incl)

# Scored words dataset
nrc_words <- log2 %>%
  left_join(nrc_me, by = "word") %>%
  mutate(aha = ifelse(date2 < as.Date("2016-04-15"), "Pre", "Post")) %>%
  filter(!is.na(sentiment))

# Plot dimensions over time (just all decr cause overall length did..)
ggplot(nrc_words) +
  geom_joy(aes(
    x = date2,
    y = sentiment, 
    fill = sentiment),
    rel_min_height = 0.01,
    alpha = 0.7,
    scale = 3) +
  theme_joy() +
  labs(title = "Twitter #rstats sentiment analysis",
       x = "Tweet Date",
       y = "Sentiment") + 
  scale_fill_discrete(guide=FALSE)

# Merge with word counts by day
nrcall <- merge(nrc_words, logfuller, by = c("date2", "thoughts", "gender", "scale"), sort = TRUE)

# Collapse
nrcall_norm <- nrcall %>%
  group_by(date2, thoughts, gender, scale, sentiment, aha, n) %>%
  count(sentiment) %>%
  mutate(avg = nn / n)

#Normalized Plot?
ggplot(nrcall_norm, aes(date2, sentiment, height = avg)) + geom_ridgeline()
  
##---------------------------------------------------------------------
## Sentiment - Radar Charts
##---------------------------------------------------------------------

# Data by sentiment and aha!
radar_o <- nrc_words %>% 
  group_by(sentiment,aha) %>%
  count(sentiment) %>%
  spread(aha,n)

radar_m <- nrc_words %>% 
  filter(gender == "Masculine") %>% 
  group_by(sentiment,aha) %>%
  count(sentiment) %>%
  spread(aha,n)

radar_f <- nrc_words %>% 
  filter(gender == "Feminine") %>% 
  group_by(sentiment,aha) %>%
  count(sentiment) %>%
  spread(aha,n)

# JavaScript radar chart default package settings
allaha<- chartJSRadar(radar_m)
allaha

mascaha<- chartJSRadar(radar_m)
mascaha

femaha<- chartJSRadar(radar_m)
femaha
