setwd("C:/Users/Kelsey/Desktop/Gender-Tracker/data")

install.packages("tidytext")
install.packages("stringr")
install.packages("tidyverse")
install.packages("libridate")
library(tidytext)
library(tidyverse)
library(stringr)
library(libridate)

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
## Sentiment
##---------------------------------------------------------------------


