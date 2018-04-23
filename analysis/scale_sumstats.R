setwd("C:/Users/Kelsey/Desktop/Gender-Tracker/data")

install.packages("ggplot2")
library(ggplot2)

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

# Format Date
#-------------------------------------------

log$date <- as.Date(log$date, format = "%m/%d/%Y")
class(log$date)
head(log)

log$changeto1[is.na(log$changeto1)] <- log$scale[is.na(log$changeto1)]
log$changeto2[is.na(log$changeto2)] <- log$scale[is.na(log$changeto2)]

head(log)

##---------------------------------------------------------------------
## Summary Stats - Year
##---------------------------------------------------------------------

# seperate by Year
log$year = as.numeric(format(log$date, '%Y'))

# Freq table
#-------------------------------------------

#as.matrix(table(log$scale))

days = table(log$gender)
percent = round(prop.table(days)*100,1)

table = cbind(days, percent)
#table = addmargins(table, margin=1)
table

yrtbl <- table(log$gender, log$year)
prop.table(yrtbl, 2)

# Display Stats
#-------------------------------------------

cat("Total Days : ", nrow(log), "\n")
cat("Mean Gender: ", mean(log$scale), "\n")

cat("Mean Gender by Year: ", "\n")
aggregate(log$scale, list(log$year), mean)

table

# Histogram
#-------------------------------------------

#hist(log$scale, breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5))

b <- ggplot(data=log, aes(factor(scale)))
b <- b + geom_bar(data=log, aes(factor(scale), fill = factor(gender)), colour="purple4")
b <- b + geom_text(stat='count', aes(label=..count..), vjust=-1, colour="purple4")
b <- b + scale_fill_manual(values = c("lightpink", "lightblue", "lightyellow1")) 
b <- b + theme(legend.position = "none") + scale_y_continuous(expand = c(0,0),  limits = c(0,150))
b <- b + facet_wrap( ~ year, ncol=1)
b

##---------------------------------------------------------------------
## Summary Stats - Pre/post Aha!
##---------------------------------------------------------------------

# seperate by Aha!
log$aha <- 1
log[1:535,]$aha <- 0

# Freq table
#-------------------------------------------

# Proportion spilt
yrtbl2 <- table(log$gender, log$aha)
prop.table(yrtbl2, 2)

#Avg Gender
aggregate(log$scale, list(log$aha), mean)

# Histogram
#-------------------------------------------

#hist(log$scale, breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5))

b <- ggplot(data=log, aes(factor(scale)))
b <- b + geom_bar(data=log, aes(factor(scale), fill = factor(gender)), colour="purple4")
b <- b + geom_text(stat='count', aes(label=..count..), vjust=-1, colour="purple4")
b <- b + scale_fill_manual(values = c("lightpink", "lightblue", "lightyellow1")) 
b <- b + theme(legend.position = "none") + scale_y_continuous(expand = c(0,0),  limits = c(0,230))
b <- b + facet_wrap( ~ aha, ncol=1)
b

# Frequency of switches
#-------------------------------------------

# num of switches 
# avg amt of days in category before switch

# Record previous days gender
n <- nrow(log)
if (n > 1) for(i in 2:n) log$prev_gender[i] <- log$gender[i-1]
log$prev_gender[1] <- "Feminine"

# Indicator for if there was a cata switch
log$switch <- ifelse(log$prev_gender != log$gender, 1, 0)
  
# Mean number of switch days by aha
aggregate(log$switch, list(log$aha), mean)

# Running count of days in category
n <- nrow(log)
number <- 0
for(i in 1:n) {
  if (log$switch[i] == 1) {
    number <- 1
    log$catacnt[i] <- number
  } 
  else {
    number <- number + 1
    log$catacnt[i] <- number
  }
}

# Flag last row before switch
n <- nrow(log)
log$keep <- 0
for(i in 1:n-1) {
  if (log$switch[i+1] == 1) {
    log$keep[i] <- 1
  }
}

# Subset to last rows
lastobs <- log[ which(log$keep==1),]

# avg amt of days in category before switch, by aha
aggregate(log$catacnt, list(log$aha), mean)
