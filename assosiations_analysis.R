# Exploring associations between airline, sentiment and reasons for negative comments
# Tools: Association plots + Multiple Correspondence Analysis

library(readr)
library(ca)
library(vcd)

# load tweets
airline <- read_csv("./data/Tweets.csv")

# Give shorter variable names 
colnames(airline)[c(2,4,6)] <- c("s","r","a")

# Replace missing reason with "no comment" (which actually corresponds to positive sentiment)
airline[is.na(airline$r), 4] <- "No comment"

### Association Plots ###

# Sentiment vs Airline
assoc(structable(airline$s ~ airline$a),shade=TRUE)

# Reason vs Airline (label abbreviation is needed here)
assoc(structable(airline$r ~ airline$a),shade=TRUE)

#Delta, Southwest and Virgin American receive more positive comments compared than the rest.

#Multiple Correspondence Analysis

# 1st and Second Principal Axes (Airlines are opposed according to Positive vs Neutral/Negative sentiment)
plot(mjca(airline[,c(2,4,6)],lambda="adjusted"),dim=c(1,2))

# 2nd and 3rd Principal Axes (Airlines are opposed according to Reasons for negative sentiment)
plot(mjca(airline[,c(2,4,6)],lambda="adjusted"),dim=c(2,3))

#Interpreting Results:

#Delta, Southwest and Virgin American receive more positive comments than the rest.
## Negative Reasons associated most with each of the three remaining airlines:
### US Airway: Customer Service, Late Flight
### United: Bad flight, Can't tell, Late flight, Lost luggage
### American: Cancelled flight, Customer survice issue, Flight booking problem


