

###### Exploratory Data Analysis ###### 

data <- read.csv("./data/Tweets.csv", header = T)

dim(data)
str(data)

head(data[,1:5])




###### Proportion of tweets with each sentiment ###### 
table(data$airline_sentiment)
prop.table(table(data$airline_sentiment))


# generate a dataframe for plotting in ggplot2
smallData <- as.data.frame(prop.table(table(data$airline_sentiment)))
colnames(smallData) <- c('Sentiment', 'Frequency')
smallData


library(ggplot2)
library(gridExtra)

# create blank theme for pie chart, otherwise it looks awful in my opinion
blank_theme <- theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = 'bold') 
  )

gbar <- ggplot(smallData, aes(x = Sentiment, y = Frequency, fill = Sentiment))
gpie <- ggplot(smallData, aes(x = "", y = Frequency, fill = Sentiment))

# overall sentiment on bar chart
plot1 <- gbar + 
  geom_bar(stat = 'identity') + 
  ggtitle("Overall Sentiment") + 
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1),
        axis.title.y = element_text(vjust = 2), 
        axis.title.x = element_text(vjust = -1))

# overall sentiment on pie chart
plot2 <- gpie + 
  geom_bar(stat = 'identity') + 
  coord_polar("y", start = 0) + 
  blank_theme +
  theme(axis.title.x = element_blank()) + 
  geom_text(aes(y = Frequency/3 + c(0, cumsum(Frequency)[-length(Frequency)]), 
            label = round(Frequency, 2)), size = 4) + 
  ggtitle('Overall Sentiment')

grid.arrange(plot1, plot2, ncol = 1, nrow = 2)











###### Proportion of tweets per airline ###### 
prop.table(table(data$airline))


# dataframe for plotting in ggplot
smallData <- as.data.frame(prop.table(table(data$airline)))
colnames(smallData) <- c('airline', 'Frequency')
smallData


gbar <- ggplot(smallData, aes(x = airline, y = Frequency, fill = airline))
gbar + 
  geom_bar(stat = 'identity') + 
  scale_fill_brewer() + 
  ggtitle('Percentage of Tweets per Airline') +
  guides(fill = FALSE) + 
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1))










###### Proportion of negative sentiment tweets per airline ###### 

prop.table(table(data$airline_sentiment, data$airline))


# dataframe for ggplot
smallData <- as.data.frame(prop.table(table(data$airline_sentiment, data$airline)))
colnames(smallData) <- c('Sentiment', 'Airline', 'Percentage_Tweets')

gbar <- ggplot(smallData, aes(x = Airline, y = Percentage_Tweets, fill = Sentiment)) + 
  ggtitle('Proportion of Tweets per Airline') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
        axis.title.x = element_text(vjust = -1))

plot1 <- gbar + 
  geom_bar(stat = 'identity')

plot2 <- gbar + 
  geom_bar(stat = 'identity', position = 'fill')

grid.arrange(plot1, plot2, ncol = 1, nrow = 2)







###### Reasons for negative sentiment tweets  ###### 

# dataframe for ggplot
smallData <- as.data.frame(prop.table(table(data$negativereason)))
colnames(smallData) <- c('Reason', 'Frequency')
smallData <- smallData[-1, ] # remove first raw as it has no reason specified
smallData


g <- ggplot(smallData, aes(x = Reason, y = Frequency)) + 
  geom_bar(stat = 'identity', fill = 'pink')
g <- g + 
  ggtitle('Reasons for Negative Sentiment')
g <- g + 
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
        axis.title.x = element_text(vjust = -0.1),
        axis.text.x = element_text(angle = 30, size = 10, vjust = 1))
g






######  Reasons for negative sentiment per airline ###### 
American <- subset(data, airline == 'American')
USAirways <- subset(data, airline == 'US Airways')
Delta <- subset(data, airline == 'Delta')
Southwest <- subset(data, airline = 'Southwest')
Virgin <- subset(data, airline = 'Southwest')
United <- subset(data, airline = 'United')



g1 <- ggplot(as.data.frame(prop.table(table(American$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g1 <- g1 + ggtitle('American: Reasons for bad sentiment')
g1 <- g1 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
                 axis.title.x = element_blank(),
                 axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g2 <- ggplot(as.data.frame(prop.table(table(United$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g2 <- g2 + ggtitle('United: Reasons for bad sentiment')
g2 <- g2 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
                 axis.title.x = element_blank(),
                 axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g3 <- ggplot(as.data.frame(prop.table(table(USAirways$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g3 <- g3 + ggtitle('US Airways: Reasons for bad sentiment')
g3 <- g3 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
                 axis.title.x = element_blank(),
                 axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g4 <- ggplot(as.data.frame(prop.table(table(Delta$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g4 <- g4 + ggtitle('Delta: Reasons for bad sentiment')
g4 <- g4 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
                 axis.title.x = element_blank(),
                 axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g5 <- ggplot(as.data.frame(prop.table(table(Southwest$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g5 <- g5 + ggtitle('Southwest: Reasons for bad sentiment')
g5 <- g5 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
                 axis.title.x = element_blank(),
                 axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g6 <- ggplot(as.data.frame(prop.table(table(Virgin$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g6 <- g6 + ggtitle('Virgin: Reasons for bad sentiment')
g6 <- g6 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), 
                 axis.title.x = element_blank(),
                 axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

grid.arrange(g1, g2, ncol = 1, nrow = 2)
grid.arrange(g3, g4, ncol = 1, nrow = 2)
grid.arrange(g5, g6, ncol = 1, nrow = 2)





######  Exploratory data analysis: columns containing NAs (no data) ###### 

# fill with NA cells in dataframe containing "", " " or the string NA
data <- as.data.frame(apply(data, 2, function(x) gsub("^$|^ $", NA, x)))

# check which columns contain NA and how many
apply(data, 2, function(x) sum(is.na(x)))








###### Re-tweet analysis ###### 
table(data$retweet_count)

as.character(subset(data, retweet_count == 44)$text)
as.character(subset(data, retweet_count == 32)$text)
as.character(subset(data, retweet_count == 31)$text)
as.character(subset(data, retweet_count == 28)$text)





######  Tweet location exploration ###### 

head(unique(data$tweet_location), 50)





######  Tweet timezone study ###### 
timezone <- as.data.frame(prop.table(table(data$user_timezone)))
colnames(timezone) <- c('timezone', 'Frequency')
timezone <- timezone[order(timezone$Frequency, decreasing = TRUE),]
dim(timezone)
head(timezone, 10)






######  Location of tweets: Visualisation on maps ######  

location <- data$tweet_coord
location <- location[complete.cases(location)] # remove NAs
location <- as.data.frame(location)
location$count <-  1 # add a count column filled with 1s
location$location <- as.character(location$location)
#remove duplicate locations and count the times they appeared, write the count in the count column
location <- aggregate(count ~ location, data = location, FUN = sum)
location <- location[-5,] # removes row containing coords [0,0] which are probably wrong
coords <- strsplit(location$location, ',') 

# separate lat and long from location
lat <- NULL
long <- NULL
for (i in 1:length(coords)) {
  lat <- c(lat, substring(coords[[i]][1], 2)) # removes first character which is [
  long <- c(long, coords[[i]][2]) 
}

location$lat <- lat
location$long <- long

# remove ]
location$long <- substr(location$long, 1, nchar(location$long)-1)

location$lat <- as.numeric(location$lat)
location$long <- as.numeric(location$long)

head(location)
dim(location)


require(maps)
world_map <- map_data("world")

g1 <- ggplot()
g1 <- g1 + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group = group), colour="black", fill = 'lightblue') + 
  ggtitle("Location of tweets across the World")
g1 <- g1 + 
  geom_point(data=location, aes(x=long, y=lat, size = count), color="coral1") + 
  scale_size(name="Total Tweets")
g1 <- g1 + 
  ylim(-50, 80)

states <- map_data("state")
g2 <- ggplot()
g2 <- g2 + geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="black", fill = 'lightblue') + 
  ggtitle("Location of tweets across the States")
g2 <- g2 + geom_point(data=location, aes(x=long, y=lat, size = count), color="coral1") + scale_size(name="Total Tweets")
g2 <- g2 + xlim(-125, -65) + ylim(25, 50)
#grid.arrange(g, ncol=1, nrow = 2)

grid.arrange(g1, g2, ncol=1, nrow = 2)



