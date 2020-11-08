# Heuristic Models
# Look at the Seattle weather in the data folder. 
# Come up with a heuristic model to predict if it will rain today. 
# Keep in mind this is a time series, which means that you only know what happened historically (before a given date). 
# One example of a heuristic model is: It will rain tomorrow if it rained more than 1 inch (>1.0 PRCP) today. 
# Describe your heuristic model in the next cell.

# Your model here:

### if it rained yesterday but not today, it will rain tomorrow. ###


# Here is an example of how to build and populate a hurestic model:

url <- 'https://raw.githubusercontent.com/daniel-dc-cd/data_science/master/module_4_ML/data/seattle_weather_1948-2017.csv'
destfile <- "data.csv"
curl::curl_download(url, destfile)
df <- read.csv(destfile)
df[is.na(df)]<-0

# Create an empty dataframe
heuristic_df<-data.frame(matrix(, nrow=nrow(df)-2, ncol=0))
heuristic_df$Yesterday<-0
heuristic_df$Today<-0
heuristic_df$Tomorrow<-0
heuristic_df$Guess<-FALSE
heuristic_df$Rain_tomorrow<-FALSE
heuristic_df$Correct<-FALSE

# View first 10 rows of each dataframe
head(df, 10)
head(heuristic_df, 10)

# Build a loop to add your heuristic model guesses as a column to this dataframe

# Here is an example loop that populates the dataframe created earlier
# with the total percip from yesterday and today
# then the guess is set to true if if it rained yesterday but not today


for(z in seq(1,nrow(df)-2,1)) {
  #start at time 2 in the data frame
  i <- z + 2
  
  #pull values from the dataframe
  yesterday <- df[(i-2),2]
  today <- df[(i-1),2]
  tomorrow <- df[i,2]
  rain_tomorrow <- tomorrow>0
  
  heuristic_df[z,1] <- yesterday
  heuristic_df[z,2] <- today
  heuristic_df[z,3] <- tomorrow
  heuristic_df[z,4] <- FALSE # set guess default to False
  heuristic_df[z,5] <- rain_tomorrow
  
 
  if((today == 0) & (yesterday > 0)){
      heuristic_df[z,4] <- TRUE
   }

  if(heuristic_df[z,4] == heuristic_df[z,5])  heuristic_df[z,6] <- TRUE
  else heuristic_df[z,6] <- FALSE
}


#Evaluate the performance of the Heuristic model

# Confusion matrix
# creates a confusion matrix 
CM <- table(heuristic_df$Correct, heuristic_df$Guess) 
CM

# Evaluating the model using Accuracy as evaluation metric
Acc <- sum(diag(CM)) / sum(CM) 
Acc
cat('Accuracy: ', (Acc*100), sep = " ") 

#the accuracy of your predicitions
sum(heuristic_df$Correct)/nrow(heuristic_df)