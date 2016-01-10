############################
#Read in data
############################
library(stringr)
library(ggplot2)

today = as.Date("2012/11/05")
as.numeric.factor = function(x) {as.numeric(levels(x))[x]}



poll.ratings =  read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/pollster-ratings.tsv", header=TRUE, sep="\t")
head(poll.ratings)

nat.polls2012 = read.csv("2012_poll_data.csv", header=TRUE, sep="\t")
state.polls2012 = read.csv("2012_poll_data_states.csv", header=TRUE, sep='\t')

###Combine two datasets into one data frame, polls2012
nat.polls2012$State = "USA"
polls2012 = rbind(nat.polls2012, state.polls2012)

#format dates
dates = str_split_fixed(as.character(polls2012$Date), " - ", 2)
polls2012$StartDate = as.Date(paste("2012", dates[,1], sep="/")) 
polls2012$EndDate = as.Date(paste("2012", dates[,2], sep="/")) 
polls2012$Date = floor(rowMeans(matrix(c(polls2012$StartDate, polls2012$EndDate), ncol=2)))
polls2012$Date = as.Date(polls2012$Date, origin="1970-01-01")

#format "spread"
polls2012$Spread = as.character(polls2012$Spread)
spread = str_split_fixed(polls2012$Spread, " ", 2)
obama = which(spread[,1] == "Obama")
polls2012$Spread[obama] = as.numeric(spread[obama,2])
polls2012$Spread[-obama] = -as.numeric(spread[-obama,2])
polls2012$Spread[which(is.na(polls2012$Spread))] = 0
polls2012$Spread = as.numeric(polls2012$Spread)

#format sample
polls2012$Sample = as.character(polls2012$Sample)
sample = str_split_fixed(polls2012$Sample, " ", 2)
polls2012$Type = sample[,2]
polls2012$Sample = as.numeric(sample[,1])


head(polls2012)




############################
#Weighted polling average
############################
#http://fivethirtyeight.com/features/how-fivethirtyeight-calculates-pollster-ratings/#fn-31


polls.beforetoday = which(polls2012$EndDate < today)
polldata = polls2012[polls.beforetoday,]
polldata$Poll = factor(polldata$Poll, levels=levels(poll.ratings$Pollster))
nax = which(is.na(polldata$Poll))
polldata = polldata[-nax,] #take out polls that haven't been rated by 538


#Calculating recency
#http://fivethirtyeight.com/features/how-the-fivethirtyeight-senate-forecast-model-works/#fn-13
daysfrom = as.numeric(difftime(as.Date("2012/11/01"), today, unit='days'))
recencyrate = log(2)/(14 + 0.2*daysfrom)
timedif = as.numeric(difftime(today, polldata$Date))

polldata$recency = exp(-recencyrate * timedif)


#This originally pulled pollster rating data in
#No longer strictly necessary, but I'm keeping it in because I don't want to debug
#the line that comes after it, where NA values get a sample size of 600
polldata$rating = NA
for (i in 1:length(poll.ratings$Polls)){
  pollster = poll.ratings$Pollster[i]
  idx = which(polldata$Poll == pollster)
  
  polldata$rating[idx] = poll.ratings$Predictive.Plus.Minus[i]
}

polldata$Sample[which(is.na(polldata$Sample))] = 600 #538 policy


#Dataset and description of data can be found at
#https://github.com/fivethirtyeight/data/tree/master/pollster-ratings
polls = read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw-polls.tsv", header=TRUE, sep="\t")
polls = polls[grep(pattern = ".*Pres.*", x=polls$race),] #select only polls for presidential elections
polls$polldate = as.Date(polls$polldate, format='%m/%d/%Y')
polls$electiondate = as.Date(polls$electiondate, format='%m/%d/%Y')

###################
#This was not included in the final predictions because it turns out that 
#the historical polling data I was using did not include all of the pollsters
#in the 2012 election. (I ended up using 538's pollster ratings on GitHub). 
#But I spent some time on it so I wanted to include it in the code to hand in.
##

polls$sample.error = 80 * polls$samplesize^-0.5
polls$PIE = polls$error - polls$sample.error
polls$IAE = NA 
polls$plus.min = NA 
n = length(polls$PIE)
for (i in 1:n){
  race = polls$race[i]
  others = polls[polls$race == race,]
  same = which(others$pollster == polls$pollster[i])
  others = others[-same,]
  polls$IAE[i] = ifelse(length(others$pollno != 0), mean(others$PIE), 0)
  polls$plus.min[i] = polls$PIE[i] - polls$IAE[i]
} 



se = sd(polls$IAE)/sqrt(length(polls$IAE)) 
pollsters = data.frame(pollster = levels(factor(polls$pollster)), PIE=NA, IAE=NA, plus.min = NA)
for (i in 1:length(pollsters$pollster)){
  pster = as.character(pollsters$pollster[i])
  pollsters$PIE[i] = mean(polls$PIE[polls$pollster == pster])
  pollsters$IAE[i] = mean(polls$IAE[polls$pollster == pster])
  
  pollsters$plus.min[i] = mean(polls$plus.min[polls$pollster == pster])
  
}
#this is basically comparison to methodologically perfect poll
pollsters$plus.min = pollsters$plus.min + 1.49

###################


for (i in 1:length(polldata$Poll)) {
  pster = as.character(polldata$Poll[i])
  tot.error = poll.ratings$Simple.Plus.Minus[poll.ratings$Pollster == pster] + 1.49 + 80*polldata$Sample[i]^-0.5
  ESS = 6400 * (tot.error^-2) #effective sample size
  polldata$rating[i] = ESS/283
}


#Weighting by sample weight
polldata$Sampleweight = sqrt(polldata$Sample/600)
#http://fivethirtyeight.com/features/polls-now-weighted-by-sample-size/


##########################
##Likely Voter adjustment
##########################

rv = which(polldata$Type == "RV")
polldata$spread[rv] = polldata$spread[rv] - 2.7
#registered voter polls tend to differ from likely voter polls by 2.7 percentage points




##########################
##Trend Line Adjustment
##########################
#Create "week" variable
#Week is defined using "today"
polldata$Week = today - 7*floor(difftime(today, polldata$Date, unit='days')/7)

polldata$week = as.numeric(floor(difftime(today, polldata$Date, unit='days')/7))

polldata$StatePollster = paste(polldata$State,polldata$Poll)


statepollsters = data.frame(StatePollsters = as.character(levels(factor(polldata$StatePollster))))

##Fitting LOESS to state/pollster subgroups with more than 5 data points
polldata$TAdjustment = NA
trend.fits = lapply(statepollsters$StatePollsters, function(x){
  data = polldata[polldata$StatePollster == x,]
  fit = NA
  if (length(data$Poll) < 5) {
    0
  }
  else {
    fit = loess(Spread ~ week, data=data, span=0.85, model=TRUE, surface='direct')
    predict(fit, newdata=data.frame(week = 1)) - predict(fit, newdata=data)
  }
})


for (i in 1:length(statepollsters$StatePollsters)) {
  polldata$TAdjustment[polldata$StatePollster == statepollsters$StatePollsters[i]] = trend.fits[[i]] #apparently doing this inside lapply breaks R
}

polldata$TAdjusted = polldata$TAdjustment + polldata$Spread
max(polldata$TAdjusted)

#################
##House effects adjustment
#################

library(reshape2)
s = state.abb
s[51] = "USA"

#see which how many polls each state has for each pollster
#so I can remove the states that only have polls by a single pollster
count.states = dcast(polldata, State + Poll ~., length) 

ok.idx = which(polldata$State == "OK" )
la.idx = which(polldata$State == "LA" )
ar.idx = which(polldata$State == "AR" )

housefx.fit = lm(TAdjusted ~ State + Poll, data=polldata[-c(ok.idx, la.idx, ar.idx),])

#511 things
library(car)
vif(housefx.fit)
res = housefx.fit$residuals

crPlots(housefx.fit) 

outlierTest(housefx.fit) #744, 748, 746, 743, 745
plot(housefx.fit$fitted.values ~ res)

cd=cooks.distance(housefx.fit)
plot(cd)

outlier = which(res == max(res))

qqnorm(res)
qqline(res)


#removing outliers
housefx.fit2 = lm(TAdjusted ~ Poll + State, data=polldata[-c(ok.idx, la.idx, ar.idx, outlier),])
res = housefx.fit2$residuals

vif(housefx.fit2)

crPlots(housefx.fit2) 

outlierTest(housefx.fit2)

plot(housefx.fit2$fitted.values ~ res)
abline(0,0,col="red")

qqnorm(res)
qqline(res)


plot(cooks.distance(housefx.fit2))

housefx = summary(housefx.fit) #change to summary(housefx.fit2) to get prediction w/o outlier



##Use coefficients as house effect and 90% CI as "buffer" for house effect
hfx = housefx$coefficients[-seq(1,42,1),1]
se = housefx$coefficients[-seq(1,42,1),2]

X = model.matrix(housefx.fit)
p = rankMatrix(X)
p = p[1]

buffers = 2*se * qt(0.95, n - p)

#create data frame housefx that has all of the house effect and buffer data for each pollster
housefx = data.frame(Poll = names(hfx), HouseEffect = as.numeric(hfx), Buffer = as.numeric(buffers))
names = str_split_fixed(housefx$Poll, "Poll", 2)
housefx$Poll = names[,2]

#use coefficients of housefx as housefx
#depending on sign of housefx:
#   - neg housefx: add buffer, subtract the sum from TAdjusted
#   - pos housefx: subtract buffer, subtract the sum from TAdjusted (I think)
housefx$Total = ifelse(housefx$HouseEffect < 0, housefx$HouseEffect + housefx$Buffer, housefx$HouseEffect - housefx$Buffer)

#use housefx data frame to add house effects for each poll conducted by the pollsters listed in the data frame
#if it is one of the pollsters that was removed, the house effect is 0, so HXAdjusted == TAdjusted
polldata$HXAdjusted = polldata$TAdjusted
for (i in 1:length(polldata$Poll)){
  pster = as.character(polldata$Poll[i])
  total.hfx = housefx$Total[housefx$Poll == pster]
  if (length(total.hfx != 0)) {
    #length of total.hfx will be 0 if it was a poll conducted by a pollster
    #who only has polls in one of the states that were taken out
    polldata$HXAdjusted[i] = polldata$TAdjusted[i] - total.hfx
  }
}



#################
##2012 predictions
################

#http://fivethirtyeight.com/features/how-the-fivethirtyeight-senate-forecast-model-works/#fn-13
#multiply final adjusted spread by all weights and then average over each state
#in Maine and Nebraska, average over congressional district
elect.votes = read.csv("electoral_votes.csv")
elect.votes$State = as.character(elect.votes$State)
elect.votes$StateAbb = NA
for (i in 1:51){
  idx= which(state.name == elect.votes$State[i])
  if (length(idx) != 0){elect.votes$StateAbb[i] = state.abb[idx]}
  else {elect.votes$StateAbb[i] = "DC"}
}
head(elect.votes) 

predicted.votes = data.frame(State = elect.votes$StateAbb, Votes = 0, region=tolower(elect.votes$State))
head(predicted.votes)

for (i in 1:51){
  if (elect.votes$StateAbb[i] == 'DC') {
    nationaldata = polldata[polldata$State == 'USA',]
    spread = nationaldata$HXAdjusted
    pop.vote = mean(spread * nationaldata$Sampleweight * nationaldata$recency * nationaldata$rating)
    predicted.votes$Votes[i] = ifelse(pop.vote > 0, elect.votes$Votes[i], 0)
    
  }
  else {
    statedata = polldata[polldata$State == elect.votes$StateAbb[i],]
    spread = statedata$HXAdjusted 
    pop.vote = mean(spread * statedata$Sampleweight * statedata$recency * statedata$rating)
    if (elect.votes$StateAbb[i] == 'ME' | elect.votes$StateAbb[i] == 'NE'){
      obama.perc = pop.vote + 50
      predicted.votes$Votes[i] = round(obama.perc/100 * elect.votes$Votes[i])
    }
    else {
      predicted.votes$Votes[i] = ifelse(pop.vote > 0, elect.votes$Votes[i], 0)
    }
  }
}

#draw Maps with predicted and actual results
suppressPackageStartupMessages(library(googleVis))

all_states = map_data("state")
##These values are so the right colors are assigned instead of a gradient
predicted.votes$Obama = ifelse(predicted.votes$Votes > 0, 0, 1)
predicted.votes$Obama[c(20,28)] = 0.5
predicted.votes$Obama = ifelse(is.na(predicted.votes$Votes), 2, predicted.votes$Obama)


map = gvisGeoMap(data.frame(state = predicted.votes$region, outcome = predicted.votes$Obama), locationvar='state', numvar='outcome', options=list(region='US',dataMode='regions',colors="['0x006295','0xBD2031', '0xFFFFFF']", showLegend = FALSE))
plot(map)

elect.votes$actual = c(0,0,0,0,55,9,7,3,3,29,0,4,0,20,0,6,0,0,0,4,10,11,16,10,0,0,0,0,6,4,14,5,29,0,0,18,0,7,20,4,0,0,0,0,0,3,13,12,0,10,0)
elect.votes$Obama = ifelse(elect.votes$actual > 0, 0, 1)
elect.votes$Obama = ifelse(is.na(elect.votes$actual), 2, elect.votes$Obama)

actual.map = gvisGeoMap(data.frame(state = elect.votes$State, outcome = elect.votes$Obama), locationvar='state', numvar='outcome', options=list(region='US',dataMode='regions',colors="['0x006295','0xBD2031']", showLegend = FALSE))
plot(actual.map)

predicted.votes$actual = elect.votes$actual

predicted.votes$error = predicted.votes$Votes - predicted.votes$actual != 0

err.idx = which(predicted.votes$error)
predicted.votes$State[err.idx]

polldata[polldata$State == 'IN',]
#AZ IN ME NE NC TN

