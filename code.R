# load packages
library(glmnet)
library(stringr)
library(plyr)

## load data ##
# read in detailed results of the tournament
detailedT = read.csv("tourney_detailed_results.csv")
tourney = detailedT[,c('season', 'wteam', 'lteam')]
# read in regular seasons detailed results
detailedR = read.csv("regular_season_detailed_results.csv")
detailed2015 = read.csv("regular_season_detailed_results_2015.csv")
detailedR = rbind(detailedR, detailed2015)
# read in seeds data
seeds = read.csv("tourney_seeds.csv")


predicting = function(year){
  ## Subset Year info:
  # select regular season data
  regSeasonYearD = detailedR[which(detailedR$season == year),]
  seedsYear = seeds[which(seeds$season == year),]
  if(year!=2015){
    # extract tournament season data
    tourneyYearD = tourney[which(tourney$season == year),]
    # extract tournament seeds data
    tourneyYearD = cbind(cbind(tourneyYearD, rep(0, dim(tourneyYearD)[1])),
                         rep(0, dim(tourneyYearD)[1]))
    colnames(tourneyYearD)[4:5] = c("wSeed", "lSeed")
    # reorganize the columns to include "wteam" and "wSeed"
    tourneyYearD = tourneyYearD[, c(1:2,4,3,5)]
    # extract seeds numbers and store them into tournament data
  } else {
    tourneyYearD = read.csv("tourney_slots_2015.csv")
    tourneyYearD$wteam = 0
    tourneyYearD$wteam[which(tourneyYearD$Actual.winning.team=='a')] <- tourneyYearD[which(tourneyYearD$Actual.winning.team == 'a'),'team.a']
    tourneyYearD$wteam[which(tourneyYearD$Actual.winning.team=='b')] <- tourneyYearD[which(tourneyYearD$Actual.winning.team == 'b'),'team.b']
    tourneyYearD$lteam = 0
    tourneyYearD$lteam[which(tourneyYearD$Actual.winning.team=='a')] <- tourneyYearD[which(tourneyYearD$Actual.winning.team == 'a'),'team.b']
    tourneyYearD$lteam[which(tourneyYearD$Actual.winning.team=='b')] <- tourneyYearD[which(tourneyYearD$Actual.winning.team == 'b'),'team.a']
  }
  for(i in 1:dim(tourneyYearD)[1]){
    tourneyYearD[i, "wSeed"] = as.character(seedsYear[which(seedsYear$team == tourneyYearD$wteam[i]),"seed"])
    tourneyYearD[i, "lSeed"] = as.character(seedsYear[which(seedsYear$team == tourneyYearD$lteam[i]),"seed"])
  } 
  ## extract actual results
  # extract first four winners
  firstFourWinners = tourneyYearD[1:4,c("wSeed", "wteam")]
  firstFourWinners[,1] = str_replace(firstFourWinners$wSeed, "[a-b]", "")
  # subset first round data
  firstRound = tourneyYearD[5:36,]
  firstRound$wSeed = str_replace(firstRound$wSeed, "[a-b]","")
  firstRound$lSeed = str_replace(firstRound$lSeed, "[a-b]","")
  # extract first round winners
  firstRoundWinners = tourneyYearD[5:36,c("wSeed", "wteam")]
  # extract second round winners
  secondRoundWinners = tourneyYearD[37:52,c("wSeed", "wteam")]
  # extract sweet sixteen winning teams
  sweetSixteenWinners = tourneyYearD[53:60,c("wSeed", "wteam")]
  # extract elite eight winning teams
  eliteEightWinners = tourneyYearD[61:64,c("wSeed", "wteam")]
  # extract final four teams
  finalFourWinners = tourneyYearD[65:66,c("wSeed", "wteam")]
  champion = tourneyYearD[67,c("wSeed", "wteam")]
  #replace 'w' and 'l' with 'a' and 'b'
  ChangeName = function(df){
    names(df) = str_replace_all(names(df), '^w', 'a.')
    names(df) = str_replace_all(names(df), '^l', 'b.')
    return(df)
  }
  regSeasonYearD = ChangeName(regSeasonYearD)
  
  format = function(df){
    #write a function named exchange which takes:
    #input: data
    #output: exchange team a and team b by a larger team ID
    #exchange corresponding results of team a and team b after team ID is changed
    exchange = function(data){
      result = 0
      #data = cbind(data, result)
      for(i in 1: nrow(data)){
        if (data$a.team[i] > data$b.team[i]){
          result[i] = 0
          #store a.team ID as temp
          temp = data$a.team[i]
          #assign b.team ID as a.team ID
          data$a.team[i] = data$b.team[i]
          #reassign b.team ID with temp
          data$b.team[i] = temp
          #store team a's data as temp.vec
          temp.vec = data[i, 9:21]
          #exchange team a's data with team b's
          data[i, 9:21] = data[i, 22:34]
          data[i, 22:34] = temp.vec
        }
        else{
          result[i] =1
        }
      }
      return(cbind(data, result))
    }
    datanew = exchange(df)
    #delete columns with unnecessary information
    datanew = datanew[, -c(1,2,3,5,7)]
    #normalize data 
    datanewS = as.data.frame(scale(datanew))
    datanewS = datanewS[, -30]
    #add the response column (binary number)
    datanewS = cbind(datanewS, datanew$result)
    #store datanewS as a matrix
    datanewS = as.matrix(datanewS)
    return(datanewS)
  }
  
  datanewS = format(regSeasonYearD)
  
  #apply exchange function to Year regular seasons results
  
  
  #write an function named average which takes:
  #input: data, index1 (first team ID) and index2 (second team ID)
  #output: average seasonal results of team a and team b 
  average = function(data, index1, index2){
    data$season = NULL
    data$daynum = NULL
    data$a.loc = NULL
    team.a = str_detect(names(data),'^a') | str_detect(names(data),'numot')
    team.b = str_detect(names(data),'^b') | str_detect(names(data),'numot')
    index1.a = data[str_detect(data$a.team, index1), team.a]
    index1.b = data[str_detect(data$b.team, index1), team.b]
    #average out relevant data of first team using apply() 
    index1.mean = (apply(index1.a,2,sum)+apply(index1.b,2,sum))/(nrow(index1.a)+nrow(index1.b))
    #remove the first row of team ID
    index1.mean = index1.mean[-1]
    index2.a = data[str_detect(data$a.team, index2), team.a]
    index2.b = data[str_detect(data$a.team, index2), team.b]
    #average out relevant data of second team using apply()
    index2.mean = (apply(index2.a,2,sum)+apply(index2.b,2,sum))/(nrow(index2.a)+nrow(index2.b))
    #remove the first row of team ID
    index2.mean = index2.mean[-1]
    
    data$a.team = NULL
    data$b.team = NULL
    #return an output of average statistics of two teams
    output = c(index1.mean[1],index2.mean[1],mean(index1.mean[2],index1.mean[2]), index1.mean[-(1:2)],index2.mean[-(1:2)])
    names(output) = names(data)
    return(as.matrix(output))
  }
  
  ## models ##
  # run LASSO regression with cross validation
  # alpha = 1
  lm1 = cv.glmnet(datanewS[,1:29], datanewS[,30], standardize=FALSE, 
                  family = "binomial",alpha = 1, 
                  type.measure = "auc") 
  # run ELASTIC regression with cross validation
  # alpha = 0.5
  lm2 = cv.glmnet(datanewS[,1:29], datanewS[,30], standardize=FALSE, 
                  family = "binomial",alpha = 0.5,
                  type.measure = "auc") 
  #run RIDGE regression with cross validation
  # alpha = 0
  lm3 = cv.glmnet(datanewS[,1:29], datanewS[,30], standardize=FALSE, 
                  family = "binomial",alpha = 0) 
  #run Bayesian generalized linear models
  library(arm)
  fit.bayes = bayesglm(datanewS[,30]~.,data=as.data.frame(datanewS[,1:29]), family = 'binomial')
  #use stepwise to choose covariates
  lm4 = step(fit.bayes,scope=list(upper=~.,lower=~1), direction="both",trace=FALSE)
  
  ## report coefficient estimates for different models
  coefEst = function(model){
    #make the format for coefficient consistent
    if(all(class(model)!="cv.glmnet")){
      coefficient = as.data.frame(coef(model))
    } else {
      coefficient = coef(model)
    }
    return(coefficient)
  }
  lasso.coef = coefEst(lm1)
  elastic.coef = coefEst(lm2)
  ridge.coef = coefEst(lm3)
  bayes.coef = coefEst(lm4)
  ###First round result
  #set the seed in the order
  seed = paste0(rep(c("W","X","Y","Z"), each = 16),
                c("01",16,"08","09","05",12,"04",13,"06",11,"03",14,"07",10,"02",15))
  firstround = NULL
  for (i in 1:length(seed)){
    if(seed[i] %in% firstRound$wSeed){
      firstround = c(firstround,firstRound[which(firstRound$wSeed == seed[i]),]$wteam)
    }
    else if(seed[i]  %in% firstRound$lSeed){
      firstround = c(firstround,firstRound[which(firstRound$lSeed == seed[i]),]$lteam)
    }
  }
  #input: firstround teamid, regular season data, seed order, model
  firstround.winners = function(firstround, data,seed,model){
    id = matrix(firstround,ncol = 2, byrow = TRUE)
    seed = matrix(seed, ncol = 2, byrow = TRUE)
    win = 0; game = 0
    seedWinner = 0
    for(i in 1:nrow(id)){
      game = average(data, id[i,1],id[i,2])
      game=t(game)
      if (class(model)[1]=="bayesglm"){
        game=as.data.frame(game)
      }
      win[i] = predict(model, game, type = "response")
      if(win[i] >= 0.5){
        win[i] = id[i,1]
        seedWinner[i] = seed[i,1]
      } else {
        win[i] = id[i,2]
        seedWinner[i] = seed[i,2]
      }
    }
    winner = data.frame(wSeed = seedWinner, wteam = win)
    return(winner)
  }
  nextround.winners = function(lastwinner, data, model){
    seed = matrix(as.matrix(lastwinner)[,1])
    id = matrix(as.matrix(lastwinner)[,2])
    win = 0; game = 0
    seedWinner = 0
    k = seq(2,nrow(id),2)
    for(i in k){
      game = average(data, id[i-1,1],id[i,1])
      game=t(game)
      if (class(model)[1]=="bayesglm"){
        game=as.data.frame(game)
      }
      win[i/2] = predict(model, game, type = "response")
      if(win[i/2] >= 0.5){
        win[i/2] = id[i-1,1]
        seedWinner[i/2] = seed[i-1,1]
      } else {
        win[i/2] = id[i,1]
        seedWinner[i/2] = seed[i,1]
      }
    }
    winner = data.frame(wSeed=seedWinner,wteam=win)
    return(winner)
  }
  predictRate = function(model){
    firstRoundPredWinners = firstround.winners(as.character(firstround), regSeasonYearD,seed,model)
    rate32 = sum(firstRoundPredWinners$wteam %in% firstRoundWinners$wteam)/32
    secondRoundPredWinners = nextround.winners(firstRoundPredWinners,regSeasonYearD,model)
    rate16 = sum(secondRoundPredWinners$wteam %in% secondRoundWinners$wteam)/16
    sweetSixteenPredWinners = nextround.winners(secondRoundPredWinners,regSeasonYearD,model)
    rate8 = sum(sweetSixteenPredWinners$wteam %in% sweetSixteenWinners$wteam)/8
    eliteEightPredWinners = nextround.winners(sweetSixteenPredWinners,regSeasonYearD,model)
    rate4 = sum(eliteEightPredWinners$wteam %in% eliteEightWinners$wteam)/4
    finalFourPredWinners = nextround.winners(eliteEightPredWinners,regSeasonYearD,model)
    rate2 = sum(finalFourPredWinners$wteam %in% finalFourWinners$wteam)/2
    PredChampion = nextround.winners(finalFourPredWinners,regSeasonYearD,model)
    rate1 = sum(PredChampion$wteam %in% champion$wteam)
    return(list(rate = c(rate32,rate16,rate8,rate4,rate2,rate1),
                wteam = list(firstRoundPredWinners,secondRoundPredWinners,
                             sweetSixteenPredWinners,eliteEightPredWinners,
                             finalFourPredWinners,PredChampion)))
  }
  lasso.rate = predictRate(lm1)
  elastic.rate = predictRate(lm2)
  ridge.rate = predictRate(lm3)
  bayes.rate = predictRate(lm4)
  return(list(coef = list(lasso.coef,elastic.coef,ridge.coef,bayes.coef),
              pred.rate = list(lasso.rate,elastic.rate,ridge.rate,bayes.rate)))
}
result2012 <- predicting(2012)
result2013 <- predicting(2013)
result2014 <- predicting(2014)
result2015 <- predicting(2015)