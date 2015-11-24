
detailedR = read.csv("regular_season_detailed_results.csv")
tourneySlots2015=read.csv("tourney_slots_2015.csv")

    winProb=c()
for (i in 1:nrow(tourneySlots2015)){
    #extract games of strong seed A as the winning team in 2003-2014
    team_a=detailedR[which(detailedR[,3]==tourneySlots2015[i,5]),]
    #extract games in which strong seed A beat weak seed B given it won 
    win_b=team_a[which(team_a[,5]==tourneySlots2015[i,6]),]
    #extract games of weak seed B as the winning team in 2003-2014
    team_b=detailedR[which(detailedR[,3]==tourneySlots2015[i,6]),]
    #extract games in which weak seed B beat strong seed A given it won 
    win_a=team_b[which(team_b[,5]==tourneySlots2015[i,5]),]
    #given total games strong seed A played against weak seed B
    #calculate proporation strong seed A beat weak seed B 
    winProb[i]=(dim(win_b)[1]-dim(win_a)[1])/(dim(win_b)[1]+dim(win_a)[1])
}

#match each proportion calculated above with index of the game
prob=cbind(1:67,winProb)
#extract games strong seed A always won
win_all=prob[which(prob[,2]==1),]
#extract games strong seed A won as many times as it was beat by weak seed B
win_lose=prob[which(prob[,2]==0),]
#extract games strong seed A won three times as it was beat by weak seed B
win3_lose1=prob[which(prob[,2]==0.5),]
#extract games strong seed A was always beat by weak seed B
lose_all=prob[which(prob[,2]==-1),]
#extract games strong seed A was beat by weak seed B twice as it beat weak seed B
win1_lose2=prob[which(prob[,2]==-1/3),]
#extract all the games that had happened with index of the game in the tournament
history=prob[which(!is.na(prob[,2])),]

#record how many times historical results is consistent with actual tournament
count=0
for (i in 1:nrow(history)) {
    if (history[i,2]<0&tourneySlots2015[history[i,1],7]=="b"){
       count=count+1 
    } else{
        if (history[i,2]>0&tourneySlots2015[history[i,1],7]=="a"){
        count=count+1
        } else {count=count}
        
    }
}

#calculate proportion of historical results is consistent actual tournament
RightProb=count/nrow(history)
#create an matrix that stores the previous calculated output
output=matrix(c(nrow(win_all),length(win3_lose1)-1,
              length(win1_lose2)-1,nrow(win_lose),
              nrow(lose_all), count, nrow(history),RightProb),nrow =1,ncol=8)
colnames(output)=c("W", "W1L1", "W1L2", "W1L1","L","Count of corrected results",
                   "Number of games played","Correct probability")
reference=data.frame("W:Strong seed A always beat weak seed B ","W3L1: Strong seed A won three times as it was beat by weak seed B")


