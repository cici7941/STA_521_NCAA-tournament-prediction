
detailedR = read.csv("regular_season_detailed_results.csv")
tourneySlots2015=read.csv("tourney_slots_2015.csv")

    winProb=c()
for (i in 1:nrow(tourneySlots2015)){
    team_a=detailedR[which(detailedR[,3]==tourneySlots2015[i,5]),]
    win_b=team_a[which(team_a[,5]==tourneySlots2015[i,6]),]
    team_b=detailedR[which(detailedR[,3]==tourneySlots2015[i,6]),]
    win_a=team_b[which(team_b[,5]==tourneySlots2015[i,5]),]
    winProb[i]=(dim(win_b)[1]-dim(win_a)[1])/(dim(win_b)[1]+dim(win_a)[1])
}

prob=cbind(1:67,winProb)
win_all=prob[which(prob[,2]==1),]
win_lose=prob[which(prob[,2]==0),]
win3_lose1=prob[which(prob[,2]==0.5),]
lose_all=prob[which(prob[,2]==-1),]
win1_lose2=prob[which(prob[,2]==-1/3),]
history=prob[which(!is.na(prob[,2])),]

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

RightProb=count/nrow(history)
output=matrix(c(nrow(win_all),length(win3_lose1)-1,
              length(win1_lose2)-1,nrow(win_lose),
              nrow(lose_all), count, nrow(history),RightProb),nrow =1,ncol=8)
colnames(output)=c("W", "W1L1", "W1L2", "W1L1","L","Count of corrected results",
                   "Number of games played","Correct probability")
library(xtable)
print(xtable(as.data.frame(output)),comment=FALSE)
