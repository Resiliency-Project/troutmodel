## BROOK and BROWN TROUT IBM ##
## DEVELOPED BY SHANE FLINN ##
## ADAPTED BY KAILI GREGORY

# script for scenario of constant habitat conditions

for(i in 1:ncol(unit.scoresBKT)){
  r.bkt[,i,] <- r.bkt[,1,]
  r.bnt[,i,] <- r.bnt[,1,]

  kBKT[,i,] <- kBKT[,1,]
  kBNT[,i,] <- kBNT[,1,]

  unit.scores_recBKT[,i,] <- unit.scores_recBKT[,1,]
  unit.scores_recBNT[,i,] <- unit.scores_recBNT[,1,]
  
  unit.scoresBKT[,i,] <- unit.scoresBKT[,1,]
  unit.scoresBNT[,i,] <- unit.scoresBNT[,1,]
 
}