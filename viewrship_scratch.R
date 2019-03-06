

tv <- read.table('~/Documents/Homework/Stat469/Viewership.txt', header = TRUE)


plot(log(tv$Viewers)~tv$ShowNum)
mean(log(tv$Viewers))
sd(log(tv$Viewers))
median(log(tv$Viewers))
aggregate(log(tv$Viewers)~tv$Season, FUN=mean)

mod <- lm(log(Viewers)~., data = tv)
#p,d,q,P,D,Q


ts_models <- c()
for (i in 0:2) {
  for (j in 0:2) {
    for (k in 0:1) {
      for (l in 0:1) {
        ts_models <- rbind(ts_models, c(i,0,j,k,1,l))
      }
    }
  }
}




