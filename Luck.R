set.seed(888)

Talent <- rnorm(5000, 95, 10)
hist(Talent)

Luck <- rnorm(5000, 10, 5)
hist(Luck)

Overall <- list()
Overall$Talent <- Talent
Overall$Luck <- Luck
Overall$Score <- Talent+Luck
Overall <- as.data.frame(Overall)

library(ggplot2)
Overall<- Overall[order(-Overall$Score),]

Choosen <- Overall[1:10,]

cand <- seq(1,nrow(Choosen))

Choosen2 <- cbind(cand,Choosen)

library(reshape2)

Choosen3 <- melt(Choosen2[,1:3],                                 
                   id.vars = c("cand"))

Choosen3 <- Choosen3[order(Choosen3$cand,decreasing = TRUE),]

Choosen3$variable <- factor(Choosen3$variable,levels=c("Luck","Talent"))

ggplot(Choosen3, aes(fill=variable, y=value, x=cand)) + 
  geom_bar(stat="identity") +
  geom_vline(xintercept = 10.5)

top10_talent <- mean(Choosen2$Talent[1:10])
top10_luck <- mean(Choosen2$Luck[1:10])

top10_talent 
top10_luck

top50_talent <- mean(Overall$Talent[45:55])
top50_luck <- mean(Overall$Luck[45:55])

top50_talent
top50_luck


Talent <- as.data.frame(Talent)
Luck <- as.data.frame(Luck)

Tlt <- ggplot(Talent,aes(x=Talent))+geom_histogram(bins=30,fill="black") +
        geom_vline(xintercept = top10_talent, colour = "red") +
        geom_vline(xintercept = top50_talent, colour = "blue") 


lck <- ggplot(Luck,aes(x=Luck))+geom_histogram(bins=30,fill="black")+
        geom_vline(xintercept = top10_luck, colour = "red") +
        geom_vline(xintercept = top50_luck, colour = "blue") 

library(ggpubr)
ggarrange(Tlt,lck)


