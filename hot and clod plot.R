jpeg("gene.jpg",width = 7800,height = 7000,res=1000)
hot_cold <- read.table("hot_cold_genenumber.txt" , header = F)
colnames(hot_cold) <- c("a","b","c")
summary(hot_cold)
hot_cold$b <- as.factor(hot_cold$b)
hot_cold$c <- as.factor(hot_cold$c)

hot <- subset(hot_cold,c=="hot")
cold <- subset(hot_cold,c=="cold")
simulated <- subset(hot_cold,c=="simulated")

#new_order <- with(hot_cold,reorder(b,a,mean))
my_plot <- boxplot(a~c*b,data=hot_cold,ylab="Number",xlab=" ",ylim=c(0,30),xaxt="n",
                   col=c(rgb(0.9,0.9,0.5,0.6),rgb(0,0.7,0.5,0.6),rgb(0.1,0.2,0.7,0.6)))
my_name <- sapply(strsplit(my_plot$names,'\\.'),function(x) x[[2]])
my_name <- my_name[seq(1,length(my_name),3)]
axis(side = 1,at=seq(2,16,3),labels=my_name)

for(i in seq(1.5,15,1)){
  abline(v=i,lty=3,col="grey")
}
for(i in seq(0.5,16,3)){
  abline(v=i,lty=1,col=rgb(0,0,0,1))
}
legend_name <- sapply(strsplit(my_plot$names,'\\.'),function(x) x[[1]])
legend_name <- legend_name[c(1,2,3)]
legend("topleft",legend=legend_name,bty = 'n',pch=15,pt.cex=2,cex=0.8,inset=c(0.05,0),
       col=c(rgb(0.9,0.9,0.5,0.6),rgb(0,0.7,0.5,0.6),rgb(0.1,0.2,0.7,0.6)))


hot_gene <- data.frame(hot_cold$a,paste(hot_cold$c,".",hot_cold$b,sep = ""))
colnames(hot_gene) <- c("num","type")
library(multcompView)
model <- aov(num~type,hot_gene)
summary(model)
library(agricolae)
out <- LSD.test(model,"type",p.adj="none")
out$group

lever_label <- data.frame(rownames(out$groups),out$groups$groups)
colnames(lever_label) <- c("type","lever")

library("plyr")

names <- as.data.frame(my_plot$names)
colnames(names) <- "type"
letter <- merge(names,lever_label,by="type",all=T)
letter <- cbind(letter,sapply(strsplit(letter$type,'\\.'),function(x) x[[2]]))
colnames(letter) <- c('type','lever','order')
letter <- letter[order(letter$order),]
#text(x=seq(0.7,15),y=my_plot$stats[5,]+1,letter$lever,cex=0.8,col=19)
text(x=seq(1,15),y=my_plot$stats[5,]+0.5,paste("n=",my_plot$n,' ',letter$lever,sep=""),cex=0.6,col="red")
for(i in 1:length(my_plot$n)){
  my_jitter <- jitter(rep(i,my_plot$n[i]),amount=0.3)
  this_level <- subset(subset(hot_cold,b==strsplit(my_plot$names,'\\.')[[i]][2]),
                       c==strsplit(my_plot$names,'\\.')[[i]][1])$a
  points(my_jitter,this_level,pch=19,cex=0.2,col=rgb(0,0,0,0.8))
}

hot_gene <- data.frame(hot_cold$a,paste(hot_cold$c,".",hot_cold$b,sep = ""))
colnames(hot_gene) <- c("num","type")
library(multcompView)
model <- aov(num~type,hot_gene)
summary(model)
library(agricolae)
out <- LSD.test(model,"type",p.adj="none")
out$group

lever_label <- data.frame(rownames(out$groups),out$groups$groups)
colnames(lever_label) <- c("type","lever")

dev.off()