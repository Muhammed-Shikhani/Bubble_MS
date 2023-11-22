setwd("/home/shikhani/Documents/Bubble_data/Data_2022_08_13/")
data <- read.csv("Bubble_MS_sample_data_20221018.csv")
data=data[order(data$Depth_meter),]

data$rO2=1-(0.79/((data$Depth_meter*0.1)+1))

data$pch <- 19
data$pch[(which(data$Gas_amount<80 | data$Duration_h>7))] <- 1

data$ol <- 1
#data$ol[which(data$sample_number == 16 | data$sample_number== 37)] <- 2
data$ol[(which(data$Gas_amount<80 | data$Duration_h>7))] <- 4
data19 <- data[which(data$pch==19),]
data1 <- data[which(data$pch==1),]

h19 <- hist(data19$Oxygen_percent,breaks=c(20,25, 30, 35,40, 45,50))


dat19 <- rbind(h19$counts)

h1 <- hist(data1$Oxygen_percent,breaks=c(20,25, 30, 35,40, 45,50))

dat1 <- rbind(h1$counts)



png(filename = "O2_depth_4panels.png",width = 250,height = 200,units = "mm",res = 300)


l <-layout(matrix(c(1:4), 2, 2, byrow = TRUE), widths=c(0.67, 0.33))
par(mar=c(1, 1, 1, 1),
    oma = c(3, 3, 0.2, 0.2))

plot((data19$Depth_meter), data19$Oxygen_percent,pch=data19$pch, xlim = c(0,5), ylab="Oxygen (%)", xlab="Depth (m)", ylim=c(20,50), las=1, col= data19$ol)
arrows((data19$Depth_meter)+0.2, data19$Oxygen_percent,(data19$Depth_meter)-0.2, data19$Oxygen_percent,length=0.05, angle=90, code=3, col=data19$ol)
lines((c(0,data1$Depth_meter)*1),c(0.21,data1$rO2)*100, col="red")
mtext("Oxygen (%)",side=2,col="black",line=2.5)  
text(0.1,48,"(A)")


barplot(as.matrix(dat19),col=c(1), space = 0, axes = FALSE, horiz=T)
axis(side = 2, at=c(0,1,2,3,4,5,6),labels = c(20,25, 30, 35,40, 45,50),las=2)
axis(side = 1)
text(0.5,5.7,"(B)")

plot((data1$Depth_meter), data1$Oxygen_percent,pch=data1$pch, xlim = c(0,5), ylab="Oxygen (%)", xlab="Depth (m)", ylim=c(20,50), las=1, col= data1$ol)
arrows((data1$Depth_meter)+0.2, data1$Oxygen_percent,(data1$Depth_meter)-0.2, data1$Oxygen_percent,length=0.05, angle=90, code=3, col=data1$ol)
lines((c(0,data1$Depth_meter)*1),c(0.21,data1$rO2)*100, col="red")
mtext("Oxygen (%)",side=2,col="black",line=2.5) 
mtext("Depth (m)",side=1,col="black",line=2.5)  
legend("bottomright", legend =c( "fast experiment", "slow experiment","Theory"),pch = c(19,1,NA), lty = c(NA,NA,1), col=c(1,4,2),bty = "n",)
text(0.1,48,"(C)")

b1 <- barplot(as.matrix(dat1),col=c("lightblue2"), space = 0, axes = FALSE, horiz=T)
axis(side = 2, at=c(0,1,2,3,4,5,6),labels = c(20,25, 30, 35,40, 45,50),las=2)
axis(side = 1)
mtext("count",side=1,col="black",line=2.5)  
text(0.5,5.7,"(D)")
dev.off()


