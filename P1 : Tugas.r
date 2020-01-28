data = read.csv("~/Documents/data.csv", header = TRUE,sep = ",")
View(data)
data$Tahun

g_range<-range(0, 1000000)
plot(data$Volume,type="o",col="blue",yli=g_range,axes=FALSE,ann=FALSE)
axis(1,at=1:9,lab=c("2010","2011","2012","2013","2014","2015","2016","2017","2018"))
axis(2,las=1,at=1000*c(0 : 1000000))
box()         #buat bikin box nya
lines(data$Nilai,type='o',pch=22,lty=2,col="red") #pch : tipe dotnya, lty : tipe garis
title(main="Volume dan Nilai Produksi Susu Perusahaan Sapi Perah Tahun 2010-2018",col.main="red",font.main=4)
title(xlab="Tahun",col.lab=rgb(0,0.5,0))
title(ylab="Jumlah",col.lab=rgb(0,0.5,0))
legend(1,g_range[2]-1,c("Volume","Nilai"),cex=0.8,col=c("blue","red"),pch=21:22,lty=1:2)

#nomor 2
var1 <- data$Volume
var2 <- data$Nilai
var3 <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018")
typeof(var1)
typeof(var2)
dataset <- data.frame(var1,var2,var3)
names(dataset)<-c("Volume", "Nilai", "string")
dataset$string <- NULL
barplot(as.matrix(dataset),main = "Volume dan Nilai Produksi Susu Perusahaan Sapi Perah Tahun 2010-2018", xlab="tahun", ylab ="jumlah", beside = TRUE,col = rainbow(9),ylim=c(0,1000000))
legend("topleft", c("2010","2011","2012","2013","2014","2015","2016","2017","2018"),cex = 0.6, bty ="n",fill=rainbow(9))


