library(help = "datasets")
library(readxl)


Air_quality1 = read_excel("AirQualityUCI.xlsx")

View(Air_quality1)

mean(Air_quality1$`CO(GT)`,na.rm = TRUE)
Air_quality1$`CO(GT)`[is.na(Air_quality1$`CO(GT)`)]=(mean(Air_quality1$`CO(GT)`,na.rm=TRUE))

mean(Air_quality1$`PT08.S1(CO)`,na.rm = TRUE)
Air_quality1$`PT08.S1(CO)`[is.na(Air_quality1$`PT08.S1(CO)`)]=(mean(Air_quality1$`PT08.S1(CO)`,na.rm=TRUE))

mean(Air_quality1$`NMHC(GT)`,na.rm = TRUE)
Air_quality1$`NMHC(GT)`[is.na(Air_quality1$`NMHC(GT)`)]=(mean(Air_quality1$`NMHC(GT)`,na.rm=TRUE))

mean(Air_quality1$`C6H6(GT)`,na.rm = TRUE)
Air_quality1$`C6H6(GT)`[is.na(Air_quality1$`C6H6(GT)`)]=(mean(Air_quality1$`C6H6(GT)`,na.rm = TRUE))

mean(Air_quality1$`PT08.S2(NMHC)`,na.rm = TRUE)
Air_quality1$`PT08.S2(NMHC)`[is.na(Air_quality1$`PT08.S2(NMHC)`)]=(mean(Air_quality1$`PT08.S2(NMHC)`,na.rm=TRUE))

mean(Air_quality1$`NOx(GT)`,na.rm = TRUE)
Air_quality1$`NOx(GT)`[is.na(Air_quality1$`NOx(GT)`)]=(mean(Air_quality$`NOx(GT)`,na.rm = TRUE))

mean(Air_quality1$`PT08.S3(NOx)`,na.rm=TRUE)
Air_quality1$`PT08.S3(NOx)`[is.na(Air_quality1$`PT08.S3(NOx)`)]=(mean(Air_quality1$`PT08.S3(NOx)`,na.rm=TRUE))

mean(Air_quality1$`NO2(GT)`,na.rm = TRUE)
Air_quality1$`NO2(GT)`[is.na(Air_quality1$`NO2(GT)`)]=(mean(Air_quality1$`NO2(GT)`,na.rm = TRUE))

mean(Air_quality1$`PT08.S4(NO2)`,na.rm = TRUE)
Air_quality1$`PT08.S4(NO2)`[is.na(Air_quality1$`PT08.S4(NO2)`)]=(mean(Air_quality1$`PT08.S4(NO2)`,na.rm = TRUE))

mean(Air_quality1$`PT08.S5(O3)`,na.rm = TRUE)
Air_quality1$`PT08.S5(O3)`[is.na(Air_quality1$`PT08.S5(O3)`)]=(mean(Air_quality1$`PT08.S5(O3)`,na.rm=TRUE))

mean(Air_quality1$T,na.rm = TRUE)
Air_quality1$T[is.na(Air_quality1$T)]=(mean(Air_quality1$T,na.rm=TRUE))

mean(Air_quality1$RH,na.rm = TRUE)
Air_quality1$RH[is.na(Air_quality1$RH)]=(mean(Air_quality1$RH,na.rm=TRUE))

mean(Air_quality1$AH,na.rm = TRUE)
Air_quality1$AH[is.na(Air_quality1$AH)]=(mean(Air_quality1$AH,na.rm=TRUE))

summary(Air_quality1)
length(Air_quality1)
boxplot(Air_quality1)

r1=2.600+1.5*IQR(Air_quality1$`CO(GT)`)
r2=1.200-1.5*IQR(Air_quality1$`CO(GT)`)

Air_quality1$`CO(GT)`[Air_quality1$`CO(GT)`>r1]=r1
Air_quality1$`CO(GT)`[Air_quality1$`CO(GT)`<r2]=r2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1)
boxplot(Air_quality1$`CO(GT)`)

ru1=1221.2+1.5*IQR(Air_quality1$`PT08.S1(CO)`)
ru2=1074.5-1.5*IQR(Air_quality1$`PT08.S1(CO)`)

Air_quality1$`PT08.S1(CO)`[Air_quality1$`PT08.S1(CO)`>ru1]=ru1
Air_quality1$`PT08.S1(CO)`[Air_quality1$`PT08.S1(CO)`<ru2]=ru2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1)
boxplot(Air_quality1$`PT08.S1(CO)`)

rul1=218.8+1.5*IQR(Air_quality1$`NMHC(GT)`)
rul2=218.8-1.5*IQR(Air_quality1$`NMHC(GT)`)

Air_quality1$`NMHC(GT)`[Air_quality1$`NMHC(GT)`>rul1]=rul1
Air_quality1$`NMHC(GT)`[Air_quality1$`NMHC(GT)`<rul2]=rul2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1)
boxplot(Air_quality1$`NMHC(GT)`)

rul1=13.636+1.5*IQR(Air_quality1$`C6H6(GT)`)
rul2=4.591-1.5*IQR(Air_quality1$`C6H6(GT)`)

Air_quality1$`C6H6(GT)`[Air_quality1$`C6H6(GT)`>rul1]=rul1
Air_quality1$`C6H6(GT)`[Air_quality1$`C6H6(GT)`<rul2]=rul2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1)
boxplot(Air_quality1$`C6H6(GT)`)

rul1=557+1.5*IQR(Air_quality1$`PT08.S2(NMHC)`)
rul2=557-1.5*IQR(Air_quality1$`PT08.S2(NMHC)`)

Air_quality1$`PT08.S2(NMHC)`[Air_quality1$`PT08.S2(NMHC)`>rul1]=rul1
Air_quality1$`PT08.S2(NMHC)`[Air_quality1$`PT08.S2(NMHC)`<rul2]=rul2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1)
boxplot(Air_quality1$`PT08.S2(NMHC)`)

rul1=284.2+1.5*IQR(Air_quality1$`NOx(GT)`)
rul2=112-1.5*IQR(Air_quality1$`NOx(GT)`)

Air_quality1$`NOx(GT)`[Air_quality1$`NOx(GT)`>rul1]=rul1
Air_quality1$`NOx(GT)`[Air_quality1$`NOx(GT)`<rul2]=rul2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1)
boxplot(Air_quality1$`NOx(GT)`)

rul1=960.2+1.5*IQR(Air_quality1$`PT08.S3(NOx)`)
rul2=665.5-1.5*IQR(Air_quality1$`PT08.S3(NOx)`)

Air_quality1$`PT08.S3(NOx)`[Air_quality1$`PT08.S3(NOx)`>rul1]=rul1
Air_quality1$`PT08.S3(NOx)`[Air_quality1$`PT08.S3(NOx)`<rul2]=rul2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1$`PT08.S3(NOx)`)

rul1=133.0+1.5*IQR(Air_quality1$`NO2(GT)`)
rul2=85.9-1.5*IQR(Air_quality1$`NO2(GT)`)

Air_quality1$`NO2(GT)`[Air_quality1$`NO2(GT)`>rul1]=rul1
Air_quality1$`NO2(GT)`[Air_quality1$`NO2(GT)`<rul2]=rul2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1)
boxplot(Air_quality1$`NO2(GT)`)

rul1=1662+1.5*IQR(Air_quality1$`PT08.S4(NO2)`)
rul2=1242-1.5*IQR(Air_quality1$`PT08.S4(NO2)`)

Air_quality1$`PT08.S4(NO2)`[Air_quality1$`PT08.S4(NO2)`>rul1]=rul1
Air_quality1$`PT08.S4(NO2)`[Air_quality1$`PT08.S4(NO2)`<rul2]=rul2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1)
boxplot(Air_quality1$`PT08.S4(NO2)`)

rul1=1255.2+1.5*IQR(Air_quality1$`PT08.S5(O3)`)
rul2=741.8-1.5*IQR(Air_quality1$`PT08.S5(O3)`)

Air_quality1$`PT08.S5(O3)`[Air_quality1$`PT08.S5(O3)`>rul1]=rul1
Air_quality1$`PT08.S5(O3)`[Air_quality1$`PT08.S5(O3)`<rul2]=rul2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1)
boxplot(Air_quality1$`PT08.S5(O3)`)



rul1=24.07+1.5*IQR(Air_quality1$T)
rul2=12.03-1.5*IQR(Air_quality1$T)

Air_quality1$T[Air_quality1$T>rul1]=rul1
Air_quality1$T[Air_quality1$T<rul2]=rul2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1)
boxplot(Air_quality1$T)


rul1=0.7461+1.5*IQR(Air_quality1$AH)
rul2=1.2962-1.5*IQR(Air_quality1$AH)

Air_quality1$AH[Air_quality1$AH>rul1]=rul1
Air_quality1$AH[Air_quality1$AH<rul2]=rul2
Air_quality1
summary(Air_quality1)
boxplot(Air_quality1)
boxplot(Air_quality1$AH)

hist(Air_quality1$`CO(GT)`,breaks=10,col='RED',main='Histogram')
hist(Air_quality1$`PT08.S1(CO)`,breaks=10,col='blue',main='Histogram')
hist(Air_quality1$`NMHC(GT)`,breaks=10,col='yellow',main='Histogram')
hist(Air_quality1$`C6H6(GT)`,breaks=10,col='green',main='Histogram')
hist(Air_quality1$`PT08.S2(NMHC)`,breaks=10,col='Violet',main='Histogram')
hist(Air_quality1$`NOx(GT)`,breaks=10,col='PINK',main='Histogram')
hist(Air_quality1$`PT08.S3(NOx)`,breaks=10,col='Grey',main='Histogram')
hist(Air_quality1$`NO2(GT)`,breaks=10,col='skyblue',main='Histogram')
hist(Air_quality1$`PT08.S4(NO2)`,breaks=10,col='purple',main='Histogram')
hist(Air_quality1$`PT08.S5(O3)`,breaks=10,col='Brown',main='Histogram')
hist(Air_quality1$T,breaks=10,col='Black',border='white',main='Histogram')
hist(Air_quality1$RH,breaks=10,col='turquoise',main='Histogram')
hist(Air_quality1$AH,breaks=10,col='darkgreen',main='Histogram')


dotchart(Air_quality1$`CO(GT)`,labels=row.names(Air_quality1),groups =Air_quality1$Date ,cex=0.75,color=m$color)
dotchart(Air_quality1$`PT08.S1(CO)`,labels=row.names(Air_quality1),groups =Air_quality1$Date ,cex=0.75,color=m$color)
dotchart(Air_quality1$`NMHC(GT)`,labels=row.names(Air_quality1),groups =Air_quality1$Date ,cex=0.75,color=m$color)
dotchart(Air_quality1$`C6H6(GT)`,labels=row.names(Air_quality1),groups =Air_quality1$Date ,cex=0.75,color=m$color)
dotchart(Air_quality1$`PT08.S2(NMHC)`,labels=row.names(Air_quality1),groups =Air_quality1$Date ,cex=0.75,color=m$color)
dotchart(Air_quality1$`NOx(GT)`,labels=row.names(Air_quality1),groups =Air_quality1$Date ,cex=0.75,color=m$color)
dotchart(Air_quality1$`PT08.S3(NOx)`,labels=row.names(Air_quality1),groups =Air_quality1$Date ,cex=0.75,color=m$color)
dotchart(Air_quality1$`NO2(GT)`,labels=row.names(Air_quality1),groups =Air_quality1$Date ,cex=0.75,color=m$color)
dotchart(Air_quality1,`PT08.S4(NO2)`,labels=row.names(Air_quality1),groups =Air_quality1$Date ,cex=0.75,color=m$color)
dotchart(Air_quality1$T,labels=row.names(Air_quality1),groups =Air_quality1$Date ,cex=0.75,color=m$color)
dotchart(Air_quality1$AH,labels=row.names(Air_quality1),groups =Air_quality1$Date ,cex=0.75,color=m$color)


plot(Air_quality1$`CO(GT)`,Air_quality1$`NMHC(GT)`,main='scatterplot',xlab='`CO(GT)`',ylab='`NMHC(GT)`',col=c("red","blue"))
plot(Air_quality1$`PT08.S1(CO)`,Air_quality1$`C6H6(GT)`,main='scatterplot',xlab='`PT08.S1(CO)`',ylab='`C6H6(GT)`',col=c("red","blue"))
plot(Air_quality1$`NOx(GT)`,Air_quality1$`NO2(GT)`,main='scatterplot',xlab='`NOx(GT)`',ylab='`NO2(GT)`',col=c("red","blue"))
plot(Air_quality1$`PT08.S2(NMHC)`,Air_quality1$`PT08.S3(NOx)`,main='scatterplot',xlab='`PT08.S2(NMHC)`',ylab='`PT08.S3(NOx)`',col=c("red","blue"))
plot(Air_quality1$`PT08.S4(NO2)`,Air_quality1$`PT08.S5(O3)`,main='scatterplot',xlab='`PT08.S4(NO2)`',ylab='`PT08.S5(O3)`',col=c("red","blue"))

plot(Air_quality1$RH,Air_quality1$AH,main='scatterplot',xlab='RH',ylab='AH',col=c("red","blue"))

install.packages("lattice")
install.packages("datasets")
library(lattice)
library(datasets)

bwplot(Air_quality1$`CO(GT)`,breaks=10,col='blue',main='bwplot',xlab='`CO(GT)`')
bwplot(Air_quality1$`PT08.S1(CO)`,breaks=10,col='blue',main='bwplot',xlab='`PT08.S1(CO)`')

bwplot(Air_quality1$`NMHC(GT)`,breaks=10,col='blue',main='bwplot',xlab='`NMHC(GT)`')
bwplot(Air_quality1$`C6H6(GT)`,breaks=10,col='blue',main='bwplot',xlab='`C6H6(GT)`')
bwplot(Air_quality1$`NOx(GT)`,breaks=10,col='blue',main='bwplot',xlab='`NOx(GT)`')
bwplot(Air_quality1$`NO2(GT)`,breaks=10,col='blue',main='bwplot',xlab='`NO2(GT)`')
bwplot(Air_quality1$T,breaks=10,col='blue',main='bwplot',xlab='T')
bwplot(Air_quality1$AH,breaks=10,col='blue',main='bwplot',xlab='AH')


barchart(Air_quality1$`CO(GT)`,Air_quality1$`NMHC(GT)`,breaks=10,col='blue',main='barchart',xlab='`CO(GT)`',ylab='`NMHC(GT)`')
barchart(Air_quality$`PT08.S1(CO)`,Air_quality1$`PT08.S2(NMHC)`,breaks=10,col='blue',main='barchart',xlab='`PT08.S1(CO)`',ylab='`PT08.S2(NMHC)`')
barchart(`NOx(GT)`~`NO2(GT)`,data=Air_quality1,col='red',main='barchart')
barchart(RH~AH,data=Air_quality1,col='green',main='barchart')

densityplot(Air_quality1$`CO(GT)`,Air_quality1$`NMHC(GT)`,breaks=10,col='blue',main='Densityplot',xlab='`CO(GT)`',ylab='`NMHC(GT)`')
densityplot(Air_quality$`PT08.S1(CO)`,Air_quality1$`PT08.S2(NMHC)`,breaks=10,col='red',main='Densityplot',xlab='`PT08.S1(CO)`',ylab='`PT08.S2(NMHC)`')
densityplot(`NOx(GT)`~`NO2(GT)`,data=Air_quality1,col='Yellow',main='Densityplot',)
densityplot(RH~AH,data=Air_quality1,col='green',main='Densityplot')

