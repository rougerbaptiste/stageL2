rm(list=ls())

donnees1 <- read.table("HauteursFinal.csv", header = T, sep=";")
donnees <- data.frame(donnees1)

moyennes <- aggregate(donnees$hauteur,by=list(donnees$année,donnees$pop,donnees$lig),mean)



select1FPr <- moyennes[moyennes[,3]=="F252",]
selectFPr <- select1FPr[moyennes[,2]=="PRECOCE",4]
f252pre <- selectFPr[!is.na(selectFPr)]

select1FTe <- moyennes[moyennes[,3]=="F252",]
selectFTe <- select1FTe[moyennes[,2]=="TEMOIN",4]
f252tem <- selectFTe[!is.na(selectFTe)]

select1FTa <- moyennes[moyennes[,3]=="F252",]
selectFTa <- select1FTa[moyennes[,2]=="TARDIF",4]
f252tar <- selectFTa[!is.na(selectFTa)]



select1MPr <- moyennes[moyennes[,3]=="MBS",]
selectMPr <- select1MPr[moyennes[,2]=="PRECOCE",4]
mbspre <- selectMPr[!is.na(selectMPr)]

select1MTe <- moyennes[moyennes[,3]=="MBS",]
selectMTe <- select1MTe[moyennes[,2]=="TEMOIN",4]
mbstem <- selectMTe[!is.na(selectMTe)]

select1MTa <- moyennes[moyennes[,3]=="MBS",]
selectMTa <- select1MTa[moyennes[,2]=="TARDIF",4]
mbstar <- selectMTa[!is.na(selectMTa)]

absc <- 1998:2014
absc <- absc[-14]
pdf("plot_pop-dist.pdf", onefile=T)

plot(f252pre ~ absc, type = "l" ,
	ylim=c(90,180),
	col="blue",
	xlab="Années",
	ylab="Hauteur en cm",
	main="Représentation pour chaque population de la variation\n de la taille en fonction de l'année"
)
lines(f252tem ~ absc, type = "l", col="black")
lines(f252tar ~ absc, type = "l", col="red")
lines(mbspre ~ absc , type = "l", col="blue", lty=2)
lines(mbstem ~ absc , type = "l", col="black", lty=2)
lines(mbstar ~ absc , type = "l", col="red", lty=2)
legend(
	"bottomleft",
	c("F252 Précoce","F252 Témoin","F252 Tardif","MBS Précoce","MBS Témoin","MBS Tardif"),
	col=c("blue","black","red","blue","black","red"),
	lwd=c(1,1,1,1,1,1),
    lty=c(1,1,1,2,2,2),
)

####

f252relpre <- f252pre - f252tem
f252reltar <- f252tar - f252tem

mbsrelpre <- mbspre - mbstem
mbsreltar <- mbstar - mbstem
temoin <- rep(0, length(f252relpre))


plot(f252relpre ~ absc ,
	type = "l",
	ylim=c(-10,25),
	col="blue",
	xlab="Années",
	ylab="Hauteur relative en cm",
	main="Représentation pour chaque population de la variation \n de la taille en fonction de l'année"
)
lines(f252reltar ~ absc, type="l",col="red")
lines(mbsrelpre ~ absc, type="l",col="blue", lty=2)
lines(mbsreltar ~ absc, type="l",col="red", lty=2)
lines(temoin ~ absc , type="l")
legend(
	"topleft",
	c("F252 Précoce","F252 Tardif","MBS Précoce","MBS Tardif","Témoin"),
	col=c("blue","red","blue","red","black"),
	lwd=c(1,1,1,1,1),
    lty=c(1,1,2,2,1),
)
dev.off()
