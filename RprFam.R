rm(list=ls())

donnees1 <- read.table("HauteursFinal.csv", header = T, sep=";")
donnees <- data.frame(donnees1)

moyennes <- aggregate(donnees$hauteur,by=list(donnees$année,donnees$fam),mean)



selectF252 <- moyennes[moyennes[,2]=="F252",3]
f252 <- selectF252[!is.na(selectF252)]

selectF27 <- moyennes[moyennes[,2]=="F-27",3]
f27 <- selectF27[!is.na(selectF27)]
f27 <- c(f27, rep(NA, 3))

## F31...

selectF36 <-  moyennes[moyennes[,2]=="F-36",3]
f36 <- selectF36[!is.na(selectF36)]

selectF39 <-  moyennes[moyennes[,2]=="F-39",3]
f39 <- selectF39[!is.na(selectF39)]


selectMBS <-  moyennes[moyennes[,2]=="MBS",3]
mbs <- selectMBS[!is.na(selectMBS)]

selectM40 <-  moyennes[moyennes[,2]=="M-40",3]
m40 <- selectM40[!is.na(selectM40)]

selectM49 <-  moyennes[moyennes[,2]=="M-49",3]
m49 <- selectM49[!is.na(selectM49)]

selectM52 <-  moyennes[moyennes[,2]=="M-52",3]
m52 <- selectM52[!is.na(selectM52)]

selectM53 <-  moyennes[moyennes[,2]=="M-53",3]
m53 <- selectM53[!is.na(selectM53)]

##M56



absc <- 1998:2014
absc <- absc[-14]

pdf("plot_fam-dist.pdf", onefile=T)
plot(
	f252 ~ absc ,
	type = "l" , 
	ylim=c(90,180), 
	col="black", 
	xlab="Années", 
	ylab="Hauteur en cm",
	main="Représentation pour chaque famille de la variation\n de la taille en fonction de l'année"
)
lines(f27 ~ absc, type = "l", col="red")
lines(f36 ~ absc, type = "l", col="blue")
lines(f39 ~ absc, type = "l", col="purple")
lines(mbs ~ absc, type = "l", col="black", lty=2)
lines(m40 ~ absc, type = "l", col="red", lty=2)
lines(m49 ~ absc, type = "l", col="blue", lty=2)
lines(m52 ~ absc, type = "l", col="purple", lty=2)
lines(m53 ~ absc, type = "l", col="orange", lty=2)
legend(
	"topleft",
	c("F252","F-27","F-36","F-39","MBS", "M-40","M-49","M-52","M-53"),
	col=c("black","red","blue","purple","black","red","blue","purple","orange"),
	lwd=c(1,1,1,1,1,1,1,1,1),
    lty=c(1,1,1,1,2,2,2,2,2),
)

f27rel <- f27 - f252
f36rel <- f36 - f252
f39rel <- f39 - f252
#~ f27rel <- f27 - f252
#~ f27rel <- f27 - f252
m40rel <- m40 - mbs
m49rel <- m49 - mbs
m52rel <- m52 - mbs
m53rel <- m53 - mbs

temoin <- rep(0,length(absc))
plot(
	f27rel ~ absc ,
	type = "l" , 
	ylim=c(-15,35), 
	col="red", 
	xlab="Années", 
	ylab="Hauteur en cm",
	main="Représentation pour chaque famille de la variation\n relative de la taille en fonction de l'année"
)
lines(f36rel ~ absc, type = "l", col="blue")
lines(f39rel ~ absc, type = "l", col="purple")
lines(m40rel ~ absc, type = "l", col="red", lty=2)
lines(m49rel ~ absc, type = "l", col="blue", lty=2)
lines(m52rel ~ absc, type = "l", col="purple", lty=2)
lines(m53rel ~ absc, type = "l", col="orange", lty=2)
lines(temoin ~ absc, type = "l", col="black")
legend(
	"topleft",
	c("F-27","F-36","F-39", "M-40","M-49","M-52","M-53"),
	col=c("red","blue","purple","red","blue","purple","orange"),
	lwd=c(1,1,1,1,1,1,1),
    lty=c(1,1,1,2,2,2,2),
)
dev.off()
