rm(list=ls()) # on supprime tous les éléments de R

donnees1 <- read.table("CorrectedDatas.csv", header = T, sep=";") # on charge les données de hauteurs
donnees <- data.frame(donnees1)

moyennes <- aggregate(donnees$hauteur,by=list(donnees$année,donnees$fam),mean) # on aggrège les moyennes des données de hauteurs par année et par famille


# La procédure consiste en 2 étapes :
#  - sélection de la famille
#  - suppression des valeurs NA

selectF252 <- moyennes[moyennes[,2]=="F252",3]
f252 <- selectF252[!is.na(selectF252)]

selectF27 <- moyennes[moyennes[,2]=="F-27",3]
f27 <- selectF27[!is.na(selectF27)]
f27 <- c(f27, rep(NA, 3)) # on rajoute des NA à la fin pour avoir un vecteur de la bonne taille (fin de F27 en 2010)

## F31...
selectF31 <- moyennes[moyennes[,2]=="F-31",3]
f31 <- selectF31[!is.na(selectF31)]
f31 <- c(f31 , rep(NA,10)) # on rajoute des NA car F31 est séparé en plusieurs familles par la suite

selectF317 <- moyennes[moyennes[,2]=="F-31-7",3]
f317 <- selectF317[!is.na(selectF317)]
f317 <- c(rep(NA,3), f31[4] , f317) # on rajoute des NA car F31-7 apparait en 2002

selectF318 <- moyennes[moyennes[,2]=="F-31-8",3]
f318 <- selectF318[!is.na(selectF318)]
f318 <- c(rep(NA,3), f31[4] , f318) # même chose que pour F31-7


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
selectM56 <- moyennes[moyennes[,2]=="M-56",3]
m56 <- selectM56[!is.na(selectM56)]
m56 <- c(m56 , rep(NA,9)) # on rajoute des NA à la fin pour compléter les années manquantes



absc <- 1998:2014 #absc est l'abscisse du graphe, à laquelle on retire 2011
absc <- absc[-14]

#~ pdf("plot_fam-dist.pdf", onefile=T)
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
lines(f31 ~ absc, type="l", col="orange")
lines(f317 ~ absc, type="l",col="chocolate")
lines(f318 ~ absc,type="l",col="gold")
lines(f36 ~ absc, type = "l", col="blue")
lines(f39 ~ absc, type = "l", col="purple")
lines(mbs ~ absc, type = "l", col="black", lty=2)
lines(m40 ~ absc, type = "l", col="red", lty=2)
lines(m49 ~ absc, type = "l", col="blue", lty=2)
lines(m52 ~ absc, type = "l", col="purple", lty=2)
lines(m53 ~ absc, type = "l", col="orange", lty=2)
lines(m56 ~ absc,type = "l" , col="chocolate", lty=2)
legend(
	"topleft",
	c("F252","F-27","F-31","F31-7","F31-8","F-36","F-39","MBS", "M-40","M-49","M-52","M-53","M-56"),
	col=c("black","red","orange","chocolate","gold","blue","purple","black","red","blue","purple","orange","chocolate"),
	lwd=c(1,1,1,1,1,1,1,1,1,1,1,1,1),
    lty=c(1,1,1,1,1,1,1,2,2,2,2,2,2),
)

# on calcule les valeurs de hauteur relatives au témoin pour chaque famille
f27rel <- f27 - f252
f31rel <- f31 - f252
f317rel <- f317 - f252
f318rel <- f318 - f252
f36rel <- f36 - f252
f39rel <- f39 - f252
m40rel <- m40 - mbs
m49rel <- m49 - mbs
m52rel <- m52 - mbs
m53rel <- m53 - mbs
m56rel <- m56 - mbs

temoin <- rep(0,length(absc)) # pour tracer la ligne du témoin. abline ?
plot(
	f27rel ~ absc ,
	type = "l" , 
	ylim=c(-15,35), 
	col="red", 
	xlab="Années", 
	ylab="Hauteur en cm",
	main="Représentation pour chaque famille de la variation\n relative de la taille en fonction de l'année"
)
abline(lm(f27rel ~ absc), col="red")
lines(f31rel ~ absc , type="l", col ="orange")
#~ abline(lm(f31rel ~ absc), col="orange")
lines(f317rel~ absc, type="l", col = "chocolate")
abline(lm(f317rel ~ absc), col="chocolate")
lines(f318rel~ absc,type="l" , col = "gold")
abline(lm(f318rel ~ absc), col="gold")
lines(f36rel ~ absc, type = "l", col="blue")
abline(lm(f36rel ~ absc), col="blue")
lines(f39rel ~ absc, type = "l", col="purple")
abline(lm(f39rel ~ absc), col="purple")
lines(m40rel ~ absc, type = "l", col="red", lty=2)
abline(lm(m40rel ~ absc), col="red", lty=2)
lines(m49rel ~ absc, type = "l", col="blue", lty=2)
abline(lm(m49rel ~ absc), col="blue", lty=2)
lines(m52rel ~ absc, type = "l", col="purple", lty=2)
abline(lm(m52rel ~ absc), col="purple", lty=2)
lines(m53rel ~ absc, type = "l", col="orange", lty=2)
abline(lm(m53rel ~ absc), col="orange", lty=2)
lines(m56rel ~ absc, type = "l", col="chocolate", lty=2)
abline(lm(m56rel ~ absc), col="chocolate", lty=2)
lines(temoin ~ absc, type = "l", col="black")
legend(
	"topleft",
	c("F-27", "F31","F31-7","F31-8","F-36","F-39", "M-40","M-49","M-52","M-53","M-56"),
	col=c("red", "orange", "chocolate", "gold","blue","purple","red","blue","purple","orange","chocolate"),
	lwd=c(1,1,1,1,1,1,1,1,1,1,1),
    lty=c(1,1,1,1,1,1,2,2,2,2,2),
)
#~ dev.off()
