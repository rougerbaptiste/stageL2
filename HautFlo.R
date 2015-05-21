rm(list=ls()) # supprime tous les éléments stockés dans R


#Définition du code couleur
mycolors <- c("#4d9221", "green","#c51b7d","red","red","red3","black",
		"#01665e","#5ab4ac","#fe9929","#d8b365","#8c510a","grey")
names(mycolors) <- c("F-36","F-39","F-27","F-31","F-31-7","F-31-8","F252","M-49","M-52","M-56","M-40","M-53","MBS")



donnees <- read.table("CorrectedDatas.csv", header = T, sep=";") # on charge les données de hauteurs
donneeshaut <- data.frame(donnees)
donneeshaut$AN <- as.factor(donneeshaut$AN)

#donneeshaut <- donneeshaut[ donneeshaut$année != 1998 , ]
donneeshaut <- donneeshaut[ donneeshaut$année != 2013 , ]
donneeshaut <- donneeshaut[ donneeshaut$année != 2014 , ]

donneesfloF252 <- read.table("sdpreco_STcor_F252.csv", header = T , sep=";") #on charge les données delforaison de F252
donneesfloMBS <- read.table("sdpreco_STcor_MBS.csv" , header = T , sep=";") # on charge les doonnées de floraison de MBS

donneesfloMBS[,8] <- as.factor(donneesfloMBS[,8])


donneesflo <- rbind.data.frame(donneesfloF252,donneesfloMBS)
donneesflo <- donneesflo[ donneesflo$an != 1997 ,]
#donneesflo <- donneesflo[ donneesflo$an != 1998 ,]
donneesflo <- donneesflo[ donneesflo$an != 2011 ,]

moyenneshaut <- aggregate(donneeshaut$HT,by=list(donneeshaut$année, donneeshaut$génération , donneeshaut$lig ,donneeshaut$pop,donneeshaut$famille,donneeshaut$progeniteur),mean) # on aggrège les moyennes des données de hauteurs par année et par famille
colnames(moyenneshaut) <- c("an","generation","lig","pop","fam","proge","haut")


moyennesflo <- aggregate(donneesflo$STcor,by=list(donneesflo$an,donneesflo$lig,donneesflo$pop,donneesflo$ancestor,donneesflo$fam),mean)
colnames(moyennesflo) <- c("an","lig","pop","fam","proge","flo")

moy <- merge(moyenneshaut,moyennesflo,all.x=TRUE,all.y=TRUE, na.rm=T)
keeplist <- !is.na(moy$flo)
moy <- moy[keeplist,]
keeplist <- !is.na(moy$haut)
moy <- moy[keeplist,]

pdf("plot_haut-flo.pdf", onefile=T) # on sort les graphique dans un pdf

for(lignee in c("F252","MBS")){
	pdf(paste("plot_haut-flo_",lignee,".pdf"), onefile=T) # on sort les graphique dans un pdf
	plot(moy$haut[moy$lig==lignee]~moy$flo[moy$lig==lignee], 
			col=mycolors[as.character(moy$fam[moy$lig == lignee])] , 
			pch=3,
			xlab="Date de floraison",
			ylab="Hauteur"
	
	
	)
	#text(moy$flo[moy$lig==lignee],moy$haut[moy$lig==lignee], as.character(moy$generation[moy$lig==lignee]) , pos=3)
	title(lignee)
	if(lignee == "F252"){
		legend(
				"topleft",
				names(mycolors[1:7]),
				col=mycolors[1:7],
				lwd=c(1,1,1),
				lty=c(1,1,1),
		)
	}else{
		legend(
				"topleft",
				names(mycolors[8:13]),
				col=mycolors[8:13],
				lwd=c(1,1,1),
				lty=c(1,1,1),
		)
	}
	dev.off()
	
}

#~ plot(moy$haut[moy$lig=="MBS" & moy$fam=="M-40"]~moy$flo[moy$lig=="MBS" & moy$fam=="M-40"])
pdf("plot_haut-flo_fam.pdf", onefile=T) # on sort les graphique dans un pdf
for (lignee in  c("F252","MBS")){
	familles <- levels(droplevels(moy$fam[moy$lig==lignee]))
	for (famille in familles){
		
		print(c(lignee , famille))
		plot(moy$haut[moy$lig==lignee & moy$fam==famille]~moy$flo[moy$lig==lignee & moy$fam==famille],
			xlab="Date de floraison",
			ylab="Hauteur en cm",
			pch=3
		
		)
		title(paste( lignee,famille,sep=":" ))
	}
}
dev.off()
pdf("plot_haut-flo_m49.pdf", onefile=T)
plot(moy$haut[moy$fam=="M-49"]~moy$flo[moy$fam=="M-49"],
			xlab="Date de floraison",
			ylab="Hauteur en cm",
			#pch=as.numeric(as.factor(moy$an[moy$fam=="M-49"])),
			type="n",
			col = mycolors["M-49"]
		
		)
title("M-49")
text(moy$flo[moy$fam=="M-49"],moy$hau[moy$fam=="M-49"],label=moy$an[moy$fam=="M-49"]-min(moy$an[moy$fam=="M-49"])+1)
dev.off()

pdf("plot_haut-flo_f39.pdf", onefile=T)
plot(moy$haut[moy$fam=="F-39"]~moy$flo[moy$fam=="F-39"],
			xlab="Date de floraison",
			ylab="Hauteur en cm",
			#pch=as.numeric(as.factor(moy$an[moy$fam=="F-39"])),
			type="n",
			col = mycolors["F-39"]
		
		)
title("F-39")
text(moy$flo[moy$fam=="F-39"],moy$hau[moy$fam=="F-39"],label=moy$an[moy$fam=="F-39"]-min(moy$an[moy$fam=="F-39"])+1)

dev.off()
dev.off()
