source("costants.r")


plot.barplot<-function(dataframe,title){
	
	
	min <- min(dataframe[,1])
	minName <-row.names(dataframe)[(which(dataframe[,1]==min(dataframe[,1])))]
	
	max <- max(dataframe[,1])
	maxName <-row.names(dataframe)[(which(dataframe[,1]==max(dataframe[,1])))]
	
	tot<-min+max
	
	percMin<-round((min/tot)*100,1)
	percMax<-round((max/tot)*100,1)
	
	tabMinMax <- c(min,max)
	tabMinMaxPerc <- c(percMin,percMax)
	tabMinMaxPercForPrint <- c(percMin,percMax)
	tabName <- c(minName,maxName)
	
	for(i in (2:(dim(dataframe)[2]))) {
		
		
		min <- min(dataframe[,i])
		
		minName <-row.names(dataframe)[(which(dataframe[,i]==min(dataframe[,i])))]
		
		max <- max(dataframe[,i])
	     
		maxName <-row.names(dataframe)[(which(dataframe[,i]==max(dataframe[,i])))]
		
		tot<-min+max
		
		percMin<-round((min/tot)*100,1)
		percMax<-round((max/tot)*100,1)
		
		tabMinMax <- c(tabMinMax,c(min,max))
		tabName <-c(tabName,c(minName,maxName))
		tabMinMaxPercForPrint <-c(tabMinMaxPercForPrint,c(percMin,percMax))
	
		tabMinMaxPerc = rbind(tabMinMaxPerc,c(percMin,percMax))
		
		
	}
	
	
	bp<-barplot(t(as.matrix(tabMinMaxPerc)) , beside=TRUE, main=title, ylab="Frequenza relativa", ylim=c(0,100), xaxt="n", col=c("red","green")) 
	
	text(seq(3,70,by=3), par("usr")[3]-(2.0), srt = 20, adj = 1.1, xpd =TRUE , labels =COLUMNS,cex=0.65)
	
	text(x=bp,tabMinMaxPercForPrint+13,labels=paste(tabName,tabMinMax),xpd=NA,cex=0.45,srt=90)
	
}

															#ex. rowsNumber: c(1,5,7), ex color: c(red,blue,violet) 
plot.barplotGlobal<-function(dataframe,rowsNumber,columnStart,columnEnd,color){
	
	par(mfcol=c(columnStart,columnEnd))
	
	namesNations<-c()
	
	#take the nations' name
	for (i in 1:dim(dataframe)[1]){
		if(i %in% rowsNumber){
			namesNations<-c(namesNations,rownames(dataframe)[i])
		}
	}
	
	
	for(i in columnStart:columnEnd){
		
		rows<-NULL
		
		for(j in rowsNumber){
		
			rows<-c(rows,dataframe[j,i])	
		}
		
		print(rows)
		
		br<-barplot(rows,names.arg=namesNations,ylim=c(0,max(dataframe[,i])),ylab=COLUMNS[i],col=color)
		
	}
		
}

plot.pie <-function(column,title){
	freqRel <- round((column/sum(column))*100,1)
	pie(column,labels=paste(DIVISION.LABELS,freqRel,"%"),main=title)
}

plot.boxplot <- function(dataframe,columnNumber,adjustsMin=0,adjustsMax=0,color="green"){
	
	print(summary(dataframe[,columnNumber]))
	
	
	print(paste("Varianza:",var(dataframe[,columnNumber])))
	
	standardDeviation <- sd(dataframe[,columnNumber])
	
	print(paste("Deviazione standard:", standardDeviation))
	
	print(paste("Coefficiente di variazione:", standardDeviation/abs(mean(dataframe[,columnNumber]))))
	
	myboxplot <- boxplot(dataframe[,columnNumber],main=COLUMNS[columnNumber],col=color)
	
	#print all the information in the boxplot
	info <- myboxplot$stats
	outlier <- myboxplot$out
	                             #row
	numberOfRow <- dim(dataframe)[1]
	
	textMin <-c()
	textMax <-c()
	
	
	#min and max
	for(i in 1:numberOfRow){
		
		
		if(dataframe[i,columnNumber] %in% info){
			#the min
			if(dataframe[i,columnNumber]== info[1]){                           
				
				#more min
				textMin <- c(textMin,rownames(dataframe)[i])
			#the max
			} else  if(dataframe[i,columnNumber]== info[5]){
				
				#more max
				textMax <- c(textMax,rownames(dataframe)[i])
				
			}
			
		} else if(dataframe[i,columnNumber] %in% outlier){
																			#right				
			text(1,dataframe[i,columnNumber],rownames(dataframe)[i],cex=0.6,pos=4)
		}
		
		

	}
	
					                                  #below
	text(1,info[1]+ adjustsMin, textMin,cex=0.6,pos=1)
	
												     #above
	text(1,info[5]+ adjustsMax,textMax,cex=0.6,pos=3)

	
	myOrder <- order(dataframe[,columnNumber])
	
	if((numberOfRow %% 2)==0){
		text(1,info[3], paste(rownames(dataframe)[myOrder[numberOfRow/2]],rownames(dataframe)[myOrder[(numberOfRow/2)+1]],"- Median:",info[3]),cex=0.6,pos=3)
	} else {
		text(1,info[3], paste(rownames(dataframe)[myOrder[round(numberOfRow/2)]],"- Median:",info[3]),cex=0.6,pos=3)
	}
		
	
}

#part2

plot.normalConfidence <- function(population,oneMinusAlpha,sigma){
	
	n <- length(population)
	
	alpha <- 1- oneMinusAlpha
	
	alphaHalf <- alpha/2

	leftLimit <- mean(population)-qnorm(1-alphaHalf,mean=0,sd=1)*sigma/sqrt(length(population))
	print(c("Left limit: ",leftLimit))
	rightLimit <- mean(population)+qnorm(1-alphaHalf,mean=0,sd=1)*sigma/sqrt(length(population))
	
	print(c("Right limit: ",rightLimit))
	
	print(c("Difference: ",abs(rightLimit-leftLimit)))
	
	
	quantile <- qnorm(1-alphaHalf,mean=0,sd=1)
	
	print(c("Quantile 1-alpha: ",quantile))
	
	#grafic generation code
	
	curve(dnorm(x,mean=0,sd=1),from=-4, to=4, axes=FALSE, xlab="", ylab="Densità", main="Densità normale intervallo di confidenza 1-alpha per μ con varianza nota")
	axis(1,c(-4,-quantile,0,quantile,4), c("",abbreviate(-quantile,5),0,abbreviate(quantile,4),""))
	vals<-seq(-3,-quantile, length =100)
	x1<-c(-4,vals ,-quantile,-4)
	y1<-c(0,dnorm(vals),0,0)
	polygon (x1,y1,density =20,angle=45)
	
	vals<-seq(quantile,4, length =100)
	x1<-c(quantile,vals ,4,1)
	y1<-c(0,dnorm(vals),0,0)
	
	polygon (x1,y1,density =20,angle=45)
	abline(h=0)
	text(-3.2,0.03, expression (alpha/2))
	text(-3.2,0.05, "Regione di rifiuto")
	text(3.2,0.05, "Regione di rifiuto")
	text(3.2,0.03, expression (alpha/2))
	text(0,0.03, expression (1-alpha))
	text(0,0.1, "Regione di accettazione")
	
}

plot.studentConfidence1 <- function(population,oneMinusAlpha=0.95){
	
	n <- length(population)
	alpha <- 1- oneMinusAlpha
	
	alphaHalf <- alpha/2
	
	print(alphaHalf)
	
	#left
	print(c("Left limit: ",mean(population)-qt(1-alphaHalf,df=n-1)*sd(population)/sqrt(n)))
	
	#rigth
	print(c("Right limit: ",mean(population)+qt(1-alphaHalf,df=n-1)*sd(population)/sqrt(n)))

	z <- qt(1-alphaHalf,df=n-1)
	
	print(c("Quantile: ",z))
	
	#graphic
	curve(dt(x, df=n-1), from=-4, to=4, axes=FALSE, xlab="", ylab="Densità", main="Densità di Student con n-1 gradi di libertà intervallo di confidenza 1-alpha=0.95 per μ con varianza non nota ")
	
	axis(1,c(-4,-z,0,z,4), c("",abbreviate(-z,5),0,abbreviate(z,4),""))
	
	vals<-seq(-3,-z, length =100)
	x1<-c(-4,vals ,-z,-4)
	
	y1<-c(0,dt(vals,n-1),0,0)
	
	polygon (x1,y1,density =20,angle=45)
	
	vals<-seq(z,4, length =100)
	
	x1<-c(z,vals ,4,1)
    y1<-c(0,dt(vals,n-1),0,0)
    polygon (x1,y1,density =20,angle=45)
    abline(h=0)
    text(0,0.03, expression (1-alpha))
    text(0,0.05, "Regione di accettazione")
    text(2.5,0.06, expression(alpha/2))
    text(2.5,0.09, "Regione di rifiuto")
    text(-2.5,0.06, expression(alpha/2))
    text(-2.5,0.09, "Regione di rifiuto")

}

plot.studentConfidence2<- function(population,oneMinusAlpha=0.99){
	
	n <- length(population)
	alpha <- 1- oneMinusAlpha
	
	alphaHalf <- alpha/2
	
	print(alphaHalf)
	
	#left
	print(c("Left limit: ",mean(population)-qt(1-alphaHalf,df=n-1)*sd(population)/sqrt(n)))
	
	#rigth
	print(c("Right limit: ",mean(population)+qt(1-alphaHalf,df=n-1)*sd(population)/sqrt(n)))

	z <- qt(1-alphaHalf,df=n-1)
	
	print(c("Quantile: ",z))
	
	#graphic
	
	curve(dt(x, df=n-1), from=-4, to=4, axes=FALSE, xlab="", ylab="Densità", main="Densità di Student con n-1 gradi di libertà intervallo di confidenza 1-alpha=0.95 per μ con varianza non nota ")
	
	axis(1,c(-4,-z,0,z,4), c("",abbreviate(-z,5),0,abbreviate(z,4),""))
	
	vals<-seq(-3,-z, length =100)
	x1<-c(-4,vals ,-z,-4)
	
	y1<-c(0,dt(vals,n-1),0,0)
	
	polygon (x1,y1,density =20,angle=45)
	
	vals<-seq(z,4, length =100)
	
	x1<-c(z,vals ,4,1)
    y1<-c(0,dt(vals,n-1),0,0)
    polygon (x1,y1,density =20,angle=45)
    abline(h=0)

    text(3.0,0.03,expression(alpha/2))
    text(3.0,0.05,"Regione di rifiuto")
    text(-3.0,0.05,"Regione di rifiuto")
    text(-3.0,0.03,expression(alpha/2))
    text(0,0.03, expression (1-alpha))
    text(0,0.05, "Regione di accettazione")


}

																									#0.01            #0.05
plot.confidenceComparison <-function(population,myMean=300,MySd=25,myFrom=200,myTo=400, alphaHalf1=0.005,  alphaHalf2=0.025,sigma){
	
	curve(dnorm(x,mean= myMean,sd= MySd),from=myFrom,to=myTo,ylim=c(0,0.03), ylab="Densità",xlab="x",main="Confronto fra i due intervalli di confidenza")
	
	arrows(301,0.025,310,0.025,code=1,length = 0.10)
	text(315,0.025,"Media")
	abline(v=myMean)
	
	#first
	leftLimit <- mean(population)-qnorm(1-alphaHalf1,mean=0,sd=1)*sigma/sqrt(length(population))
	rightLimit <- mean(population)+qnorm(1-alphaHalf1,mean=0,sd=1)*sigma/sqrt(length(population))
	vals<-seq(leftLimit, rightLimit, length =50)
	x1<-c(leftLimit, vals, rightLimit)
	y1<-c(0,dnorm(vals, mean= myMean, sd= MySd),0)
	polygon (x1,y1,density=30,angle=45,col='blue')
	
	xLegend <-c(350,370,370,350,350)
	yLegend <-c(0.025,0.025,0.03,0.03,0.025)
	polygon (xLegend, yLegend,density=50,angle=45,col='blue')	
	text(382, 0.0275,"Stima a=0.01")
	
	
	#second
	leftLimit <- mean(population)-qnorm(1-alphaHalf2,mean=0,sd=1)*sd(population)/sqrt(length(population))
	rightLimit <- mean(population)+qnorm(1-alphaHalf2,mean=0,sd=1)*sd(population)/sqrt(length(population))
	vals<-seq(leftLimit, rightLimit, length =50)
	x1<-c(leftLimit, vals, rightLimit)
	y1<-c(0,dnorm(vals, mean= myMean, sd= MySd),0)
	polygon (x1,y1,density=50,angle=-45,col='red')

	xLegend <-c(350,370,370,350,350)
	yLegend <-c(0.02,0.02,0.025,0.025,0.02)
	polygon (xLegend, yLegend,density=50,angle=-45,col='red')
	text(382, 0.0225,"Stima a=0.05")
	
}

plot.confidenceComparisonStudent <-function(population,myMean=300,MySd=25,myFrom=200,myTo=400, alphaHalf1=0.005,  alphaHalf2=0.025){
	
	curve(dnorm(x,mean= myMean,sd= MySd),from=myFrom,to=myTo,ylim=c(0,0.03), ylab="Densità",xlab="x",main="Confronto fra i due intervalli di confidenza")
	
	arrows(301,0.025,310,0.025,code=1,length = 0.10)
	text(315,0.025,"Media")
	abline(v=myMean)
	
	n <- length(population)
	
	#first
	leftLimit <- mean(population)-qt(1-alphaHalf1,df=n-1)*sd(population)/sqrt(n)
	rightLimit <- mean(population)+qt(1-alphaHalf1,df=n-1)*sd(population)/sqrt(n)
	vals<-seq(leftLimit, rightLimit, length =50)
	x1<-c(leftLimit, vals, rightLimit)
	y1<-c(0,dnorm(vals, mean= myMean, sd= MySd),0)
	polygon (x1,y1,density=30,angle=45,col='green')
	
	xLegend <-c(350,370,370,350,350)
	yLegend <-c(0.025,0.025,0.03,0.03,0.025)
	polygon (xLegend, yLegend,density=50,angle=45,col='green')	
	text(382, 0.0275,"Stima a=0.01")
	
	
	#second
	leftLimit <- mean(population)-qt(1-alphaHalf2,df=n-1)*sd(population)/sqrt(n)
	rightLimit <- mean(population)+qt(1-alphaHalf2,df=n-1)*sd(population)/sqrt(n)
	vals<-seq(leftLimit, rightLimit, length =50)
	x1<-c(leftLimit, vals, rightLimit)
	y1<-c(0,dnorm(vals, mean= myMean, sd= MySd),0)
	polygon (x1,y1,density=50,angle=-45,col='violet')

	xLegend <-c(350,370,370,350,350)
	yLegend <-c(0.02,0.02,0.025,0.025,0.02)
	polygon (xLegend, yLegend,density=50,angle=-45,col='violet')
	text(382, 0.0225,"Stima a=0.05")
	
}

plot.chiQConfidence1 <- function(population,mymean,myalpha){
	
	n<-length(population)
	x<-population
	alpha<-myalpha
	varianza<-var(x)
	print(c("Varianza: ",varianza))
	
	print(c("Gradi di libertà: ",n))
	z1<-qchisq(1-alpha,df=n)
	print(c("Z1",1-alpha,z1))
	z2<-qchisq(alpha,df=n)
	print(c("Z2",alpha,z2))
	print(c("Limit1 ",1-alpha,(((n-1)*varianza)+n*((mean(x)-mymean)**2))/qchisq(1-alpha,df=n)))
	print(c("Limit2 ", alpha,(((n-1)*varianza)+n*((mean(x)-mymean)**2))/qchisq(alpha,df=n)))
	curve(dchisq(x, df=n), from=z2-z2/10, to=z1+z1/10, axes=FALSE,xlab="",ylab="Densità",main="Densità chi-quadrato con n-1 gradi di libertà intervallo di confidenza per la varianza con μ non noto,1-alpha=0.95")
	axis(1,c(z2-z2/10,z2,n-2,z1,z1+z1/10),c("",abbreviate(z2,minl=5),n-2,abbreviate(z1,minl=5),""))
	vals<-seq(z2-z2/10,z2, length =100)
	x1<-c(z2-z2/10,vals ,z2,0)
	y1<-c(0,dchisq(vals,n),0,0)
	polygon (x1,y1,density =20,angle=45)
	vals<-seq(z1,z1+z1/10, length =100)
	x1<-c(z1,vals ,z1+z1/10,1)
	y1<-c(0,dchisq(vals,n),0,0)
	polygon (x1,y1,density =20,angle=45)
	abline(h=0)
	text(z1+3.5,0.004,expression(alpha/2))
	text(z1+3.5,0.0055,"Regione di rifiuto")
	text(98,0.003,expression(1-alpha))
	text(98,0.0045,"Regione di accettazione")
	text(z2-3.5,0.0055,"Regione di rifiuto")
	text(z2-3.5,0.004,expression(alpha/2))

}

plot.chiQConfidence2 <- function(population,mymean,myalpha,epsilon,oneminusalpha,pop){
	
	n<-length(population)
	x<-population
	alpha<-myalpha
	varianza<-var(x)
	print(c("Varianza: ",varianza))
	
	print(c("Gradi di libertà: ",n))
	z1<-qchisq(1-alpha,df=n)
	print(c("Z1",1-alpha,z1))
	z2<-qchisq(alpha,df=n)
	print(c("Z2",alpha,z2))
	print(c("Limit1 ",1-alpha,(((n-1)*varianza)+n*((mean(x)-mymean)**2))/qchisq(1-alpha,df=n)))
	print(c("Limit2 ", alpha,(((n-1)*varianza)+n*((mean(x)-mymean)**2))/qchisq(alpha,df=n)))
	curve(dchisq(x, df=n), from=z2-z2/10, to=z1+z1/10, axes=FALSE,xlab="",ylab="Densità",main=c("Densità chi-quadrato con n-1 gradi di libertà intervallo di confidenza per la varianza con μ non noto,1-alpha=",oneminusalpha,pop))
	axis(1,c(z2-z2/10,z2,n-2,z1,z1+z1/10),c("",abbreviate(z2,minl=5),n-2,abbreviate(z1,minl=5),""))
	vals<-seq(z2-z2/10,z2, length =100)
	x1<-c(z2-z2/10,vals ,z2,0)
	y1<-c(0,dchisq(vals,n),0,0)
	text(z2-3.5,max(y1)+epsilon,"Regione di rifiuto",cex=0.5)
	text(z2-3.5,max(y1)+3*epsilon,expression(alpha/2),cex=0.5)
	
	
	polygon (x1,y1,density =20,angle=45)
	vals<-seq(z1,z1+z1/10, length =100)
	x1<-c(z1,vals ,z1+z1/10,1)
	y1<-c(0,dchisq(vals,n),0,0)
	
	text(z1+3.5,max(y1)+epsilon,"Regione di rifiuto",cex=0.5)
	text(z1+3.5,max(y1)+3*epsilon,expression(alpha/2),cex=0.5)
	
	
	polygon (x1,y1,density =20,angle=45)
	abline(h=0)
	
	text(n-2,max(y1)+2*epsilon,"Regione di accettazione",cex=0.5)
	text(n-2,max(y1)+3*epsilon,expression(1-alpha),cex=0.5)

}

plot.chiQConfidence3 <- function(population,myalpha){
	
   n<-length(population)
   x<-population
   alpha<-myalpha
   varianza<-var(x)
   z1<-qchisq(1-alpha,df=n-1)
   print(c("Z1",1-alpha,z1))
   z2<-qchisq(alpha,df=n-1)
   print(c("Z2",alpha,z2))
   
   print(c("Left",((n-1)*varianza)/qchisq(1-alpha,df=n-1)))
   
   print(c("Right",((n-1)*varianza)/qchisq(alpha,df=n-1)))
   
   curve(dchisq(x, df=n), from=z2-                      z2/10,to=z1+z1/10,axes=FALSE,xlab="",ylab="Densità",main="Densità chi quadrato con n-1 gradi di libertà intervallo di confidenza per la varianza con μ non noto, 1-alpha=0.95")
   axis(1,c(z2-z2/10,z2,n-2,z1,z1+z1/10),c("",abbreviate(z2,minl=5),n-2,abbreviate(z1,minl=5),""))
   vals<-seq(z2-z2/10,z2, length =100)
   x1<-c(z2-z2/10,vals ,z2,0)
   y1<-c(0,dchisq(vals,n),0,0)
   polygon (x1,y1,density =20,angle=45)
   vals<-seq(z1,z1+z1/10, length =100)
   x1<-c(z1,vals ,z1+z1/10,1)
   y1<-c(0,dchisq(vals,n),0,0)
   polygon (x1,y1,density =20,angle=45)
   abline(h=0)
   text(z1+3.5,0.004,expression(alpha/2))
   text(z1+3.5,0.0055,"Regione di rifiuto")
   text(98,0.003,expression(1-alpha))
   text(98,0.0045,"Regione di accettazione")
   text(z2-3.5,0.0055,"Regione di rifiuto")
   text(z2-3.5,0.004,expression(alpha/2))

}

homogeneity <- function(dataframe,metodo,myCut=3){
	
	#NB quando in un cluster ci sta un solo elemento da problemi
	
	n <-nrow(dataframe)
	#misura di non omogenità statistica totale 
	
	#In R utilizzando le funzione apply(X, 2, mean), apply(X, 2, var) e apply(X, 2, sd) è possibile calcolare la media campionaria, la varianza campionaria e la deviazione standard campionaria delle colonne di una matrice X 
	trHI <- (n-1)*sum(apply(dataframe,2,var))
 
	print(c("Misura di omogeneità statistica totale: ",trHI))
	
	#misure di non omogeneità tra i gruppi
	
	dati <- scale(dataframe)
	matriceDistanze <- dist(dati,method="euclidean", diag=TRUE, upper=TRUE)

	if (metodo=="centroid")
		matriceDistanze <- matriceDistanze^2
	
	tree <- hclust(matriceDistanze,method=metodo)
	taglio <-cutree(tree,k=myCut,h=NULL)
	num <-table(taglio)
	tagliolist<-list(taglio)
	
	#Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form - by sono le suddivisioni e l'argomento
	#successivo è la funzione (nel nostro caso la varianza)
	agvar <- aggregate(dataframe,tagliolist,var)[,-1]
	#misura di omogeneità dei vari gruppi in base a quante partizioni sono le partizione
	sum<-0
		
	for(i in 1:(myCut)){
		
		value <- (num[[i]]-1)*sum(agvar[i,])
		
		if(!is.na(value))
			sum <- sum +value 
		print(c("Misura omogeneità cluster",value))
	}

	print(c("Somma (within): ",sum))
	between <- trHI-sum
	print(c("Between: ",between))
	print(c("Between/total", between/trHI))
	
}

