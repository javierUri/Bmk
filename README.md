# Bmk
#busqueda de bmk


require(quantmod)
require(PerformanceAnalytics)
require(timeSeries)
require(fPortfolio)
require(xts)

setwd("C:/Users/JavierUriguen/Desktop/Javier/R-Opp Diffs/Opp Divs Bmk")

data=read.zoo("Data.csv",format="%d/%m/%Y",sep=",",dec=".",header=TRUE,FUN=as.Date)






RollCorr<-function (Ra, Rb, width = 12, xaxis = TRUE, legend.loc = NULL, 
    colorset = (1:12), ..., fill = NA) 
{
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)
    for (column.a in 1:columns.a) {
        for (column.b in 1:columns.b) {
            merged.assets = merge(Ra[, column.a, drop = FALSE], 
                Rb[, column.b, drop = FALSE])
            column.calc = rollapply(na.omit(merged.assets[, , 
                drop = FALSE]), width = width, FUN = function(x) cor(x[, 
                1, drop = FALSE], x[, 2, drop = FALSE]), by = 1, 
                by.column = FALSE, fill = fill, align = "right")
            column.calc.tmp = xts(column.calc)
            colnames(column.calc.tmp) = paste(columnnames.a[column.a], 
                columnnames.b[column.b], sep = " to ")
            column.calc = xts(column.calc.tmp, order.by = time(column.calc))
            if (column.a == 1 & column.b == 1) 
                Result.calc = column.calc
            else Result.calc = merge(Result.calc, column.calc)
        }
    }
    print(Result.calc, xaxis = xaxis, colorset = colorset, 
        legend.loc = legend.loc, ylim = c(-1, 1), ...)
}



mat6<-data
for(i in 1:ncol(data)){
	mat6[,i]<-RollCorr(data[,17],data[,i],6)
}


mat12<-data
for(i in 1:ncol(data)){
	mat12[,i]<-RollCorr(data[,17],data[,i],12)
}


mat18<-data
for(i in 1:ncol(data)){
	mat18[,i]<-RollCorr(data[,17],data[,i],18)
}


mat<-merge(mat6,mat12,mat18)


#write.table(mat, file="RollCorrTab.csv",sep=",",dec=".")




matdat<-matrix("NA",ncol(mat),4)
colnames(matdat)<-c("average","Max","Min","Nr Day over 50")
rownames(matdat)<-colnames(mat)

for(i in 1:ncol(mat)){
	matdat[i,1]<-mean(na.omit(mat[,i]))
	matdat[i,2]<-max(na.omit(mat[,i]))
	matdat[i,3]<-min(na.omit(mat[,i]))
	matdat[i,4]<-count(subset(mat[,i],mat[,i]>0.5))
}



##############
#matriz con medias de todos los Bmks (todos con todos)

x=1
for(i in 1:ncol(data)){
	for(j in 1:ncol(data)){
		dum[,x]<- (data[,i]+data[,j])/2
		nombres <- paste(colnames(data[i]),"&",colnames(data[j]))
		x=x+1
	}
}



x=1
for(i in 1:ncol(data)){
	for(j in 1:ncol(data)){
		ca[x]<-paste(i,"&",j)
		x<-x+1
	}
}


colnames(dum)<-ca


#################
# calculos de las correlaciones Div Growth con la matriz de medias

dum6<-dum
for(i in 1:ncol(dum)){
	dum6[,i]<-RollCorr(data[,17],dum[,i],6)
}


dum12<-dum
for(i in 1:ncol(dum)){
	dum12[,i]<-RollCorr(data[,17],dum[,i],12)
}

dum18<-dum
for(i in 1:ncol(dum)){
	dum18[,i]<-RollCorr(data[,17],dum[,i],18)
}

dumCorr<-merge(dum6,dum12,dum18)


#################
# resumen de los datos de correlacion entre matriz de medias de retornos y Div Growth


matdum<-matrix("NA",ncol(dumCorr),4)
colnames(matdum)<-c("average","Max","Min","Nr. Months over 0.5")
rownames(matdum)<-colnames(dumCorr)



for(i in 1:nrow(matdum)){
	matdum[i,1]<-mean(na.omit(dumCorr[,i]))
	matdum[i,2]<-max(na.omit(dumCorr[,i]))
	matdum[i,3]<-min(na.omit(dumCorr[,i]))
	matdum[i,4]<-count(subset(dumCorr[,i],dumCorr[,i]>0.5))
}

# datos son los datos originales para el estudio
# mat es la matiz con las TimeSeries de las correlaciones de 6,12,18
# matdat resumen den Correlaciones
# dum es una matriz de perf de 50% / 50% contra todos
# dumCorr las correlaciones de dum frente Div Growth
# matdum resumen den Correlaciones



write.table(data, file="datos.csv",sep=",",dec=".")
write.table(mat, file="RollCorr1.csv",sep=",",dec=".")
write.table(matdum, file="RollCorrResum.csv",sep=",",dec=".")
write.table(dum, file="Mix.csv",sep=",",dec=".")
write.table(dumCorr, file="RollCorr2.csv",sep=",",dec=".")




