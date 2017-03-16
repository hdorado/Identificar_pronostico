
#Predicción de cluster a pronóstico
#Hugo Andrés Dorado
#15-03-2017

source("fun_partir_pronostico.R")

setwd("D:/GIT_HUB_REPOSITORIOS/Identificar_pronostico")

pronostico0 <- read.csv("CLUSTERING_FILES/escenario_prom_forecast.csv")

#126 dias yopal

nrow(pronostico0)

vars       <- c("tmax","tmin","precip","srad")
pronostico <- pronostico0
diasCiclo  <- 126

localityForecast <- splitForecast(vars,pronostico,diasCiclo)

#Utilizar vecino mas cercano para hacer el pronóstico

load("CLUSTERING_FILES/listClimatEvent.RData")

eventListNts <- lapply(evenN,function(x){x[-which(names(x)=="RHUM")]})

kNeigh <-30

membAll <- read.csv("CLUSTERING_FILES/Yopal_cluster.csv")[,"Cluster"]

#Actualizacion de parametros

allweather <- do.call(rbind,evenF)[c("ESOL","LOG10RAIN","TMAX","TMIN")]

maxM <- apply(allweather,2,max)
minM <- apply(allweather,2,min)

parClim <- parClim[-which(names(parClim)=="RHUM")]

parClim <- rbind(parClim,maxM=maxM,minM=minM)

ClasifCaCluster <- list()

for(i in 1:length(localityForecast) ){
    
    forecast <- localityForecast[[i]]
    
    names(forecast) <- c("TMAX","TMIN","RAIN","ESOL")
    
    forecast$ESOL <- forecast$ESOL*100/4.8
    
    forecast$LOG10RAIN <- forecast$RAIN
    
    forecast$LOG10RAIN[forecast$LOG10RAIN==0] <- 0.05
    
    forecast$LOG10RAIN <- log10(forecast$LOG10RAIN)
    
    #lapply(evenF,summary)
    
    names(forecast) <- c("TMAX","TMIN","RAIN","ESOL","LOG10RAIN")
    
   
    ClasifCaCluster[[i]] <- pronosticProces(forecast,eventListNts,kNeigh,membAll,parClim,
                    stand="min_max",vars=c("ESOL","LOG10RAIN","TMAX","TMIN"))
    
}

mejoresCluster <- do.call(rbind,
        lapply(ClasifCaCluster,function(x){ x[which.max(x$Freq)[1],] })
    )

date <- as.Date(as.Date("2017-02-17"):as.Date("2017-07-17"), origin = "1970-01-01")

mejoresCluster <- data.frame(Fecha = date[1:nrow(mejoresCluster)] ,mejoresCluster)

save(ClasifCaCluster,mejoresCluster,file="ClasificacionYopal_03040506_2017.RData")

write.csv(mejoresCluster,file="ClasificacionYopal_03040506_2017.csv")
