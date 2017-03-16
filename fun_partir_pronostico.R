
#Funciones para partir pronostico por listas 
#Hugo Andres Dorado B.
#15/03/2017

rm(list=ls())

splitForecast <- function(vars,pronostico,diasCiclo){
    diasPronostico <- nrow(pronostico)
    
    patronesPronostico <- list()
    
    pron.vars <- pronostico[vars]
    
    for(i in 1:(diasPronostico-diasCiclo+1)){
        var  <- i:diasPronostico
        extr <- var[1:diasCiclo]
        patronesPronostico[[i]] <- pron.vars[extr,]
    }
    
    patronesPronostico
}


pronosticProces <- function(forecast,eventListNts,kNeigh,membAll,parClim,stand="min_max",vars=c("ESOL","LOG10RAIN","TMAX","TMIN"))
{  
    require(dtw)
    
    forecast1    <- as.matrix(forecast[vars])
    
    if(stand=="min_max"){
        
        maxM <- as.numeric(parClim[3,])
        minM <- as.numeric(parClim[4,])
        
        forecast1 <- (forecast1-matrix(minM,nrow(forecast1),ncol(forecast1),byrow=T))*matrix(1/(maxM-minM),nrow(forecast1),ncol(forecast1),byrow=T)
        
        
    }else if(stand=="norm"){
        meanValM <- as.numeric(parClim[1,])
        sdValM   <- as.numeric(parClim[2,])
        
        forecast1 <- as.data.frame((forecast1-matrix(meanValM,nrow(forecast1),ncol(forecast1),byrow=T))*(1/(matrix(sdValM,nrow(forecast1),ncol(forecast1),byrow=T))))
    }else{
        
        return(cat(paste("No se conoce el metodo de estandarización",stand)))
    }
    
    
    lForecast <- forecast1
    
    clusterForecast <- list(0)
    
    k         <- kNeigh
    newTs     <- lForecast
    dist       <- 0
    
    for(j in 1:length(eventListNts)){ dist[j] <- dtw(newTs,eventListNts[[j]])$dist}
    s <- sort(as.vector(dist), index.return=TRUE)
    clusterForecast <-table(membAll[s$ix[1:k]])
    #print(paste("nearest neighbor scenario: ",i))
    #print(table(membAll[s$ix[1:k]]))
    
    tablaFinal <- as.data.frame(clusterForecast)
    names(tablaFinal) <- c("Cluster","Freq")
    tablaFinal
}

