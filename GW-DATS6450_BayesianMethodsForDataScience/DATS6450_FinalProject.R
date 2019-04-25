# Bayesian Method for Data Science (DATS 6450, Fall 2018)
# Final Project
# Author(s): Will Noone

# Preliminaries ------------------------------------------------

# Optional generic preliminaries:
setwd("D:/Graduate Programs/GW MS Data Science/DATS 6450 - Bayesian Methods/Project")
graphics.off() 
rm(list=ls())  

#Load Libraries
library(dplyr)
library(tibble)
library(tidyverse)

#Load Utilities
source("DBDA2E-utilities.R")

# Analysis #1 Files Settings & Data Load -----------------------

# Optional: Specify filename root and graphical format for saving output.
fileNameRoot = "Output/Analysis1/Hierarchy-POST-" 
graphFileType = "pdf" 

# Read the data 
myData1 <- read.csv(file="Data/Analysis1_data.csv") 

# Analysis #1 Functions ------------------------------------------

genMCMC = function( data , zName="z" , NName="N" , sName="s" , cName="c" ,
                    numSavedSteps=50000 , saveName=NULL , thinSteps=1 ,
                    runjagsMethod=runjagsMethodDefault , 
                    nChains=nChainsDefault ) { 
  require(rjags)
  require(runjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  # N.B.: This function expects the data to be a data frame, 
  # with one component z being a vector of integer # successes,
  # one component N being a vector of integer # attempts,
  # one component s being a factor of subject identifiers,
  # and one component c being a factor of category identifiers, with
  # subjects nested in categories.
  z = data[[zName]]
  N = data[[NName]]
  s = data[[sName]]
  c = data[[cName]]
  Nsubj = length(unique(s))
  Ncat =  length(unique(c))
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    z = z ,
    N = N ,
    c = as.numeric(c) , # c in JAGS is numeric, in R is possibly factor
    Nsubj = Nsubj ,
    Ncat = Ncat
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
    model {
      for ( sIdx in 1:Nsubj ) {
        z[sIdx] ~ dbin( theta[sIdx] , N[sIdx] )
        theta[sIdx] ~ dbeta( omega[c[sIdx]]*(kappa[c[sIdx]]-2)+1 , 
                             (1-omega[c[sIdx]])*(kappa[c[sIdx]]-2)+1 ) 
      }
      for ( cIdx in 1:Ncat ) {
        omega[cIdx] ~ dbeta( omegaO*(kappaO-2)+1 , 
                             (1-omegaO)*(kappaO-2)+1 )
        kappa[cIdx] <- kappaMinusTwo[cIdx] + 2
        kappaMinusTwo[cIdx] ~ dgamma( 0.01 , 0.01 ) # mean=1 , sd=10 (generic vague)
      }
      omegaO ~ dbeta( 1.0 , 1.0 ) 
      #omegaO ~ dbeta( 1.025 , 1.075 ) # mode=0.25 , concentration=2.1
      kappaO <- kappaMinusTwoO + 2
      kappaMinusTwoO ~ dgamma( 0.01 , 0.01 )  # mean=1 , sd=10 (generic vague)
      #kappaMinusTwoO ~ dgamma( 1.01005 , 0.01005012 )  # mode=1 , sd=100
      #kappaMinusTwoO ~ dgamma( 1.105125 , 0.1051249 )  # mode=1 , sd=10
      #kappaMinusTwoO ~ dgamma( 1.105125 , 0.01051249 )  # mode=10 , sd=100
    }
  
  " # close quote for modelString
  writeLines( modelString , con="Specifications/Analysis1/TEMPmodel.txt" )
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Initial values of MCMC chains based on data:
  initsList = function() {
    thetaInit = rep(NA,Nsubj)
    for ( sIdx in 1:Nsubj ) { # for each subject
      resampledZ = rbinom(1, size=N[sIdx] , prob=z[sIdx]/N[sIdx] )
      thetaInit[sIdx] = resampledZ/N[sIdx]
    }
    thetaInit = 0.001+0.998*thetaInit # keep away from 0,1    
    kappaInit = 100 # lazy, start high and let burn-in find better value
    return( list( theta=thetaInit , 
                  omega=aggregate(thetaInit,by=list(c),FUN=mean)$x ,
                  omegaO=mean(thetaInit) ,
                  kappaMinusTwo=rep(kappaInit-2,Ncat) ,
                  kappaMinusTwoO=kappaInit-2 ) )
  }
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "theta","omega","kappa","omegaO","kappaO") 
  adaptSteps = 500             # Number of steps to adapt the samplers
  burnInSteps = 500            # Number of steps to burn-in the chains
  
  useRunjags = TRUE
  if ( useRunjags ) {
    runJagsOut <- run.jags( method=runjagsMethod ,
                            model="Specifications/Analysis1/TEMPmodel.txt" , 
                            monitor=parameters , 
                            data=dataList ,  
                            inits=initsList , 
                            n.chains=nChains ,
                            adapt=adaptSteps ,
                            burnin=burnInSteps , 
                            sample=ceiling(numSavedSteps/nChains) ,
                            thin=thinSteps ,
                            summarise=FALSE ,
                            plots=FALSE )
    codaSamples = as.mcmc.list( runJagsOut )
  } else {
    # Create, initialize, and adapt the model:
    jagsModel = jags.model( "Specifications/Analysis1/TEMPmodel.txt", 
                            data=dataList , 
                            inits=initsList , 
                            n.chains=nChains , 
                            n.adapt=adaptSteps )
    # Burn-in:
    cat( "Burning in the MCMC chain...\n" )
    update( jagsModel , n.iter=burnInSteps )
    # The saved MCMC chain:
    cat( "Sampling final MCMC chain...\n" )
    codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                                n.iter=ceiling(numSavedSteps*thinSteps/nChains), 
                                thin=thinSteps )
  }  
  
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
} # end function



smryMCMC = function(  codaSamples , compVal=0.5 , rope=NULL , 
                      diffSVec=NULL , diffCVec=NULL , 
                      compValDiff=0.0 , ropeDiff=NULL , 
                      saveName=NULL ) {
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  summaryInfo = NULL
  rowIdx = 0
  # omega:
  for ( parName in grep("omega",colnames(mcmcMat),value=TRUE) ) {
    summaryInfo = rbind( summaryInfo , 
                         summarizePost( mcmcMat[,parName] ,
                                        compVal=compVal , ROPE=rope ) )
    rowIdx = rowIdx+1
    rownames(summaryInfo)[rowIdx] = parName
  }
  # kappa:
  for ( parName in grep("kappa",colnames(mcmcMat),value=TRUE) ) {
    summaryInfo = rbind( summaryInfo , 
                         summarizePost( mcmcMat[,parName] ,
                                        compVal=NULL , ROPE=NULL ) )
    rowIdx = rowIdx+1
    rownames(summaryInfo)[rowIdx] = parName
  }
  # theta:
  for ( parName in grep("theta",colnames(mcmcMat),value=TRUE) ) {
    summaryInfo = rbind( summaryInfo , 
                         summarizePost( mcmcMat[,parName] ,
                                        compVal=compVal , ROPE=rope ) )
    rowIdx = rowIdx+1
    rownames(summaryInfo)[rowIdx] = parName
  }
  # differences of theta's:
  if ( !is.null(diffSVec) ) {
    Nidx = length(diffSVec)
    for ( t1Idx in 1:(Nidx-1) ) {
      for ( t2Idx in (t1Idx+1):Nidx ) {
        parName1 = paste0("theta[",diffSVec[t1Idx],"]")
        parName2 = paste0("theta[",diffSVec[t2Idx],"]")
        summaryInfo = rbind( summaryInfo , 
                             summarizePost( mcmcMat[,parName1]-mcmcMat[,parName2] ,
                                            compVal=compValDiff , ROPE=ropeDiff ) )
        rowIdx = rowIdx+1
        rownames(summaryInfo)[rowIdx] = paste0(parName1,"-",parName2)
      }
    }
  }
  # differences of omega's:
  if ( !is.null(diffCVec) ) {
    Nidx = length(diffCVec)
    for ( t1Idx in 1:(Nidx-1) ) {
      for ( t2Idx in (t1Idx+1):Nidx ) {
        parName1 = paste0("omega[",diffCVec[t1Idx],"]")
        parName2 = paste0("omega[",diffCVec[t2Idx],"]")
        summaryInfo = rbind( summaryInfo , 
                             summarizePost( mcmcMat[,parName1]-mcmcMat[,parName2] ,
                                            compVal=compValDiff , ROPE=ropeDiff ) )
        rowIdx = rowIdx+1
        rownames(summaryInfo)[rowIdx] = paste0(parName1,"-",parName2)
      }
    }
  }
  # save:
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  show( summaryInfo )
  return( summaryInfo )
} # end function


plotMCMC = function( codaSamples , 
                     data , zName="z" , NName="N" , sName="s" , cName="c" ,
                     compVal=0.5 , rope=NULL , 
                     diffSList=NULL , diffCList=NULL , 
                     compValDiff=0.0 , ropeDiff=NULL , 
                     saveName=NULL , saveType="jpg" ) {
  #-----------------------------------------------------------------------------
  # N.B.: This function expects the data to be a data frame, 
  # with one component z being a vector of integer # successes,
  # one component N being a vector of integer # attempts,
  # one component s being a factor of subject identifiers,
  # and one component c being a factor of category identifiers, with
  # subjects nested in categories.
  z = data[[zName]]
  N = data[[NName]]
  s = data[[sName]]
  c = data[[cName]]
  Nsubj = length(unique(s))
  Ncat =  length(unique(c))
  # Now plot the posterior:
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  
  # kappa:
  parNames = sort(grep("kappa",colnames(mcmcMat),value=TRUE))
  nPanels = length(parNames)
  nCols = 4
  nRows = ceiling(nPanels/nCols)
  openGraph(width=2.5*nCols,height=2.0*nRows)
  par( mfcol=c(nRows,nCols) )
  par( mar=c(3.5,1,3.5,1) , mgp=c(2.0,0.7,0) )
  #xLim = range( mcmcMat[,parNames] )
  xLim=quantile(mcmcMat[,parNames],probs=c(0.000,0.995))
  mainLab = c(levels(myData1[[cName]]),"Overall")
  mainIdx = 0
  for ( parName in parNames ) {
    mainIdx = mainIdx+1
    postInfo = plotPost( mcmcMat[,parName] , compVal=compVal , ROPE=rope ,
                         xlab=bquote(.(parName)) , cex.lab=1.25 , 
                         main=mainLab[mainIdx] , cex.main=1.5 ,
                         xlim=xLim , border="skyblue" )
  }  
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"Kappa",sep=""), type=saveType)
  }
  
  # omega:
  parNames = sort(grep("omega",colnames(mcmcMat),value=TRUE))
  nPanels = length(parNames)
  nCols = 4
  nRows = ceiling(nPanels/nCols)
  openGraph(width=2.5*nCols,height=2.0*nRows)
  par( mfcol=c(nRows,nCols) )
  par( mar=c(3.5,1,3.5,1) , mgp=c(2.0,0.7,0) )
  #xLim = range( mcmcMat[,parNames] )
  xLim=quantile(mcmcMat[,parNames],probs=c(0.001,0.999))
  mainLab = c(levels(myData1[[cName]]),"Overall")
  mainIdx = 0
  for ( parName in parNames ) {
    mainIdx = mainIdx+1
    postInfo = plotPost( mcmcMat[,parName] , compVal=compVal , ROPE=rope ,
                         xlab=bquote(.(parName)) , cex.lab=1.25 , 
                         main=mainLab[mainIdx] , cex.main=1.5 ,
                         xlim=xLim , border="skyblue" )
  }  
  if ( !is.null(saveName) ) {
    saveGraph( file=paste(saveName,"Omega",sep=""), type=saveType)
  }
  
  # Plot individual omega's and differences:
  if ( !is.null(diffCList) ) {
    for ( compIdx in 1:length(diffCList) ) {
      diffCVec = diffCList[[compIdx]]
      Nidx = length(diffCVec)
      temp=NULL
      for ( i in 1:Nidx ) {
        temp = c( temp , which(levels(myData1[[cName]])==diffCVec[i]) )
      }
      diffCVec = temp
      openGraph(width=2.5*Nidx,height=2.0*Nidx)
      par( mfrow=c(Nidx,Nidx) )
      xLim = range(c( compVal, rope,
                      mcmcMat[,paste0("omega[",diffCVec,"]")] ))
      for ( t1Idx in 1:Nidx ) {
        for ( t2Idx in 1:Nidx ) {
          parName1 = paste0("omega[",diffCVec[t1Idx],"]")
          parName2 = paste0("omega[",diffCVec[t2Idx],"]")
          if ( t1Idx > t2Idx) {  
            # plot.new() # empty plot, advance to next
            par( mar=c(3,3,3,1) , mgp=c(2.0,0.7,0) , pty="s" )
            nToPlot = 700
            ptIdx = round(seq(1,chainLength,length=nToPlot))
            plot ( mcmcMat[ptIdx,parName2] , mcmcMat[ptIdx,parName1] , 
                   cex.main=1.25 , cex.lab=1.25 , 
                   xlab=levels(myData1[[cName]])[diffCVec[t2Idx]] , 
                   ylab=levels(myData1[[cName]])[diffCVec[t1Idx]] , 
                   col="skyblue" )
            abline(0,1,lty="dotted")
          } else if ( t1Idx == t2Idx ) {
            par( mar=c(3,1.5,3,1.5) , mgp=c(2.0,0.7,0) , pty="m" )
            postInfo = plotPost( mcmcMat[,parName1] , 
                                 compVal=compVal , ROPE=rope , 
                                 cex.main=1.25 , cex.lab=1.25 , 
                                 xlab=bquote(.(parName1)) ,
                                 main=levels(myData1[[cName]])[diffCVec[t1Idx]] ,  
                                 xlim=xLim )
          } else if ( t1Idx < t2Idx ) {
            par( mar=c(3,1.5,3,1.5) , mgp=c(2.0,0.7,0) , pty="m" )
            postInfo = plotPost( mcmcMat[,parName1]-mcmcMat[,parName2] , 
                                 compVal=compValDiff , ROPE=ropeDiff , 
                                 cex.main=1.25 , cex.lab=1.25 , 
                                 xlab=bquote("Difference of "*omega*"'s") , 
                                 main=paste( 
                                   levels(myData1[[cName]])[diffCVec[t1Idx]] ,
                                   "-",
                                   levels(myData1[[cName]])[diffCVec[t2Idx]] ) )
          }
        }
      }
      if ( !is.null(saveName) ) {
        saveGraph( file=paste0(saveName,"OmegaDiff",compIdx), type=saveType)
      }
    }
  }
  
  # Plot individual theta's and differences:
  if ( !is.null(diffSList) ) {
    for ( compIdx in 1:length(diffSList) ) {
      diffSVec = diffSList[[compIdx]]
      Nidx = length(diffSVec)
      temp=NULL
      for ( i in 1:Nidx ) {
        temp = c( temp , which(myData1[[sName]]==diffSVec[i]) )
      }
      diffSVec = temp
      openGraph(width=2.5*Nidx,height=2.0*Nidx)
      par( mfrow=c(Nidx,Nidx) )
      xLim = range(c(compVal,rope,mcmcMat[,paste0("theta[",diffSVec,"]")],
                     z[diffSVec]/N[diffSVec]))
      for ( t1Idx in 1:Nidx ) {
        for ( t2Idx in 1:Nidx ) {
          parName1 = paste0("theta[",diffSVec[t1Idx],"]")
          parName2 = paste0("theta[",diffSVec[t2Idx],"]")
          if ( t1Idx > t2Idx) {  
            # plot.new() # empty plot, advance to next
            par( mar=c(3,3,3,1) , mgp=c(2.0,0.7,0) , pty="s" )
            nToPlot = 700
            ptIdx = round(seq(1,chainLength,length=nToPlot))
            plot ( mcmcMat[ptIdx,parName2] , mcmcMat[ptIdx,parName1] , cex.lab=1.25 ,
                   xlab=s[diffSVec[t2Idx]] , 
                   ylab=s[diffSVec[t1Idx]] , 
                   col="skyblue" )
            abline(0,1,lty="dotted")
          } else if ( t1Idx == t2Idx ) {
            par( mar=c(3,1.5,3,1.5) , mgp=c(2.0,0.7,0) , pty="m" )
            postInfo = plotPost( mcmcMat[,parName1] , 
                                 compVal=compVal , ROPE=rope , 
                                 cex.main=1.25 , cex.lab=1.25 , 
                                 xlab=bquote(.(parName1)) ,
                                 main=paste0( s[diffSVec[t1Idx]] , 
                                              " (",c[diffSVec[t1Idx]],")") ,  
                                 xlim=xLim )
            points( z[diffSVec[t1Idx]]/N[diffSVec[t1Idx]] , 0 , 
                    pch="+" , col="red" , cex=3 )
            text( z[diffSVec[t1Idx]]/N[diffSVec[t1Idx]] , 0 , 
                  bquote(list( z==.(z[diffSVec[t1Idx]]) ,
                               N==.(N[diffSVec[t1Idx]]) )) , 
                  adj=c( (z[diffSVec[t1Idx]]/N[diffSVec[t1Idx]]-xLim[1])/
                           (xLim[2]-xLim[1]),-3.25) , col="red" )
          } else if ( t1Idx < t2Idx ) {
            par( mar=c(3,1.5,3,1.5) , mgp=c(2.0,0.7,0) , pty="m" )
            postInfo = plotPost( mcmcMat[,parName1]-mcmcMat[,parName2] , 
                                 compVal=compValDiff , ROPE=ropeDiff , 
                                 cex.main=0.67 , cex.lab=1.25 , 
                                 xlab=bquote("Difference of "*theta*"'s") , 
                                 main=paste( 
                                   s[diffSVec[t1Idx]] , 
                                   " (",c[diffSVec[t1Idx]],")" ,
                                   "\n -",
                                   s[diffSVec[t2Idx]] , 
                                   " (",c[diffSVec[t2Idx]],")" ) )
            points( z[diffSVec[t1Idx]]/N[diffSVec[t1Idx]]
                    - z[diffSVec[t2Idx]]/N[diffSVec[t2Idx]] , 0 , 
                    pch="+" , col="red" , cex=3 )
          }
        }
      }
      if ( !is.null(saveName) ) {
        saveGraph( file=paste0(saveName,"ThetaDiff",compIdx), type=saveType)
      }
    }
  }
} # end function



# Analysis #1 Drivers --------------------------------------------

# Specify Model & Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData1 ,
                    zName="Successful_Attacks", 
                    NName="Total_Incidents", 
                    sName="Terror_Group",
                    cName="Region", 
                    numSavedSteps=11000 , 
                    thinSteps=20 )

# Display diagnostics of chain, for specified parameters:
for ( parName in c("omega[1]","omegaO","kappa[1]","kappaO", "theta[1]") ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName ,
            saveName=fileNameRoot , saveType=graphFileType )
}
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=NULL )

# Display posterior information:
plotMCMC( mcmcCoda , 
          data=myData1 ,
          zName="Successful_Attacks", 
          NName="Total_Incidents", 
          sName="Terror_Group", 
          cName="Region",
          compVal=NULL ,
          diffCList=list( c("South Asia","Middle East & North Africa")) ,
          diffSList=list( c("Maoists_South Asia","Taliban_South Asia")) ,
          compValDiff=0.0 )







# Analysis #2 File Settings & Data Load -----------------------

#Load Analysis 2 data set
myData2_unimputed = read.csv( file="Data/Analysis2_data.csv" ) 

#Impute NA over all zeros
myData2_unimputed[myData2_unimputed == 0] <- NA

#Replace Binary zeros for Success dependent variable
myData2_unimputed$success[is.na(myData2_unimputed$success)] <- 0
#View(myData2_unimputed)

#Function to impute median for NA values
median_impute=function(x){
  x[is.na(x)] <- median(x, na.rm=TRUE) 
  x 
}

myData2_imputed <- myData2_unimputed %>% 
  mutate_if(is.numeric, median_impute)
#View(myData2_imputed)


# Analysis #2 Functions ----------------------------------------------------

genMCMC = function( data , xName="x" , yName="y" , 
                    numSavedSteps=10000 , thinSteps=1 , saveName=NULL ,
                    runjagsMethod=runjagsMethodDefault , 
                    nChains=nChainsDefault ) { 
  require(runjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  y = data[,yName]
  x = as.matrix(data[,xName],ncol=length(xName))
  # Do some checking that data make sense:
  if ( any( !is.finite(y) ) ) { stop("All y values must be finite.") }
  if ( any( !is.finite(x) ) ) { stop("All x values must be finite.") }
  cat("\nCORRELATION MATRIX OF PREDICTORS:\n ")
  show( round(cor(x),3) )
  cat("\n")
  flush.console()
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    x = x ,
    y = y ,
    Nx = dim(x)[2] ,
    Ntotal = dim(x)[1]
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
    # Standardize the data:
    data {
      for ( j in 1:Nx ) {
        xm[j]  <- mean(x[,j])
        xsd[j] <-   sd(x[,j])
        for ( i in 1:Ntotal ) {
          zx[i,j] <- ( x[i,j] - xm[j] ) / xsd[j]
        }
      }
    }
  # Specify the model for standardized data:
  model {
    for ( i in 1:Ntotal ) {
      # In JAGS, ilogit is logistic:
      y[i] ~ dbern( mu[i] )
      mu[i] <- ( guess*(1/2) 
                 + (1.0-guess)*ilogit(zbeta0+sum(zbeta[1:Nx]*zx[i,1:Nx])) )
    }
    # Priors vague on standardized scale:
    zbeta0 ~ dnorm( 0 , 1/2^2 )  
    for ( j in 1:Nx ) {
      zbeta[j] ~ dnorm( 0 , 1/2^2 )
    }
    guess ~ dbeta(1,9)
    # Transform to original scale:
    beta[1:Nx] <- zbeta[1:Nx] / xsd[1:Nx] 
    beta0 <- zbeta0 - sum( zbeta[1:Nx] * xm[1:Nx] / xsd[1:Nx] )
  }
  " # close quote for modelString
  # Write out modelString to a text file
  writeLines( modelString , con="Specifications/TEMPmodel.txt" )
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Let JAGS do it...
  
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "beta0" ,  "beta" ,  
                  "zbeta0" , "zbeta" , "guess" )
  adaptSteps = 500  # Number of steps to "tune" the samplers
  burnInSteps = 1000
  runJagsOut <- run.jags( method=runjagsMethod ,
                          model="Specifications/TEMPmodel.txt" , 
                          monitor=parameters , 
                          data=dataList ,  
                          #inits=initsList , 
                          n.chains=nChains ,
                          adapt=adaptSteps ,
                          burnin=burnInSteps , 
                          sample=ceiling(numSavedSteps/nChains) ,
                          thin=thinSteps ,
                          summarise=FALSE ,
                          plots=FALSE )
  codaSamples = as.mcmc.list( runJagsOut )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
} # end function


smryMCMC = function(  codaSamples , 
                      saveName=NULL ) {
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples)
  paramName = colnames(mcmcMat)
  for ( pName in paramName ) {
    summaryInfo = rbind( summaryInfo , summarizePost( mcmcMat[,pName] ) )
  }
  rownames(summaryInfo) = paramName
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  return( summaryInfo )
} # end function


plotMCMC = function( codaSamples , data , xName="x" , yName="y" ,
                     showCurve=FALSE ,  pairsPlot=FALSE ,
                     saveName=NULL , saveType="jpg" ) {
  # showCurve is TRUE or FALSE and indicates whether the posterior should
  #   be displayed as a histogram (by default) or by an approximate curve.
  # pairsPlot is TRUE or FALSE and indicates whether scatterplots of pairs
  #   of parameters should be displayed.
  #-----------------------------------------------------------------------------
  y = data[,yName]
  x = as.matrix(data[,xName])
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  guess = mcmcMat[,"guess"]
  zbeta0 = mcmcMat[,"zbeta0"]
  zbeta  = mcmcMat[,grep("^zbeta$|^zbeta\\[",colnames(mcmcMat))]
  if ( ncol(x)==1 ) { zbeta = matrix( zbeta , ncol=1 ) }
  beta0 = mcmcMat[,"beta0"]
  beta  = mcmcMat[,grep("^beta$|^beta\\[",colnames(mcmcMat))]
  if ( ncol(x)==1 ) { beta = matrix( beta , ncol=1 ) }
  #-----------------------------------------------------------------------------
  if ( pairsPlot ) {
    # Plot the parameters pairwise, to see correlations:
    openGraph()
    nPtToPlot = 1000
    plotIdx = floor(seq(1,chainLength,by=chainLength/nPtToPlot))
    panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...) {
      usr = par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y))
      txt = format(c(r, 0.123456789), digits=digits)[1]
      txt = paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex=1.5 ) # was cex=cex.cor*r
    }
    pairs( cbind( beta0 , beta , guess )[plotIdx,] ,
           labels=c( "beta[0]" , 
                     paste0("beta[",1:ncol(beta),"]\n",xName) , "guessing" ) , 
           lower.panel=panel.cor , col="skyblue" )
    if ( !is.null(saveName) ) {
      saveGraph( file=paste(saveName,"PostPairs",sep=""), type=saveType)
    }
  }
  #-----------------------------------------------------------------------------
  # Data with posterior predictive:
  # If only 1 predictor:
  if ( ncol(x)==1 ) {
    openGraph(width=7,height=6)
    par( mar=c(3.5,3.5,2,1) , mgp=c(2.0,0.7,0) )
    plot( x[,1] , y , xlab=xName[1] , ylab=yName , 
          cex=2.0 , cex.lab=1.5 , col="black" , main="Data with Post. Pred." )
    abline(h=0.5,lty="dotted")
    cVec = floor(seq(1,chainLength,length=30))
    xWid=max(x)-min(x)
    xComb = seq(min(x)-0.1*xWid,max(x)+0.1*xWid,length=201)
    for ( cIdx in cVec ) {
      lines( xComb , 
             guess[cIdx]*0.5 
             + (1.0-guess[cIdx])*1/(1+exp(-(beta0[cIdx]+beta[cIdx,1]*xComb ))) , 
             lwd=1.5 ,
             col="skyblue" )
      xInt = -beta0[cIdx]/beta[cIdx,1]
      arrows( xInt,0.5, xInt,-0.04, length=0.1 , col="skyblue" , lty="dashed" )
    }
    if ( !is.null(saveName) ) {
      saveGraph( file=paste(saveName,"DataThresh",sep=""), type=saveType)
    }
  }
  # If only 2 predictors:
  if ( ncol(x)==2 ) {
    openGraph(width=7,height=7)
    par( mar=c(3.5,3.5,2,1) , mgp=c(2.0,0.7,0) )
    plot( x[,1] , x[,2] , pch=as.character(y) , xlab=xName[1] , ylab=xName[2] ,
          col="black" , main="Data with Post. Pred.")
    cVec = floor(seq(1,chainLength,length=30))
    for ( cIdx in cVec ) {
      abline( -beta0[cIdx]/beta[cIdx,2] , -beta[cIdx,1]/beta[cIdx,2] , col="skyblue" )
    }
    if ( !is.null(saveName) ) {
      saveGraph( file=paste(saveName,"DataThresh",sep=""), type=saveType)
    }
  }
  #-----------------------------------------------------------------------------
  # Marginal histograms:
  
  decideOpenGraph = function( panelCount , saveName , finished=FALSE , 
                              nRow=1 , nCol=3 ) {
    # If finishing a set:
    if ( finished==TRUE ) {
      if ( !is.null(saveName) ) {
        saveGraph( file=paste0(saveName,ceiling((panelCount-1)/(nRow*nCol))), 
                   type=saveType)
      }
      panelCount = 1 # re-set panelCount
      return(panelCount)
    } else {
      # If this is first panel of a graph:
      if ( ( panelCount %% (nRow*nCol) ) == 1 ) {
        # If previous graph was open, save previous one:
        if ( panelCount>1 & !is.null(saveName) ) {
          saveGraph( file=paste0(saveName,(panelCount%/%(nRow*nCol))), 
                     type=saveType)
        }
        # Open new graph
        openGraph(width=nCol*7.0/3,height=nRow*2.0)
        layout( matrix( 1:(nRow*nCol) , nrow=nRow, byrow=TRUE ) )
        par( mar=c(4,4,2.5,0.5) , mgp=c(2.5,0.7,0) )
      }
      # Increment and return panel count:
      panelCount = panelCount+1
      return(panelCount)
    }
  }
  
  # Original scale:
  panelCount = 1
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( beta0 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(beta[0]) , main="Intercept" )
  for ( bIdx in 1:ncol(beta) ) {
    panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
    histInfo = plotPost( beta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                         xlab=bquote(beta[.(bIdx)]) , main=xName[bIdx] )
  }
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( guess , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote("Mix. Coef.") , main="'guessing'" )
  panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(saveName,"PostMarg") )
  
  # Standardized scale:
  panelCount = 1
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  histInfo = plotPost( zbeta0 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(z*beta[0]) , main="Intercept" )
  for ( bIdx in 1:ncol(beta) ) {
    panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
    histInfo = plotPost( zbeta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                         xlab=bquote(z*beta[.(bIdx)]) , main=xName[bIdx] )
  }
  panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(saveName,"PostMargZ") )
  
  #-----------------------------------------------------------------------------
} # end function




# Analysis #2 Model 1 Drivers ------------------------------------------------------


myData2 = myData2_imputed
yName = "success" ; xName = c("X.SH.XPD.GHED.GD.ZS.","X.SE.XPD.TOTL.GD.ZS.")
fileNameRoot = "Output/Analysis2/Analysis2-robust-"
numSavedSteps=15000 ; thinSteps=2
graphFileType = "pdf" 

# Load the relevant model into R's working memory:
#source("Jags-Ydich-XmetMulti-MlogisticRobust.R")

# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData2 , xName=xName , yName=yName , 
                    numSavedSteps=numSavedSteps , thinSteps=thinSteps , 
                    saveName=fileNameRoot )


# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}

# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , 
                        saveName=fileNameRoot )
#show(summaryInfo)

# Display posterior information:
plotMCMC( mcmcCoda , data=myData2 , xName=xName , yName=yName , 
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType )


# Analysis #2 Model 2 Drivers ------------------------------------------------------


myData2 = myData2_imputed
yName = "success" ; xName = c("X.IT.NET.USER.ZS.","X.SP.RUR.TOTL.ZS.")
fileNameRoot = "Output/Analysis2/Analysis2-robust-"
numSavedSteps=15000 ; thinSteps=2
graphFileType = "pdf" 

# Load the relevant model into R's working memory:
#source("Jags-Ydich-XmetMulti-MlogisticRobust.R")

# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData2 , xName=xName , yName=yName , 
                    numSavedSteps=numSavedSteps , thinSteps=thinSteps , 
                    saveName=fileNameRoot )


# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}

# Get summary statistics of chain:
#summaryInfo = smryMCMC( mcmcCoda , 
                        #saveName=fileNameRoot )
#show(summaryInfo)

# Display posterior information:
plotMCMC( mcmcCoda , data=myData2 , xName=xName , yName=yName , 
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType )










