###############################################################################################################################################
# data_load.R
#
# Code to read in the data for the paper "Plateau: a new method for ecologically plausible
# climate envelopes for species distribution modelling" by Brewer, O'Hara, Anderson and Ohlemueller, to appear in Methods in
# Ecology and Evolution.
# This file should be called by Plateau.rmd, and is not intended for use by users (it is specific to the data set used in that paper).
###############################################################################################################################################
AFEData <- read.csv("Data/AFE17spp.csv")
cat(paste("Read AFE data,",nrow(AFEData),"observations.\n"))
ClimateData <- read.csv("Data/climate raw data.csv")
cat(paste("Read climate data,",nrow(ClimateData),"observations.\n"))
###############################################################################################################################################
# Set names etc
EnvNames <- c("dro", "gdd", "tc", "tw")
EnvLabels <- c("Drought Index","Growing Degree Days","Mean Temp of Coldest Month","Mean Temp of Warmest Month")
names(EnvLabels) <- EnvNames
Treenames <- c("abialbafe", "betnanafe", "betpenafe", "betpubafe", "cassatafe",
   "fagsylafe", "ileaquafe", "juncommafe", "junoxyafe", "lardecafe", "picabiafe",
   "pinhalafe", "pinsylafe", "quecocafe", "queileafe", "quepubafe", "querobafe")
# Sort climate data
num.ClimateData <- nrow(ClimateData)
nvar.ClimateData <- 4
# Remove the northern islands
AFEData <- AFEData[AFEData$afelat<75,]
AFEData <- AFEData[!(AFEData$afelat>70 & AFEData$afelong<0),]
num.AFEData <- nrow(AFEData)
AFEData.lat <- AFEData$afelat
AFEData.long <- AFEData$afelong
library(oce,quietly = TRUE)
# Either run the code here for Future.List.2045 or read in the .RData created
# using the code below
if(file.exists("Data/AFE.List.Mapping.RData")) {
    load("Data/AFE.List.Mapping.RData")
}else{
    Future.List.2045 <- list()
    for(i in 1:num.AFEData){
        Future.List.2045[[i]] <- 1
    }
    for(i in 1:num.ClimateData){
        Future.List.2045[[which.min(geodDist(lon1=AFEData.long,lat1=AFEData.lat,lon2=ClimateData$long[i],lat2=ClimateData$lat[i]))]] <- c(Future.List.2045[[which.min(geodDist(lon1=AFEData.long,lat1=AFEData.lat,lon2=ClimateData$long[i],lat2=ClimateData$lat[i]))]],i)
    }
    # Now set up future climate data for projections:
    for(i in 1:num.AFEData){
        Future.List.2045[[i]] <- Future.List.2045[[i]][-1]
    }
    if(!file.exists("Data/")) dir.create("Data/")
    save(Future.List.2045,file="Data/AFE.List.Mapping.RData")
}
Future.Climate <- list()
Future.Climate$Current.1945 <- array(NA,dim=c(num.AFEData,nvar.ClimateData),dimnames=list(NULL,EnvNames))
Future.Climate$B1.2045 <- array(NA,dim=c(num.AFEData,nvar.ClimateData),dimnames=list(NULL,EnvNames))
Future.Climate$A1FI.2045 <- array(NA,dim=c(num.AFEData,nvar.ClimateData),dimnames=list(NULL,EnvNames))
Future.Climate$B1.2095 <- array(NA,dim=c(num.AFEData,nvar.ClimateData),dimnames=list(NULL,EnvNames))
Future.Climate$A1FI.2095 <- array(NA,dim=c(num.AFEData,nvar.ClimateData),dimnames=list(NULL,EnvNames))
for(i in 1:num.AFEData){
    Future.Climate$Current.1945[i,"dro"] <- mean(ClimateData$dro1945[Future.List.2045[[i]]])
    Future.Climate$Current.1945[i,"gdd"] <- mean(ClimateData$gdd1945[Future.List.2045[[i]]])
    Future.Climate$Current.1945[i,"tc"] <- mean(ClimateData$Tc1945[Future.List.2045[[i]]])
    Future.Climate$Current.1945[i,"tw"] <- mean(ClimateData$Tw1945[Future.List.2045[[i]]])
    Future.Climate$B1.2045[i,"dro"] <- mean(ClimateData$dro2045B1[Future.List.2045[[i]]])
    Future.Climate$B1.2045[i,"gdd"] <- mean(ClimateData$gdd2045B1[Future.List.2045[[i]]])
    Future.Climate$B1.2045[i,"tc"] <- mean(ClimateData$Tc2045B1[Future.List.2045[[i]]])
    Future.Climate$B1.2045[i,"tw"] <- mean(ClimateData$Tw2045B1[Future.List.2045[[i]]])
    Future.Climate$A1FI.2045[i,"dro"] <- mean(ClimateData$dro2045A1fi[Future.List.2045[[i]]])
    Future.Climate$A1FI.2045[i,"gdd"] <- mean(ClimateData$gdd2045A1fi[Future.List.2045[[i]]])
    Future.Climate$A1FI.2045[i,"tc"] <- mean(ClimateData$Tc2045A1fi[Future.List.2045[[i]]])
    Future.Climate$A1FI.2045[i,"tw"] <- mean(ClimateData$Tw2045A1fi[Future.List.2045[[i]]])
    Future.Climate$B1.2095[i,"dro"] <- mean(ClimateData$dro2095B1[Future.List.2045[[i]]])
    Future.Climate$B1.2095[i,"gdd"] <- mean(ClimateData$gdd2095B1[Future.List.2045[[i]]])
    Future.Climate$B1.2095[i,"tc"] <- mean(ClimateData$Tc2095B1[Future.List.2045[[i]]])
    Future.Climate$B1.2095[i,"tw"] <- mean(ClimateData$Tw2095B1[Future.List.2045[[i]]])
    Future.Climate$A1FI.2095[i,"dro"] <- mean(ClimateData$dro2095A1fi[Future.List.2045[[i]]])
    Future.Climate$A1FI.2095[i,"gdd"] <- mean(ClimateData$gdd2095A1fi[Future.List.2045[[i]]])
    Future.Climate$A1FI.2095[i,"tc"] <- mean(ClimateData$Tc2095A1fi[Future.List.2045[[i]]])
    Future.Climate$A1FI.2095[i,"tw"] <- mean(ClimateData$Tw2095A1fi[Future.List.2045[[i]]])
}
# Compute values for the climate variables
AFEData$dro <- Future.Climate$Current.1945[,"dro"]
AFEData$gdd <- Future.Climate$Current.1945[,"gdd"]
AFEData$tc <- Future.Climate$Current.1945[,"tc"]
AFEData$tw <- Future.Climate$Current.1945[,"tw"]
# Now get rid of the isolated cells
AFE.index <- !is.nan(AFEData$dro)
AFEData <- AFEData[AFE.index,]
Future.Climate$Current.1945 <- Future.Climate$Current.1945[AFE.index,]
Future.Climate$B1.2045 <- Future.Climate$B1.2045[AFE.index,]
Future.Climate$A1FI.2045 <- Future.Climate$A1FI.2045[AFE.index,]
Future.Climate$B1.2095 <- Future.Climate$B1.2095[AFE.index,]
Future.Climate$A1FI.2095 <- Future.Climate$A1FI.2095[AFE.index,]
num.AFEData <- nrow(AFEData)
# Pull out the lat/long coordinates for plotting maps
coords.list <- list(long=AFEData$afelong,lat=AFEData$afelat)
###############################################################################################################################################
# Set up the WinBUGS model, with multiple car.normals, one per clique - needed as these should be separate CARs - but don't use a CAR if the
# clique is too small.
# Start by working out the neighbourhood structure for spatial modelling
neigh.list <- list()
num.neighs <- numeric(num.AFEData)
dist.limit <- 60 # was 60
num.vec <- 1:num.AFEData
Data.lat <- AFEData$afelat
Data.long <- AFEData$afelong
for(i in 1:num.AFEData){
    num1.vec <- num.vec[-i]
    neigh.list[[i]] <- num1.vec[geodDist(longitude1=rep(Data.long[i],num.AFEData-1),latitude1=rep(Data.lat[i],num.AFEData-1),
        longitude2=Data.long[-i],latitude2=Data.lat[-i]) <= dist.limit]
    num.neighs[i] <- length(neigh.list[[i]])
}
neigh.real.list <- unlist(neigh.list)
# Clique check (not using unlist-ed neigh.list)
clique.list <- list()
NClique <- 1
clique.list[[1]] <- c(1,neigh.list[[1]])
hits <- logical(num.AFEData)
for(i in 2:num.AFEData){
    for(j in 1:NClique){
        hits[j] <- any(c(i,neigh.list[[i]]) %in% clique.list[[j]])
    }
    if(sum(hits[1:NClique])==1){
        clique.list[[(1:NClique)[hits[1:NClique]]]] <- unique(c(clique.list[[(1:NClique)[hits[1:NClique]]]],i,neigh.list[[i]]))
    }
    if(sum(hits[1:NClique])==0){
        NClique <- NClique + 1
        clique.list[[NClique]] <- c(i,neigh.list[[i]])
    }
    if(sum(hits[1:NClique])>1){
        combined.vec <- i
        for(j in (1:NClique)[hits[1:NClique]]){
            combined.vec <- unique(c(combined.vec,clique.list[[j]]))
        }
        temp.list <- list()
        new.NClique <- NClique - sum(hits[1:NClique]) + 1
        temp.list[[1]] <- combined.vec
        count <- 1
        for(j in 1:NClique){
            if(hits[j]==0){
                count <- count + 1
                temp.list[[count]] <- clique.list[[j]]
            }
        }
        NClique <- new.NClique
        clique.list <- temp.list
    }
}
clique.size <- numeric(NClique)
for(j in 1:NClique){
    clique.list[[j]] <- sort(clique.list[[j]])
    clique.size[j] <- length(clique.list[[j]])
}
neigh.list <- neigh.real.list
# Now check counts within cliques
clique.presence <- numeric(NClique)
for(j in 1:NClique){
    clique.presence[j] <- sum(AFEData[clique.list[[j]],species])
}
fix.spatial.REs <- rep(NA,num.AFEData)
for(j in 1:NClique){
    if(clique.presence[j]==0){
        fix.spatial.REs[clique.list[[j]]] <- 0
    }
    if(clique.size[j]<=5){
        fix.spatial.REs[clique.list[[j]]] <- 0
    }
}
mat.num.neighs <- array(NA,dim=c(NClique,num.AFEData))
mat.adj <- array(NA,dim=c(NClique,length(neigh.list)))
in.clique <- numeric(num.AFEData)
for(j in 1:NClique){
    mat.num.neighs[j,1:clique.size[j]] <- num.neighs[clique.list[[j]]]
    in.clique[clique.list[[j]]] <- j
}
in.clique.newnum <- numeric(num.AFEData)
for(i in 1:num.AFEData){
    in.clique.newnum[i] <- (1:clique.size[in.clique[i]])[clique.list[[in.clique[i]]]==i]
}
mat.adj.list <- vector("list", NClique)
numnum <- 1
for(i in 1:num.AFEData){
    if(num.neighs[i]==0){
        mat.adj.list[[in.clique[i]]] <- 0
    }else{
        mat.adj.list[[in.clique[i]]] <- c(mat.adj.list[[in.clique[i]]],in.clique.newnum[neigh.list[numnum:(numnum+num.neighs[i]-1)]])
    }
    numnum <- numnum + num.neighs[i]
}
mat.num.adj <- numeric(NClique)
for(j in 1:NClique){
    mat.num.adj[j] <- length(mat.adj.list[[j]])
    mat.adj[j,1:mat.num.adj[j]] <- mat.adj.list[[j]]
}
mat.u <- array(NA,dim=c(NClique,num.AFEData))
for(i in 1:num.AFEData){
    mat.u[in.clique[i],in.clique.newnum[i]] <- fix.spatial.REs[i]
}
NBigClique <- sum(clique.size>=5)
NB <- (1:NClique)[clique.size>=5]
u.clique.end <- cumsum(clique.size)
u.clique.start <- c(1,u.clique.end[-NClique]+1)
adj.clique.end <- cumsum(mat.num.adj)
adj.clique.start <- c(1,adj.clique.end[-NClique]+1)
adj <- unlist(mat.adj.list)
ulist <- list()
numlist <- list()
for(i in 1:NClique){
    ulist[[i]] <- mat.u[i,1:clique.size[i]]
    numlist[[i]] <- mat.num.neighs[i,1:clique.size[i]]
}
u <- unlist(ulist)
num <- unlist(numlist)
detach("package:oce")
###############################################################################################################################################
