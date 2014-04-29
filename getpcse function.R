library(sna)
library(network)
#library(igraph)
library(plm)

getpcse <- function(x, thresh=1){
  
  thresh <- thresh
  
  ## extract residuals
  uhat <- try(x$residuals, silent=T)
  
  ## extract indices
  groupind<-as.numeric(attr(x$model, "index")[,1])
  timeind<-as.numeric(attr(x$model, "index")[,2])
  
  
  #Alternative Approach to unbalancedness
  units <- unique(groupind)
  N <- length(units)
  time <- unique(timeind)
  t <- length(time)
  brows <- c()
  for (i in 1:t) {
    br <- which(timeind == time[i])
    check <- length(br) == N
    if (check) {
      brows <- c(brows, br)
    }
  }
  if(length(brows)==0) {stop("No balanced rows in data.")}
  
  #Concatenate the balanced rows
  groupind <- groupind[brows]
  timeind <- timeind[brows]
  uhat <- uhat[brows]
  t <- length(unique(timeind))
    
  #Make the weighted matrix
  mat.order <- order(groupind, timeind) #Get the matrix ordering
  matdat <- matrix(uhat[mat.order], nrow=t) #Makes a T by N matrix
  names(matdat) <- sort(unique(groupind))
  E.hat<-crossprod(matdat, matdat)/(t-1)
  SDs<-apply(matdat, 2, sd) #get the standard deviation for each unit
  SDmat<-tcrossprod(SDs, SDs) #get the product of standard deviations for each unit pair
  
  #Divide each error in the E-hat matrix by the product of standard deviations
  cormat<-E.hat/SDmat #Gives a standardized measure based on the standard deviations of the individual units in that pair
  
  size<-apply(cormat, 2, abs)
  size<-apply(size,2, sum, na.rm=T)
  net<-round(cormat)
  posneg <- ifelse(cormat>0, 1, ifelse(cormat<0, -1, 0))
  net <- ifelse(abs(net)>=thresh, net, 0)
  #matnet<-network(net, directed=F)
  
  #Running Community Detection
#  edgelist <- as.matrix.network(matnet, matrix.type="edgelist")
#  net <- graph.edgelist(edgelist, directed=F)
#  com <- fastgreedy.community(net)
  
  #Plot the correlation of errors across units - only looking at those that are correlated close to whole numbers (so omits very minor correlations<1). Then blacken those nodes with mostly positive correlations and whiten those with mostly negative correlations with the rest of the units. 
#  par(mfrow=c(1,2))
#  plot(matnet, vertex.cex=(size^.27 + -.1), vertex.sides=50, 
#        label.cex=0.6, vertex.col=com$membership, displayisolates=F,
#        label=network.vertex.names(matnet), xlab="", edge.lwd=(abs(round(cormat))^.2), edge.col=ifelse(cormat>0, "grey", NA), cex.main=.8, main="Positive Spatial Correlation \n of Errors by Unit", displaylabels=F)
#  plot(matnet, vertex.cex=(size^.27 + -.1), vertex.sides=50, 
#       label.cex=0.6, vertex.col=com$membership, displayisolates=F,
#       label=network.vertex.names(matnet), xlab="", edge.lwd=(abs(round(cormat))^.2), edge.col=ifelse(cormat<0, "grey", NA), cex.main=.8, main="Negative Spatial Correlation \n of Errors by Unit", displaylabels=F)
 
 # matnet
net
}

#Working on a way to run an ergm using the model matrix and aggregating data by the group index
#getergm <- function(net, mod){
  groupind<-as.numeric(attr(x$model, "index")[,1])  
  k <- length(length(formula(mod5)))-1
  modmat <- matrix(NA, ncol=k, nrow=length(unique(groupind)))
  modmat <- for(i in )
  net <- net
  for(i in 1:k){
  net %v% paste("var", k, sep="") <- modmat[, k]
  }
  fmod <- for(i in 1:k){paste(, sep="")}
  ergm(net~)
  
}


data("EmplUK", package="plm")
Em <- pdata.frame(EmplUK)
mod1 <- plm(emp~1, model="pooling", data=EmplUK, na.action=na.exclude)
net <- getpcse(mod1)
net2 <- apply(net, 2, function(x) abs(x)/sum(abs(x)))
bootnet <- matrix(rbinom(length(net2), 1, prob=net2), ncol=ncol(net2))
gplot(bootnet)
n.reps <- 100
deg <- matrix(NA, nrow=nrow(net2), ncol=n.reps)
for(i in 1:n.reps){
bootnet <- matrix(rbinom(length(net2), 1, prob=net2), ncol=ncol(net2))
#gplot(bootnet)
deg[,i] <- degree(bootnet, gmode="graph")
}
deg <- apply(deg, 1, mean)
d.dat <- data.frame(deg=deg, firm=1:length(deg))
dat <- merge(d.dat, EmplUK, by="firm")
mod1 <- lm(emp~wage, data=dat)
mod2 <- lm(emp~wage+deg, data=dat)
anova(mod1, mod2)

data("Cigar")
mod2 <- plm(price~pop+cpi, data=Cigar)
net <- getpcse(mod2)
net2 <- apply(net, 2, function(x) abs(x)/sum(abs(x)))
bootnet <- matrix(rbinom(length(net2), 1, prob=net2), ncol=ncol(net2))
gplot(bootnet)
n.reps <- 100
deg <- matrix(NA, nrow=nrow(net2), ncol=n.reps)
for(i in 1:n.reps){
  bootnet <- matrix(rbinom(length(net2), 1, prob=net2), ncol=ncol(net2))
  #gplot(bootnet)
  deg[,i] <- degree(bootnet, gmode="graph")
}
deg <- apply(deg, 1, mean)
d.dat <- data.frame(deg=deg, state=1:length(deg))
dat <- merge(d.dat, Cigar, by="state")
mod1 <- lm(price~ndi+pop, data=dat)
mod2 <- lm(price~ndi+pop+deg, data=dat)


data("Grunfeld")
mod3 <- plm(capital~inv+value, data=Grunfeld)
getpcse(mod3)

data("Gasoline")
mod4 <- plm(lgaspcar~lincomep+lrpmg, data=Gasoline)
getpcse(mod4)

data("Crime")
mod5 <- plm(crmrte~avgsen, data=Crime)
getpcse(mod5)


