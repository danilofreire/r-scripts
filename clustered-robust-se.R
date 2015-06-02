clx <- 
  function(fm, dfcw, cluster){
    # R-codes (www.r-project.org) for computing
    # clustered-standard errors. Mahmood Arai, Jan 26, 2008.
    
    # The arguments of the function are:
    # fitted model, cluster1 and cluster2
    # You need to install libraries `sandwich' and `lmtest'
    
    # reweighting the var-cov matrix for the within model
    library(sandwich);library(lmtest)
    M <- length(unique(cluster))   
    N <- length(cluster)           
    K <- fm$rank                        
    dfc <- (M/(M-1))*((N-1)/(N-K))  
    uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)*dfcw
    coeftest(fm, vcovCL) }

mclx <- 
  function(fm, dfcw, cluster1, cluster2){
    # R-codes (www.r-project.org) for computing multi-way 
    # clustered-standard errors. Mahmood Arai, Jan 26, 2008. 
    # See: Thompson (2006), Cameron, Gelbach and Miller (2006)
    # and Petersen (2006).
    # reweighting the var-cov matrix for the within model
    
    # The arguments of the function are:
    # fitted model, cluster1 and cluster2
    # You need to install libraries `sandwich' and `lmtest'
    
    library(sandwich);library(lmtest)
    cluster12 = paste(cluster1,cluster2, sep="")
    M1  <- length(unique(cluster1))
    M2  <- length(unique(cluster2))   
    M12 <- length(unique(cluster12))
    N   <- length(cluster1)          
    K   <- fm$rank             
    dfc1  <- (M1/(M1-1))*((N-1)/(N-K))  
    dfc2  <- (M2/(M2-1))*((N-1)/(N-K))  
    dfc12 <- (M12/(M12-1))*((N-1)/(N-K))  
    u1j   <- apply(estfun(fm), 2, function(x) tapply(x, cluster1,  sum)) 
    u2j   <- apply(estfun(fm), 2, function(x) tapply(x, cluster2,  sum)) 
    u12j  <- apply(estfun(fm), 2, function(x) tapply(x, cluster12, sum)) 
    vc1   <-  dfc1*sandwich(fm, meat=crossprod(u1j)/N )
    vc2   <-  dfc2*sandwich(fm, meat=crossprod(u2j)/N )
    vc12  <- dfc12*sandwich(fm, meat=crossprod(u12j)/N)
    vcovMCL <- (vc1 + vc2 - vc12)*dfcw
    coeftest(fm, vcovMCL)}
