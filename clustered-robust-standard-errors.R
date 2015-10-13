# Create function for clustered robust standard errors, 
# by Jack Blumenau, Matthew King and David Romney,  "Not by the Pen Alone" (2014)
# http://dx.doi.org/10.7910/DVN/25668 UNF:5:v4e3ZBbIy7fHqgoL4VpXDQ==


## vcovCluster.r
## function to compute var-cov matrix using clustered robust standard errors
## inputs:
## model = model object from call to lm or glm
## cluster = vector with cluster ID indicators
## output:
## cluster robust var-cov matrix
## to call this for a model directly use:
## coeftest(model,vcov = vcovCluster(model, cluster))
## formula is similar to Stata's , cluster command

vcovCluster <- function(
        model,
        cluster
)
{
        require(sandwich)
        require(lmtest)
        if(nrow(model.matrix(model)) != length(cluster)){
                stop("Check your data: Cluster variable has different N than model")
        }
        M <- length(unique(cluster))
        N <- length(cluster)
        K <- model$rank
        if(M < 50){
                warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
        }
        dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
        uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
        rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
        return(rcse.cov)
}

# Now there's a package that calculates bootstrapped standard errors automatically.
library(clusterSEs)
cl <- cluster.bs.glm(model, dat = df, cluster = ~ country)
summary(cl)