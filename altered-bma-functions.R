###########################################################
### bic.glmMN and aic.glmMN are slighly altered versions of bic.glm, a program
### from the BMA package. Please note that these functions are likely to throw 
### an error when `nbest' is set too high. Ideally we would want this parameter  
### to be set in the millions, but the current code does not allocate memory
### in an efficient way. In our results, we set `nbest' at 100,000.
### This means that some parameter estimates may vary very slightly from
### our published results when they are recalculated.
### Written by Montgomery and Nyhan (2010)
###########################################################

###########################################################
### We have added four additional inputs for this function: inter.list,
### keep.list, all.none.list, and either.or list. Each should consist of a list
### of vectors. For inter.list, the first two entries in each vector should
### be the base terms, while the last entry should be the interaction.
### For the mement, this function requires that you provide it with a matrix of
### potential covariates rather than specifying an equation. You must
### identify the variables of interest by their column number in the matrix
### rather than by variable name. See our examples for more details.
###########################################################

library(BMA)

bic.glmMN<-function (x, y, glm.family, wt = rep(1, nrow(x)), strict = FALSE,
    prior.param = c(rep(0.5, ncol(x))),
    OR = 100000000000000000000000000000000000000000,
    maxCol = 30,
    OR.fix = 20, nbest = 1000000, dispersion = NULL, factor.type = TRUE,
    factor.prior.adjust = FALSE, occam.window = TRUE, call = NULL,
    inter.list=NULL,keep.list=NULL, all.none.list=NULL,
    either.or.list=NULL)
{
    leaps.glm <- function(info, coef, names.arg, nbest = nbest) {
        names.arg <- names.arg
        if (is.null(names.arg))
            names.arg <- c(as.character(1:9), LETTERS, letters)[1:ncol(info)]
        if (length(names.arg) < ncol(info))
            stop("Too few names")
        bIb <- coef %*% info %*% coef
        kx <- ncol(info)
        maxreg <- nbest * kx
        if (kx < 3)
            stop("Too few independent variables")
        imeth <- 1
        df <- kx + 1
        Ib <- info %*% coef
        rr <- cbind(info, Ib)
        rr <- rbind(rr, c(Ib, bIb))
        it <- 0
        n.cols <- kx + 1
        nv <- kx + 1
        nf <- 0
        no <- 1e+05
        ib <- 1
        mb <- nbest
        nd <- n.cols
        nc <- 4 * n.cols
        rt <- matrix(rep(0, times = nd * nc), ncol = nc)
        rt[, 1:n.cols] <- rr
        iw <- c(1:(kx + 1), rep(0, times = 4 * nd))
        nw <- length(iw)
        rw <- rep(0, times = 2 * mb * kx + 7 * nd)
        nr <- length(rw)
        t1 <- 2
        s2 <- -1
        ne <- 0
        iv <- 0
        nret <- mb * kx
        Subss <- rep(0, times = nret)
        RSS <- Subss
        ans <- .Fortran("fwleaps", as.integer(nv), as.integer(it),
            as.integer(kx), as.integer(nf), as.integer(no), as.integer(1),
            as.double(2), as.integer(mb), as.double(rt), as.integer(nd),
            as.integer(nc), as.integer(iw), as.integer(nw), as.double(rw),
            as.integer(nr), as.double(t1), as.double(s2), as.integer(ne),
            as.integer(iv), as.double(Subss), as.double(RSS),
            as.integer(nret), PACKAGE = "BMA")
        regid <- ans[[21]]/2
        r2 <- ans[[20]]
        nreg <- sum(regid > 0)
        regid <- regid[1:nreg]
        r2 <- r2[1:nreg]
        which <- matrix(TRUE, nreg, kx)
        z <- regid
        which <- matrix(as.logical((rep.int(z, kx)%/%rep.int(2^((kx -
            1):0), rep.int(length(z), kx)))%%2), byrow = FALSE,
            ncol = kx)
        size <- which %*% rep(1, kx)
        label <- character(nreg)
        sep <- if (all(nchar(names.arg) == 1))
            ""
        else ","
        for (i in 1:nreg) label[i] <- paste(names.arg[which[i,
            ]], collapse = sep)
        ans <- list(r2 = r2, size = size, label = label, which = which)
        return(ans)
    }
    factor.names <- function(x) {
        out <- list()
        for (i in 1:ncol(x)) if (is.factor(x[, i]))
            out[[i]] <- levels(x[, i])
        else out <- c(out, list(NULL))
        attributes(out)$names <- names(x)
        return(out)
    }
    create.assign <- function(xx) {
        asgn <- list()
        asgn[[1]] <- 1
        cnt <- 2
        for (i in 1:ncol(x)) {
            if (!is.factor(x[, i]))
                size <- 1
            else size <- length(levels(x[, i])) - 1
            asgn[[i + 1]] <- cnt:(cnt + size - 1)
            cnt <- cnt + size
        }
        names(asgn) <- c("(Intercept)", attributes(x)$names)
        return(asgn)
    }
    dropcols <- function(x, y, glm.family, wt, maxCols = 30) {
        vnames <- attributes(x)$names
        nvar <- length(vnames)
        isfac <- rep(FALSE, times = nvar)
        for (i in 1:nvar) isfac[i] <- is.factor(x[, i])
        nlevels <- rep(NA, times = nvar)
        for (i in 1:nvar) if (isfac[i])
            nlevels[i] <- length(levels(x[, i]))
        any.dropped <- FALSE
        mm <- model.matrix(terms.formula(~., data = x), data = x)
        designx <- attributes(mm)$assign
        n.designx <- length(designx)
        designx.levels <- rep(1, times = n.designx)
        for (i in 2:n.designx) if (isfac[designx[i]])
            designx.levels[i] <- sum(designx[1:i] == designx[i]) +
                1
        x.df <- data.frame(x = x)
        glm.out <- glm(y ~ ., family = glm.family, weights = wt,
            data = x.df)
        glm.assign <- create.assign(x)
        while (length(glm.out$coefficients) > maxCol) {
            any.dropped <- TRUE
            dropglm <- drop1(glm.out, test = "Chisq")
            dropped <- which.max(dropglm$"Pr(Chi)"[-1]) + 1
            x.df <- x.df[, -(dropped - 1)]
            designx.levels <- designx.levels[-dropped]
            designx <- designx[-dropped]
            glm.out <- glm(y ~ ., family = glm.family, weights = wt,
                data = x.df)
        }
        remaining.vars <- unique(designx[-1])
        new.nvar <- length(remaining.vars)
        dropped.vars <- vnames[-remaining.vars]
        dropped.levels <- NULL
        ncol.glm <- ncol(x.df) - 1
        x.df <- x.df[-(ncol.glm + 1)]
        xx <- data.frame(matrix(rep(NA, times = new.nvar * nrow(x.df)),
            ncol = new.nvar))
        new.names = rep(NA, times = new.nvar)
        for (i in 1:new.nvar) {
            cvar <- remaining.vars[i]
            lvls <- designx.levels[cvar == designx]
            if (isfac[cvar]) {
                if (length(lvls) != length(levels(x[, cvar]))) {
                  newvar <- (as.matrix(x.df[, cvar == designx[-1]]) %*%
                    cbind(lvls - 1)) + 1
                  xx[, i] <- factor(levels(x[, cvar])[newvar])
                  new.names[i] <- vnames[cvar]
                  removed.levels <- levels(x[, cvar])[-c(1, lvls)]
                  dropped.levels <- c(dropped.levels, paste(vnames[cvar],
                    "_", removed.levels, sep = ""))
                }
                else {
                  xx[, i] <- factor(x[, cvar])
                  new.names[i] <- vnames[cvar]
                }
            }
            else {
                xx[, i] <- x[, cvar]
                new.names[i] <- vnames[cvar]
            }
        }
        dropped <- c(dropped.vars, dropped.levels)
        return(list(mm = xx, any.dropped = any.dropped, dropped = dropped,
            var.names = new.names, remaining.vars = remaining.vars))
    }
    if (is.null(call))
        cl <- match.call()
    else cl <- call
    options(contrasts = c("contr.treatment", "contr.treatment"))
    prior.weight.denom <- 0.5^ncol(x)
    x <- data.frame(x)
    names.arg <- names(x)
    if (is.null(names.arg))
        names.arg <- paste("X", 1:ncol(x), sep = "")
    x2 <- na.omit(x)
    used <- match(row.names(x), row.names(x2))
    omitted <- seq(nrow(x))[is.na(used)]
    if (length(omitted) > 0) {
        wt <- wt[-omitted]
        x <- x2
        y <- y[-omitted]
        warning(paste("There were ", length(omitted), "records deleted due to NA's"))
    }
    leaps.x <- x
    output.names <- names(x)
    fn <- factor.names(x)
    factors <- !all(unlist(lapply(fn, is.null)))
    x.df <- data.frame(x = x)
    glm.out <- glm(y ~ ., family = glm.family, weights = wt,
        data = x.df)
    glm.assign <- create.assign(x)
    fac.levels <- unlist(lapply(glm.assign, length)[-1])
    if (factors) {
        cdf <- cbind.data.frame(y = y, x)
        mm <- model.matrix(formula(cdf), data = cdf)[, -1, drop = FALSE]
        mmm <- data.frame(matrix(mm, nrow = nrow(mm), byrow = FALSE))
        names(mmm) <- dimnames(mm)[[2]]
        output.names <- names(mmm)
        if (factor.type) {
            for (i in 1:length(names(x))) {
                if (!is.null(fn[[i]])) {
                  nx <- names(x)[i]
                  coefs <- glm.out$coef[glm.assign[[i + 1]]]
                  old.vals <- x[, i]
                  new.vals <- c(0, coefs)
                  new.vec <- as.vector(new.vals[match(old.vals,
                    fn[[i]])])
                  leaps.x[, nx] <- new.vec
                }
            }
        }
        else {
            new.prior <- NULL
            for (i in 1:length(names(x))) {
                addprior <- prior.param[i]
                if (!is.null(fn[[i]])) {
                  k <- length(fn[[i]])
                  if (factor.prior.adjust)
                    addprior <- rep(1 - (1 - prior.param[i])^(1/(k -
                      1)), k - 1)
                  else addprior <- rep(prior.param[i], k - 1)
                }
                new.prior <- c(new.prior, addprior)
            }
            prior.param <- new.prior
            x <- leaps.x <- mmm
        }
    }
    xx <- data.frame()
    xx <- dropcols(leaps.x, y, glm.family, wt, maxCol)
    var.names <- xx$var.names
    remaining <- xx$remaining.vars
    leaps.x <- xx$mm
    reduced <- xx$any.dropped
    dropped <- NULL
    if (reduced)
        dropped <- xx$dropped
    nvar <- length(x[1, ])
    x <- x[, remaining, drop = FALSE]
    x <- data.frame(x)
    fac.levels <- fac.levels[remaining]
    output.names <- list()
    for (i in 1:length(var.names)) {
        if (is.factor(x[, i]))
            output.names[[i]] <- levels(x[, i])
        else output.names[[i]] <- NA
    }
    xnames <- names(x)
    names(leaps.x) <- var.names
    x.df <- data.frame(x = leaps.x)
    glm.out <- glm(y ~ ., family = glm.family, weights = wt,
        data = x.df, x = TRUE)
    glm.assign <- create.assign(leaps.x)
    if (factor.type == FALSE)
        fac.levels <- unlist(lapply(glm.assign, length)[-1])
    famname <- glm.out$family["family"]$family
    if (is.null(dispersion)) {
        if (famname == "poisson" | famname == "binomial")
            dispersion <- FALSE
        else dispersion <- TRUE
    }
    nobs <- length(y)
    resid <- resid(glm.out, "pearson")
    rdf <- glm.out$df.resid
    is.wt <- !all(wt == rep(1, nrow(x)))
    if (is.wt) {
        resid <- resid * sqrt(wt)
        excl <- wt == 0
        if (any(excl)) {
            warning(paste(sum(excl), "rows with zero wts not counted"))
            resid <- resid[!excl]
        }
    }
    phihat <- sum(resid^2)/rdf
    if (dispersion)
        disp <- phihat
    else disp <- 1
    coef <- glm.out$coef[-1]
    p <- glm.out$rank
    R <- glm.out$R
    rinv <- diag(p)
    rinv <- backsolve(R, rinv)
    rowlen <- drop(((rinv^2) %*% rep(1, p))^0.5)
    sigx <- rowlen %o% sqrt(disp)
    correl <- rinv %*% t(rinv) * outer(1/rowlen, 1/rowlen)
    cov <- correl * sigx %*% t(sigx)
    info <- solve(cov[-1, -1])
    if (ncol(x) > 2) {
        a <- leaps.glm(info, coef, names.arg = names(leaps.x),
            nbest = nbest)


       inter.remove<-function(obj, this){
           out <- NULL
           base1<-this[1]
           base2<-this[2]
           inter<-this[3]
           selector1<-(obj$which[,inter]==TRUE & obj$which[,base1]==FALSE)==FALSE
           selector2<-(obj$which[,inter]==TRUE & obj$which[,base2]==FALSE)==FALSE
           selector<-(selector1*selector2)==1

           out$which<-obj$which[selector,]
           out$size<-obj$size[selector]
           out$r2<-obj$r2[selector]
           out$label<-obj$label[selector]
           out
       }

        keep.remove<-function(obj, keep){
            out<-NULL
            selector<-obj$which[,keep]==TRUE
            out$which<-obj$which[selector,]
            out$size<-obj$size[selector]
            out$r2<-obj$r2[selector]
            out$label<-obj$label[selector]
            out
        }

        all.none.remove<-function(obj, all.none){
            out<-NULL
            number<-length(all.none)
            selector1<-obj$which[,all.none[1]]==TRUE
            selector2<-obj$which[,all.none[1]]==FALSE
            for (i in 2:number){
                selector1 <- selector1 * (obj$which[,all.none[i]]== TRUE)
                selector2 <- selector2 * (obj$which[,all.none[i]]== FALSE)
            }
            selector<- selector1 == 1 | selector2==1
            out$which<-obj$which[selector,]
            out$size<-obj$size[selector]
            out$r2<-obj$r2[selector]
            out$label<-obj$label[selector]
            out
        }

        either.or.remove<-function(obj, either.or){
            out<-NULL
            eo1<-either.or[1]
            eo2<-either.or[2]
            selector<-(obj$which[,eo1]==TRUE & obj$which[,eo2]==TRUE)==FALSE
            out$which<-obj$which[selector,]
            out$size<-obj$size[selector]
            out$r2<-obj$r2[selector]
            out$label<-obj$label[selector]
            out
        }

        if(!is.null(inter.list)){
            for (i in 1:length(inter.list)){
               a <- inter.remove(obj=a, this=inter.list[[i]])
            }
        }

        if(!is.null(keep.list)){
            for (i in 1:length(keep.list)){
               a <- keep.remove(obj=a, keep=keep.list[[i]])
            }
        }

        if(!is.null(all.none.list)){
            for (i in 1:length(all.none.list)){
               a <- all.none.remove(obj=a, all.none=all.none.list[[i]])
            }
        }

        if(!is.null(either.or.list)){
            for (i in 1:length(either.or.list)){
            a <- either.or.remove(obj=a, either.or=either.or.list[[i]])
            }
        }



        a$r2 <- pmin(pmax(0, a$r2), 0.999)
        a$r2 <- c(0, a$r2)
        a$size <- c(0, a$size)
        a$label <- c("NULL", a$label)
        a$which <- rbind(rep(FALSE, ncol(x)), a$which)
        nmod <- length(a$size)
        prior.mat <- matrix(rep(prior.param, nmod), nmod, ncol(leaps.x),
            byrow = TRUE)
        prior <- apply(a$which * prior.mat + (!a$which) * (1 -
            prior.mat), 1, prod)
        bIb <- as.numeric(coef %*% info %*% coef)
        lrt <- bIb - (a$r2 * bIb)
        bic <- lrt + (a$size) * log(nobs) - 2 * log(prior)
        occam <- bic - min(bic) < 2 * OR.fix * log(OR)
        size <- a$size[occam]
        label <- a$label[occam]
        which <- a$which[occam, , drop = FALSE]
        bic <- bic[occam]
        prior <- prior[occam]
    }
    else {
        nmod <- switch(ncol(x), 2, 4)
        bic <- label <- rep(0, nmod)
        which <- matrix(c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE,
            TRUE, TRUE), nmod, nmod/2)
        size <- c(0, 1, 1, 2)[1:nmod]
        sep <- if (all(nchar(names.arg) == 1))
            ""
        else ","
        prior.mat <- matrix(rep(prior.param, nmod), nmod, ncol(x),
            byrow = TRUE)
        prior <- apply(which * prior.mat + (!which) * (1 - prior.mat),
            1, prod)
        for (k in 1:nmod) {
            if (k == 1)
                label[k] <- "NULL"
            else label[k] <- paste(names.arg[which[k, ]], collapse = sep)
        }
    }
    nmod <- length(label)
    model.fits <- as.list(rep(0, nmod))
    dev <- rep(0, nmod)
    df <- rep(0, nmod)
    for (k in 1:nmod) {
        if (sum(which[k, ]) == 0) {
            glm.out <- glm(y ~ 1, family = glm.family, weights = wt)
        }
        else {
            x.df <- data.frame(x = x[, which[k, ]])
            glm.out <- glm(y ~ ., data = x.df, family = glm.family,
                weights = wt)
        }
        dev[k] <- glm.out$deviance
        df[k] <- glm.out$df.residual
        model.fits[[k]] <- matrix(0, nrow = length(glm.out$coef),
            ncol = 2)
        model.fits[[k]][, 1] <- glm.out$coef
        coef <- glm.out$coef
        p <- glm.out$rank
        R <- glm.out$R
        rinv <- diag(p)
        rinv <- backsolve(R, rinv)
        rowlen <- drop(((rinv^2) %*% rep(1, p))^0.5)
        sigx <- rowlen %o% sqrt(disp)
        correl <- rinv %*% t(rinv) * outer(1/rowlen, 1/rowlen)
        cov <- correl * sigx %*% t(sigx)
        model.fits[[k]][, 2] <- sqrt(diag(cov))
    }
    bic <- dev/disp - df * log(nobs) - 2 * log(prior)
    if (occam.window)
        occam <- bic - min(bic) < 2 * log(OR)
    else occam = rep(TRUE, length(bic))
    dev <- dev[occam]
    df <- df[occam]
    size <- size[occam]
    label <- label[occam]
    which <- which[occam, , drop = FALSE]
    bic <- bic[occam]
    prior <- prior[occam]
    model.fits <- model.fits[occam]
    postprob <- exp(-0.5 * (bic - min(bic)))/sum(exp(-0.5 * (bic -
        min(bic))))
    order.bic <- order(bic, size, label)
    dev <- dev[order.bic]
    df <- df[order.bic]
    size <- size[order.bic]
    label <- label[order.bic]
    which <- which[order.bic, , drop = FALSE]
    bic <- bic[order.bic]
    prior <- prior[order.bic]
    postprob <- postprob[order.bic]
    model.fits <- model.fits[order.bic]
    nmod <- length(bic)
    if (strict & (nmod != 1)) {
        occam <- rep(TRUE, nmod)
        for (k in (2:nmod)) for (j in (1:(k - 1))) {
            which.diff <- which[k, ] - which[j, ]
            if (all(which.diff >= 0))
                occam[k] <- FALSE
        }
        dev <- dev[occam]
        df <- df[occam]
        size <- size[occam]
        label <- label[occam]
        which <- which[occam, , drop = FALSE]
        bic <- bic[occam]
        prior <- prior[occam]
        postprob <- postprob[occam]
        postprob <- postprob/sum(postprob)
        model.fits <- model.fits[occam]
    }
    bic <- bic + 2 * log(prior)
    probne0 <- round(100 * t(which) %*% as.matrix(postprob),
        1)
    nmod <- length(bic)
    nvar <- max(unlist(glm.assign))
    Ebi <- rep(0, nvar)
    SDbi <- rep(0, nvar)
    EbiMk <- matrix(rep(0, nmod * nvar), nrow = nmod)
    sebiMk <- matrix(rep(0, nmod * nvar), nrow = nmod)
    for (i in (1:ncol(x))) {
        whereisit <- glm.assign[[i + 1]]
        if (any(which[, i]))
            for (k in (1:nmod)) if (which[k, i] == TRUE) {
                spot <- sum(which[k, (1:i)])
                posMk <- (c(0, cumsum(fac.levels[which[k, ]])) +
                  1)[spot]
                posMk <- posMk:(posMk + fac.levels[i] - 1) +
                  1
                EbiMk[k, whereisit] <- model.fits[[k]][posMk,
                  1]
                sebiMk[k, whereisit] <- model.fits[[k]][posMk,
                  2]
            }
    }
    for (k in 1:nmod) {
        EbiMk[k, 1] <- model.fits[[k]][1, 1]
        sebiMk[k, 1] <- model.fits[[k]][1, 2]
    }
    Ebi <- postprob %*% EbiMk
    Ebimat <- matrix(rep(Ebi, nmod), nrow = nmod, byrow = TRUE)
    SDbi <- sqrt(postprob %*% (sebiMk^2) + postprob %*% ((EbiMk -
        Ebimat)^2))
    CSDbi <- rep(0, nvar)
    CEbi <- CSDbi
    for (i in (1:ncol(x))) {
        sel <- which[, i]
        if (sum(sel) > 0) {
            cpp <- rbind(postprob[sel]/sum(postprob[sel]))
            CEbi[glm.assign[[i + 1]]] <- as.numeric(cpp %*% EbiMk[sel,
                glm.assign[[i + 1]]])
            CSDbi[glm.assign[[i + 1]]] <- sqrt(cpp %*% (sebiMk[sel,
                glm.assign[[i + 1]]]^2) + cpp %*% ((EbiMk[sel,
                glm.assign[[i + 1]]] - CEbi[glm.assign[[i + 1]]])^2))
        }
    }
    CSDbi[1] <- SDbi[1]
    CEbi[1] <- Ebi[1]
    names(output.names) <- var.names
    result <- list(postprob = postprob, label = label, deviance = dev,
        size = size, bic = bic, prior.param = prior.param, prior.model.weights = prior/prior.weight.denom,
        family = famname, disp = disp, which = which, probne0 = c(probne0),
        postmean = as.vector(Ebi), postsd = as.vector(SDbi),
        condpostmean = CEbi, condpostsd = CSDbi, mle = EbiMk,
        se = sebiMk, namesx = var.names, reduced = reduced, dropped = dropped,
        call = cl, n.models = length(postprob), n.vars = length(probne0),
        nests = length(Ebi), output.names = output.names, assign = glm.assign,
        factor.type = factor.type, design = leaps.x, x = x, y = y)
    class(result) <- "bic.glm"
    result
}







aic.glmMN<-function (x, y, glm.family, wt = rep(1, nrow(x)), strict = FALSE,
    prior.param = c(rep(0.5, ncol(x))),
    OR = 1000000000000000000000000000000000000000000,
    maxCol = 30,
    OR.fix = 20, nbest = 1000000, dispersion = NULL, factor.type = TRUE,
    factor.prior.adjust = FALSE, occam.window = TRUE, call = NULL,
   inter.list=NULL,keep.list=NULL, all.none.list=NULL,
    either.or.list=NULL)
    {
    leaps.glm <- function(info, coef, names.arg, nbest = nbest) {
        names.arg <- names.arg
        if (is.null(names.arg))
            names.arg <- c(as.character(1:9), LETTERS, letters)[1:ncol(info)]
        if (length(names.arg) < ncol(info))
            stop("Too few names")
        bIb <- coef %*% info %*% coef
        kx <- ncol(info)
        maxreg <- nbest * kx
        if (kx < 3)
            stop("Too few independent variables")
        imeth <- 1
        df <- kx + 1
        Ib <- info %*% coef
        rr <- cbind(info, Ib)
        rr <- rbind(rr, c(Ib, bIb))
        it <- 0
        n.cols <- kx + 1
        nv <- kx + 1
        nf <- 0
        no <- 1e+05
        ib <- 1
        mb <- nbest
        nd <- n.cols
        nc <- 4 * n.cols
        rt <- matrix(rep(0, times = nd * nc), ncol = nc)
        rt[, 1:n.cols] <- rr
        iw <- c(1:(kx + 1), rep(0, times = 4 * nd))
        nw <- length(iw)
        rw <- rep(0, times = 2 * mb * kx + 7 * nd)
        nr <- length(rw)
        t1 <- 2
        s2 <- -1
        ne <- 0
        iv <- 0
        nret <- mb * kx
        Subss <- rep(0, times = nret)
        RSS <- Subss
        ans <- .Fortran("fwleaps", as.integer(nv), as.integer(it),
            as.integer(kx), as.integer(nf), as.integer(no), as.integer(1),
            as.double(2), as.integer(mb), as.double(rt), as.integer(nd),
            as.integer(nc), as.integer(iw), as.integer(nw), as.double(rw),
            as.integer(nr), as.double(t1), as.double(s2), as.integer(ne),
            as.integer(iv), as.double(Subss), as.double(RSS),
            as.integer(nret), PACKAGE = "BMA")
        regid <- ans[[21]]/2
        r2 <- ans[[20]]
        nreg <- sum(regid > 0)
        regid <- regid[1:nreg]
        r2 <- r2[1:nreg]
        which <- matrix(TRUE, nreg, kx)
        z <- regid
        which <- matrix(as.logical((rep.int(z, kx)%/%rep.int(2^((kx -
            1):0), rep.int(length(z), kx)))%%2), byrow = FALSE,
            ncol = kx)
        size <- which %*% rep(1, kx)
        label <- character(nreg)
        sep <- if (all(nchar(names.arg) == 1))
            ""
        else ","
        for (i in 1:nreg) label[i] <- paste(names.arg[which[i,
            ]], collapse = sep)
        ans <- list(r2 = r2, size = size, label = label, which = which)
        return(ans)
    }
    factor.names <- function(x) {
        out <- list()
        for (i in 1:ncol(x)) if (is.factor(x[, i]))
            out[[i]] <- levels(x[, i])
        else out <- c(out, list(NULL))
        attributes(out)$names <- names(x)
        return(out)
    }
    create.assign <- function(xx) {
        asgn <- list()
        asgn[[1]] <- 1
        cnt <- 2
        for (i in 1:ncol(x)) {
            if (!is.factor(x[, i]))
                size <- 1
            else size <- length(levels(x[, i])) - 1
            asgn[[i + 1]] <- cnt:(cnt + size - 1)
            cnt <- cnt + size
        }
        names(asgn) <- c("(Intercept)", attributes(x)$names)
        return(asgn)
    }
    dropcols <- function(x, y, glm.family, wt, maxCols = 30) {
        vnames <- attributes(x)$names
        nvar <- length(vnames)
        isfac <- rep(FALSE, times = nvar)
        for (i in 1:nvar) isfac[i] <- is.factor(x[, i])
        nlevels <- rep(NA, times = nvar)
        for (i in 1:nvar) if (isfac[i])
            nlevels[i] <- length(levels(x[, i]))
        any.dropped <- FALSE
        mm <- model.matrix(terms.formula(~., data = x), data = x)
        designx <- attributes(mm)$assign
        n.designx <- length(designx)
        designx.levels <- rep(1, times = n.designx)
        for (i in 2:n.designx) if (isfac[designx[i]])
            designx.levels[i] <- sum(designx[1:i] == designx[i]) +
                1
        x.df <- data.frame(x = x)
        glm.out <- glm(y ~ ., family = glm.family, weights = wt,
            data = x.df)
        glm.assign <- create.assign(x)
        while (length(glm.out$coefficients) > maxCol) {
            any.dropped <- TRUE
            dropglm <- drop1(glm.out, test = "Chisq")
            dropped <- which.max(dropglm$"Pr(Chi)"[-1]) + 1
            x.df <- x.df[, -(dropped - 1)]
            designx.levels <- designx.levels[-dropped]
            designx <- designx[-dropped]
            glm.out <- glm(y ~ ., family = glm.family, weights = wt,
                data = x.df)
        }
        remaining.vars <- unique(designx[-1])
        new.nvar <- length(remaining.vars)
        dropped.vars <- vnames[-remaining.vars]
        dropped.levels <- NULL
        ncol.glm <- ncol(x.df) - 1
        x.df <- x.df[-(ncol.glm + 1)]
        xx <- data.frame(matrix(rep(NA, times = new.nvar * nrow(x.df)),
            ncol = new.nvar))
        new.names = rep(NA, times = new.nvar)
        for (i in 1:new.nvar) {
            cvar <- remaining.vars[i]
            lvls <- designx.levels[cvar == designx]
            if (isfac[cvar]) {
                if (length(lvls) != length(levels(x[, cvar]))) {
                  newvar <- (as.matrix(x.df[, cvar == designx[-1]]) %*%
                    cbind(lvls - 1)) + 1
                  xx[, i] <- factor(levels(x[, cvar])[newvar])
                  new.names[i] <- vnames[cvar]
                  removed.levels <- levels(x[, cvar])[-c(1, lvls)]
                  dropped.levels <- c(dropped.levels, paste(vnames[cvar],
                    "_", removed.levels, sep = ""))
                }
                else {
                  xx[, i] <- factor(x[, cvar])
                  new.names[i] <- vnames[cvar]
                }
            }
            else {
                xx[, i] <- x[, cvar]
                new.names[i] <- vnames[cvar]
            }
        }
        dropped <- c(dropped.vars, dropped.levels)
        return(list(mm = xx, any.dropped = any.dropped, dropped = dropped,
            var.names = new.names, remaining.vars = remaining.vars))
    }
    if (is.null(call))
        cl <- match.call()
    else cl <- call
    options(contrasts = c("contr.treatment", "contr.treatment"))
    prior.weight.denom <- 0.5^ncol(x)
    x <- data.frame(x)
    names.arg <- names(x)
    if (is.null(names.arg))
        names.arg <- paste("X", 1:ncol(x), sep = "")
    x2 <- na.omit(x)
    used <- match(row.names(x), row.names(x2))
    omitted <- seq(nrow(x))[is.na(used)]
    if (length(omitted) > 0) {
        wt <- wt[-omitted]
        x <- x2
        y <- y[-omitted]
        warning(paste("There were ", length(omitted), "records deleted due to NA's"))
    }
    leaps.x <- x
    output.names <- names(x)
    fn <- factor.names(x)
    factors <- !all(unlist(lapply(fn, is.null)))
    x.df <- data.frame(x = x)
    glm.out <- glm(y ~ ., family = glm.family, weights = wt,
        data = x.df)
    glm.assign <- create.assign(x)
    fac.levels <- unlist(lapply(glm.assign, length)[-1])
    if (factors) {
        cdf <- cbind.data.frame(y = y, x)
        mm <- model.matrix(formula(cdf), data = cdf)[, -1, drop = FALSE]
        mmm <- data.frame(matrix(mm, nrow = nrow(mm), byrow = FALSE))
        names(mmm) <- dimnames(mm)[[2]]
        output.names <- names(mmm)
        if (factor.type) {
            for (i in 1:length(names(x))) {
                if (!is.null(fn[[i]])) {
                  nx <- names(x)[i]
                  coefs <- glm.out$coef[glm.assign[[i + 1]]]
                  old.vals <- x[, i]
                  new.vals <- c(0, coefs)
                  new.vec <- as.vector(new.vals[match(old.vals,
                    fn[[i]])])
                  leaps.x[, nx] <- new.vec
                }
            }
        }
        else {
            new.prior <- NULL
            for (i in 1:length(names(x))) {
                addprior <- prior.param[i]
                if (!is.null(fn[[i]])) {
                  k <- length(fn[[i]])
                  if (factor.prior.adjust)
                    addprior <- rep(1 - (1 - prior.param[i])^(1/(k -
                      1)), k - 1)
                  else addprior <- rep(prior.param[i], k - 1)
                }
                new.prior <- c(new.prior, addprior)
            }
            prior.param <- new.prior
            x <- leaps.x <- mmm
        }
    }
    xx <- data.frame()
    xx <- dropcols(leaps.x, y, glm.family, wt, maxCol)
    var.names <- xx$var.names
    remaining <- xx$remaining.vars
    leaps.x <- xx$mm
    reduced <- xx$any.dropped
    dropped <- NULL
    if (reduced)
        dropped <- xx$dropped
    nvar <- length(x[1, ])
    x <- x[, remaining, drop = FALSE]
    x <- data.frame(x)
    fac.levels <- fac.levels[remaining]
    output.names <- list()
    for (i in 1:length(var.names)) {
        if (is.factor(x[, i]))
            output.names[[i]] <- levels(x[, i])
        else output.names[[i]] <- NA
    }
    xnames <- names(x)
    names(leaps.x) <- var.names
    x.df <- data.frame(x = leaps.x)
    glm.out <- glm(y ~ ., family = glm.family, weights = wt,
        data = x.df, x = TRUE)
    glm.assign <- create.assign(leaps.x)
    if (factor.type == FALSE)
        fac.levels <- unlist(lapply(glm.assign, length)[-1])
    famname <- glm.out$family["family"]$family
    if (is.null(dispersion)) {
        if (famname == "poisson" | famname == "binomial")
            dispersion <- FALSE
        else dispersion <- TRUE
    }
    nobs <- length(y)
    resid <- resid(glm.out, "pearson")
    rdf <- glm.out$df.resid
    is.wt <- !all(wt == rep(1, nrow(x)))
    if (is.wt) {
        resid <- resid * sqrt(wt)
        excl <- wt == 0
        if (any(excl)) {
            warning(paste(sum(excl), "rows with zero wts not counted"))
            resid <- resid[!excl]
        }
    }
    phihat <- sum(resid^2)/rdf
    if (dispersion)
        disp <- phihat
    else disp <- 1
    coef <- glm.out$coef[-1]
    p <- glm.out$rank
    R <- glm.out$R
    rinv <- diag(p)
    rinv <- backsolve(R, rinv)
    rowlen <- drop(((rinv^2) %*% rep(1, p))^0.5)
    sigx <- rowlen %o% sqrt(disp)
    correl <- rinv %*% t(rinv) * outer(1/rowlen, 1/rowlen)
    cov <- correl * sigx %*% t(sigx)
    info <- solve(cov[-1, -1])
    if (ncol(x) > 2) {
        a <- leaps.glm(info, coef, names.arg = names(leaps.x),
            nbest = nbest)




       inter.remove<-function(obj, this){
           out <- NULL
           base1<-this[1]
           base2<-this[2]
           inter<-this[3]
           selector1<-(obj$which[,inter]==TRUE & obj$which[,base1]==FALSE)==FALSE
           selector2<-(obj$which[,inter]==TRUE & obj$which[,base2]==FALSE)==FALSE
           selector<-(selector1*selector2)==1

           out$which<-obj$which[selector,]
           out$size<-obj$size[selector]
           out$r2<-obj$r2[selector]
           out$label<-obj$label[selector]
           out
       }

        keep.remove<-function(obj, keep){
            out<-NULL
            selector<-obj$which[,keep]==TRUE
            out$which<-obj$which[selector,]
            out$size<-obj$size[selector]
            out$r2<-obj$r2[selector]
            out$label<-obj$label[selector]
            out
        }

        all.none.remove<-function(obj, all.none){
            out<-NULL
            number<-length(all.none)
            selector1<-obj$which[,all.none[1]]==TRUE
            selector2<-obj$which[,all.none[1]]==FALSE
            for (i in 2:number){
                selector1 <- selector1 * (obj$which[,all.none[i]]== TRUE)
                selector2 <- selector2 * (obj$which[,all.none[i]]== FALSE)
            }
            selector<- selector1 == 1 | selector2==1
            out$which<-obj$which[selector,]
            out$size<-obj$size[selector]
            out$r2<-obj$r2[selector]
            out$label<-obj$label[selector]
            out
        }

        either.or.remove<-function(obj, either.or){
            out<-NULL
            eo1<-either.or[1]
            eo2<-either.or[2]
            selector<-(obj$which[,eo1]==TRUE & obj$which[,eo2]==TRUE)==FALSE
            out$which<-obj$which[selector,]
            out$size<-obj$size[selector]
            out$r2<-obj$r2[selector]
            out$label<-obj$label[selector]
            out
        }

        if(!is.null(inter.list)){
            for (i in 1:length(inter.list)){
               a <- inter.remove(obj=a, this=inter.list[[i]])
            }
        }
        if(!is.null(keep.list)){
            for (i in 1:length(keep.list)){
               a <- keep.remove(obj=a, keep=keep.list[[i]])
            }
        }
        if(!is.null(all.none.list)){
            for (i in 1:length(all.none.list)){
               a <- all.none.remove(obj=a, all.none=all.none.list[[i]])
            }
        }
        if(!is.null(either.or.list)){
            for (i in 1:length(either.or.list)){
            a <- either.or.remove(obj=a, either.or=either.or.list[[i]])
            }
        }


        a$r2 <- pmin(pmax(0, a$r2), 0.999)
        a$r2 <- c(0, a$r2)
        a$size <- c(0, a$size)
        a$label <- c("NULL", a$label)
        a$which <- rbind(rep(FALSE, ncol(x)), a$which)
        nmod <- length(a$size)
        prior.mat <- matrix(rep(prior.param, nmod), nmod, ncol(leaps.x),
            byrow = TRUE)
        prior <- apply(a$which * prior.mat + (!a$which) * (1 -
            prior.mat), 1, prod)
        bIb <- as.numeric(coef %*% info %*% coef)
        lrt <- bIb - (a$r2 * bIb)
        aic <- lrt + (a$size) * log(nobs) - 2 * log(prior)
        occam <- aic - min(aic) < 2 * OR.fix * log(OR)
        size <- a$size[occam]
        label <- a$label[occam]
        which <- a$which[occam, , drop = FALSE]
        aic <- aic[occam]
        prior <- prior[occam]
    }
    else {
        nmod <- switch(ncol(x), 2, 4)
        aic <- label <- rep(0, nmod)
        which <- matrix(c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE,
            TRUE, TRUE), nmod, nmod/2)
        size <- c(0, 1, 1, 2)[1:nmod]
        sep <- if (all(nchar(names.arg) == 1))
            ""
        else ","
        prior.mat <- matrix(rep(prior.param, nmod), nmod, ncol(x),
            byrow = TRUE)
        prior <- apply(which * prior.mat + (!which) * (1 - prior.mat),
            1, prod)
        for (k in 1:nmod) {
            if (k == 1)
                label[k] <- "NULL"
            else label[k] <- paste(names.arg[which[k, ]], collapse = sep)
        }
    }
    nmod <- length(label)
    model.fits <- as.list(rep(0, nmod))
    dev <- rep(0, nmod)
    df <- rep(0, nmod)
    for (k in 1:nmod) {
        if (sum(which[k, ]) == 0) {
            glm.out <- glm(y ~ 1, family = glm.family, weights = wt)
        }
        else {
            x.df <- data.frame(x = x[, which[k, ]])
            glm.out <- glm(y ~ ., data = x.df, family = glm.family,
                weights = wt)
        }
        dev[k] <- glm.out$deviance
        df[k] <- glm.out$df.residual
        model.fits[[k]] <- matrix(0, nrow = length(glm.out$coef),
            ncol = 2)
        model.fits[[k]][, 1] <- glm.out$coef
        coef <- glm.out$coef
        p <- glm.out$rank
        R <- glm.out$R
        rinv <- diag(p)
        rinv <- backsolve(R, rinv)
        rowlen <- drop(((rinv^2) %*% rep(1, p))^0.5)
        aic[k]<-glm.out$aic
        sigx <- rowlen %o% sqrt(disp)
        correl <- rinv %*% t(rinv) * outer(1/rowlen, 1/rowlen)
        cov <- correl * sigx %*% t(sigx)
        model.fits[[k]][, 2] <- sqrt(diag(cov))
    }
    if (occam.window)
        occam <- aic - min(aic) < 2 * log(OR)
    else occam = rep(TRUE, length(aic))
    dev <- dev[occam]
    df <- df[occam]
    size <- size[occam]
    label <- label[occam]
    which <- which[occam, , drop = FALSE]
    aic <- aic[occam]
    prior <- prior[occam]
    model.fits <- model.fits[occam]
    postprob <- exp(-0.5 * (aic - min(aic)))/sum(exp(-0.5 * (aic -
        min(aic))))
    order.aic <- order(aic, size, label)
    dev <- dev[order.aic]
    df <- df[order.aic]
    size <- size[order.aic]
    label <- label[order.aic]
    which <- which[order.aic, , drop = FALSE]
    aic <- aic[order.aic]
    prior <- prior[order.aic]
    postprob <- postprob[order.aic]
    model.fits <- model.fits[order.aic]
    nmod <- length(aic)
    if (strict & (nmod != 1)) {
        occam <- rep(TRUE, nmod)
        for (k in (2:nmod)) for (j in (1:(k - 1))) {
            which.diff <- which[k, ] - which[j, ]
            if (all(which.diff >= 0))
                occam[k] <- FALSE
        }
        dev <- dev[occam]
        df <- df[occam]
        size <- size[occam]
        label <- label[occam]
        which <- which[occam, , drop = FALSE]
        aic <- aic[occam]
        prior <- prior[occam]
        postprob <- postprob[occam]
        postprob <- postprob/sum(postprob)
        model.fits <- model.fits[occam]
    }
    probne0 <- round(100 * t(which) %*% as.matrix(postprob),
        1)
    nmod <- length(aic)
    nvar <- max(unlist(glm.assign))
    Ebi <- rep(0, nvar)
    SDbi <- rep(0, nvar)
    EbiMk <- matrix(rep(0, nmod * nvar), nrow = nmod)
    sebiMk <- matrix(rep(0, nmod * nvar), nrow = nmod)
    for (i in (1:ncol(x))) {
        whereisit <- glm.assign[[i + 1]]
        if (any(which[, i]))
            for (k in (1:nmod)) if (which[k, i] == TRUE) {
                spot <- sum(which[k, (1:i)])
                posMk <- (c(0, cumsum(fac.levels[which[k, ]])) +
                  1)[spot]
                posMk <- posMk:(posMk + fac.levels[i] - 1) +
                  1
                EbiMk[k, whereisit] <- model.fits[[k]][posMk,
                  1]
                sebiMk[k, whereisit] <- model.fits[[k]][posMk,
                  2]
            }
    }
    for (k in 1:nmod) {
        EbiMk[k, 1] <- model.fits[[k]][1, 1]
        sebiMk[k, 1] <- model.fits[[k]][1, 2]
    }
    Ebi <- postprob %*% EbiMk
    Ebimat <- matrix(rep(Ebi, nmod), nrow = nmod, byrow = TRUE)
    SDbi <- sqrt(postprob %*% (sebiMk^2) + postprob %*% ((EbiMk -
        Ebimat)^2))
    CSDbi <- rep(0, nvar)
    CEbi <- CSDbi
    for (i in (1:ncol(x))) {
        sel <- which[, i]
        if (sum(sel) > 0) {
            cpp <- rbind(postprob[sel]/sum(postprob[sel]))
            CEbi[glm.assign[[i + 1]]] <- as.numeric(cpp %*% EbiMk[sel,
                glm.assign[[i + 1]]])
            CSDbi[glm.assign[[i + 1]]] <- sqrt(cpp %*% (sebiMk[sel,
                glm.assign[[i + 1]]]^2) + cpp %*% ((EbiMk[sel,
                glm.assign[[i + 1]]] - CEbi[glm.assign[[i + 1]]])^2))
        }
    }
    CSDbi[1] <- SDbi[1]
    CEbi[1] <- Ebi[1]
    names(output.names) <- var.names
    result <- list(postprob = postprob, label = label, deviance = dev,
        size = size, aic = aic, prior.param = prior.param, prior.model.weights = prior/prior.weight.denom,
        family = famname, disp = disp, which = which, probne0 = c(probne0),
        postmean = as.vector(Ebi), postsd = as.vector(SDbi),
        condpostmean = CEbi, condpostsd = CSDbi, mle = EbiMk,
        se = sebiMk, namesx = var.names, reduced = reduced, dropped = dropped,
        call = cl, n.models = length(postprob), n.vars = length(probne0),
        nests = length(Ebi), output.names = output.names, assign = glm.assign,
        factor.type = factor.type, design = leaps.x, x = x, y = y)
    class(result) <- "aic.glm"
    result
}


summary.aic.glm<-function (object, n.models = 5, digits = max(3, getOption("digits") -
    3), conditional = FALSE, display.dropped = FALSE, ...)
{
    x <- object
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    if (display.dropped & x$reduced) {
        cat("\nThe following variables were dropped prior to averaging:\n")
        cat(x$dropped)
        cat("\n")
    }
    n.models <- min(n.models, x$n.models)
    sel <- 1:n.models
    cat("\n ", length(x$postprob), " models were selected")
    cat("\n Best ", n.models, " models (cumulative posterior probability = ",
        round(sum(x$postprob[sel]), digits), "): \n\n")
    x$namesx <- c("Intercept", x$namesx)
    nms <- length(x$namesx)
    ncx <- length(unlist(x$assign))
    nvar <- rep(0, times = n.models)
    for (i in 1:(nms - 1)) nvar <- nvar + as.numeric(as.vector(rbind(rep(1,
        length(x$assign[[i + 1]]))) %*% (t(x$mle[sel, x$assign[[i +
        1]], drop = FALSE] != 0)) > 0))
    modelposts <- format(round(x$postprob[sel], 3), digits = 3)
    coeffs <- t(x$mle[sel, , drop = FALSE])
    cfaic <- rbind(x$aic[sel], coeffs)
    cfaicf <- format(cfaic, digits = digits)
    coeffsf <- cfaicf[-1, , drop = FALSE]
    aic <- cfaicf[1, , drop = FALSE]
    postmeans <- format(x$postmean, digits = digits)
    postsds <- format(x$postsd, digits = digits)
    postmeans[is.na(x$postmean)] <- ""
    postsds[is.na(x$postsd)] <- ""
    if (conditional) {
        cpostmeans <- format(x$condpostmean, digits = digits)
        cpostsds <- format(x$condpostsd, digits = digits)
        cpostmeans[is.na(x$condpostmean)] <- ""
        cpostsds[is.na(x$condpostsd)] <- ""
    }
    varposts <- format(round(x$probne0, 1), digits = 3)
    strlength <- nchar(coeffsf[1, 1])
    decpos <- nchar(unlist(strsplit(coeffsf[2, 1], "\\."))[1])
    offset <- paste(rep(" ", times = decpos - 1), sep = "", collapse = "")
    offset2 <- paste(rep(" ", times = decpos + 1), sep = "",
        collapse = "")
    modelposts <- paste(offset, modelposts, sep = "")
    nvar <- paste(offset2, nvar, sep = "")
    dotoffset <- round(max(nchar(coeffsf))/2)
    zerocoefstring <- paste(paste(rep(" ", times = dotoffset),
        collapse = "", sep = ""), ".", sep = "")
    coeffsf[coeffs == 0] <- zerocoefstring
    coeffsf[is.na(coeffs)] <- ""
    avp <- NULL
    outnames <- c(NA, x$output.names)
    names(outnames)[1] <- "Intercept"
    varposts <- c("100", varposts)
    for (i in 1:nms) {
        avp <- rbind(avp, varposts[i])
        if (!is.na(outnames[[i]][1]))
            avp <- rbind(avp, cbind(rep("", times = length(x$assign[[i]]))))
    }
    top <- cbind(postmeans, postsds)
    if (conditional)
        top <- cbind(top, cpostmeans, cpostsds)
    top <- cbind(top, coeffsf)
    atop <- NULL
    for (i in 1:nms) {
        if (!is.na(outnames[[i]][1]))
            atop <- rbind(atop, rbind(rep("", times = ncol(top))))
        atop <- rbind(atop, top[x$assign[[i]], ])
    }
    top <- cbind(avp, atop)
    linesep <- rep("", times = ncol(top))
    offset <- c("", "", "")
    if (conditional)
        offset <- c(offset, c("", ""))
    bottom <- rbind(c(offset, nvar), c(offset, aic), c(offset,
        modelposts))
    out <- rbind(top, linesep, bottom)
    vnames <- NULL
    for (i in 1:nms) {
        vnames <- c(vnames, names(outnames[i]))
        blnk <- paste(rep(" ", times = nchar(names(outnames[i]))),
            collapse = "")
        if (!is.na(outnames[i][1]))
            vnames <- c(vnames, paste(blnk, unlist(outnames[i])[-1],
                sep = "."))
    }
    row.names(out) <- c(vnames, "", "nVar", "AIC", "post prob")
    colnms <- c("p!=0", " EV", "SD")
    if (conditional)
        colnms <- c(colnms, "cond EV", "cond SD")
    colnms <- c(colnms, paste("model ", 1:n.models, sep = ""))
    dimnames(out)[[2]] <- colnms
    print.default(out, print.gap = 2, quote = FALSE, ...)
}

plot.aic.glm<-function (x, e = 1e-04, mfrow = NULL, include = 1:length(x$namesx),
    ...)
{
    plotvar <- function(prob0, mixprobs, means, sds, Emean, Esd,
        name, e = 1e-04, nsteps = 500, ...) {
        if (prob0 == 1) {
            xlower <- -0
            xupper <- 0
            xmax <- 1
        }
        else {
            qmin <- qnorm(e/2, Emean, Esd)
            qmax = qnorm(1 - e/2, Emean, Esd)
            xlower <- min(qmin, 0)
            xupper <- max(0, qmax)
        }
        xx <- seq(xlower, xupper, length.out = nsteps)
        yy <- rep(0, times = length(xx))
        maxyy <- 1
        if (prob0 < 1) {
            for (j in 1:length(means)) yy <- yy + mixprobs[j] *
                dnorm(xx, means[j], sds[j])
            maxyy <- max(yy)
        }
        ymax <- max(prob0, 1 - prob0)
        plot(c(xlower, xupper), c(0, ymax), type = "n", xlab = "",
            ylab = "", main = name, ...)
        lines(c(0, 0), c(0, prob0), lty = 1, lwd = 3, ...)
        lines(xx, (1 - prob0) * yy/maxyy, lty = 1, lwd = 1, ...)
    }
    vars <- unlist(x$assign[include + 1])
    nvar <- length(vars)
    probs <- NULL
    for (i in include) probs <- c(probs, rep(x$probne0[i], times = length(x$assign[[i +
        1]])))
    nms <- NULL
    for (i in include) {
        if (is.na(x$output.names[i][1]))
            nms <- c(nms, names(x$output.names[i]))
        else nms <- c(nms, paste(names(x$output.names[i]), unlist(x$output.names[i])[-1],
            sep = "."))
    }
    wwhich <- NULL
    for (i in include) {
        wwhich <- cbind(wwhich, matrix(rep(x$which[, i], times = length(x$assign[[i +
            1]])), ncol = length(x$assign[[i + 1]])))
    }
    if (!is.null(mfrow)) {
        lo <- mfrow
        losize <- lo[1] * lo[2]
    }
    else {
        layoutsizes <- c(1, 4, 9)
        layouts <- rbind(c(1, 1), c(2, 2), c(3, 3))
        layout <- max((1:length(layoutsizes))[layoutsizes <=
            nvar])
        losize <- layoutsizes[layout]
        lo <- layouts[layout, ]
    }
    keep.mfrow = par()$mfrow
    par(mfrow = c(1, 1))
    par(ask = FALSE)
    par(mfrow = lo)
    ngroups <- ceiling(nvar/losize)
    for (k in 1:ngroups) {
        for (ii in ((k - 1) * losize + 1):min(k * losize, nvar)) {
            i <- vars[ii] - 1
            prob0 <- 1 - probs[i]/100
            sel <- wwhich[, i]
            mixprobs <- x$postprob[sel]/(1 - prob0)
            means <- x$mle[sel, i + 1]
            sds <- x$se[sel, i + 1]
            Emean <- x$condpostmean[i + 1]
            Esd <- x$condpostsd[i + 1]
            name <- nms[i]
            plotvar(prob0, mixprobs, means, sds, Emean, Esd,
                name, e = e, ...)
        }
        par(ask = TRUE)
    }
    par(mfrow = keep.mfrow)
    par(ask = FALSE)
}
