glmRep <- function(reg, data, by, family=gaussian) {
    # repeat a glm on subsets of a data.frame
    # reg = regression formula. e.g. y ~ x
    # data = data.frame
    # by = string indicating level 1 indicator column name. e.g. 'sub'
    # family = error dist and link function as in glm
    L1.names <- unique(data[[by]])

    coefs <- c()
    for (i in 1:length(L1.names)) {
        ss <- paste0(by, "==L1.names[", i, "]")
        nameDat <- subset(data, eval(parse(text=ss)))
        m <- eval(glm(as.formula(reg), family, nameDat))
        coefs <- rbind(coefs, rbind(array(coefficients(m))))
    }

    coefs <- data.frame(coefs)
    colnames(coefs) <- variable.names(m)
    coefs <- cbind(L1.names, coefs)
    colnames(coefs)[1] <- by
    return(coefs)
}

groupStats <- function(dat) {
#compute summary stats for glmRep output data.frame.     
dat <- dat[sapply(dat, is.numeric)]
nrow <- dim(dat)[2]
out <- data.frame(mean=rep(0,nrow), se=rep(0, nrow), t=rep(0,nrow), p=rep(0,nrow))
rownames(out) <- colnames(dat)

for (i in 1:nrow) {
    out[i, "mean"] <- mean(dat[,i])
    out[i, "se"] <- sd(dat[,i]) / sqrt(length(dat[,i]))
    out[i, "t"] <- out[i, "mean"] / out[i, "se"]
    out[i, "p"] <- t.test(dat[,i])[[3]]
}
return(out)
}
