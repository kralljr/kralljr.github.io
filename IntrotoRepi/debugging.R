
glmOR <- function(dat, vars1, outcome) {
    
    # Set up outcome matrix
    or <- matrix(nrow = length(vars), ncol = 4)
    for(i in 1 : length(vars)) {
        # Run univariate regressions for each variable i
        coef1 <- glm(dat[, outcome] ~ dat[, vars[i]], data = dat,
                     family = "binomial")
        # Organize output
        lbub <- exp(confint(coef1)[2, ])
        or[i, ] <- c(exp(coef1$coef[2]), lbub)
        
    }
    # Add in variable names
    or <- data.frame(vars, or)
    # Add column names
    colnames(or) <- c("variable", "or", "lb", "ub")
    
    # Add type to univariate results
    or <- mutate(or, type = "univariate")
    
    # Get equation for multivariate results
    eqn <- paste(outcome, "~", paste(vars, collapse = " + "))
    # Run multivariate model
    coef2 <- glm(eval(eqn), data = dat, family = "binomial")
    lbub <- exp(confint(coef2))
    # Organize output
    mor <- data.frame(vars, cbind(exp(coef2$coef2[-1]), lbub) )
    colnames(mor) <- c("variable", "or", "lb", "ub")
    # Add regression type
    mor <- mutate(mor, type = "multivariate")
    # browser()
    # Get all regression output
    output <- rbind(or, mor)
    
    return(output)
}