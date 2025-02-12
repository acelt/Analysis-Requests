ci_model <-  function(data, y_var, x_vars){
    object <- glm(data = data,
                family = "quasibinomial",
                formula = data[[y_var]]/100 ~ data[[x_vars[1]]]:data[[x_vars[2]]])
    return(object)
  
}

tdat_ttest <-  function(data, y_var, x_vars){
  
}

logit_to_real_cis <- function(object,level=0.95) {
  
  sum.obj <- summary(object)
  z.score <- c(qnorm((1-level)/2),qnorm(1-(1-level)/2))
  
  logit.means <- c(sum.obj$coefficients[1,1],sum.obj$coefficients[1,1]+sum.obj$coefficients[-1,1])
  real.means <- plogis(logit.means)
  
  n.groups <- dim(sum.obj$coefficients)[1]
  
  out.df <- data.frame(Group=rownames(sum.obj$coefficients),Mean=real.means,LCI=NA,UCI=NA)
  
  logit.var <- numeric(n.groups)
  for(i in 1:n.groups) {
    if(i==1) {logit.var[i] <- vcov(object)[i,i]}
    if(i>1) {
      vc <- vcov(object)[c(1,i),c(1,i)]
      logit.var[i] <- matrix(c(1,1),nrow=1,ncol=2) %*% vc %*% matrix(c(1,1),nrow=2,ncol=1)
    }
    out.df[i,c('LCI','UCI')] <- plogis(logit.means[i]+z.score*sqrt(logit.var[i]))
  }
  
  return(out.df)
}


log_to_real_cis <- function(object,level=0.95) {
  
  sum.obj <- summary(object)
  z.score <- c(qnorm((1-level)/2),qnorm(1-(1-level)/2))
  
  log.means <- c(sum.obj$coefficients[1,1],sum.obj$coefficients[1,1]+sum.obj$coefficients[-1,1])
  real.means <- exp(log.means)
  
  n.groups <- dim(sum.obj$coefficients)[1]
  
  out.df <- data.frame(Group=rownames(sum.obj$coefficients),Mean=real.means,LCI=NA,UCI=NA)
  
  log.var <- numeric(n.groups)
  for(i in 1:n.groups) {
    if(i==1) {log.var[i] <- vcov(object)[i,i]}
    if(i>1) {
      vc <- vcov(object)[c(1,i),c(1,i)]
      logit.var[i] <- matrix(c(1,1),nrow=1,ncol=2) %*% vc %*% matrix(c(1,1),nrow=2,ncol=1)
    }
    out.df[i,c('LCI','UCI')] <- exp(log.means[i]+z.score*sqrt(log.var[i]))
  }
  
  return(out.df)
}
