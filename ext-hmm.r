#' fit moveHMM models
#'
#' @param prep
#' @param covs_list
#' @param null_model
#' @param known
#'
#' @return
#' @export
#'
#' @examples
hmm_fit_models <- function(prep, covs_list=list() ,null_model=T,known=NULL) {
  # Params
  stepDist <- "gamma"
  angleDist <- "vm"
  nbStates <- 2

  #state1 resting
  #state moving

  #(state1,state2)
  mu0 <- c(.1,.5)
  #(state1,state2)
  sigma0 <- c(.1,0.3)

  #mu0 <- c(rnorm(1,.5,.1),rnorm(1,.1,.1))
  #sigma0 <- c(rnorm(1,.3,.1),rnorm(1,.1,.1))

  #mu0 <- c(1.5,1)
  #sigma0 <- c(2,1)

  zero_mass0 <- c(0,1) #if we have duplicates (step-length zero) the first zero_mass should not be zero
  stepPar0 <- c(mu0,sigma0) #no zero-inflation, so no zero-mass included

  #kappa is the concentration
  angleMean <- c(0,0) #specify when we want them to be fixed
  kappa0 <- c(1,.1)
  # the angle mean is not estimated, so only the concentration parameter is needed
  anglePar0 <- kappa0

  forms <- list()
  if(null_model)
    forms <- c(forms,~1)

  if(length(covs_list))
  {
    forms_ <- lapply(covs_list, function(cov_l) as.formula(paste0('~',paste0(cov_l,collapse = '+'))) )
    forms <- c(forms, forms_)
  }

  models <- lapply(1:length(forms),function(i){
    fit <- moveHMM::fitHMM(formula=forms[[i]],
                           data=prep,
                           stepDist=stepDist,angleDist=angleDist,nbStates=nbStates,
                           stepPar0=stepPar0,
                           anglePar0=anglePar0,angleMean = angleMean,
                           knownStates = known)
  })
  names(models) <- as.character(forms)
  return(models)
}

#' aic report
#'
#' @param models
#'
#' @return a dataframe of aic sorted values for each fitted model of hidden markov chain model.
#' @export
#'
#' @examples
hmm_aic_reporting <- function(models) {
  aic_agree <- lapply(1:length(models) , function(i){
    n <- names(models)[i]
    aic <- AIC(models[[i]])
    #agreement
    prep1 <- moveHMM::stateProbs(models[[1]])[,1] > .5
    prep2 <- moveHMM::stateProbs(models[[i]])[,1] > .5
    confus <- table(prep2,prep1)
    (agree <- sum(diag(confus))/sum(confus))
    data.frame(name=n,aic=aic,state_agreement=agree)
  })
  aic_agree <- do.call(rbind,aic_agree)

  #order by aic values
  aic_agree <- aic_agree[order(aic_agree$aic,decreasing = F) , ]
  return(aic_agree)
}

#' plot confidence intervals
#'
#' @param m the model from moveHMM
#'
#' @return nothing. it only plots
#' @export
#'
#' @examples
#' plot(m)
hmm_plot_CI <- function(m) {

  coef_ci <- function(df_ci,header) {
    plotrix::plotCI(1:nrow(df_ci),df_ci[,1], ui=df_ci[,3], li=df_ci[,2], xaxt='n',
                    xlab="Covariate",ylab="95% Confidence Interval",lwd=2)

    axis(1,1:nrow(df_ci),rownames(df_ci))
    title(header)
    grid()
    abline(h=0,col=2,lty=2)
  }
  pp <- mapply(function(beta,lower,upper,rows)
  {
    df <- data.frame(beta,lower,upper)
    rownames(df) <- rows
    df
  },
  as.data.frame(m$mle$beta),
  as.data.frame(moveHMM::CI(m)$beta$lower),
  as.data.frame(moveHMM::CI(m)$beta$upper),
  MoreArgs = list(rows=rownames(m$mle$beta)),
  SIMPLIFY = F)
  names(pp) <- colnames(m$mle$beta)
  n <- ncol(m$mle$beta)
  op <- par(mfrow=c(1,n),mar=c(3,4,1,2))
  options(warn=-1)
  invisible(mapply(coef_ci,pp,names(pp)))
  options(warn=0)
  par(op)
}
