#' @title Pseudo R2 for Binomial Logistic Mixed-Effects Models
#'
#' @description This function pseudo R2 (Nagelkerke and Cox & Snell) for mixed-effects binomial logistic regression models.
#' @param f A vector of two character strings (e.g. c("dv ~ a + b", "(1 + a | Subject)")). The first element is the fixed-effect part of the regression formula (e.g. "DP ~ Predictor1 + Predictor1") and the second part is the random effect structure (e.g. "(1 | Subject)").
#' @param d The data set on which the model was fit
#' @export
#' @keywords pseudo R2, mixed-effects binomial logistic regression, function
#' @seealso
#' @return NULL
#' @aliases
#' @examples \dontrun{
#' Example code will come later!
#' }
pseudor2 <- function(f, d) {
  lmer.full= lmer(formula= as.formula(paste(f, collapse="+")), d, family="binomial")
  logLik.lmer.full= as.numeric(logLik(lmer.full))
  N.lmer.full= nrow(lmer.full@X)
  cat(paste("Full mixed model: L=", logLik.lmer.full, ", N=", N.lmer.full, "\n", sep=""))

  lmer.intercept= lmer(formula= as.formula(paste(unlist(strsplit(f[1], "~"))[1], paste("1", f[2], sep=" + "), sep="~ ")), data= d, family="binomial")
  logLik.lmer.intercept= as.numeric(logLik(lmer.intercept))
  N.lmer.intercept= nrow(lmer.intercept@X)
  cat(paste("Intercept mixed model: L=", logLik.lmer.intercept, ", N=", N.lmer.intercept, "\n", sep=""))

  lrm.full= lrm(formula= as.formula(f[1]), data= d)
  logLik.lrm.intercept= as.numeric(deviance(lrm.full)[1] / - 2)
  N.lrm.intercept= as.numeric(lrm.full$stats[1])
  cat(paste("Intercept ordinary model: L=", logLik.lrm.intercept, ", N=", N.lrm.intercept, "\n", sep=""))

  coxsnell.lmer= 1 - exp((logLik.lmer.intercept - logLik.lmer.full) * (2/N.lmer.full))
  nagelkerke.lmer= coxsnell.lmer / (1 - exp(logLik.lmer.intercept * (2/N.lmer.full)))
  cat(paste("Full model evaluated against mixed intercept model:\n\tCoxSnell R2: ", coxsnell.lmer, "\n\tNagelkerke R2: ", nagelkerke.lmer,"\n", sep=""))
  coxsnell.lrm= 1 - exp((logLik.lrm.intercept - logLik.lmer.full) * (2/N.lmer.full))
  nagelkerke.lrm= coxsnell.lrm / (1 - exp(logLik.lrm.intercept * (2/N.lmer.full)))
  cat(paste("Full model evaluated against ordinary intercept model:\n\tCoxSnell R2: ", coxsnell.lrm, "\n\tNagelkerke R2: ", nagelkerke.lrm,"\n", sep=""))
  coxsnell.lrm.2= 1 - exp((logLik.lrm.intercept - logLik.lmer.intercept) * (2/N.lmer.full))
  nagelkerke.lrm.2= coxsnell.lrm.2 / (1 - exp(logLik.lrm.intercept * (2/N.lmer.full)))
  cat(paste("Mixed intercept model evaluated against ordinary intercept model:\n\tCoxSnell R2: ", coxsnell.lrm.2, "\n\tNagelkerke R2: ", nagelkerke.lrm.2,"\n", sep=""))
}


