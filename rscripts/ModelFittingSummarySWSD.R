#' @title Summary Tables for the Step-Wise Step-Down Model Fitting Process of Mixed-Effects Binomial Logistic Regression Models
#'
#' @description This function produces summary tables for fixed-effects binomial logistic regressions by extracting the relevent information from a glm and an lrm object.
#' @param mdlcmp A list of model comparisons that result from anova results (e.g. m0m1: anova(m0, m1, test = "Chi))
#' @export
#' @keywords binomial logistic regression, logistic regression, summary table, function
#' @seealso
#' @return NULL
#' @examples \dontrun{
#' Example code will come later!
#' }
mdl.fttng.swsd <- function(mdlcmp){
  mdl.cmp.df1 <- sapply(mdlcmp, function(x) {
    p.nice <- function(z) {
      as.vector(unlist(sapply(z, function(w) {
        ifelse(w < .001, return("p < .001***"),
               ifelse(w < .01, return("p <  .01 **"),
                      ifelse(w < .05, return("p <  .05  *"),
                             ifelse(w < .1, return("p <  .10(*)"), return("n.s."))))) } ))) }

    formula <- ifelse(length(attr(x, "heading")) == 4, gsub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[4]),
                      ifelse(length(attr(x, "heading")) == 5,
                             paste(gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[4])),
                                   gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[5]))),
                             ifelse(length(attr(x, "heading")) == 6,
                                    paste(gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[5])),
                                          gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[6]))), paste("error"))))
    formula <- gsub(" ", "", formula)
    formula.cm <- ifelse(length(attr(x, "heading")) == 4, gsub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[3]),
                         ifelse(length(attr(x, "heading")) == 5,
                                paste(gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[3]))),
                                ifelse(length(attr(x, "heading")) == 6,
                                       paste(gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[3])),
                                             gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[4]))), paste("error"))))
    formula.cm <- gsub(" ", "", formula.cm)
    splt.formula <- strsplit(formula, "[~|+|*]")
    splt.formula.cm <- strsplit(formula.cm, "[~|+|*]")
    model <- ifelse(length(attr(x, "heading")) == 4, gsub(": .*", "", attr(x, "heading")[4]),
                    ifelse(length(attr(x, "heading")) == 5, gsub(": .*", "", attr(x, "heading")[5]),
                           ifelse(length(attr(x, "heading")) == 6, gsub(": .*", "", attr(x, "heading")[6]), "NA")))
    added <- c(as.vector(unlist(splt.formula)), as.vector(unlist(splt.formula.cm)))
    added <- names(which(table(added)==1))
    added   <- paste(added, collapse = "+")
    comp    <- gsub(": .*", "", attr(x ,"heading")[3])
    df      <- x[[1]][[2]]
    aic     <- round(x[[2]][[2]], 2)
    bic     <- round(x[[3]][[2]], 2)
    ll      <- round(x[[4]][[2]], 2)
    dev     <- round(x[[5]][[2]], 2)
    x2      <- round(x[[6]][[2]], 2)
    x2df    <- x[[7]][[2]]
    p       <- round(x[[8]][[2]], 5)
    sig <-  p.nice(z = p)
    mdl.fttng <- cbind(model, formula, added, comp, df, aic, bic, ll, dev, x2, x2df, p, sig)
  }  )
  mdl.cmp.df2 <- t(mdl.cmp.df1)
  colnames(mdl.cmp.df2) <- c("Model", "Formula", "Term Deleted", "Compared to...", "DF", "AIC", "BIC", "LogLikelihood", "Residual Deviance", "X2", "X2DF", "p-value", "Significance")
  mdl.cmp.df2 <- as.data.frame(mdl.cmp.df2)
  return(mdl.cmp.df2)
}
