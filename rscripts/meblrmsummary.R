#' @title Summary Tables for Mixed-Effects Binomial Logistic Regressions
#'
#' @description This function produces summary tables for mixed-effects binomial logistic regressions by extracting the relevent information from the final and a base-line glmer objects as well as their glm analogs.
#' @param glm0 A base-line glm object of family "binomial" with the intercept as the sole predictor.
#' @param glm1 A glm object of family "binomial" which is analogous to the final glmer object.
#' @param glmer0 A base-line glmer object of family "binomial" with the intercept as the sole predictor.
#' @param glmer1 The final glmer object of family "binomial".
#' @param dpvar A vector containign the values of the dependent variable on which the models were fit.
#' @export
#' @keywords mixed-Effects binomial logistic regression, mixed-effects logistic regression, summary table, function
#' @seealso
#' @return NULL
#' @aliases
#' @examples \dontrun{
#' Example code will come later!
#' }
meblrm.summary <- function(glm0, glm1, glmer0, glmer1, dpvar) {
  p.nice <- function(z) {
    as.vector(unlist(sapply(z, function(w) {
      ifelse(w < .001, return("p < .001***"),
             ifelse(w < .01, return("p <  .01 **"),
                    ifelse(w < .05, return("p <  .05  *"),
                           ifelse(w < .1, return("p <  .10(*)"), return("n.s."))))) } ))) }

  LLglm0 <- logLik(glm0)
  LLglmer0 <- logLik(glmer0)
  LLR01 <- as.vector(- 2 * (LLglm0 - LLglmer0))
  df <- attr(LLglmer0, "df") - attr(LLglm0, "df")
  p <- pchisq(LLR01, df, lower.tail = FALSE)
  headranef <- c("Group(s)", "Variance", "Std. Dev.", " ", "  ", "L.R. X2", "DF", "Pr", "Significance")
  ranef <- c(names(summary(glmer1)[[9]]), round(summary(glmer1)[[13]][[1]][[1]],2),
             round(as.data.frame(VarCorr(glmer1))[[5]], 2),
             "", "", round(LLR01, 2), df, round(p, 4), p.nice(p))

  # take vif-mer function from https://github.com/aufrank/R-hacks/blob/master/mer-utils.R on 14th August, 2014
  vif.mer <- function (fit) {
    ## adapted from rms::vif
    v <- vcov(fit)
    nam <- names(fixef(fit))
    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0) {
      v <- v[-(1:ns), -(1:ns), drop = FALSE]
      nam <- nam[-(1:ns)]
    }
    d <- diag(v)^0.5
    v <- diag(solve(v/(d %o% d)))
    names(v) <- nam
    v
  }
  kappa.mer <- function (fit, scale = TRUE, center = FALSE,
                         add.intercept = TRUE, exact = FALSE) {
    X <- fit@X
    nam <- names(fixef(fit))
    ## exclude intercepts
    nrp <- sum(1 * (nam == "(Intercept)"))
    if (nrp > 0) {
      X <- X[, -(1:nrp), drop = FALSE]
      nam <- nam[-(1:nrp)]
    }
    if (add.intercept) {
      X <- cbind(rep(1), scale(X, scale = scale, center = center))
      kappa(X, exact = exact)
    } else {
      kappa(scale(X, scale = scale, center = scale), exact = exact)
    }
  }
  colldiag.mer <- function (fit, scale = TRUE, center = FALSE,
                            add.intercept = TRUE) {
    result <- NULL
    if (center)
      add.intercept <- FALSE
    if (is.matrix(fit) || is.data.frame(fit)) {
      X <- as.matrix(fit)
      nms <- colnames(fit)
    }
    else if (class(fit) == "mer") {
      nms <- names(fixef(fit))
      X <- fit@X
      if (any(grepl("(Intercept)", nms))) {
        add.intercept <- FALSE
      }
    }
    X <- X[!is.na(apply(X, 1, all)), ]
    if (add.intercept) {
      X <- cbind(1, X)
      colnames(X)[1] <- "(Intercept)"
    }
    X <- scale(X, scale = scale, center = center)
    svdX <- svd(X)
    svdX$d
    condindx <- max(svdX$d)/svdX$d
    dim(condindx) <- c(length(condindx), 1)
    Phi = svdX$v %*% diag(1/svdX$d)
    Phi <- t(Phi^2)
    pi <- prop.table(Phi, 2)
    colnames(condindx) <- "cond.index"
    if (!is.null(nms)) {
      rownames(condindx) <- nms
      colnames(pi) <- nms
      rownames(pi) <- nms
    } else {
      rownames(condindx) <- 1:length(condindx)
      colnames(pi) <- 1:ncol(pi)
      rownames(pi) <- 1:nrow(pi)
    }
    result <- data.frame(cbind(condindx, pi))
    zapsmall(result)
  }
  maxcorr.mer <- function (fit, exclude.intercept = TRUE) {
    so <- summary(fit)
    corF <- so@vcov@factors$correlation
    nam <- names(fixef(fit))
    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0 & exclude.intercept) {
      corF <- corF[-(1:ns), -(1:ns), drop = FALSE]
      nam <- nam[-(1:ns)]
    }
    corF[!lower.tri(corF)] <- 0
    maxCor <- max(corF)
    minCor <- min(corF)
    if (abs(maxCor) > abs(minCor)) {
      zapsmall(maxCor)
    } else {
      zapsmall(minCor)
    }
  }

  # continue with setting up table
  coefs <- summary(glmer1)[[10]]

  se <- sqrt(diag(vcov(glmer1)))
  cilwr <- fixef(glmer1) - 1.96 * se
  ciupr <- fixef(glmer1) + 1.96 * se
  coef.df <- data.frame(
    round(coefs[, 1], 2),
    c("", round(vif.mer(glmer1), 2)),
    round(exp(coefs[, 1]), 2),
    round(exp(cilwr), 2),
    round(exp(ciupr), 2),
    round(coefs[, 2], 2),
    round(coefs[, 3], 2),
    round(coefs[, 4], 4),
    p.nice(coefs[, 4]))
  colnames(coef.df) <- c(colnames(coefs)[1],
                         "VIF",
                         "OddsRatio",
                         "CI(2.5%)",
                         "CI(97.5%)",
                         colnames(coefs)[2],
                         colnames(coefs)[3],
                         colnames(coefs)[4],
                         "Significance")

  coef.df <- rbind(colnames(coef.df), coef.df)
  coef.df <- as.data.frame(coef.df)
  colnames(coef.df) <- c(colnames(coefs)[1],
                         "VIF",
                         "OddsRatio",
                         "CI(2.5%)",
                         "CI(97.5%)",
                         colnames(coefs)[2],
                         colnames(coefs)[3],
                         colnames(coefs)[4],
                         "Significance")

  mdl.statz <- c(rep("", 8), "Value")
  groups <- c(rep("", 8), summary(glmer1)[[9]][[1]])
  nbcases <- c(rep("", 8), length(fitted(glmer1)))
  obs0 <- c(rep("", 8), sum(dpvar == 0))
  obs1  <- c(rep("", 8), sum(dpvar == 1))
  resdev <- c(rep("", 8), round(summary(glmer1)[[3]][[1]][[8]], 2))

  logisticPseudoR2s <- function(glm0, glmer0, glmer1) {
    dev <- deviance(glmer1)
    nullDev <- deviance(glmer0)
    modelN <-  length(fitted(glmer1))

    nullDev.glm <- glm0$null.deviance
    R.l.glm <-  1-dev/nullDev.glm
    R.cs.glm <- 1-exp(-(nullDev.glm-dev)/modelN)
    R.n.glm <- R.cs.glm/(1-(exp(-(nullDev.glm/modelN))))

    return(c(R.l.glm,      # Hosmer and Lemeshow R^2
             R.cs.glm,            # Cox and Snell R^2
             R.n.glm))            # Nagelkerke R^2
  }

  r2s <- logisticPseudoR2s(glm0, glmer0, glmer1)
  R2Nagelkerke <- c(rep("", 8), round(r2s[[3]], 3))
  R2HosmerLemeshow <- c(rep("", 8), round(r2s[[1]], 3))
  R2CoxSnell <- c(rep("", 8), round(r2s[[2]], 3))

  probs <- 1/(1+exp(-fitted(glmer1)))
  modstatz <- somers2(probs, as.numeric(dpvar)-1)

  C <- c(rep("", 8), round(modstatz[[1]], 3))
  Dxy <- c(rep("", 8), round(modstatz[[2]], 3))
  AIC <- c(rep("", 8), round(summary(glmer1)[[14]][[1]], 2))
  BIC <- c(rep("", 8), round(summary(glmer1)[[14]][[2]], 2))


  dpvarneg <- sapply(dpvar, function(x) ifelse(x == 1, 0, 1))
  correct <- sum(dpvar * (predict(glmer1, type = "response") >= 0.5)) + sum(dpvarneg * (predict(glmer1, type="response") < 0.5))
  tot <- length(dpvar)
  predict.acc <- (correct/tot)*100

  Accuracy <- c(rep("", 8), paste(round(predict.acc, 2), "%", sep = "", collapse = ""))

  L0 <- logLik(glm0)
  L1 <- logLik(glmer1)
  L01 <- as.vector(- 2 * (L0 - L1))
  df <- attr(L1, "df") - attr(L0, "df")

  ModelLikelihoodRatioTest <- c(rep("", 5),
                                paste("L.R. X2: ", round(L01, 2), sep = "", collapse = ""),
                                paste("DF: ", df, sep = "", collapse = ""),
                                paste("p-value: ", round(pchisq(L01, df, lower.tail = FALSE), 5), sep = "", collapse = ""),
                                paste("sig: ", p.nice(pchisq(L01, df, lower.tail = FALSE)), sep = "", collapse = ""))

  ranef.tb <- rbind(ranef)
  ranef.df <- as.data.frame(ranef.tb)
  colnames(ranef.df) <- colnames(coef.df)

  gblstz.tb <- rbind(mdl.statz, groups, nbcases, obs0, obs1, resdev,
                     R2Nagelkerke, R2HosmerLemeshow, R2CoxSnell, C, Dxy, AIC, BIC, Accuracy, ModelLikelihoodRatioTest)
  gblstz.df <- as.data.frame(gblstz.tb)
  colnames(gblstz.df) <- colnames(coef.df)

  blr.tb <- rbind(ranef.df, coef.df, gblstz.df)
  colnames(blr.tb) <- headranef
  rownames(blr.tb) <- c("Random Effect(s)", "Fixed Effect(s)", rownames(coefs),
                        "Model statistics", "Number of Groups", "Number of cases in model",
                        "Observed misses", "Observed successes",
                        "Residual deviance", "R2 (Nagelkerke)", "R2 (Hosmer & Lemeshow)",
                        "R2 (Cox & Snell)", "C", "Somers' Dxy", "AIC", "BIC", "Prediction accuracy", "Model Likelihood Ratio Test")
  blr.df <- as.data.frame(blr.tb)
  return(blr.df)
}
