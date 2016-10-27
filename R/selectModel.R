#' Model selection
#'
#' Model selection using the stepwise procedure and the chosen criterion.
#'
#' @param X a numeric matrix or an object of class big.matrix (see 'Details').
#' The rows of \code{X} contain the samples, the columns of \code{X} contain
#' the observed variables. If your have variables in rows, see 'Details'.
#'
#' @param y a numeric vector of responses. The length of \code{y} must equal
#' the number of rows of \code{X}.
#'
#' @param Xm a numeric matrix. Additional variables which will be included
#' in the first step of the model selection procedure.
#'
#' @param stay a numeric vector. Columns from \code{Xm} which should be
#' included in all model selection steps.
#'
#' @param intercept a logical. If \code{TRUE}, the intercept will be included.
#'
#' @param minpv a numeric. Variables from \cite{X} with p-values for the
#' Pearson correlation tests higher than \code{minpv} will be excluded from
#' the model selection procedure. If you do not want to do this, set
#' \code{minpv=1} (not recommended if you have big data).
#'
#' @param multif a logical. If \code{TRUE}, the multi-forward step will be
#' performed (see 'Details').
#'
#' @param maxf a numeric, a maximal number of variables in the final model
#' in the multi-forward step.
#'
#' @param minb a numeric, a minimal number of variables in the final model
#' in the backward selection (see 'Details').
#'
#' @param crit a function defining a model selection criterion. You can use
#' your own function or one of these: \code{bic}, \code{mbic}, \code{mbic2},
#' \code{aic}, \code{maic}, \code{maic2}.
#'
#' @param crit.multif a function defining a model selection criterion in the
#' multi-forward step. See \code{crit} (but only criteria with small penalties
#' are recommended).
#'
#' @param maxp a numeric. If \code{X} is big, it will be splitted into parts
#' with \code{maxp} elements. It will not change results, but it is
#' necessary if your computer does not have enough RAM. Set to a lower value
#' if you still have problems.
#'
#' @param verbose a logical. Set \code{FALSE} if you do not want to see any
#' information during the selection procedure.
#'
#' @param file.out a character string, If not \code{NULL} (and \code{minpv<1}),
#' the  variables with p-value < \code{minpv} will be saved to \code{file.out}
#' (txt file).
#'
#' @param ... optional arguments to \code{crit} (you cannot send optional
#' arguments to \code{crit.multif})
#'
#' @details
#' To find the best model, the following algorithm (a modification of the
#' stepwise selection) is used [3]. In the first step the Pearson correlation
#' coefficients between \code{y} and all columns of \code{X} are calculated
#' and columns with p-values for the Pearson correlation tests higher than
#' \code{minpv} will be excluded from the model selection procedure.
#' In the second step (multi-forward) we start with the null model and add
#' variables which decrease \code{crit.multif} (in order from the smallest
#' p-value). The step is finished after we add \code{maxf} variables or none
#' of remaining variables improve \code{crit.multif}. Then the classical
#' backward selection is performed (with \code{crit}). When there is no
#' variables to remove, the last step, the classical stepwise procedure, is
#' performed (with \code{crit}).
#'
#' Results from this four-step procedure should be very similar to the
#' classical stepwise procedure (when we start with the null model and do not
#' omit variables with high p-values) but the first one is much quicker.
#' The most time-consuming part is the forward step in the
#' stepwise selection (in the multi-forward step we do not add the best
#' variable but any which decrease \code{crit.multif}) and it is performed less
#' often when we start with a reasonable model (sometimes you can find the best
#' model without using the stepwise selection). But you can omit the first three
#' steps if you set \code{multif=FALSE} and \code{minpv=1}. Resignation from
#' the multi-forward step can be reasonable when you expect that the final
#' model should be very small (a few variables).
#'
#' If your data are too big to store in RAM, you should read them with the
#' \code{read.big.matrix} function form the \code{bigmemory} packages. The
#' \code{selectModel} function will recognize that \code{X} is not an ordinary
#' matrix and split your data to smaller parts. It will not change results but
#' is necessary to work with big data.
#'
#' The default criterion in the model selection procedure is a modification of
#' the Bayesian Information Criterion, mBIC [1]. It was constructed to control
#' the so-called Family-wise Error Rate (FWER) at the level near 0.05 when you
#' have a lot of explanatory variables and only a few of them should stay in
#' the final model. If you are interested in controlling the so-called False
#' Discovery Rate (FDR) is such type of data, you can change \code{crit} to
#' \code{mBIC2} [2], which control FDR at the level near 0.05. There are more
#' criteria to choose from or you can easily define your own (see 'Examples')
#'
#' If you have variables in rows, you have to transpose \code{X}. It can be
#' problematic if your data are big, so you can use the
#' \code{transposeBigMatrix} function from this package.
#'
#' @return An object of class lm, the final model.
#'
#' @author Piotr Szulc
#'
#' @references
#' [1] M. Bogdan, J.K. Ghosh, R.W. Doerge (2006), "Modifying the Schwarz
#' Bayesian Information Criterion to locate multiple interacting quantitative
#' trait loci", Genetics 167: 989-999.
#'
#' [2] F. Frommlet, A. Chakrabarti, M. Murawska, M. Bogdan (2011), "Asymptotic
#' Bayes optimality under sparsity for generally distributed effect sizes under
#' the alternative". Technical report at arXiv:1005.4753.
#'
#' [3] F. Frommlet, F. Ruhaltinger, P. Twarog, M. Bogdan (2012), "A model
#' selection approach to genome wide association studies", Computational
#' Statistics and Data Analysis 56: 1038-1051.
#'
#' @examples
#' set.seed(1)
#' n <- 100
#' M <- 10
#' X <- matrix(rnorm(M*n), ncol=M)
#' y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n)
#' fit <- selectModel(X, y, p=M)
#' summary(fit)
#'
#' # more examples: type ?bigstep
#'
#' @export

selectModel <-
  function(X, y, Xm=NULL, stay=NULL, intercept=TRUE, minpv=0.15, multif=TRUE,
           maxf=min(ncol(X), 70), minb=0, crit=mbic, crit.multif=bic, maxp=1e7,
           verbose=TRUE, file.out=NULL, ...) {

  if (!(class(X) %in% c("matrix", "data.frame", "big.matrix")))
    stop("X has to be a matrix, data.frame or big.matrix.")
  if (methods::is(X, "data.frame"))
    X <- as.matrix(X)
  y <- as.numeric(unlist(y, use.names=FALSE))
  n <- length(y)
  stopifnot(nrow(X) == n)
  M <- ncol(X)
  namesX <- colnames(X)
  namesXm <- colnames(Xm)
  if (is.null(namesX))
    namesX <- paste0("X", 1:M)
  if (!is.null(Xm)) {
    if (!class(Xm) %in% c("numeric", "matrix", "data.frame"))
      stop("Xm has to be a numeric vector, matrix od data.frame.")
    if (class(Xm) %in% c("numeric", "data.frame"))
      Xm <- as.matrix(Xm)
    if (is.null(namesXm))
      namesXm <- paste0("Xm", 1:ncol(Xm))
  }
  if (verbose)
    message("The desing matrix has ", n, " rows and ", M, " columns.")
  if (intercept) {
    Xm <- cbind(matrix(1, n), Xm)
    namesXm <- c("intercept", namesXm)
    stay <- c(1, stay + 1)
  }

  bigdata <- methods::is(X, "big.matrix")
  if (minpv < 1) {
    ord <- prepareMatrix(X, y, minpv, maxp, verbose, write, file.out)
    nX <- length(ord)
    if (verbose)
      message("The desing matrix has been reduced to ", nX, " columns.")
  } else {
    ord <- 1:M
  }

  res <- doAllSteps(X, y, Xm, stay, ord, multif, maxf, minb, crit, crit.multif,
                    maxp, verbose, ...)
  model <- c(namesXm[res$Xm], namesX[res$X])
  Xmodel <- cbind(Xm[, res$Xm], X[, res$X])
  colnames(Xmodel) <- model
  fit <- stats::lm(y ~ . - 1, as.data.frame(Xmodel))

  return(fit)
}
