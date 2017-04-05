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
#' @param fitFun a function which fits the regression model and calculate
#' the logarithm of the likelihood function (loglike). You can use your own
#' function or one of these: \code{fitLinear}, \code{fitLogistic},
#' \code{fitPoisson}.
#'
#' @param crit a function defining the model selection criterion. You can use
#' your own function or one of these: \code{bic}, \code{mbic}, \code{mbic2},
#' \code{aic}, \code{maic}, \code{maic2}.
#'
#' @param Xm a numeric matrix. Additional variables which will be included
#' in the first step of the model selection procedure.
#'
#' @param stay a numeric vector. Columns from \code{Xm} which should be
#' included in the model in all selection steps.
#'
#' @param minpv a numeric. Variables from \cite{X} with p-values for the
#' likelihood ratio tests (see 'Details') higher than \code{minpv} will be
#' excluded from the model selection procedure. If you do not want to do this,
#' set \code{minpv=1} (not recommended if you have big data).
#'
#' @param multif a logical. If \code{TRUE}, the multi-forward step will be
#' performed (see 'Details').
#'
#' @param crit.multif a function defining the model selection criterion in the
#' multi-forward step. See \code{crit} (but only criteria with small penalties
#' are recommended).
#'
#' @param maxf a numeric, a maximal number of variables in the final model
#' in the multi-forward step.
#'
#' @param minb a numeric, a minimal number of variables in the final model
#' in the backward selection (see 'Details').
#'
#' @param fastST a logical. If \code{TRUE}, the Pearson correlation
#' coefficients between \code{y} and all columns of \code{X} are calculated
#' instead of the likelihood ratio tests (see 'Details'). It is faster but
#' works only if you do not have any missing values.
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
#' the variables with p-value < \code{minpv} will be saved to \code{file.out}
#' (txt file).
#'
#' @param ... optional arguments to \code{crit} or \code{crit.multif}.
#'
#' @details
#' To find the best model (linear or generalized), the following algorithm
#' (a modification of the stepwise selection) is used [3]. In the first step
#' the likelihood ratio tests between two regression models are performed:
#' 1) with only the intercept, 2) with the intercept and every single variable
#' from the matrix \code{X}. P-values are calculated and variables with
#' p > \code{minpv} are excluded from the model selection procedure.
#' In the second step (multi-forward) we start with the null model and add
#' variables which decrease \code{crit.multif} (in order from the smallest
#' p-value). The step is finished after we add \code{maxf} variables or none
#' of remaining variables improve \code{crit.multif}.
#' Then the classical backward selection is performed (with \code{crit}).
#' When there is no variables to remove, the last step, the classical stepwise
#' procedure, is performed (with \code{crit}).
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
#' \code{read.big.matrix} function from the \code{bigmemory} packages. The
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
#' \code{mBIC2} [2], which controls FDR at the level near 0.05. There are more
#' criteria to choose from or you can easily define your own (see 'Examples')
#'
#' If you do not have the desing matrix in one file, you have to combine them.
#' It can be problematic if your data are big, so you can use the
#' \code{combineBigMatrices} function from this package. You can use it
#' also when you have to transpose \code{X} (because you have variables in
#' rows) or you want to change names of columns.
#'
#' @return The names of variables in the final model.
#'
#' @author Piotr Szulc
#'
#' @references
#' [1] M. Bogdan, J.K. Ghosh, R.W. Doerge (2004), "Modifying the Schwarz
#' Bayesian Information Criterion to locate multiple interacting quantitative
#' trait loci", Genetics 167: 989-999.
#'
#' [2] F. Frommlet, A. Chakrabarti, M. Murawska, M. Bogdan (2011), "Asymptotic
#' Bayes optimality under sparsity for generally distributed effect sizes
#' under the alternative". Technical report at arXiv:1005.4753.
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
#' colnames(X) <- 1:M
#' y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n)
#' selectModel(X, y, p=M)
#'
#' # more examples: type ?bigstep
#'
#' @export

selectModel <-
  function(X, y, fitFun=fitLinear, crit=mbic, Xm=NULL, stay=NULL, minpv=0.15,
           multif=TRUE, crit.multif=bic, maxf=min(ncol(X), 70), minb=0,
           fastST=FALSE, maxp=1e6, verbose=TRUE, file.out=NULL, ...) {

  # matrix X
  if (!(class(X) %in% c("numeric", "matrix", "data.frame", "big.matrix")))
    stop("X has to be a numeric vector, matrix, data.frame or big.matrix.")
  if (class(X) %in% c("numeric", "data.frame"))
    X <- as.matrix(X)
  if (class(X) == "matrix" & is.null(colnames(X)))
    colnames(X) <- 1:ncol(X)

  # y
  y <- as.numeric(unlist(y, use.names=FALSE))
  n <- length(y)
  stopifnot(n == nrow(X))

  # matrix Xm
  if (!is.null(Xm)) {
    stopifnot(n == nrow(Xm))
    if (!class(Xm) %in% c("numeric", "matrix", "data.frame"))
      stop("Xm has to be a numeric vector, matrix od data.frame.")
    if (class(Xm) %in% c("numeric", "data.frame"))
      Xm <- as.matrix(Xm)
    nXm <- ncol(Xm)
    if (!is.null(stay))
      Xm <- Xm[, c(stay, (1:nXm)[-stay]), drop=FALSE]  # change order
  }
  Xm <- cbind(matrix(1, nrow=n), Xm)
  stay <- length(stay) + 1  # number of first variables which have to stay

  if (verbose)
    message("The desing matrix has ", n, " rows and ", ncol(X), " columns.")

  Xm <- allSteps(X, y, fitFun, crit, Xm, stay, minpv, multif, crit.multif,
                 maxf, minb, fastST, maxp, verbose, file.out, ...)
  model <- colnames(Xm)[-1]

  return(model)
}
