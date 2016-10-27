#' Model selection
#'
#' Model selection using the stepwise procedure and the chosen criterion.
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
#' \dontrun{
#' # data1
#' set.seed(1)
#' n <- 100
#' M <- 50
#' X <- matrix(rnorm(n*M), ncol=M)
#' snp <- 10*(1:5)
#' y <- rowSums(X[, snp]) + rnorm(n)
#'
#' # you can use classical criteria to such type of data:
#' fit <- selectModel(X, y, crit=aic)
#' summary(fit)
#' selectModel(X, y, crit=bic)
#'
#'
#' # data2
#' set.seed(1)
#' n <- 1e3
#' M <- 1e4
#' X <- matrix(rnorm(M*n), ncol=M)
#' snp <- 1e3*(1:10)
#' y <- rowSums(X[, snp]) + rnorm(n, sd=2)
#' Q <- matrix(rnorm(n*5), n, 5)  # additional variables
#'
#' # single tests + multi-forward + multi-backward + stepwise using mBIC
#' selectModel(X, y, p=M)
#' selectModel(X, y, p=M, multif=FALSE)  # only single tests + stepwise
#' selectModel(X, y, p=M, multif=FALSE, minpv=1)  # only stepwise
#'
#' # you can start with a model with variables in Q and force that
#' # 1st and 5th would not be removed
#' selectModel(X, y, Xm=Q, stay=c(1, 5), p=M, crit=mbic2)
#'
#' # after reducing the size of matrix X (removing variables with
#' # p-value > minpv), save it to a file (you can use it in another
#' # model selection, it will be faster)
#' selectModel(X, y, p=M, file.out="Xshort")
#'
#' # you can define your own criterion
#' # (you have to put 'rss', 'k' and 'n' in a list of parameters)
#' myCrit <- function(rss, k, n, c1=2, c2=3) {
#'   c1*log(rss) + sqrt(k*c2)/5
#' }
#' selectModel(X, y, multif=FALSE, crit=myCrit, c1=1.5)
#' selectModel(X, y, multif=FALSE,
#'             crit=function(rss, k, n) 1.4*log(rss) + sqrt(k*3)/5)
#'
#' selectModel(X, y, crit=bic)  # bad idea...
#'
#'
#' # data3
#' X <- read.big.matrix("X.txt", sep=" ", type="char", head=TRUE)
#' y <- read.table("Trait.txt")
#' Q <- read.table("Q.txt")
#' selectModel(X, y, p=M)
#' selectModel(X, y, p=M, minpv=0.001) # if you do not have time...
#' }
#'
#' @importFrom methods is
#' @importFrom utils txtProgressBar setTxtProgressBar write.table
#' @importFrom stats cor pnorm complete.cases lm
#' @importFrom speedglm speedlm.fit
#' @importFrom bigmemory read.big.matrix as.big.matrix
#'
#' @name bigstep

NULL
