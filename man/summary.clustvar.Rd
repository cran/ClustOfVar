\name{summary.clustvar}
\alias{summary.clustvar}
\title{
Summary of a 'hclustvar' object
}
\description{
This is a method for the function summary for objects of the class \code{clustvar}.
}
\usage{
\method{summary}{clustvar}(object, \dots)
}
\arguments{
  \item{object}{
an object of class \code{clustvar}.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\value{
Returns a list of matrices of squared loadings i.e. for each cluster of variables, the squared loadings on first principal component of PCAmix.
For quantitative variables (resp. qualitative), squared loadings are the squared correlations (resp. the correlation ratios) with the first PC (the cluster center).
If the partition of variables has been obtained with \code{kmeansvar} the number of iteration until convergence is also indicated.
}

\author{Marie Chavent <marie.chavent@u-bordeaux2.fr>, Vanessa Kuentz, Benoit Liquet, Jerome Saracco}


\seealso{
\code{\link{kmeansvar}}, \code{\link{cutreevar}}
}
\examples{
data(decathlon) 
part<-kmeansvar(X.quanti=decathlon[,1:10],init=5)
summary(part)
}

