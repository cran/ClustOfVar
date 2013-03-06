\name{predict.clustvar}
\alias{predict.clustvar}
\title{Scores of new objects on the synthetic variables of a given partition}
\usage{
 \method{predict}{clustvar}(object, X.quanti = NULL, X.quali = NULL,...)
}
\arguments{
  \item{object}{an object of class clustvar}

  \item{X.quanti}{numeric matrix of data for the new
  objects}

  \item{X.quali}{a categorical matrix of data for the new
  objects}
  
  \item{\dots}{
  further arguments passed to or from other methods.
  }
}
\value{
  Returns the matrix of the scores of the new objects on
  the k syntetic variables of the k-clusters partition
  given in input.
}
\description{
  A partition of variables obtained with kmeansvar or with
  cutreevar is given in input. Each cluster of this
  partition is associated with a synthetic variable which
  is a linear combination of the variables of the cluster.
  The coefficients of these k linear combinations (one for
  each cluster) are used here to calculate new scores of a
  objects described in a new dataset (with the same
  variables). The output is the matrix of the scores of
  these new objects on the k synthetic variables.
}
\examples{
data(wine)
n <- nrow(wine)
sub <- sample(1:n,10)
X.quanti <- wine[sub,c(3:29)] #learning sample
part <-kmeansvar(X.quanti,init=5)
X.quanti.t <- wine[-sub,c(3:29)]
new <- predict(part,X.quanti.t)
}
\author{
  Marie Chavent \email{marie.chavent@math.u-bordeaux1.fr},
  Vanessa Kuentz, Benoit Liquet, Jerome Saracco
}

