\name{plot.clustab}
\alias{plot.clustab}
\title{
Plot of an index of stability of partitions of variables
}
\description{
Plot of the index of stability of the partitions against the number of clusters.
}

\usage{
\method{plot}{clustab}(x, nmin = NULL, nmax = NULL, \dots)
}

\arguments{
  \item{x}{
an object of class \code{clusab}.
}
  \item{nmin}{
the minimum number of clusters in the plot.
}
  \item{nmax}{
the maximum number of clusters in the plot.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}

\author{Marie Chavent <marie.chavent@u-bordeaux2.fr>, Vanessa Kuentz, Benoit Liquet, Jerome Saracco}

\seealso{
\code{\link{stability}}
}
\examples{
data(decathlon) 
tree <- hclustvar(X.quanti=decathlon[,1:10])
stab<-stability(tree,B=20)
plot(stab,nmax=7)
}
\keyword{ cluster }
\keyword{ multivariate }
