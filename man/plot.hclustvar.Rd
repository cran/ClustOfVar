\name{plot.hclustvar}
\alias{plot.hclustvar}
\title{
Dendrogram of the hierarchy of variables
}
\description{
Dendrogram of the hierarchy of variables resulting from \code{hclustvar} and aggregation levels plot.
}
\usage{
\method{plot}{hclustvar}(x,which=c(1:2), ask = prod(par("mfcol")) < length(which) && dev.interactive(),sub="", \dots)
}

\arguments{
  \item{x}{
an object of class \code{hclustvar}.
}
\item{which}{
if one of the two plots is required, specify a subset of the numbers 1:2
}
 \item{ask}{
logical; if TRUE, the user is _ask_ed before each plot.
}
  \item{sub}{
a sub title for the plot.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}

\author{Marie Chavent <marie.chavent@u-bordeaux2.fr>, Vanessa Kuentz, Benoit Liquet, Jerome Saracco}

\seealso{
\code{\link{hclustvar}}
}
\examples{
data(wine)
X.quanti <- wine[,c(3:29)] 
X.quali <- wine[,c(1,2)] 
tree <- hclustvar(X.quanti,X.quali)
plot(tree)

# 2 plots on 1 page
par(mfrow = c(1, 2))
plot(tree) 

 # plot just the dendrogram
plot(tree,which=1)
}
\keyword{ cluster }
\keyword{ multivariate }
