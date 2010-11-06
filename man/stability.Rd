\name{stability}
\alias{stability}
\title{
Stability of partitions from a hierarchy of variables 
}
\description{
Evaluates the stability of partitions obtained from a hierarchy of \code{p} variables. This hierarchy is performed with \code{hclustvar} and the stability of the partitions of 2 to p-1 clusters is evaluated with a bootstrap approach.
The boostrap approch is the following: \code{hclustvar} is applied to \code{B} boostrap samples of the \code{n} rows. 
The partitions of 2 to p-1 clusters obtained from the B bootstrap hierarchies are compared with the partitions from the initial hierarchy .
The mean of the corrected Rand indices is plotted according to the number of clusters. This graphical representation helps in the determination of a suitable numbers of clusters.
}
\usage{
stability(tree, B = 100, graph = TRUE)
}
\arguments{
  \item{tree}{
an object of class \code{hclustvar}.
}
  \item{B}{
the number of bootstrap samples.
}
  \item{graph}{
boolean,  if 'TRUE' a graph is displayed.
}
}
\value{
\item{matCR}{matrix of corrected Rand indices.}
\item{meanCR}{vector of mean corrected Rand indices.}
}

\references{
Chavent M., Kuentz V., Liquet B., Saracco J. (2012). ClustOfVar: An R Package for the Clustering Of Variables.\emph{Journal of Statistical Software}, 50(13), 1-16. \cr

Chavent M., Kuentz V., Sarraco J. (2012). Orthogonal rotation in PCAMIX. \emph{Advances in Data Analysis and Classification}, 6(2),131-146.
}

\author{Marie Chavent <marie.chavent@u-bordeaux2.fr>, Vanessa Kuentz, Benoit Liquet, Jerome Saracco}


\seealso{
\code{\link{plot.clustab}}, \code{\link{hclustvar}}
}
\examples{
data(decathlon) 
tree <- hclustvar(X.quanti=decathlon[,1:10])
stab<-stability(tree,B=20)
plot(stab,nmax=7)
dev.new()
boxplot(stab$matCR[,1:7])
}
\keyword{ cluster }
\keyword{ multivariate }
