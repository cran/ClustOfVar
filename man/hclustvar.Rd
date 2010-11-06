\name{hclustvar}
\alias{hclustvar}
\title{
Hierarchical clustering of variables
}
\description{
Ascendant hierarchical clustering of a set of variables. 
Variables can be quantitative, qualitative or a mixture of both.
The aggregation criterion is the decrease in homogeneity for the cluster being merged.
The homogeneity of a cluster is the sum of the correlation ratio (for qualitative variables) and the squared correlation (for quantitative variables) between the variables and the center of the cluster which is the first principal component of PCAmix.
PCAmix is defined for a mixture of qualitative and quantitative variables and includes ordinary principal component analysis (PCA) and multiple correspondence analysis (MCA) as special cases.
Missing values are replaced by means for quantitative variables and by zeros in the indicator matrix for qualitative variables.
}
\usage{
hclustvar(X.quanti = NULL, X.quali = NULL)
}
\arguments{
  \item{X.quanti}{
a numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
}
  \item{X.quali}{
a categorical matrix of data, or an object that can be coerced to such a matrix (such as a character vector, a factor or a data frame with all factor columns).
}
}

\value{
\item{height }{a set of p-1 non-decreasing real values: the values of the aggregation criterion.
}
\item{clusmat }{a p by p matrix with group memberships where each column k corresponds to the elements of the partition in k clusters.}
\item{merge }{a p-1 by 2 matrix.  Row i of \code{merge} describes the merging of clusters at step i of the clustering. 
If an element j in the row is negative, then observation -j was merged at this stage.  
If j is positive then the merge was with the cluster formed at the (earlier) stage j of the algorithm.  
Thus negative entries in \code{merge} indicate agglomerations of singletons, and positive entries indicate agglomerations of non-singletons.
}
}

\references{
Chavent M., Kuentz V., Liquet B., Saracco J. (2012). ClustOfVar: An R Package for the Clustering Of Variables.\emph{Journal of Statistical Software}, 50(13), 1-16. \cr

Chavent M., Kuentz V., Sarraco J. (2012). Orthogonal rotation in PCAMIX. \emph{Advances in Data Analysis and Classification}, 6(2),131-146.
}

\author{Marie Chavent <marie.chavent@u-bordeaux2.fr>, Vanessa Kuentz, Benoit Liquet, Jerome Saracco}

\seealso{
 \code{\link{cutreevar}}, \code{\link{plot.hclustvar}}, \code{\link{stability}},\code{\link{kmeansvar}}
}
\examples{

#quantitative variables
data(decathlon)
tree <- hclustvar(decathlon[,1:10])
plot(tree)

#qualitative variables with missing values
data(vnf) 
tree_NA <- hclustvar(X.quali=vnf)
plot(tree_NA)
dev.new()
vnf2<-na.omit(vnf)
tree <- hclustvar(X.quali=vnf2)
plot(tree)

#mixture of quantitative and qualitative variables
data(wine)
X.quanti <- wine[,c(3:29)] 
X.quali <- wine[,c(1,2)] 
tree <- hclustvar(X.quanti,X.quali)
plot(tree)
}

\keyword{ cluster }
\keyword{ multivariate }
