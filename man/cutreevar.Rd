% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cutreevar.R
\name{cutreevar}
\alias{cutreevar}
\title{Cut a tree into groups of variables}
\usage{
cutreevar(obj, k = NULL, matsim = FALSE)
}
\arguments{
\item{obj}{an object of class 'hclustvar'.}

\item{k}{an integer scalar with the desired number of clusters.}

\item{matsim}{boolean, if TRUE, the matrices of similarities between
variables in same cluster are calculated.}
}
\value{
\item{var}{a list of matrices of squared loadings i.e. for each
cluster of variables, the squared loadings on first principal component of
PCAmix. For quantitative variables (resp. qualitative), squared loadings
are the squared correlations (resp. the correlation ratios) with the first
PC (the cluster center).}
\item{sim}{a list of matrices of similarities
i.e. for each cluster, similarities between their variables.  The
similarity between two variables is defined as a square cosine: the square
of the Pearson correlation when the two variables are quantitative; the
correlation ratio when one variable is quantitative and the other one is
qualitative; the square of the canonical correlation between two sets of
dummy variables, when the two variables are qualitative. \code{sim} is
'NULL' if 'matsim' is 'FALSE'. }
\item{cluster}{a vector of integers
indicating the cluster to which each variable is allocated.} 
\item{wss}{the
within-cluster sum of squares for each cluster: the sum of the correlation
ratio (for qualitative variables) and the squared correlation (for
quantitative variables) between the variables and the center of the
cluster.} 
\item{E}{the pourcentage of homogeneity which is accounted by the
partition in k clusters.} 
\item{size}{the number of variables in each
cluster.} 
\item{scores}{a n by k numerical matrix which contains the k
cluster centers. The center of a cluster is a synthetic variable: the first
principal component calculated by PCAmix.  The k columns of \code{scores}
contain the scores of the n observations units on the first PCs of the k
clusters.} 
\item{coef}{a list of the coefficients of the linear
combinations defining the synthetic variable of each cluster.}
}
\description{
Cuts a hierarchical tree of variables resulting from \code{hclustvar} into
several clusters by specifying the desired number of clusters.
}
\examples{
data(decathlon)
tree <- hclustvar(decathlon[,1:10])
plot(tree)
#choice of the number of clusters
stability(tree,B=40)
part <- cutreevar(tree,4)
print(part)
summary(part)

}
\seealso{
\code{\link{hclustvar}}, \code{\link{summary.clustvar}}, \code{\link{plot.clustvar}}, \code{\link{predict.clustvar}}
}
