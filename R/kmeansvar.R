kmeansvar <-
function(X.quanti=NULL,X.quali=NULL,init,iter.max=150,nstart=1,matsim=FALSE)
{
#init:  Either the number of clusters or a vector with group memberships
#iter.max: The maximum number of iterations allowed.
#nstart: If init is a number, how many random sets should be chosen?

	cl <- match.call()
	rec <- recod(X.quanti,X.quali)
	n <- rec$n
	p <- rec$p		#total number of variables
	p1 <- rec$p1	#number of numerical variables
	X <- rec$X 		#data matrix (mixed if necessary)
	Xn <- rec$Xn	#data matrix without any replication
	Z <- rec$Z		#data matrix for input in SVD
	indexj <- rec$indexj #variables indices in columns of Z
	G <- rec$G		#dummy variables if X.quanti non null
	Gcod <- rec$Gcod	
	X.quanti<-rec$X.quanti
	X.quali<-rec$X.quali


	partinit <- function(centers)
	{
		A <- matrix(,p,k)
		for (g in 1:k) {
			sim <- function(col) {
				mixedVarSim(centers[,g],col)
				}
			if (p1>0) A[1:p1,g]<-apply(X[,1:p1],2,sim)
			if (p1!=p) A[(p1+1):p,g]<-apply(X[,(p1+1):p],2,sim)
			}	
		part <- apply(A,1,which.max)
		return(part)
	}

	if (missing(init)) 
        stop("'init' must be a number or a vector")
	if (length(init) == 1) {
		k <- init 
		pn <- ncol(Xn)
     		if (pn < k) 
            	stop("more cluster than distinct variables.")
      	centers <- Xn[,sample.int(pn, k), drop = FALSE]
		part <- as.factor(partinit(centers))
		} else {
			if (!is.integer(init))
				stop("init must be a vector of integer")
			if (nstart!=1)
				stop("nstart must be equal to one")
        		part <- as.factor(init)
       		k <- length(levels(part))
	  		if (p < k) 
            		stop("more cluster centers than variables")
	  		if (length(which(init>k))> 0)
				stop("clusters must be numbered from 1 to k")
	  		if (p != length(part)) 
        			stop("the length of init must be equal to the number of variables")
            }

	do_one <- function(part)
	{
		A <- matrix(,p,k)
		iter <- 0
		diff <- TRUE
		while ((iter< iter.max) && diff)
		{
			iter <- iter+1
			#Representation step
			indexk<-NULL
			for (i in 1:length(indexj))
				indexk[i] <- part[indexj[i]]
			latent.var <- matrix(,n,k)
			sv<-rep(NA,k)
			for (g in 1:k) {
				Zclass <- Z[,which(indexk==g)]
				latent <- clusterscore(Zclass)
				latent.var[,g] <- latent$f
				sv[g] <- latent$sv }
			#Affectation step
			for (g in 1:k){
				if (p1>0) {
					sl.quant <- function(col) {
						mixedVarSim(col,latent.var[,g])
					}
					A[1:p1,g]<-apply(X.quanti,2,sl.quant)
					}
				if (p1!=p)	{
					sl.qual <- function(col) {
						mixedVarSim(latent.var[,g],col)
					}
					A[(p1+1):p,g]<-apply(X.quali,2,sl.qual)
					}	
			}
			part2 <- apply(A,1,which.max)
			diff <- !all(part==part2)
			part <- part2
		}
		wss <- sv^2 #within cluster sum of squares
		return(list(latent.var=latent.var,part=part,wss=wss,iter=iter))
	}

	res<-do_one(part)
	if (nstart >= 2) {
		best <- sum(res$wss)
        	for (i in 2:nstart) {
            	centers <- Xn[,sample.int(pn, k), drop = FALSE]
			part<-as.factor(partinit(centers))
            	res2 <- do_one(part)
            	if ((z <- sum(res2$wss)) > best) {
               		res <- res2
                		best <- z}
        	}
    	}

	part <- res$part
	iter <- res$iter
	names(part) <- colnames(X)
	res <- descript(part,rec,cl,matsim,iter=iter)
	class(res) <- "clustvar"
	return(res)
}

