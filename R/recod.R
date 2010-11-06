recod <-
function(X.quanti,X.quali)
{
	G <- NULL
	Gcod <- NULL
	if (!is.null(X.quanti)){
		if (is.factor(X.quanti))
			stop("All variables in X.quanti must be numerical")
		if (is.numeric(X.quanti)) 
			X.quanti <- data.frame(X.quanti)
		for (v in 1:ncol(X.quanti)) {
        		if (!is.numeric(X.quanti[, v])) 
          	 		stop("All variables in X.quanti must be numeric")}
		n1 <- nrow(X.quanti)
		p1 <- ncol(X.quanti)
		Z1 <- recodquant(X.quanti)
		Xn1 <- unique(as.matrix(X.quanti),MARGIN=2)	
	}
	if (!is.null(X.quali)){
		if (is.numeric(X.quali))
			stop("All variables in X.quali must be categorical")
		if (is.factor(X.quali)) 
			X.quali <- data.frame(X.quali)
		for (v in 1:ncol(X.quali)) {
       	 if (is.numeric(X.quali[, v])) 
           		stop("All variables in X.quali must be categorical") }
		n2 <- nrow(X.quali)
		p2 <- ncol(X.quali)
		G <- recodqual(X.quali)
		ns <- apply(G,2,sum)
		ps <- ns/nrow(G)
		Gcod <- sweep(G,MARGIN=2,STATS=sqrt(ps),FUN="/") 
		moy<-apply(Gcod,2,mean)
		Z2 <- sweep(Gcod,MARGIN=2,STATS=moy,FUN="-") 
		Xn2 <- unique(as.matrix(X.quali),MARGIN=2)
		nb.moda <- function(moda) {
			moda <- as.factor(moda)
			length(levels(moda))
			}
		nbmoda <- apply(X.quali,2, nb.moda)
		indexj2<-NULL
		for (j in 1:ncol(X.quali)) {
			indexj2 <- c(indexj2,rep(j,nbmoda[j]))}
	}
	if (!is.null(X.quanti)&& !is.null(X.quali))
		if (n1==n2) {
			n <- n1
			p <- p1+p2
			Z <- cbind(Z1,Z2)
			X <- cbind.data.frame(X.quanti,X.quali)
			Xn <- cbind.data.frame(Xn1,Xn2)
			indexj <- c(1:p1,indexj2+p1)
		} else 
			stop("number of objects in X.quanti and X.quali must be the same")
	if (!is.null(X.quanti)&& is.null(X.quali)) {
		n <- n1
		p <- p1
		Z <- Z1
		X <- X.quanti
		Xn <- Xn1
		indexj <- 1:p1
	}
	if (is.null(X.quanti)&& !is.null(X.quali)){
		n <- n2
		p <- p2
		p1 <- 0
		Z <- Z2
		X <- X.quali
		Xn <- Xn2
		indexj <- indexj2 
	} 
	if (is.null(X.quanti)&& is.null(X.quali))
		stop("A data matrix must be given")

	if (is.null(colnames(X))) 
		colnames(X) <- paste("V", 1:ncol(X), sep = "")
	for (j in 1:ncol(X)) if (colnames(X)[j] == "") 
		colnames(X)[j] <- paste("V", j, sep = "")
		
	return(list(X=X,Xn=Xn,Z=Z,n=n,p=p,p1=p1,indexj=indexj,G=G,Gcod=Gcod,X.quanti=X.quanti,X.quali=X.quali))
}

