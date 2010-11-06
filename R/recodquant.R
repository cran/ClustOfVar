recodquant <-
function(X)
	{
		X <- as.matrix(X)
		Xcod <- apply(X,2,missing.mean)
		red <- sqrt((nrow(X)-1)/nrow(X))
		Z<- scale(Xcod,scale=(sd(Xcod)*red)) 
		return(Z)
	}

