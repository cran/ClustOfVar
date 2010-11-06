recodquant <-
function(X)
	{
		X <- as.matrix(X)
		Xcod <- apply(X,2,missing.mean)
		red <- sqrt((nrow(X)-1)/nrow(X))
		sd.Xcod <- apply(Xcod,2,sd)
	    Z<- scale(Xcod,scale=sd.Xcod*red) 
		apply(Z,1,function(x) sum(is.na(x))) 
		if (sum(is.na(Z))!= 0) stop("There are columns in X.quanti where all the values are identical")
		return(Z)
	}
