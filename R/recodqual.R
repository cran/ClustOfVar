recodqual <-
function(X)
	{
		X <- as.matrix(X)
		GNA <- tab.disjonctif.NA(X)
		G <- replace(GNA,is.na(GNA),0)	
		return(G)	
	}

