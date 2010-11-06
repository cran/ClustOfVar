descript<-function(part,rec,cl,matsim,iter=FALSE) 
{	
	k<-length(levels(as.factor(part)))
	
	Z<-rec$Z
	X<-rec$X	
	G<-rec$G
	p1<-rec$p1	
	indexj<-rec$indexj 
	n<-rec$n
	p<-rec$p
	indexk<-NULL
	for (i in 1:length(indexj))
		indexk[i]<-part[indexj[i]]
	latent.var <- matrix(,n,k)
	sv<-rep(NA,k)

	#desc <- structure(vector(mode = "list", length = k), names = paste("cluster", 1:k, sep = ""))
	var <- structure(vector(mode = "list", length = k), names = paste("cluster", 1:k, sep = ""))
	sim <- structure(vector(mode = "list", length = k), names = paste("cluster", 1:k, sep = ""))

      for (g in 1:k)  {
		Zclass<-Z[,which(indexk==g)]
		latent<-clusterscore(Zclass)
		latent.var[,g]<- latent$f
		sv[g]<-latent$sv
		clus<-which(part==g)
		#quali<- NULL
		#quanti<-NULL
		#tab<-list()
		C <- matrix(NA, length(clus), 1)
		colnames(C)[1]<-"squared loading"
		rownames(C)<-names(clus)
		for (i in 1:length(clus))
			C[i,1] <- mixedVarSim(latent.var[,g],X[,clus[i]])
		var[[g]] <- C
		tabcos2 <- matrix(NA, length(clus), length(clus))
		colnames(tabcos2)<-names(clus)
		rownames(tabcos2)<-names(clus)
		diag(tabcos2)<-1
		if ((length(clus)>1) && (matsim==TRUE)) {
		for (i in 1:(length(clus)-1))
			for(j in (i+1):length(clus))  {
				tabcos2[i,j] <- mixedVarSim(X[,clus[i]],X[,clus[j]])
				tabcos2[j,i]<- tabcos2[i,j]
				}
			}		
 		if (matsim==TRUE) sim[[g]] <- tabcos2 else sim[[g]] <- NULL
		}

	wss<-sv^2 
	size<-rep(NA,k)
	for (g in 1:k) size[g]<-length(which(part==g))
	colnames(latent.var) <- paste("cluster", 1:k, sep = "")
	rownames(latent.var) <-rownames(X)
	Ht<-clusterscore(Z)$sv^2
	E<-(sum(wss)-Ht)/(p-Ht)*100
      return(list(call = cl,var=var,sim=sim,cluster=part,wss=wss,E=E,size=size,scores=latent.var,rec=rec,k=k,iter=iter))
}
