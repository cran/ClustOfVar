plot.hclustvar <-
function(x, sub="", ...){
	if (!inherits(x, "hclustvar")) 
        	stop("use only with \"hclustvar\" objects")
	class(x) <- "hclust"
	plot(x=seq(length(x$height),1),x$height,xaxt = "n",ylim=c(0,max(x$height)),xlab="number of clusters",ylab="Height",main="Aggregation levels",type="n")
	#plot(x=seq(length(x$height),1),x$height,ylim=c(0,max(x$height)),xlab="number of clusters",ylab="aggregation criterion",type="n")
	points(x=seq(length(x$height),1),x$height,pch=3)
	axis(side=1,at=seq(1,length(x$height)),labels=paste(1:(length(x$height))))
	dev.new()
	plot(x, hang=-1, xlab="", sub=sub, ...)	
	class(x) <- c("hclustvar","hclust")
}

