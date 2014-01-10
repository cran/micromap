create_DF_rank <- function(data, ord.by, grouping, median.row=FALSE, rev.ord=FALSE){
	DF <- data

	DF$rank <- rank(DF[,ord.by], ties.method = "first")# create a new rank column
	if(rev.ord) DF$rank <- max(DF$rank)+1 - DF$rank
	DF <- DF[order(DF$rank),]

	m.rank <- (max(DF$rank)+1)/2
	if(median.row) DF$median <- (DF$rank==m.rank) else DF$median <- FALSE
	
	if(length(grouping)==1) grouping <- rep(grouping, ceiling(sum(1-DF$median)/grouping))
	
	iGroups2 <- cumsum(grouping) 
	if(median.row) iGroups2 <- iGroups2 + (cumsum(grouping)>m.rank)*1
	DF$pGrp <- as.numeric(cut(DF$rank, c(0,iGroups2), labels=1:length(iGroups2)))# create a new perceptual group column based on rank
	
	if(median.row){ 
	  w.median <- which(DF$median)	
	  if(DF$pGrp[w.median+1]==DF$pGrp[w.median-1]) DF$pGrp[DF$rank>m.rank] <- DF$pGrp[DF$rank>m.rank] + 1
		
	  DF$pGrp[w.median] <- (DF$pGrp[w.median+1]+DF$pGrp[w.median-1])/2
	}
		
	DF$pGrpOrd <- sapply(1:nrow(DF), function(i) sum(DF$rank[i]>subset(DF, DF$pGrp==DF$pGrp[i])$rank)+1)
	DF$color <-DF$pGrpOrd
	
	DF
}


create_DF_cat <- function(data, grp.by, cat){
	DF <- data 

	tGroups <- unique(DF[,grp.by])
	DF$pGrp <- match(DF[,grp.by],tGroups)									
	tCats <- unique(DF[,cat])
	DF$pGrpOrd <- match(DF[,cat], tCats)	
	DF$color <-DF$pGrpOrd

	DF								
}



alterForMedian <- function(DF, a){
	if(a$median.row){ 
	    if(!any(DF$median)) {
		tmpCols <- names(DF)[-(1:a$ncols)]
		tmpData <- apply(DF[,tmpCols],2,median)

		DFmedian <- transform(DF[1,], pGrpOrd=1, pGrp=a$m.pGrp, median=TRUE, rank='')
		for(k in 1:length(tmpCols)) DFmedian[,tmpCols[k]] <- tmpData[k]

		DF <- rbind(DF, DFmedian)
	      }

	    DF$color[DF$median] <- length(a$colors)
	  }

	DF
}
