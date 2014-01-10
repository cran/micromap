RankMaps <- function(pl, p, mapDF, att){
	bgcolor 	<- ifelse(!is.na(att[[p]]$panel.bgcolor), att[[p]]$panel.bgcolor, 'white')
	outer.hull 	<- att[[p]]$outer.hull		# whether we need to construct an outline of the map poygons
	ncolors 	<- length(att$colors)
	nGroups 	<- range(mapDF$pGrp[!is.na(mapDF$pGrp)])	

	mapDF$fill.color <- mapDF$pGrpOrd
	mapDF$IDpoly 	 <- paste(mapDF$ID,mapDF$poly,sep='.')

	map.all <- att[[p]]$map.all

	#*** store all the polygon data then subset only those with data
	mapDF.all <- mapDF				
        mapDF <- subset(mapDF.all, !is.na(rank))	

	#*** here we build the "outer hull" (polygon outline) if neccesary
	if(outer.hull==T){
		ii=0
		lHull <- NULL
		for(rr in unique(mapDF.all$region)){
		  ii <- ii + 1
	
		  tmpPlys <- vector("list", length(unique(subset(mapDF.all, region==rr)$poly)))
		  jj=1
		  for(pp in unique(subset(mapDF.all, region==rr)$poly)){
			tmpPlys[[jj]] <- Polygon(subset(mapDF.all, region==rr & poly==pp)[,c('coordsx','coordsy')])
		
			jj <- jj + 1	
		  }
				
		  lHull <- append(lHull, Polygons(tmpPlys, ID=rr))
		}
		spHull <- SpatialPolygons(lHull)
		
		unionHull <- unionSpatialPolygons(spHull, rep(1, length(spHull)))
		
		    lHull <- vector("list", length(unionHull@polygons))
		    for (i in 1:length(lHull)) {
		        tmp <- unionHull@polygons[[i]]
		        lHull[[i]] <- lapply(1:length(tmp@Polygons), function(j) cbind(i, 
		            j, tmp@Polygons[[j]]@labpt[1], tmp@Polygons[[j]]@labpt[2], 
		            tmp@Polygons[[j]]@coords, tmp@Polygons[[j]]@hole, 
		            tmp@Polygons[[j]]@area))
		    }

		    dHull <- NULL
		    for (i in 1:length(lHull)) {
		        for (j in 1:length(lHull[[i]])) {
		            dHull <- rbind(dHull, lHull[[i]][[j]])
		        }
		    }
		
		    dHull <- data.frame(1, dHull)
		    names(dHull) <- c("ID", "region", "poly", "lab.x", "lab.y", "coordsx", "coordsy", "hole", "area")
		    dHull <- transform(dHull, poly = (region - 1) * max(dHull$poly) + poly)
		
			dHull$fill.color <- dHull$pGrpOrd
			dHull$IDpoly <- paste(dHull$ID,dHull$poly,sep='.')
	}


	#*** If there is an odd number of polygons and a median row in our map 
	#***   we must deal with the median polygon specially
	if(att$median.row & any(mapDF$pGrp==att$m.pGrp)){
		#*** find the median polygon set the fill color for it to be gray
		#*** create copies of that polygon to be displayed in the preceding 
		#*** and subsequent perceptual groups  
		mapDF.median 		<- subset(mapDF, pGrp==att$m.pGrp)
		mapDF.median$fill.color <- ncolors+1	
		mapDF.median		<- rbind(transform(mapDF.median, pGrp=(att$m.pGrp-.5), IDpoly='median1'),
						transform(mapDF.median, pGrp=(att$m.pGrp+.5), IDpoly='median2'))
		
		mapDF <- rbind(subset(mapDF, !pGrp==att$m.pGrp), mapDF.median)
	}


	#*** Creates the map panel object 	
	pl <- ggplot(mapDF, aes(x=coordsx, y=coordsy, group=IDpoly)) 

	#*** If we are displaying polygons without associated data we 
	#***   do so first so they appear in the background
	if(map.all==T){
		if(nrow(subset(mapDF.all, is.na(pGrp)))>0) for(g in nGroups[1]:nGroups[2]) pl <- pl + 
			geom_polygon(fill=att[[p]]$nodata.fill, 
				colour=att[[p]]$nodata.border.color, 
				size=att[[p]]$nodata.border.size/2, 
				data=transform(subset(mapDF.all, is.na(pGrp)), pGrp=g))
	}


	#*** Draw polygons from all previous perceptual groups by looping through
	#***   each perceptual group and creating datasets which contain all 
	#***   previous perceptual groups or future groups depending on the 
	#***   user's shading preference
	if(att[[p]]$fill.regions=="aggregate"){
		for(g in 1:(nGroups[2]-1)) pl <- pl + 
			geom_polygon(fill=att[[p]]$withdata.fill, 
				colour=att[[p]]$withdata.border.color, 
				size=att[[p]]$withdata.border.size/2, 
				data=transform(mapDF[mapDF$pGrp>g,], pGrp=g))						

		for(g in max(nGroups[1],2):nGroups[2]) pl <- pl + 
			geom_polygon(fill=att[[p]]$inactive.fill, 
				colour=att[[p]]$inactive.border.color,  
				size=att[[p]]$inactive.border.size/2, 
				data=transform(mapDF[mapDF$pGrp<g,], pGrp=g)) 

	 }
 	
	if(att[[p]]$fill.regions=="two ended"){
		for(g in nGroups[1]:floor(att$m.pGrp)) pl <- pl + 
			geom_polygon(fill=att[[p]]$withdata.fill, 
				colour=att[[p]]$withdata.border.color,  
				size=att[[p]]$withdata.border.size/2, 
				data=transform(mapDF[mapDF$pGrp>g,], pGrp=g)) 	

		for(g in max(nGroups[1],2):floor(att$m.pGrp)) pl <- pl + 
			geom_polygon(fill=att[[p]]$inactive.fill, 
				colour=att[[p]]$inactive.border.color,  
				size=att[[p]]$inactive.border.size/2, 
				data=transform(mapDF[mapDF$pGrp<g,], pGrp=g)) 	


		for(g in ceiling(att$m.pGrp):nGroups[2]) pl <- pl + 
			geom_polygon(fill=att[[p]]$withdata.fill, 
				colour=att[[p]]$withdata.border.color,  
				size=att[[p]]$withdata.border.size/2, 
				data=transform(mapDF[mapDF$pGrp<g,], pGrp=g)) 


		for(g in ceiling(att$m.pGrp):(nGroups[2]-1)) pl <- pl + 
			geom_polygon(fill=att[[p]]$inactive.fill, 
				colour=att[[p]]$inactive.border.color,  
				size=att[[p]]$inactive.border.size/2, 
				data=transform(mapDF[mapDF$pGrp>g,], pGrp=g)) 

	 
	}

	if(att[[p]]$fill.regions=="with data"){
		for(g in nGroups[1]:nGroups[2]) pl <- pl + 
			geom_polygon(fill=att[[p]]$withdata.fill, 
				colour=att[[p]]$withdata.border.color,  
				size=att[[p]]$withdata.border.size/2, 
				data=transform(mapDF[!mapDF$pGrp==g,], pGrp=g)) 

	 }



	#*** draw polygons of current perceptual group																			
	pl <- pl + 
		geom_polygon(aes(fill=factor(fill.color)), 
			colour=att[[p]]$active.border.color, 
			size=att[[p]]$active.border.size/2, 
			data=subset(mapDF, hole==0)) + 				
		facet_grid(pGrp~.) +
		scale_fill_manual(values=c(att$colors), guide='none') +
		coord_equal() 


	  #################################
	  #################################
	  #*** insert white space for median row	
	  mapDF.median <- data.frame(pGrpOrd=1, pGrp=(max(mapDF$pGrp)+1)/2, 
					rank=(max(mapDF$rank)+1)/2, ID='median', 
					coordsx=range(mapDF$coordsx)[c(1,1,2,2,1)], 
					coordsy=median(range(mapDF$coordsy))+c(-1,1,1,-1,-1)*diff(range(mapDF$coordsy))/max(att$grouping),
					textx=median(range(mapDF$coordsx)), texty=median(range(mapDF$coordsy)),	tmp.label='Median',
					region=1, poly=1, plug=0, hole=0, IDpoly='median')
	
	  if(att$median.row) pl <- pl + 
			geom_polygon(fill='white', colour='white', data=mapDF.median) + 
				geom_text(aes(x=textx, y=texty, label='Median', hjust=.5, vjust=.4), data=mapDF.median) +
				facet_grid(pGrp~., scales="free_y", space="free")

	  #################################
	  #################################


	  #*** Throw an outer hull over the outside

	if(outer.hull==T) for(g in nGroups[1]:nGroups[2]) pl <- pl + geom_polygon(fill=NA,	 
													colour=att[[p]]$outer.hull.color, 
													size=att[[p]]$outer.hull.size, 
													data=data.frame(dHull, pGrp=g))



	  #*** "house keeping" functions to align all panels and apply the user's graphical specifications\	
	xstr.title <-  "''"

	if(!all(is.na(all_atts(att, 'xaxis.title')))){
		tmp.titles <- lapply(all_atts(att, 'xaxis.title'), function(t) if(is.na(t)|t=='') t=' ' else t)
		tmp.title <- tmp.titles[[p]]
		ns <- max(unlist(lapply(tmp.titles, function(x) length(strsplit(x, '\n')[[1]])))) - length(strsplit(tmp.title,'\n')[[1]])  
		if(ns>0) tmp.title <- paste(tmp.title, rep(' \n ',ns), sep='')
		
		xstr.title <- paste("'",tmp.title,"'",sep='')
		pl <- pl + theme(axis.title.x = element_text(size=8*att[[p]]$xaxis.title.size, colour=bgcolor)) 
	  } else {
		pl <- pl + theme(axis.title.x = element_blank()) 
	}
	

	if(any(c(all_attsb(att, 'xaxis.ticks.display'), all_attsb(att, 'xaxis.text.display')))){
		pl <- pl + theme(axis.text.x = element_text(colour=bgcolor)) +
				theme(axis.ticks = element_line(colour=bgcolor))
	 } else { 
		pl <- pl + theme(axis.text.x = element_blank()) +
				theme(axis.ticks = element_blank()) 
	 }

	# We expand the maps so that they are as big as possible. First adjust to account for map aspect ratios, 
	# then for plot aspect ratio, then for the map panel portion of the plot (based on comparitive panel width), 
	# then for number of perceptual groups (need to adjust if # pereptual groups doesnt equal # of panels)
	pGrp.spacing <-  (	(diff(range(mapDF$coordsx)) / diff(range(mapDF$coordsy))) / 
				(att$plot.width / att$plot.height) * 	
				1/length(all_atts(att, 'panel.width')) / 
				(as.numeric(att[[p]]$panel.width) / sum(as.numeric(all_atts(att, 'panel.width')))) *
				length(all_atts(att, 'panel.width'))/max(mapDF$pGrp)
			 	- 1	)/ 2
	pGrp.spacing <- max(pGrp.spacing, att$map.spacing)	


	if(att$median.row) pGrp.spacing <- pGrp.spacing


	# panel margins must be brought in for the maps to line up by default so here
	# 	we do a little adjustment to what the user has specified	
	att[[p]]$panel.margins[2] <- att[[p]]$panel.margins[2] - .25


	ystr <- paste("scale_y_continuous('', breaks=NULL, expand=c(",pGrp.spacing,",",pGrp.spacing,"))")
#	ystr <- paste("scale_y_continuous('', breaks=NULL, expand=c(0,0))")
	pl <- pl + eval(parse(text=ystr))

	pl  <- plot_opts(p, pl, att)		
	pl  <- graph_opts(p, pl, att)	

	pl <- pl + theme(panel.margin = unit(0, "lines"))

	pl 

}







CatMaps <- function(pl, p, mapDF, att){
	bgcolor <- ifelse(!is.na(att[[p]]$panel.bgcolor), att[[p]]$panel.bgcolor, 'white')

	mapDF.all <- mapDF
	mapDF <- subset(mapDF, !is.na(pGrp))

	pl <-  
		ggplot(mapDF, aes(x=coordsx, y=coordsy, group=poly)) +  
		geom_polygon(fill='white', colour='black', data=transform(mapDF.all, pGrp=NULL)) + 	
		geom_polygon(fill=att$map.color, colour='black', data=subset(mapDF, hole==0)) + 				
		geom_polygon(fill='white', colour='black', data=subset(mapDF, hole==1)) +
		geom_polygon(fill='transparent', colour='black', data=subset(transform(mapDF, pGrp=NULL), hole==1)) +
		facet_grid(pGrp~.) +
		coord_equal() 
  	
	  #*** "house keeping" functions to align all panels and apply the user's graphical specifications
	xstr.title <-  "''"

	if(!all(is.na(all_atts(att, 'xaxis.title')))){

		tmp.titles <- lapply(all_atts(att, 'xaxis.title'), function(t) if(is.na(t)|t=='') t=' ' else t)
		tmp.title <- tmp.titles[[p]]
		ns <- max(unlist(lapply(tmp.titles, function(x) length(strsplit(x, '\n')[[1]])))) - length(strsplit(tmp.title,'\n')[[1]])  
		if(ns>0) tmp.title <- paste(tmp.title, rep(' \n ',ns), sep='')
		
		xstr.title <- paste("'",tmp.title,"'",sep='')
		pl <- pl + theme(axis.title.x = element_text(size=8*att[[p]]$xaxis.title.size, colour=bgcolor)) 
	  } else {
		pl <- pl + theme(axis.title.x = element_blank()) 
	}

	
	if(any(c(all_attsb(att, 'xaxis.ticks.display'), all_attsb(att, 'xaxis.text.display')))){
		pl <- pl + scale_x_continuous(xstr.title) +
				theme(axis.text.x = element_text(colour=bgcolor)) +
				theme(axis.ticks = element_line(colour=bgcolor)) 
	 } else { 
		pl <- pl + scale_x_continuous(xstr.title, breaks=NA)
	}


	# We expand the maps so that they are as big as possible. First adjust to account for map aspect ratios, 
	# then for plot aspect ratio, then for the map panel portion of the plot (based on comparitive panel width), 
	# then for number of perceptual groups (need to adjust if # pereptual groups doesnt equal # of panels)
	pGrp.spacing <-  ((diff(range(mapDF$coordsx)) / diff(range(mapDF$coordsy))) / 
				(att$plot.width / att$plot.height) * 	
			1/length(all_atts(att, 'panel.width')) / 
				(as.numeric(att[[p]]$panel.width) / sum(as.numeric(all_atts(att, 'panel.width')))) *
			length(all_atts(att, 'panel.width'))/max(mapDF$pGrp)
			 - 1)	/ 2
	pGrp.spacing <- max(pGrp.spacing, att$map.spacing)

	att[[p]]$panel.margins[2] <- att[[p]]$panel.margins[2] - .25

	ystr <- paste("scale_y_continuous('', breaks=NULL, expand=c(",pGrp.spacing,",",pGrp.spacing,"))")
#	ystr <- paste("scale_y_continuous('', breaks=NULL, expand=c(0,0))")
	pl <- pl + eval(parse(text=ystr))

	pl  <- plot_opts(p, pl, att)		
	pl  <- graph_opts(p, pl, att)	
	pl <- pl + theme(panel.margin = unit(0, "lines")) 

	pl

}


