RankMaps <- function(pl, p, mapDF, att){
	bgcolor <- ifelse(!is.na(att[[p]]$panel.bgcolor), att[[p]]$panel.bgcolor, 'white')

	ncolors <- length(att$colors)
	mapDF$fill.color <- mapDF$pGrpOrd
	mapDF$IDpoly <- paste(mapDF$ID,mapDF$poly,sep='.')
	
	if(att$median.row & any(mapDF$pGrp==att$m.pGrp)){
		mapDF.median <- subset(mapDF, pGrp==att$m.pGrp)
		mapDF.median$fill.color <- ncolors+1
		mapDF.median <- rbind(transform(mapDF.median, pGrp=(att$m.pGrp-.5), IDpoly='median1'),
					transform(mapDF.median, pGrp=(att$m.pGrp+.5), IDpoly='median2'))
		
		mapDF <- rbind(subset(mapDF, !pGrp==att$m.pGrp), mapDF.median)
	}
	


	mapDF.all <- mapDF
        mapDF <- subset(mapDF.all, !is.na(rank))

	nGroups <- range(mapDF$pGrp)	

	  #*** Creates the object and first draws entire map with no fill 
	  #***   (by transforming data set with all perceptual groups to NULL)	
	pl <- ggplot(mapDF, aes(x=coordsx, y=coordsy, group=IDpoly)) 

	for(g in 1:nGroups[2]) pl <- pl + geom_polygon(fill=NA, colour=att[[p]]$inactive.border.color, 
										size=att[[p]]$inactive.border.size/2, 
										data=transform(mapDF.all, pGrp=g))						

	  #*** Draws polygons from all previous perceptual groups by looping through
	  #***   each perceptual group and transforming dataset to have all previous 
	  #***   perceptual groups set equal to that current one
	if(att$two.ended.maps){
		for(g in max(nGroups[1],2):floor(att$m.pGrp)) pl <- pl + 
									geom_polygon(fill=att$map.color2, colour=att[[p]]$inactive.border.color,  
												size=att[[p]]$inactive.border.size/2, 
												data=transform(mapDF[mapDF$pGrp<g,], pGrp=g)) 	
		for(g in ceiling(att$m.pGrp):(nGroups[2]-1)) pl <- pl + 
									geom_polygon(fill=att$map.color2, colour=att[[p]]$inactive.border.color,  
												size=att[[p]]$inactive.border.size/2, 
												data=transform(mapDF[mapDF$pGrp>g,], pGrp=g)) 

	  } else {
		for(g in max(nGroups[1],2):nGroups[2]) pl <- pl + 
									geom_polygon(fill=att$map.color2, colour=att[[p]]$inactive.border.color,  
												size=att[[p]]$inactive.border.size/2, 
												data=transform(mapDF[mapDF$pGrp<g,], pGrp=g)) 
	}


	  #*** draws polygons of current perceptual group																			
	pl <- pl + 
		geom_polygon(aes(fill=factor(fill.color)), colour=att[[p]]$active.border.color, 
					size=att[[p]]$active.border.size/2, data=subset(mapDF, hole==0)) + 				
		facet_grid(pGrp~.) +
		scale_fill_manual(values=c(att$colors, gray(.5)), guide='none') +
		coord_equal() 


	  #################################
	  #################################
	  #*** insert space for median row	
	  mapDF.median <- data.frame(pGrpOrd=1, pGrp=(max(mapDF$pGrp)+1)/2, 
					rank=(max(mapDF$rank)+1)/2, ID='median', 
					coordsx=range(mapDF$coordsx)[c(1,1,2,2,1)], 
					coordsy=median(range(mapDF$coordsy))+c(-1,1,1,-1,-1)*diff(range(mapDF$coordsy))/max(att$grouping),
					textx=median(range(mapDF$coordsx)), texty=median(range(mapDF$coordsy)),	tmp.label='Median',
					region=1, poly=1, plug=0, hole=0, IDpoly='median')
	
	  if(att$median.row) pl <- pl + geom_polygon(fill='white', colour='white', data=mapDF.median) + 
							geom_text(aes(x=textx, y=texty, label='Median', hjust=.5, vjust=.4), data=mapDF.median) +
							facet_grid(pGrp~., scales="free_y", space="free")

	  #################################
	  #################################



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
	
	ystr <- paste("scale_y_continuous('', breaks=NA, expand=c(",pGrp.spacing,",",pGrp.spacing,"))")
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

	ystr <- paste("scale_y_continuous('', breaks=NA, expand=c(",pGrp.spacing,",",pGrp.spacing,"))")
	pl <- pl + eval(parse(text=ystr))


	pl  <- plot_opts(p, pl, att)		
	pl  <- graph_opts(p, pl, att)	
	pl <- pl + theme(panel.margin = unit(0, "lines")) 

	pl

}



