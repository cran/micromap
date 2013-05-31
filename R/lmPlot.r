right <- function(txt, i)
	substring(txt, nchar(txt)-i+1)


all_atts <- function(a, att.name)
	unlist(unlist(a)[which(att.name == names(unlist(a)))])


all_equal2 <- function(v)
	all(unlist(sapply(1:length(v), function(x) v[x] == v[x:length(v)])))


subplot <- function(x, y)
	viewport(layout.pos.row=x, layout.pos.col=y)


#### Start of function ####

lmplot <- function(stat.data, map.data, 	# Required -- statistical data; map data
  panel.types, panel.data, 			# Required -- panel types (e.g. 'map', 'labels', 'dot.cl', etc.);
							# 	which columns in dStats to get the data from
  map.link=NULL,					# Specify in a vector (e.g. "c('Subpopulation', 'Poly_Name')") the columns from
							# 	dStats and dMap (respectively) to link the correct map to the correct data line
  nPanels=length(panel.types),		# A count of the number of panels (NOT MEANT TO ACTUALLY BE SPECIFIED BY USERS
							# 	but is used referenced in this function arguments list
  ord.by, rev.ord=FALSE,						# Column of the stat table by which to rank and order the output
  grouping, 					# Required	-- specifies which column in dStats by which to rank and order the output;
							# 	specifies the perceptual groups (e.g. "c(5,5,5,5,2)" to have 4 groups of 5 followed 
							# 	by a single group of 2			
  colors=brewer.pal(max(grouping), "Spectral"),	
  map.all=FALSE, map.color2='lightgray',
  median.row=FALSE, two.ended.maps=FALSE,

  print.file='no', print.res=300,
  
  panel.att=vector("list", nPanels),

  ### Options for entire linked micromap plot ###
  plot.header=NA,
  plot.header.size=NA,
  plot.header.color=NA,
  plot.footer=NA,
  plot.footer.size=NA,
  plot.footer.color=NA,
	
  plot.width=7,
  plot.height=7,

  map.spacing=1,
  plot.pGrp.spacing=1,
  plot.panel.spacing=1,
  plot.panel.margins=c(0,0,1,0)
) {					### end function arguments list


  # rename function inputs (for ease in coding)	---- 
dStats <- stat.data
dMap <- map.data


colors=colors[1:max(grouping)]	# errors can occur with a color list that is too long so we truncate 
						#	it to only the number of colors needed for this plot
if(median.row) colors <- c(colors, gray(.5))


pps = plot.panel.spacing*.05


###################################
## create attribute object/list ###
##################################

  # Set "plot wide" options first
plot.atts <- list(						
  			plot.pGrp.spacing=plot.pGrp.spacing*.05,
  			plot.panel.margins=plot.panel.margins,
  			ord.by=ord.by, 
  			grouping=grouping, 
  			colors=colors,
			map.color2=map.color2,
			map.spacing=map.spacing*.025,
			plot.width=plot.width,
			plot.height=plot.height)			


  # grab default attribute lists for each panel
a <- vector("list", nPanels)
for(p in 1:nPanels) {	
    att.name <- paste(panel.types[p],'_att',sep='')				# <panel type>.att is the name of the function that creates the 
    att.function <- paste(panel.types[p],'_att()',sep='')			# 	default attribute list. we check that it exists then run it
    if(exists(att.name)) a[[p]] <- eval(parse(text=att.function)) else a[[p]] <- eval(parse(text=standard_att))
}
a <- append(a, plot.atts)


  # panel data must be specified for every panel type so 
  #	we acount for it here first
for(j in 1:length(panel.types)) a[[j]] <- append(a[[j]], list(panel.data=unlist(panel.data[[j]])))
 	

  # all additional specified attributes are now dealt with
  # this loop through each sub list of the panel.att list, then loop through its entries, matching 
  # 	the names with those in "a", and the stored values in "a" are replaced with the users inputs
for(j in 1:length(panel.att)){	
    k <- unlist(panel.att[[j]][1], use.names=F)		# the first entry specifies which panel (i.e. which sublist) is being modified

    allnames <- names(panel.att[[j]])[-1]			# grab the names of the sublist entries
    for(s in allnames){	# s='header'
   	w <- match(s, names(a[[k]]))							# match the names to those in "a"
	if(is.na(w)) w <- match(paste('panel.',s,sep=''), names(a[[k]]))	# the "panel." part of the attribute name is somewhat 
												#	implies so a user may have left it off 

	  # replace the attribute or warn that no attribute by that name was found										
	if(!is.na(w)) a[[k]][[w]] <- unlist(panel.att[[j]][s], use.names=F) else warning(paste('"',s,'"',' is not a recognized attribute for panel type ','"',panel.types[j],'"', sep=''), call. = FALSE, immediate. = TRUE)
      }	

    if(!is.null(k)){
    	w <- match('left.margin', names(a[[k]]))
    	if(!is.na(w) & !is.na(a[[k]][[w]])) 	a[[k]]$panel.margins[4] <- a[[k]]$panel.margins[4] + a[[k]][[w]]
    	w <- match('right.margin', names(a[[k]]))
    	if(!is.na(w) & !is.na(a[[k]][[w]])) 	a[[k]]$panel.margins[2] <- a[[k]]$panel.margins[2] + a[[k]][[w]]
      }

    
  }

  # warnings for various entry differences that might screw up vertical alignment among panels:
if(!all_equal2(all_atts(a, 'panel.margins1')) | !all_equal2(all_atts(a, 'panel.margins3'))) warning("top and bottom panel margins should be equal in order for panels to align correctly", call. = FALSE, immediate. = TRUE)
if(!all_equal2(all_atts(a, 'panel.header.size'))) warning("header sizes should be equal in order for panels to align correctly", call. = FALSE, immediate. = TRUE)
if(!all_equal2(all_atts(a, 'xaxis.title.size'))) warning("x axis title sizes should be equal in order for panels to align correctly", call. = FALSE, immediate. = TRUE)

##########################
## data reorganization ###
##########################
  # move dStats into the DF variable, add an overall rank column, a perceptual group column based on ranks, a
  # 	within perceptual group ranking, and a column specifying if any entry row should be the median row 												
DF <- create_DF_rank(dStats, ord.by, grouping, median.row, rev.ord)

  # add entries in the attribute list so that information about the median can be passed 
  # 	among all the panel building and cleaning functions
a$median.row <- median.row			
a$two.ended.maps <- two.ended.maps	
a$m.pGrp <- (max(DF$pGrp)+1)/2
a$m.rank <- (max(DF$rank)+1)/2


  # Many panel plotting functions add extra columns to the DF table. This is
  #   created here as a reference to default back to after each panel is conctructed
DF.hold <- DF
a$ncols <- ncol(DF.hold)	# so we can keep track of how many extra columns have been added		
					# for now, this is only used in "assimilate.plot" function


# Rearrange the map data table, with a rank, pGrp, and pGrpOrd column
if(any(panel.types=='map')){
    # clear out any NA rows
  dMap <- dMap[!(is.na(dMap$coordsx)|is.na(dMap$coordsy)),]

    # if map.all is not specified we remove all polygons from mapDF that
    # 	which are not linked to the stat table
  w <- match(dMap[,map.link[2]], unique(DF[,map.link[1]]))
  if(!map.all) mapDF <- dMap[!is.na(w),] else mapDF <- dMap

    # make sure there is a hole and plug column. If not, just insert dummy columns 
  if(!'hole'%in%names(mapDF)) mapDF$hole <- 0
  if(!'plug'%in%names(mapDF)) mapDF$plug <- 0

    # link the pGrp and rank columns for plot order and facetting and
    #  make sure they're numeric    
  tmpDF <- unique(DF[,c('pGrp','rank', map.link[1])])
  w <- match(mapDF[,map.link[2]], tmpDF[,map.link[1]])
  mapDF <- cbind(pGrp=tmpDF$pGrp[w],rank=tmpDF$rank[w], mapDF)
  mapDF$pGrp <- as.numeric(mapDF$pGrp)
  mapDF$rank <- as.numeric(mapDF$rank)

    # link the pGrpOrd column for plot order and facetting
  tmpDF <- unique(DF[,c('pGrpOrd', map.link[1])])
  w <- match(mapDF[,map.link[2]], tmpDF[,map.link[1]])
  mapDF <- cbind(pGrpOrd=tmpDF$pGrpOrd[w], mapDF)
  mapDF$pGrpOrd <- as.numeric(mapDF$pGrpOrd)

  rm(tmpDF)
}


  # set up a list to store plot objects to be created later
  # 	note: each panel in the plot is a "ggplot object" 
plots <- vector("list", nPanels)


###############################
##### create plot objects #####
###############################

  # each section builds a plot object of the specified type
for(p in 1:nPanels){	

    if(panel.types[p]=='map'){

	  plots[[p]]  <- RankMaps(plots[[p]], p, mapDF, a)


      } else if(exists(as.character(paste(panel.types[p],'_build',sep='')))) {	# all graph types should have a function by the 
														# same name. first we check to see if such a function does
														# in fact exist, if so we use "eval(parse(..." to call it

	  plots[[p]] <- eval(parse(text=paste(panel.types[p],'_build(plots[[p]], p, DF, a)',sep='')))
     
      } else {
	
	  stop(paste("unknown panel type -- '",panel.types[p],"'", sep=''))		# if no such function exists, lmPlot2 throws in the towel

    }		
				
     # reset DF table (delete added tmp.data columns and what not)
    DF <- DF.hold	
}



##############################
##### construct the plot #####
##############################

lwidth <- pps
for(w in 1:length(all_atts(a,'panel.width'))) lwidth <- c(lwidth, all_atts(a,'panel.width')[w], pps)

  # sets layout according to specified widths in function arguments
lmLayout <- grid.layout(nrow = 1, ncol = length(lwidth), 
			widths = unit(lwidth, rep("null", length(lwidth))), 
			heights = unit(rep(1, length(lwidth)), rep("null", length(lwidth))))
plots$layout <- lmLayout
plots$plot.width <- plot.width
plots$plot.height <- plot.height


  # creates final LM from plot objects
if(right(print.file,4)=='.pdf') 	pdf(print.file, width = plot.width, height = plot.height)
if(right(print.file,5)=='.tiff') 	tiff(print.file, width = plot.width, height = plot.height, units='in', res=print.res)
if(right(print.file,5)=='.jpeg') 	jpeg(print.file, width = plot.width, height = plot.height, units='in', res=print.res)
if(right(print.file,4)=='.jpg') 	jpeg(print.file, width = plot.width, height = plot.height, units='in', res=print.res)
if(right(print.file,5)=='.png') 	png(print.file, width = plot.width, height = plot.height, units='in', res=print.res)
if(!right(print.file,4) %in% c('.pdf','tiff','jpeg','.png')) x11(width=plot.width, height=plot.height)

  # sets up layout and loops through panels in order plotting them out
grid.newpage()
pushViewport(viewport(layout = lmLayout))
suppressWarnings(for(p in 1:nPanels) print(plots[[p]], vp = subplot(1, p*2)))

if(right(print.file,4)%in%c('.pdf','tiff','jpeg','.png')) dev.off()

  # invisibly return the list of ggplot objects 
invisible(plots)

}   ###### END FUNCTION ######




printLMPlot <- function(plobject, name, res){
	file.name <- name
	if(right(name,4)=='.pdf')  pdf(name, width = plobject$plot.width, height = plobject$plot.height)
	if(right(name,5)=='.tiff') tiff(name, width = plobject$plot.width, height = plobject$plot.height, units='in', res=res)
	if(right(name,5)=='.jpeg') jpeg(name, width = plobject$plot.width, height = plobject$plot.height, units='in', res=res)
	if(right(name,5)=='.png')  png(name, width = plobject$plot.width, height = plobject$plot.height, units='in', res=res)
	
	grid.newpage()
	pushViewport(viewport(layout = plobject$layout))
	subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
	suppressWarnings(for(p in 1:(length(plobject)-3)) print(plobject[[p]], vp = subplot(1, p*2)))
	
	dev.off()
}