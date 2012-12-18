library(micromaps)

#Begin code for Figure 1 examples of education and poverty
### intial example
data("edPov")
data("statesShapefile")

head(statesShapefile@data )

statePolys <- create_map_table(statesShapefile, 'ST')
head(statePolys)


### basic figure 1
lmplot(stat.data=edPov,
  	map.data=statePolys,
	panel.types=c('labels', 'dot', 'dot','map'),
	panel.data=list('state','pov','ed', NA),
	ord.by='pov',   
	grouping=5, median.row=T,
	map.link=c('StateAb','ID'))


### publication figure 1a
lmplot(stat.data=edPov,  map.data=statePolys ,
	panel.types=c('labels', 'dot', 'dot','map'),
	panel.data=list('state','pov','ed', NA),
	ord.by='pov',  
	grouping=5, 
  	median.row=T,
	map.link=c('StateAb','ID'),
	
  	plot.height=9,							
	colors=c('red','orange','green','blue','purple'), 
	map.color2='lightgray',
       

	panel.att=list(list(1, header='States', panel.width=.8, align='left', text.size=.9),
			list(2, header='Percent Living Below \n Poverty Level',
				graph.bgcolor='lightgray', point.size=1.5,
				xaxis.ticks=list(10,15,20), xaxis.labels=list(10,15,20),
				xaxis.title='Percent'),
			list(3, header='Percent Adults With\n4+ Years of College',
				graph.bgcolor='lightgray', point.size=1.5,
				xaxis.ticks=list(0,20,30,40), xaxis.labels=list(0,20,30,40),
				xaxis.title='Percent'),
			list(4, header='Light Gray Means\nHighlighted Above',  
				inactive.border.color=gray(.7), inactive.border.size=2,	
				panel.width=.8)))

edPov$points <- 0	

### publication figure 1b
lmplot (stat.data=edPov, map.data=statePolys,
	panel.types=c('dot',  'labels', 'dot', 'dot', 'map'),
	panel.data=list('points', 'state', 'pov', 'ed', NA),
	map.link=c('StateAb','ID'),
	ord.by='pov', 
	grouping=5, 
	median.row=T, 
	
	plot.height=9, 
	
	colors=c('red','orange','green','blue','purple'),
	map.color2='lightgray', 

	panel.att=list(list(1, panel.width=.15, point.type=20,
					graph.border.color='white',
					xaxis.text.display=F, xaxis.line.display=F,
					graph.grid.major=F),
	
				list(2, header='States', panel.width=.8, 
					align='left', text.size=.9),

				list(3, header='Percent Living Below\nPoverty Level',
					graph.bgcolor='lightgray', point.size=1.5,
					xaxis.ticks=list(10,15,20), xaxis.labels=list(10,15,20),
					xaxis.title='Percent'),

				list(4, header='Percent Adults With\n4+ Years of College',
					graph.bgcolor='lightgray', point.size=1.5,
					xaxis.ticks=list(20,30,40), xaxis.labels=list(20,30,40), 
					xaxis.title='Percent'),
	
				list(5, header='Light Gray Means\nHighlighted Above', 
					inactive.border.color=gray(.7), inactive.border.size=2, 
					panel.width=.8)))




### publication figure 1c
myPlot <- lmplot(stat.data=edPov, map.data=statePolys,
	panel.types=c('map', 'dot',  'labels', 'dot', 'dot'),
	panel.data=list(NA, 'points', 'state', 'pov', 'ed'),
	map.link=c('StateAb','ID'),
	ord.by='pov', 
	grouping=5, 
	median.row=T,

	plot.height=9, 
	
	colors=c('red','orange','green','blue','purple'),
	map.color2='lightgray', 

	panel.att=list(list(2, panel.width=.15, point.type=20,
				graph.border.color='white',
				xaxis.text.display=F, xaxis.line.display=F,
				graph.grid.major=F),

			list(3, header='States', panel.width=.8, 
				align='left', text.size=.9),

			list(4, header='Percent Living Below\nPoverty Level',
				graph.bgcolor='lightgray', point.size=1.5,
				xaxis.ticks=list(10,15,20), xaxis.labels=list(10,15,20),
				xaxis.title='Percent'),

			list(5, header='Percent Adults With\n4+ Years of College',
				graph.bgcolor='lightgray', point.size=1.5,
				xaxis.ticks=list(20,30,40), xaxis.labels=list(20,30,40), 
				xaxis.title='Percent'),
	
			list(1, header='Light Gray Means\nHighlighted Above', 
				inactive.border.color=gray(.7), inactive.border.size=2, 
				panel.width=.8)))

printLMPlot(myPlot, name='myExhibit.tiff', res=300)




#Begin code for Figure 2 examples thinning Texas ecoregion polygons
setwd('c:/temp')
download.file("ftp://ftp.epa.gov/wed/ecoregions/tx/tx_eco_l3.zip",'tx_eco_l3.zip')
library(rgdal)
library(utils)
unzip("tx_eco_l3.zip")
txeco <- readOGR(".", "tx_eco_l3")
plot(txeco)

library(maptools)
myPolys1<-thinnedSpatialPoly(txeco,tolerance=10000, minarea=0.001, topologyPreserve = F, avoidGEOS = T)
plot(myPolys1)

library(rgeos)
txeco2 <- gSimplify(txeco, 10000, topologyPreserve=T)
class(txeco2)
txeco2 <- SpatialPolygonsDataFrame(txeco2, txeco@data)
class(txeco2)
plot(txeco2)
head(txeco2@data)
myPolys2 <- create_map_table(txeco2, 'US_L3CODE')
head(myPolys2)
myData <- txeco2@data
myData$NumericCodes <- as.numeric(levels(myData$US_L3CODE))
### basic figure 2
lmplot(stat.data=myData,
  	map.data=myPolys2,
	panel.types=c('labels', 'dot', 'map'), 
	panel.data=list('US_L3NAME','NumericCodes', NA),
	ord.by='NumericCodes', 
	grouping=3, 
	map.link=c('US_L3CODE','ID'))


### publication figure 2
lmplot(stat.data=myData,
  	map.data=myPolys2,
	panel.types=c('map', 'labels', 'dot'),
	panel.data=list(NA, 'US_L3NAME','NumericCodes'),
	ord.by='NumericCodes', 
	grouping=3, 
	map.link=c('US_L3CODE','ID'),

	plot.height=3.5,
	plot.width=6,
	colors=c('blue','purple','brown'),

	panel.att=list(list(1, panel.width=.75,
					inactive.border.color=gray(.7),
					inactive.border.size=2),
				list(2, header='Ecoregion Name',
					text.size=.75, align='left'),
		    		list(3, header='US Level 3 Code',
					graph.bgcolor='lightgray', 
					point.size=1.5)))




#Begin code for Figure 3 examples of lung cancer data
### new graph type instructions
data("lungMort")
myStats <- lungMort
head(myStats)


myStats <- subset(myStats, !StateAb=='DC')


myNewStats <- create_DF_rank(myStats, ord.by="Rate_00", grouping=5)
head(myNewStats)

### ggplot2 code:
ggplot(myNewStats) +
	geom_segment(aes(x=Rate_95, y=-pGrpOrd,
		xend=Rate_00, yend=-pGrpOrd, colour=factor(color)),
		arrow=arrow(length=unit(0.1,"cm"))) +
	facet_grid(pGrp~., scales="free_y") +
	scale_colour_manual(values=c('red','orange','green','blue','purple'), 
		guide="none")




myAtts <- sample_att()
myNumber <- 1

myAtts$colors  <- c('red','orange','green','blue','purple')
myAtts[[myNumber]]$panel.data <- c('Rate_95','Rate_00')


myColors <- myAtts$colors 			# pulls color out of the plot level section of the “myAtts” attributes list
myColumns <- myAtts[[myNumber]]$panel.data 	# looks in the panel level section numbered “myNumber” of the “myAtts” attributes list 



myColors <- myAtts$colors 			# pulls the “colors” attribute out of the plot level section of the “myAtts” attributes list
myColumns <- myAtts[[myNumber]]$panel.data 	# looks in the panel level section numbered “myNumber” of the “myAtts” attributes list 

myNewStats$data1 <- myNewStats[, myColumns[1] ]
myNewStats$data2 <- myNewStats[, myColumns[2] ]



## build graph
myPanel  <- ggplot(myNewStats) +
 			geom_segment(aes(x=data1, y=-pGrpOrd,
					xend= data2, yend=-pGrpOrd, colour=factor(color)),
					arrow=arrow(length=unit(0.1,"cm"))) +
 			facet_grid(pGrp~.) +
 			scale_colour_manual(values=myColors, 
					guide="none")

myPanel
class(myPanel)


## add in plot attributes
assimilatePlot(myPanel, myNumber, myAtts) 


## build function
arrow_plot_build <- function(myPanel, myNumber, myNewStats, myAtts){
	myColors <- myAtts$colors 			
	myColumns <- myAtts[[myNumber]]$panel.data 	

	myNewStats$data1 <- myNewStats[, myColumns[1] ]
	myNewStats$data2 <- myNewStats[, myColumns[2] ]
	
	myPanel  <- ggplot(myNewStats) +
			 geom_segment(aes(x=data1, y=-pGrpOrd,
						xend= data2, yend=-pGrpOrd,
						colour=factor(color)),
						arrow=arrow(length=unit(0.1,"cm")))  +
			facet_grid(pGrp~.) +
			scale_colour_manual(values=myColors, guide="none")
	
	myPanel <- assimilatePlot(myPanel, myNumber, myAtts) 
	
	myPanel
 }




## add median row code
arrow_plot_build <- function(myPanel, myNumber, myNewStats, myAtts){
	myColors <- myAtts$colors 			
	myColumns <- myAtts[[myNumber]]$panel.data 	

	myNewStats$data1 <- myNewStats[, myColumns[1] ]
	myNewStats$data2 <- myNewStats[, myColumns[2] ]

	myNewStats <- alterForMedian(myNewStats, myAtts)

	myPanel  <- ggplot(myNewStats) +
			 geom_segment(aes(x=data1, y=-pGrpOrd,
						xend= data2, yend=-pGrpOrd,
						colour=factor(color)),
						arrow=arrow(length=unit(0.1,"cm")))  +
			facet_grid(pGrp~.) +
			scale_colour_manual(values=myColors, guide="none")
	
	myPanel <- assimilatePlot(myPanel, myNumber, myAtts) 
	
	myPanel
 }



## build specialized attributes list
myPanelAtts <- standard_att()
myPanelAtts <- append(myPanelAtts, 
				list(line.width=1, tip.length=1))
myPanelAtts


arrow_plot_att <- function(){
	myPanelAtts <- standard_att()
	myPanelAtts <- append(myPanelAtts, 
					list(line.width=1, tip.length=1))
	myPanelAtts
}



## implement specialized attributes
arrow_plot_build <- function(myPanel, myNumber, myNewStats, myAtts){
	myColors <- myAtts$colors 			
	myColumns <- myAtts[[myNumber]]$panel.data 	
	myLineWidth <- myAtts[[myNumber]]$line.width	# Again, note that these are stored in the panel level section of the 
	myTipLength <- myAtts[[myNumber]]$tip.length	#    attributes object

	
	myNewStats$data1 <- myNewStats[, myColumns[1] ]
	myNewStats$data2 <- myNewStats[, myColumns[2] ]
	
	myNewStats <- alterForMedian(myNewStats, myAtts)
	
	myPanel  <- ggplot(myNewStats) +
				geom_segment(aes(x=data1, y=-pGrpOrd,
							xend= data2, yend=-pGrpOrd, 
							colour=factor(color)),
							arrow=arrow(length=unit(0.1*myTipLength,"cm")),	# Here, you’ll notice the “1” default above # is specifying length in tenths of a cm
							size=myLineWidth)  +
				facet_grid(pGrp~., space="free", scales="free_y") +
				scale_colour_manual(values=myColors, guide="none")

	myPanel <- assimilatePlot(myPanel, myNumber, myAtts) 

	myPanel
  }



## basic figure 3 
lmplot(stat.data=myStats,
  	map.data=statePolys,
	panel.types=c('map','labels', 'arrow_plot'),
	panel.data=list(NA,'State', list('Rate_95','Rate_00')), 
	ord.by='Rate_00', 
	grouping=5,
	map.link=c('StateAb','ID'),

	panel.att=list(list(3, line.width=1.25, tip.length=1.5)))



## publication figure 3
myStats <- lungMort
myStats$points <- 0

lmplot(stat.data=myStats, 
  	map.data=statePolys,
	panel.types=c('map', 'dot',  'labels', 'dot_cl', 'arrow_plot'),
	panel.data=list(NA, 'points', 'State',
      			list('Rate_00','Lower_00','Upper_00'),  
				list('Rate_95','Rate_00')),
	ord.by='Rate_00', 
	grouping=5, 
	median.row=T,  
	two.ended=T,
	map.link=c('StateAb','ID'),

	plot.height=10, 
	colors=c('red','orange','green','blue','purple'), 
	map.color2='lightgray',

	panel.att=list(list(1, header='Light Gray Means\n Highlighted Above',
					panel.width=1, 
					inactive.border.color=gray(.7), 
					inactive.border.size=2), 
	
			      list(2, panel.width=.15, 
					graph.border.color='white',
					xaxis.text.display=F, 
					xaxis.line.display=F,
					graph.grid.major=F, 
					point.type=20),
	
			      list(3, header='U.S.   \nStates   ', 
					align='left', text.size=.9),

		     	 	list(4, header='State 2000\n Rate and 95% CI',
					graph.bgcolor='lightgray',
					xaxis.ticks=c(20,30,40,50), 
					xaxis.labels=c(20,30,40,50),
					xaxis.title='Deaths per 100,000'),

			      list(5, header='State Rate Change\n 1995-99 to 2000-04',
					line.width=1.25, tip.length=1.5,
					graph.bgcolor='lightgray',
					xaxis.ticks=c(20,30,40,50), 
					xaxis.labels=c(20,30,40,50), 
					xaxis.title='Deaths per 100,000'))) 



#Begin code Figure 4 examples of group-categorized micromaps of vegetation coverage
### lmgroupedPlot
data("vegCov")
data("WSA3")
print(vegCov)
print(WSA3@data)

wsa.polys <- create_map_table(WSA3)
head(wsa.polys)

national.polys <-subset(wsa.polys, hole==0 & plug==0)
national.polys <- transform(national.polys, ID='National', region=4, poly=region*1000 + poly)
head(national.polys)

wsa.polys <- rbind(wsa.polys, national.polys)


### basic figure 4
lmgroupedplot(stat.data=vegCov,
	map.data=wsa.polys,
	panel.types=c('map', 'labels', 'bar_cl', 'bar_cl'),
	panel.data=list(NA,'Category',
				list('Estimate.P','LCB95Pct.P','UCB95Pct.P'),
				list('Estimate.U','LCB95Pct.U','UCB95Pct.U')),
	grp.by='Subpopulation',
	cat='Category', 
	map.link=c('Subpopulation', 'ID'))



### publication figure 4
lmgroupedplot(stat.data= vegCov,
	map.data= wsa.polys,
	panel.types=c('map', 'labels', 'bar_cl', 'bar_cl'),
	panel.data=list(NA,'Category',
		       list('Estimate.P','LCB95Pct.P','UCB95Pct.P'),
		       list('Estimate.U','LCB95Pct.U','UCB95Pct.U')),
	grp.by='Subpopulation',
	cat='Category',
	colors=c('red3','green3','lightblue'),
	map.link=c('Subpopulation', 'ID'),
	map.color='orange3',
	plot.grp.spacing=2,
	plot.width=7,
	plot.height=4,

	panel.att=list(list(1, header='Region', header.size=1.5, 
				panel.width=.75), 
			list(2, header='Category', 
				header.size=1.5, 
				panel.width=1.7),
			list(3, header='Percent', header.size=1.5, 
				xaxis.ticks.display=T, 
				xaxis.text.display=T, 
				graph.bgcolor='lightgray',
				xaxis.title='percent',
				xaxis.ticks=c(0,20,40,60),
				xaxis.labels=c(0,20,40,60)),
			list(4, header='Unit', header.size=1.5, 
				xaxis.ticks.display=T, 
				xaxis.text.display=T, 
				graph.bgcolor='lightgray',
				xaxis.title='thousands',
				xaxis.ticks=c(0, 200000,350000,550000),
				xaxis.labels=c(0, 200,350,550))))









