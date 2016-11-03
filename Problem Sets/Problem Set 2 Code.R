################################################################################

################################################################################

### Clear workspace
rm(list = ls())

### Load packages
if (!require("rworldmap")) install.packages("rworldmap"); library(rworldmap)
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer)
if (!require("classInt")) install.packages("classInt"); library(classInt)
if (!require("xlsx")) install.packages("xlsx"); library(xlsx)
### Set working directory
setwd("/Users/felix/Dropbox/GSE/Term2/DataViz/Problemset 2")

### Load data
#dat <- read.csv("data.csv",header = TRUE,sep=";",stringsAsFactors=FALSE)

dat <- read.xlsx("EPI2014.xls", sheetIndex = 3)

################################################################################

################################################################################

names <- c("ISO3v10",
           "EH...Health.Impacts",
           "EH...Air.Quality",
           "EH..Water.and.Sanitation",
           "Environmental.Health"
          )

d01    <- dat[,names]
colnames(d01) <- c("ISO3v10",
                   "Health Impacts",
                   "Air Quality", 
                   "Water and Sanitation",
                   "Environmental Health"
                   )

mapD01 <- joinCountryData2Map( d01, nameJoinColumn = "ISO3v10")

col01  <- colorRampPalette(brewer.pal(9,"Blues"))(200)
col02  <- colorRampPalette(brewer.pal(9,"Greens"))(200)
col03  <- colorRampPalette(brewer.pal(9,"Purples"))(200)
col04  <- colorRampPalette(brewer.pal(9,"Reds"))(200)

################################################################################

################################################################################

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
png("m01.png")
m01 <-mapCountryData( 
                mapD01 , 
                nameColumnToPlot = "Health Impacts",
                colourPalette = col01,
                addLegend = FALSE
              )
do.call(addMapLegend
        ,c(m01
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 3))
dev.off()

################################################################################

################################################################################

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
png("m02.png")
m02 <-mapCountryData( 
  mapD01 , 
  nameColumnToPlot = "Air Quality",
  colourPalette = col02,
  addLegend = FALSE
)
do.call(addMapLegend
        ,c(m02
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 3))
dev.off()
################################################################################

################################################################################

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
png("m03.png")
m03 <-mapCountryData( 
  mapD01 , 
  nameColumnToPlot = "Water and Sanitation",
  colourPalette = col03,
  addLegend = FALSE
)
do.call(addMapLegend
        ,c(m03
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 3))
dev.off()

################################################################################

################################################################################

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
png("m04.png")
m04 <-mapCountryData( 
  mapD01 , 
  nameColumnToPlot = "Environmental Health",
  colourPalette = col04,
  addLegend = FALSE
)
do.call(addMapLegend
        ,c(m04
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 3))
dev.off()

################################################################################

################################################################################