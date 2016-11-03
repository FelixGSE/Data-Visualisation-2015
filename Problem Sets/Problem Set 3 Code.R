setwd('/Users/felix/Dropbox/GSE/Term2/DataViz/Problemset 3')

co <- 1/255 

pers.green      <- rgb( co *  14 ,  co * 105 , co *  90 )
pers.blue       <- rgb( co *  22 ,  co *  54 , co *  92 )
pers.red        <- rgb( co *  99 ,  co *  37 , co *  35 )
pers.gray       <- rgb( co * 150 ,  co * 150 , co * 150 )
pers.orange     <- rgb( co * 186 ,  co *  85 , co *  59 )
pers.beige      <- rgb( co * 196 ,  co * 189 , co * 151 )

if (!require("devtools")) 	install.packages("devtools"); library(devtools)

install_github("vqv/ggbiplot")
library(ggbiplot)
library(RColorBrewer)

wine <- read.table("http://www.econ.upf.edu/~michael/visualdata/winequality-red.csv", 
                   sep=";",
                   check.names=F, 
                   header=T
                  )

mypalette<-brewer.pal(9,"Blues")[]

wine.sub <- scale(wine[,1:11],center=TRUE,scale = TRUE)

wine.pca <- prcomp(wine.sub, scale. = FALSE)



biplot <- ggbiplot(wine.pca, obs.scale = 0, alpha = 0.04,
         var.scale = 1,varname.size = 3.5,
         groups = factor(wine$quality,labels=c('Quality Level 1', 'Quality Level 2',
                                                  'Quality Level 3', 'Quality Level 4',
                                                  'Quality Level 5','Quality Level 6') ),
         ellipse = TRUE, 
         circle = FALSE) +
         theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  ) +
  geom_vline(xintercept = 0,colour = 'darkgray',linetype='dashed') + 
  geom_hline(yintercept = 0,colour = 'darkgray',linetype='dashed') + 
   scale_color_brewer(palette="Reds",name="") +
  coord_fixed(ratio = 1) +
  theme(legend.direction = 'horizontal', legend.position = 'bottom',legend.title=element_text('')) 


ggsave(biplot,file="biplot.png",dpi=600)

labels=c('Quality Level 1', 'Quality Level 2',
         'Quality Level 3', 'Quality Level 4',
          'Quality Level 5','Quality Level 6')
