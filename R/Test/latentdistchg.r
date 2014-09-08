setwd("c:/Code/NetworkResearch/R/Scripts/KddPaper062014/Plots");
load("df.dist.subset.diff.rdata");
load("df.inaug.dist.subset.diff.rdata");


bombingLatentDistChgPlot <- ggplot(df.dist.subset.diff, aes(X1,X2, fill=-value)) +
  geom_raster() +
  scale_fill_gradient2("", low="red", mid="gray", high="blue", guide=FALSE, limits=c(-20, 20)) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(), text=element_text(size=28)) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)
  

inaugLatentDistChgPlot <- ggplot(df.inaug.dist.subset.diff, aes(X1,X2, fill=-value)) +
  geom_raster() +
  scale_fill_gradient2("", low="red", mid="gray", high="blue", guide=FALSE, limits=c(-20, 20),
                       breaks=c(-20, 0, 20), labels=c("Closer", "Same", "Further")) +
  guides( fill=guide_colorbar(barwidth=2, barheight=20) ) +
  labs(title=element_blank()) +
  theme(panel.background=element_rect(fill="white"),
        axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(), text=element_text(size=20)) + 
  scale_x_continuous(breaks=NULL, minor_breaks=NULL) +
  scale_y_continuous(breaks=NULL)


ggsave("BombingLatentDistanceChange.png", height=5, width=5, plot=bombingLatentDistChgPlot);
ggsave("InaugurationLatentDistanceChange.png", height=5, width=6.5, plot=inaugLatentDistChgPlot);











