rm(list=ls())
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")
library("ggspatial")
library("ggplot2")
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)
OpenWindow = function (Width,Height) {
  if (Sys.info()["sysname"]=="Darwin"){  # (Darwin stands for a Mac computer)
    quartz(width=Width, height=Height)           
  } else {
    windows(width = Width, height = Height)}
}

SaveFigure=function(FileName){
  if (Sys.info()["sysname"]=="Darwin"){ # (Darwin stands for a Mac computer)
    quartz.save(paste(FileName,'.pdf',sep=''),type = c("pdf"),device = dev.cur())
  } else
    savePlot(filename = FileName,type = c("pdf"),device = dev.cur(),restoreConsole = TRUE) 
}
dataxy = read.csv("sitexy.csv")
OpenWindow(10,13)
# par(mar=c(3.2, 5, 2.2, 0.1) + 0.1)
ggplot(data = world) +
  geom_sf()+ 
  theme_bw()+
  theme(plot.margin=unit(c(0.9,1.1,0.9,0.9),'lines'),
    legend.text =element_text(size = 10,color='black'),
    legend.title = element_text(size=10, face = "plain"),
    legend.position = "none", axis.title.x = element_blank(), 
    axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
    panel.border = element_rect(fill = NA))+
  annotate(geom = "text", x = 121.1, y = 31.25, label = "Shanghai",
           fontface = "italic", color = "grey22", size = 6)+
  annotate(geom = "text", x = 122, y = 31.7, label = " Yangtze River Estuary",
           fontface = "italic", color = "grey22", size = 6) +
  annotate(geom = "text", x = 121.75, y = 30.5, label = " Hangzhou Gulf",
           fontface = "italic", color = "grey22", size = 6) +
  annotate(geom = "text", x = 122.15, y = 31.46, label = "  CDNR",
           fontface = "italic", color = "darkred", size = 5) +
  annotate(geom = "text", x = 122.10, y = 30.83, label = "Fengxian shoal",
           fontface = "italic", color = "darkred", size = 5) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(120,123.2), ylim = c(29, 33), expand = FALSE)+
  geom_point(data=dataxy, aes(x=Longitude, y=Latitude, shape=Site),size = 5)+
  guides(shape= guide_legend(nrow = 6, byrow = F))+
  # geom_point(aes(121.93,31.46),size = 3,shape = 17)+
  # geom_point(aes(121.54,30.81),size = 3,shape = 17)+
  labs(x="",y="")
SaveFigure("fig1")
