
#############################  Filogenia de las especies de Macrohongos
library(taxize)
spnames <- c("Amauroderma brasiliensis" ,
             "Amauroderma schomburgkii ",
             "Auricularia auricula",
             "Auricularia delicata",
             "Auricularia fuscosuccinea",
             "Auricularia mesentérica",
             "Auricularia nigricans",
             "Xylaria feejeensis",
             "Xylaria globosa",
             "Xylaria guianensis",
             "Xylaria longipes",
             "Xylaria multiplex",
             "Xylaria polymorpha",
             "Cordyceps militaris",
             "Cookeina speciosa" ,
             "Cookeina tricoloma")
out <- classification(spnames,db='ncbi')
tr <- class2tree(out) 
plot(tr)


BB = ~plot(tr)


library(sf)
Area       <- st_read("SHP/Vivero.geojson")
Are        <- st_transform(Area,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Punto      <- st_read("SHP/Punto.geojson")
Puntos     <- st_transform(Punto,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Punto_sd = cbind(Puntos , spnames)
Are_xy <- cbind(Are, st_coordinates(st_centroid(Are$geometry)))
library(ggspatial)
A = ggplot()+
  geom_sf(data = Are)+
  geom_sf(data = Punto_sd  , color="black", aes(fill=spnames), pch = 22, size=3)+
  scale_fill_viridis_d("Especies\nMacrohongos")+
  theme_void()+
  annotation_scale()+
  annotation_north_arrow(location="br",which_north="true",
                         style=north_arrow_fancy_orienteering ())+
  geom_label(data =  Are_xy , aes(x= X, y=Y, label = BLOQUE), size = 2.5, 
             color="black", fontface = "bold",fontfamily = "serif", alpha=0.4)

A 
# Exportacion
library(cowplot)
library(ggplot2)
library(gridGraphics)
plots =ggdraw() + coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(BB, width = 10, height = 21,x = 0.001, y = 0.001)+
  draw_plot(A, width = 19, height = 21,x = 10, y = 0.001)+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect (color = "white",
                                     fill = NA))+
  geom_segment(aes(x=9, xend=19.1, y=18.5, yend=11), 
               linetype = "dashed", color = "#FDE725FF", size = 0.4) +
  geom_segment(aes(x=9, xend=19, y=17, yend=12), 
               linetype = "dashed", color = "#FDE725FF", size = 0.4) +
  geom_segment(aes(x=9, xend=12.1, y=3, yend=13.3), 
               linetype = "dashed", color = "#440154FF" , size = 0.4) +
  geom_segment(aes(x=9, xend=13.1, y=3, yend=13.3), 
               linetype = "dashed", color = "#481A6CFF", size = 0.4) +
  geom_segment(aes(x=9, xend=13.6, y=7.8, yend=13.1), 
               linetype = "dashed", color = "#472F7DFF" , size = 0.4) +
  geom_segment(aes(x=9, xend=14.4, y=9, yend=13), 
               linetype = "dashed", color = "#414487FF" , size = 0.4) +
  geom_segment(aes(x=9, xend=16, y=6.3, yend=13.2), 
               linetype = "dashed", color = "#39568CFF" , size = 0.4) +
  geom_segment(aes(x=9, xend=18, y=10, yend=13.1), 
               linetype = "dashed", color = "#23888EFF"  , size = 0.4) +
  geom_segment(aes(x=9, xend=17.5, y=5, yend=13.2), 
               linetype = "dashed", color = "#35B779FF" , size = 0.4) +
  geom_segment(aes(x=9, xend=19, y=11.3, yend=13.2), 
               linetype = "dashed", color = "#22A884FF" , size = 0.4) +
  
  
  
  annotate(geom = "text", x = 8, y = 3, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo                       Gorky Florez Castillo                        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)+
  annotate(geom = "text", x = 3, y =20, hjust = 0, vjust = 1, 
           label = "MAPA DE AGRUPACION POR FAMILIA  DE MACROHONGOS",
           size = 8, family="serif", color = "grey20")

plots 

ggsave(plot=plots,"Mapa/01.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)

library(viridisLite)  # load package
v1 <- viridis(16)
vir_10 <- viridis(n = 16)
