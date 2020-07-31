##PLOTS FOR WVWA
##AUTHOR: Mary Lofton
##DATE: 18MAY18
#updated 31july2020 by DWH

#load packages
library(tidyverse)
library(cowplot)

#load data, currently run ccr2020 lines from CCR_heatmaps_by_year_script
carvins <- read_csv("./091218_ccr50.csv")
falling.creek <- read_csv("./091218_fcr50.csv")
beaverdam <- read_csv("./090718_bvr50.csv")

#set plot theme preferences
mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 axis.text=element_text(size=16),
                 axis.title=element_text(size=16,face="bold"))

#write plot -- CARVINS COVE
ccr_do <- ggplot(data = july22a)+ #data was "carvins" before 
  geom_path(aes(x = DO_mgL, y = Depth_m*-3.28084+1170), col = "blue", size=1.2)+
  xlab("Dissolved oxygen (milligrams per liter)")+
  ylab("Elevation (feet)")+
  scale_y_continuous(breaks = c(1100, 1130, 1140, 1150, 1170))+
  geom_hline(yintercept=1130,size=1.1)+
  geom_hline(yintercept=1140,size=1.1)+
  geom_hline(yintercept=1150,size=1.1)+
  mytheme
ccr_do

ccr_chla <- ggplot(data = july22a)+
  geom_path(aes(x = Chla_ugL, y = Depth_m*-3.28084+1170), col = "darkgreen", size=1.2)+
  xlab("Chlorophyll a (micrograms per liter)")+
  ylab("Elevation (feet)")+
  scale_y_continuous(breaks = c(1100, 1130, 1140, 1150, 1170))+
  geom_hline(yintercept=1130,size=1.1)+
  geom_hline(yintercept=1140,size=1.1)+
  geom_hline(yintercept=1150,size=1.1)+
  mytheme
ccr_chla

ccr <- plot_grid(ccr_do, ccr_chla, 
                  ncol=2, nrow=1, align="hv")

title <- ggdraw() + 
  draw_label("Carvin's Cove",
             fontface = 'bold',
             size = 16)
ccr2 <- plot_grid(title, ccr, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins
ccr2

# ggsave(plot=ccr2, file="./WVWA plots/ccr_091218.png",
#        h=8, w=12, units="in", dpi=300,scale = 1)


#write plot -- FALLING CREEK
{
fcr_do <- ggplot(data = falling.creek)+
  geom_path(aes(x = DO_mgL, y = Depth_m*-3.28084+1663), col = "blue", size = 1.2)+
  xlab("Dissolved oxygen (milligrams per liter)")+
  ylab("Elevation (feet)")+
  scale_y_continuous(breaks = c(1663, 1660.5, 1658, 1654.5, 1651, 1647, 1643, 1637, 1634))+
  mytheme
fcr_do

fcr_chla <- ggplot(data = falling.creek)+
  geom_path(aes(x = Chla_ugL, y = Depth_m*-3.28084+1663), col = "darkgreen", size = 1.2)+
  xlab("Chlorophyll a (micrograms per liter)")+
  ylab("Elevation (feet)")+
  scale_y_continuous(breaks = c(1663, 1660.5, 1658, 1654.5, 1651, 1647, 1643, 1637, 1634))+
  mytheme
fcr_chla

fcr <- plot_grid(fcr_do, fcr_chla,   
                 ncol=2, nrow=1, align="hv")

title1 <- ggdraw() + 
  draw_label("Falling Creek",
             fontface = 'bold',
             size = 16)
fcr2 <- plot_grid(title1, fcr, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins
fcr2

ggsave(plot=fcr2, file="./WVWA plots/fcr_091218.png",
       h=8, w=16, units="in", dpi=300,scale = 1)
}


#write plot -- BEAVERDAM
{
beaverdam <- beaverdam %>%
  filter(DO_mgL >=0)

bvr_do <- ggplot(data = beaverdam)+
  geom_path(aes(x = DO_mgL, y = Depth_m), col = "blue", size = 1.2)+
  xlab("Dissolved oxygen (milligrams per liter)")+
  ylab("Depth (m)")+
  scale_y_reverse(breaks = c(11,10,9,8,7,6,5,4,3,2,1,0.1))+
  mytheme
bvr_do

bvr_chla <- ggplot(data = beaverdam)+
  geom_path(aes(x = Chla_ugL, y = Depth_m), col = "darkgreen", size = 1.2)+
  xlab("Chlorophyll a (micrograms per liter)")+
  ylab("Depth (m)")+
  scale_y_reverse(breaks = c(11,10,9,8,7,6,5,4,3,2,1,0.1))+
  mytheme
bvr_chla

bvr <- plot_grid(bvr_do, bvr_chla,   
                 ncol=2, nrow=1, align="hv")

title2 <- ggdraw() + 
  draw_label("Beaverdam",
             fontface = 'bold',
             size = 16)
bvr2 <- plot_grid(title2, bvr, ncol = 1, rel_heights = c(0.1, 1)) # rel_heights values control title margins
bvr2

ggsave(plot=bvr2, file="./WVWA plots/bvr_090718.png",
       h=8, w=16, units="in", dpi=300,scale = 1)

}

