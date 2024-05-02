library(tidyverse)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(grid)
library(egg)


#NeWRAM figures

newram_scores_dist <- ggplot(final_scores_update,aes(x=final))+
  stat_bin(bins = 20, binwidth = 0.25, boundary = 0, colour="black", linewidth=1,fill="grey")+
  theme_bw()+
  scale_y_continuous(breaks=seq(0,40,by=5))+
  scale_x_continuous(limits=c(0,7.5),breaks=seq(0,7.5,by=0.5))+
  theme_classic()+
  xlab("NeWRAM Final Score")+
  ylab("Frequency")+
  ylim(0,80)+
  theme(axis.text.x=element_text(size=40,angle = 45, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=40),
        axis.title.x=element_text(size=40,vjust = -1,margin = margin(t = 40)),
        axis.title.y = element_text(size=40),
        text = element_text(size = 40),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.ticks.length=unit(.25, "cm"))


newram_scores_dist

ggsave(newram_scores_dist, file="newram_scores_dist.jpg", dpi=800, width=25, height=15, units="in")



v1_freq <- ggplot(final_scores_update,aes(x=v1))+
  stat_bin(bins = 20, binwidth = 0.10, boundary = 0, colour="black", linewidth=1,fill="grey")+
  theme_bw()+
  # scale_y_continuous(breaks=seq(0,20,by=2))+
  scale_x_continuous(limits=c(0,1),breaks=seq(0,1,by=0.10))+
  theme_classic()+
  xlab("NeWRAM Score - V1")+
  ylab("Frequency")+
  ylim(0,80)+
  theme(axis.text.x=element_text(size=40,angle = 45, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=40),
        axis.title.x=element_text(size=40,vjust = -1,margin = margin(t = 40)),
        axis.title.y = element_text(size=40),
        text = element_text(size = 40),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.ticks.length=unit(.25, "cm"))


v1_freq

ggsave(v1_freq, file="v1_freq.jpg", dpi=800, width=25, height=15, units="in")





v3_freq <- ggplot(final_scores_update,aes(x=v3))+
  stat_bin(bins = 20, binwidth = 0.10, boundary = 0, colour="black", linewidth=1,fill="grey")+
  theme_bw()+
  scale_x_continuous(limits=c(0,1),breaks=seq(0,1,by=0.10))+
  theme_classic()+
  xlab("NeWRAM Score - V3")+
  ylab("Frequency")+
  ylim(0,80)+
  theme(axis.text.x=element_text(size=40,angle = 45, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=40),
        axis.title.x=element_text(size=40,vjust = -1,margin = margin(t = 40)),
        axis.title.y = element_text(size=40),
        text = element_text(size = 40),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.ticks.length=unit(.25, "cm"))

v3_freq

ggsave(v3_freq, file="v3_freq.jpg", dpi=800, width=25, height=15, units="in")







v4_freq <- ggplot(final_scores_update,aes(x=v4_north))+
  stat_bin(bins = 20, binwidth = 0.10, boundary = 0, colour="black", linewidth=1,fill="grey")+
  theme_bw()+
  #scale_y_continuous(breaks=seq(0,30,by=2))+
  scale_x_continuous(limits=c(0,1),breaks=seq(0,1,by=0.10))+
  theme_classic()+
  xlab("NeWRAM Score - V4")+
  ylab("Frequency")+
  ylim(0,80)+
  theme(axis.text.x=element_text(size=40,angle = 45, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=40),
        axis.title.x=element_text(size=40,vjust = -1,margin = margin(t = 40)),
        axis.title.y = element_text(size=40),
        text = element_text(size = 40),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.ticks.length=unit(.25, "cm"))

v4_freq

ggsave(v4_freq, file="v4_freq.jpg", dpi=800, width=25, height=15, units="in")







v5_freq <- ggplot(final_scores_update,aes(x=v5))+
  stat_bin(bins = 20, binwidth = 0.10, boundary = 0, colour="black", linewidth=1,fill="grey")+
  theme_bw()+
  #scale_y_continuous(breaks=seq(0,30,by=2))+
  scale_x_continuous(limits=c(0,1),breaks=seq(0,1,by=0.10))+
  theme_classic()+
  xlab("NeWRAM Score - V5")+
  ylab("Frequency")+
  ylim(0,80)+
  theme(axis.text.x=element_text(size=40,angle = 45, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=40),
        axis.title.x=element_text(size=40,vjust = -1,margin = margin(t = 40)),
        axis.title.y = element_text(size=40),
        text = element_text(size = 40),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.ticks.length=unit(.25, "cm"))

v5_freq

ggsave(v5_freq, file="v5_freq.jpg", dpi=800, width=25, height=15, units="in")



v6_freq <- ggplot(final_scores_update,aes(x=v6))+
  stat_bin(bins = 20, binwidth = 0.10, boundary = 0, colour="black", linewidth=1,fill="grey")+
  theme_bw()+
  scale_x_continuous(limits=c(0,1),breaks=seq(0,1,by=0.10))+
  theme_classic()+
  xlab("NeWRAM Score - V6")+
  ylab("Frequency")+
  ylim(0,80)+
  theme(axis.text.x=element_text(size=40,angle = 45, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=40),
        axis.title.x=element_text(size=40,vjust = -1,margin = margin(t = 40)),
        axis.title.y = element_text(size=40),
        text = element_text(size = 40),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.ticks.length=unit(.25, "cm"))

v6_freq

ggsave(v6_freq, file="v6_freq.jpg", dpi=800, width=25, height=15, units="in")






figure <- ggarrange(v1_freq + rremove("ylab"), v3_freq + rremove("ylab"),
                    v4_freq + rremove("ylab"), v5_freq+ rremove("ylab"),
                    v6_freq+ rremove("ylab"),v7_freq+ rremove("ylab"),# remove axis labels from plots
                    labels = NULL,
                    nrow = 3,ncol = 2,
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

scores_freq <- annotate_figure(figure, left = textGrob("Frequency", rot = 90, vjust = 1, gp = gpar(cex = 4)))

ggsave(scores_freq, file="scores_freq.jpg", dpi=800, width=25, height=30, units="in")


##########################################################Plotting modifications

score24_c <- ggplot(data_doc_scores_add,aes(x=c,y=score24))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  ylab("NeWRAM Score - Modification 8")+
  xlab("Average C-score")+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))



score24_fqi <- ggplot(data_doc_scores_add,aes(x=fqi,y=score24))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  ylab("NeWRAM Score - Modification 8")+
  xlab("FQI")+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))


score24_native <- ggplot(data_doc_scores_add,aes(x=p_native,y=score24))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  ylab("NeWRAM Score - Modification 8")+
  xlab("Native Coverage")+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))



score24_exotic <- ggplot(data_doc_scores_add,aes(x=p_exotic,y=score24))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  ylab("NeWRAM Score - Modification 8")+
  xlab("Exotic Coverage")+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))



score24_sedge <- ggplot(data_doc_scores_add,aes(x=sedge,y=score24))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  ylab("NeWRAM Score - Modification 8")+
  xlab("Sedge Coverage")+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))


test <- lm(c~score24,data_doc_scores_add)
summary(test)




score24_grid <- plot_grid(tag_facet(score24_fqi +
                                      theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.y = element_blank(),
                                            plot.margin = margin(r = 1, l = 1) ) +
                                      facet_wrap(~"FQI"),
                                    tag_pool = "A"), 
                          tag_facet(score24_c + 
                                      theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.y = element_blank(),
                                            plot.margin = margin(r = 1, l = 1) ) +
                                      facet_wrap(~"C-score"), 
                                    tag_pool = "B" ), 
                          tag_facet(score24_native + 
                                      theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.y = element_blank(),
                                            plot.margin = margin(l = 1)  ) +
                                      facet_wrap(~"Native Coverage"),
                                    tag_pool = "C"),
                          tag_facet(score24_exotic + 
                                      theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.y = element_blank(),
                                            plot.margin = margin(l = 1)  ) +
                                      facet_wrap(~"Exotic Coverage"),
                                    tag_pool = "D"),
                          tag_facet(score24_sedge + 
                                      theme(axis.text.y = element_blank(),
                                            axis.ticks.y = element_blank(),
                                            axis.title.y = element_blank(),
                                            plot.margin = margin(l = 1)  ) +
                                      facet_wrap(~"Sedge Coverage"),
                                    tag_pool = "E"),
                          nrow = 3, align = "hv")


y_grob <- textGrob("NeWRAM Score - Modification 7", 
                   gp=gpar(fontface="bold", col="black", fontsize=50), rot=90)

score24_grid_labeled <- grid.arrange(arrangeGrob(score24_grid, left = y_grob))


ggsave(score24_grid_labeled, file="score24_grid_labeled.jpg", dpi=800, width=25, height=25, units="in")


#######################################NeWRAM scores


newram_v1 <- ggplot(data_doc_scores,aes(x=newram_cat,y=v1))+
  geom_boxplot(lwd=2)+
  theme_classic()+
  ylab("V1")+
  xlab("NeWRAM Score")+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))

newram_v1

newram_v3 <- ggplot(data_doc_scores,aes(x=new_var,y=v3))+
  geom_boxplot(lwd=2)+
  theme_classic()+
  ylab("V3")+
  xlab("NeWRAM Score")+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))+
  scale_x_discrete(limits = c("Poor", "Fair", "Good"))


newram_v4 <- ggplot(data_doc_scores,aes(x=new_var,y=v4))+
  geom_boxplot(lwd=2)+
  theme_classic()+
  ylab("V4")+
  xlab("NeWRAM Score")+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))+
  scale_x_discrete(limits = c("Poor", "Fair", "Good"))



newram_v5 <- ggplot(data_doc_scores,aes(x=new_var,y=v5))+
  geom_boxplot(lwd=2)+
  theme_classic()+
  ylab("V5")+
  xlab("NeWRAM Score")+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))+
  scale_x_discrete(limits = c("Poor", "Fair", "Good"))


newram_v6 <- ggplot(data_doc_scores,aes(x=new_var,y=v6))+
  geom_boxplot(lwd=2)+
  theme_classic()+
  ylab("V6")+
  xlab("NeWRAM Score")+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))+
  scale_x_discrete(limits = c("Poor", "Fair", "Good"))



newram_v7 <- ggplot(data_doc_scores,aes(x=new_var,y=v7))+
  geom_boxplot(lwd=2)+
  theme_classic()+
  ylab("V7")+
  xlab("NeWRAM Score")+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))+
  scale_x_discrete(limits = c("Poor", "Fair", "Good"))



newram_grid <- plot_grid(tag_facet(newram_v1 + 
                                     theme(axis.title.x = element_blank(),
                                           plot.margin = margin(l = 1)  ) ,
                                   tag_pool = "A"), 
                         tag_facet(newram_v3 + 
                                     theme(axis.title.x = element_blank(),
                                           plot.margin = margin(l = 1)  ) ,
                                   tag_pool = "B" ), 
                         tag_facet(newram_v4 + 
                                     theme(axis.title.x = element_blank(),
                                           plot.margin = margin(l = 1)  ) ,
                                   tag_pool = "C"),
                         tag_facet(newram_v5 + 
                                     theme(axis.title.x = element_blank(),
                                           plot.margin = margin(l = 1)  ) ,
                                   tag_pool = "D"),
                         tag_facet(newram_v6 + 
                                     theme(axis.title.x = element_blank(),
                                           plot.margin = margin(l = 1)  ) ,
                                   tag_pool = "E"),
                         tag_facet(newram_v7 + 
                                     theme(axis.title.x = element_blank(),
                                           plot.margin = margin(l = 1)  ) ,
                                   tag_pool = "F"),
                         nrow = 3, align = "hv")



x_grob <- textGrob("NeWRAM Score", 
                   gp=gpar(fontface="bold", col="black", fontsize=50))

newram_grid_labeled <- grid.arrange(arrangeGrob(newram_grid, bottom = x_grob))


ggsave(newram_grid_labeled, file="newram_grid_labeled.jpg", dpi=800, width=25, height=25, units="in")



###########################################################################

ggplot(data=veg_summaries_new,aes(x=fqi,y=c))+
  geom_smooth(method = "lm")+
  geom_point()


ldi_table_1k %>% 
  merge(ldi_table_2k)


ldi_500m_final <- ggplot(data=final_scores_update,aes(x=ldi_500m,y=final,color=wis_cat_update))+
  geom_point(size=5)+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))

ggsave(ldi_500m_final, file="ldi_500m_final.jpg", dpi=600, width=25, height=15, units="in")



ldi_1k_final <- ggplot(data=final_scores_update,aes(x=ldi_1k,y=final,color=wis_cat_update))+
  geom_point(size=5)+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))

ggsave(ldi_1k_final, file="ldi_1k_final.jpg", dpi=600, width=25, height=15, units="in")



ldi_2k_final <- ggplot(data=final_scores_update,aes(x=ldi_2k,y=final,color=wis_cat_update))+
  geom_point(size=5)+
  theme(axis.text.x=element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.title.x=element_text(size=50),
        axis.title.y = element_text(size=50),
        text = element_text(size = 50))


ggsave(ldi_2k_final, file="ldi_2k_final.jpg", dpi=600, width=25, height=15, units="in")

