library(tidyverse)
library(janitor)

v1_non_riverine <- Tabulat_waypoin_1mile_new_TableToExcel %>% 
  select(-ORIG_FID) %>% 
  gather(key = "code",value = "cover",2:15) %>% 
  mutate(`Land Cover Class`=code) %>% 
  select(-code) %>% 
  merge(nlcd_2018_newram) %>% 
  merge(waypoint_corners_XYTableToPoint7_PointsToLine5_TableToExcel) %>% 
  mutate(class=`Land Cover Class`) %>% 
  select(-Shape_Length,-`Land Cover Class`) %>% 
  group_by(class,loc) %>% 
  reframe(cover=sum(cover)) %>% 
  spread(class,cover) %>%
  clean_names() %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         v1=((open_water/tot_cover)*10+(deciduous_forest/tot_cover)*10+
               (shrub_scrub/tot_cover)*10+(grassland_herbaceous/tot_cover)*10+(emergent_herbaceous_wetlands/tot_cover)*10+
               (pasture_hay/tot_cover)*9+(cultivated_crops/tot_cover)*3+
               (developed_open_space/tot_cover)*3+(developed_low_intensity/tot_cover)*2+
               (developed_medium_intensity/tot_cover)*1+(developed_high_intensity/tot_cover)*0)/10) %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="sw","sw1",loc)) %>% 
  select(loc,v1) 




v2_non_riverine <- Tabulat_waypoin_1mile_new_TableToExcel %>% 
  select(-ORIG_FID) %>% 
  gather(key = "code",value = "cover",2:15) %>% 
  mutate(`Land Cover Class`=code) %>% 
  select(-code) %>% 
  merge(nlcd_2018_newram) %>% 
  merge(waypoint_corners_XYTableToPoint7_PointsToLine5_TableToExcel) %>% 
  mutate(class=`Land Cover Class`) %>% 
  select(-Shape_Length,-`Land Cover Class`) %>% 
  group_by(class,loc) %>% 
  reframe(cover=sum(cover)) %>% 
  spread(class,cover) %>%
  clean_names() %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         ldi_1mile_prop_mod=((cultivated_crops/tot_cover)+
                               (developed_open_space/tot_cover)+(developed_low_intensity/tot_cover)+
                               (developed_medium_intensity/tot_cover)+(developed_high_intensity/tot_cover))) %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="sw","sw1",loc)) %>% 
  select(loc,ldi_1mile_prop_mod) %>% 
  mutate(v2=1-ldi_1mile_prop_mod) %>% 
  select(-ldi_1mile_prop_mod)





final_scores_non_riverine <- v1_non_riverine %>% 
  merge(v2_non_riverine) %>% 
  merge(v3_update) %>%
  merge(v4_update_north) %>% 
  merge(v5_scores_update) %>% 
  merge(v6_scores_update) %>% 
  filter(loc!="sw1") %>% 
  mutate(final=v1+1.5*v2+v3+v4_north+2*v5+v6,
         wis_cat_update=ifelse(wis_categories== "wet","wet","dry"))




##############################################################Correlations



######################################################final

cor.test(final_scores_non_riverine$final,final_scores_non_riverine$fqi, method = 'kendall')

cor.test(final_scores_non_riverine$final,final_scores_non_riverine$relative_native, method = 'kendall')

cor.test(final_scores_non_riverine$final,final_scores_non_riverine$relative_exotic, method = 'kendall')

cor.test(final_scores_non_riverine$final,final_scores_non_riverine$wwis, method = 'kendall')

cor.test(final_scores_non_riverine$final,final_scores_non_riverine$nh4_mean, method = 'kendall')

cor.test(final_scores_non_riverine$final,final_scores_non_riverine$top_nmean, method = 'kendall')

cor.test(final_scores_non_riverine$final,final_scores_non_riverine$om_mean, method = 'kendall')

cor.test(final_scores_non_riverine$final,final_scores_non_riverine$bulk_dens_mean, method = 'kendall')

cor.test(final_scores_non_riverine$final,final_scores_non_riverine$spring_depth21_cm_mean, method = 'kendall')

cor.test(final_scores_non_riverine$final,final_scores_non_riverine$spring_depth22_cm_mean, method = 'kendall')

cor.test(final_scores_non_riverine$final,final_scores_non_riverine$top_nmean, method = 'kendall')



cor.test(final_scores_non_riverine$final,final_scores_non_riverine$ldi_500m, method = 'kendall')
cor.test(final_scores_non_riverine$final,final_scores_non_riverine$ldi_1k, method = 'kendall')
cor.test(final_scores_non_riverine$final,final_scores_non_riverine$ldi_2k, method = 'kendall')



######################################################V1

cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$fqi, method = 'kendall')

cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$relative_native, method = 'kendall')

cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$relative_exotic, method = 'kendall')

cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$wwis, method = 'kendall')

cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$nh4_mean, method = 'kendall')

cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$top_nmean, method = 'kendall')

cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$om_mean, method = 'kendall')

cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$bulk_dens_mean, method = 'kendall')

cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$spring_depth21_cm_mean, method = 'kendall')

cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$spring_depth22_cm_mean, method = 'kendall')

cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$top_nmean, method = 'kendall')



cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$ldi_500m, method = 'kendall')
cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$ldi_1k, method = 'kendall')
cor.test(final_scores_non_riverine$v1,final_scores_non_riverine$ldi_2k, method = 'kendall')


######################################################V2

cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$fqi, method = 'kendall')

cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$relative_native, method = 'kendall')

cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$relative_exotic, method = 'kendall')

cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$wwis, method = 'kendall')

cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$nh4_mean, method = 'kendall')

cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$top_nmean, method = 'kendall')

cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$om_mean, method = 'kendall')

cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$bulk_dens_mean, method = 'kendall')

cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$spring_depth22_cm_mean, method = 'kendall')

cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$spring_depth22_cm_mean, method = 'kendall')

cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$top_nmean, method = 'kendall')


cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$ldi_500m, method = 'kendall')
cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$ldi_1k, method = 'kendall')
cor.test(final_scores_non_riverine$v2,final_scores_non_riverine$ldi_2k, method = 'kendall')