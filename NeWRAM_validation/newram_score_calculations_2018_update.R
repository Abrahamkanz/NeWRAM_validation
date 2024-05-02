
library(tidyverse)
library(janitor)



v1_update <- Tabulat_study_w4_TableToExcel %>% 
  select(-OBJECTID) %>% 
  gather(key = "code",value = "cover",1:15) %>% 
  mutate(`Land Cover Class`=code) %>% 
  select(-code) %>% 
  merge(nlcd_2018_newram) %>% 
  select(-Score) %>% 
  spread(`Land Cover Class`,cover) %>%
  clean_names() %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         v1=((open_water/tot_cover)*10+(deciduous_forest/tot_cover)*10+(mixed_forest/tot_cover)*10+
               (shrub_scrub/tot_cover)*10+(grassland_herbaceous/tot_cover)*10+
               (pasture_hay/tot_cover)*9+(cultivated_crops/tot_cover)*3+
               (developed_open_space/tot_cover)*3+(developed_low_intensity/tot_cover)*2+
               (developed_medium_intensity/tot_cover)*1+(developed_high_intensity/tot_cover)*0)/10)






ldi_table_1k <- Tabulat_waypoint_2018_1k_TableToExcel %>% 
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
         ldi_1k=(open_water/tot_cover)*10+(deciduous_forest/tot_cover)*10+(mixed_forest/tot_cover)*10+
           (shrub_scrub/tot_cover)*10+(grassland_herbaceous/tot_cover)*10+(emergent_herbaceous_wetlands/tot_cover)*10+
           (pasture_hay/tot_cover)*9+(cultivated_crops/tot_cover)*3+
           (developed_open_space/tot_cover)*3+(developed_low_intensity/tot_cover)*2+
           (developed_medium_intensity/tot_cover)*1+(developed_high_intensity/tot_cover)*0) %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="sw","sw1",loc)) %>% 
  select(loc,ldi_1k) 




ldi_table_1k %>% 
  summarise(mean=mean(ldi_1k),
            median=median(ldi_1k),
            min=min(ldi_1k),
            max=max(ldi_1k),
            se=sd(ldi_1k)/sqrt(79))


test <- ldi_table_1k %>% 
  merge(veg_complex)



ldi_table_2k <- Tabulat_waypoint_2018_2k_TableToExcel %>% 
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
         ldi_2k=(open_water/tot_cover)*10+(deciduous_forest/tot_cover)*10+(mixed_forest/tot_cover)*10+
           (shrub_scrub/tot_cover)*10+(grassland_herbaceous/tot_cover)*10+(emergent_herbaceous_wetlands/tot_cover)*10+
           (pasture_hay/tot_cover)*9+(cultivated_crops/tot_cover)*3+
           (developed_open_space/tot_cover)*3+(developed_low_intensity/tot_cover)*2+
           (developed_medium_intensity/tot_cover)*1+(developed_high_intensity/tot_cover)*0) %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="sw","sw1",loc)) %>% 
  select(loc,ldi_2k) 



test <- Tabulat_waypoint_2018_2k_TableToExcel %>% 
  select(-OBJECTID) %>% 
  gather(key = "code",value = "cover",2:15) %>% 
  mutate(`Land Cover Class`=code) %>% 
  select(-code) %>% 
  merge(nlcd_2018_newram) %>% 
  merge(waypoint_corners_XYTableToPoint7_PointsToLine5_TableToExcel) %>% 
  mutate(class=`Land Cover Class`) %>% 
  select(-Shape_Length,-`Land Cover Class`) 


ldi_table_2k %>% 
  summarise(mean=mean(ldi_2k),
            median=median(ldi_2k),
            min=min(ldi_2k),
            max=max(ldi_2k),
            se=sd(ldi_2k)/sqrt(79))



test <- ldi_table_2k %>% 
  merge(veg_complex)



ldi_table_500m <- Tabulat_waypoint_2018_500m_TableToExcel %>% 
  select(-ORIG_FID) %>% 
  gather(key = "code",value = "cover",2:13) %>% 
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
         ldi_500m=(open_water/tot_cover)*10+(deciduous_forest/tot_cover)*10+
           (shrub_scrub/tot_cover)*10+(grassland_herbaceous/tot_cover)*10+(emergent_herbaceous_wetlands/tot_cover)*10+
           (pasture_hay/tot_cover)*9+(cultivated_crops/tot_cover)*3+
           (developed_open_space/tot_cover)*3+(developed_low_intensity/tot_cover)*2+
           (developed_medium_intensity/tot_cover)*1+(developed_high_intensity/tot_cover)*0) %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="sw","sw1",loc)) %>% 
  select(loc,ldi_500m)



test <- Tabulat_waypoint_2018_500m_TableToExcel %>% 
  select(-ORIG_FID) %>% 
  gather(key = "code",value = "cover",2:13) %>% 
  mutate(`Land Cover Class`=code) %>% 
  select(-code) %>% 
  merge(nlcd_2018_newram) %>% 
  merge(waypoint_corners_XYTableToPoint7_PointsToLine5_TableToExcel) %>% 
  mutate(class=`Land Cover Class`) %>% 
  select(-Shape_Length,-`Land Cover Class`) %>% 
  group_by(class,loc) %>% 
  reframe(cover=sum(cover)) %>% 
  spread(class,cover) %>%
  clean_names()



ldi_table_500m %>% 
  summarise(mean=mean(ldi_500m),
            median=median(ldi_500m),
            min=min(ldi_500m),
            max=max(ldi_500m),
            se=sd(ldi_500m)/sqrt(79))


###########################################################New V2
#All of HUC-8 is impacted, all receive a score of 0?


Tabulat_study_w4_TableToExcel %>% 
  select(-OBJECTID) %>% 
  gather(key = "code",value = "cover",1:15) %>% 
  mutate(`Land Cover Class`=code) %>% 
  select(-code) %>% 
  merge(nlcd_2018_newram) %>% 
  select(-Score) %>% 
  spread(`Land Cover Class`,cover) %>%
  clean_names() %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         v2=((cultivated_crops/tot_cover)+
               (developed_open_space/tot_cover)+(developed_low_intensity/tot_cover)+
               (developed_medium_intensity/tot_cover)+(developed_high_intensity/tot_cover)))




############################################################New V3 Calculation

newram_all_update <- newram_all %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc))


average_area_v3 <- buffer_100m_tabulate_TableToExcel %>% 
  merge(waypoint_corners_XYTableToPoint7_PointsToLine5_TableToExcel) %>% 
  select(-Shape_Length,-OBJECTID,-ORIG_FID) %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         non_buffer=`Open Water`+`Developed, Low Intensity`+`Cultivate Crops`,
         perc_non=non_buffer/tot_cover,
         perc_buffer=1-perc_non) %>% 
  select(loc,perc_buffer) %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="sw","sw1",loc))



v3_update <- v3_scores %>% 
  select(loc,v3) %>% 
  mutate(continuity = 1.0) %>% 
  select(-v3) %>% 
  merge(newram_all_update) %>% 
  merge(average_area_v3) %>% 
  mutate(v3=(continuity+inv_buffer+perc_buffer)/3) %>% 
  select(loc,v3)


range(v3_update$v3)

final_scores_update %>% 
  summarise(mean=mean(v3),
            se=sd(v3)/sqrt(78))



############################################################New V4 Calculation
#This variable was previously V5



v4_update_north <- v5_calc_north %>%
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(veg_summaries_new) %>% 
  merge(data_doc_mod) %>% 
  mutate(v4_north=v5_indv) %>% 
  select(loc,v4_north)


range(final_scores_update$v4_north)

final_scores_update %>% 
  summarise(mean=mean(v4_north),
            se=sd(v4_north)/sqrt(78))


v5_scores_mod2 %>% 
  summarise(mean=mean(v5_score),
            se=sd(v5_score)/sqrt(79))

v4_update_north %>% 
  summarise(mean=mean(v4_north),
            se=sd(v4_north)/sqrt(79))

v4_update_custom <- v5_scores_mod2 %>%
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(veg_summaries_new) %>% 
  merge(data_doc_mod) %>% 
  mutate(v4=v5_score) %>% 
  select(loc,v4)

range(v4_update_custom$v4)

############################################################New V5 Calculation
#This variable was previously V6


v5_scores_complex <- newram_all %>% 
  select(loc,hydro) %>% 
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(complexes) %>% 
  group_by(complex) %>% 
  reframe(v5=mean(hydro)) %>% 
  merge(veg_complex) %>% 
  merge(ldi_table_1k)


v5_scores_complex %>%
  filter(complex!="sw1") %>% 
  reframe(mean=mean(v5),
          se=sd(v5)/79,
          var=var(v5),
          max=max(v5),
          min=min(v5))

v5_scores_update <- newram_all %>% 
  select(loc,hydro) %>% 
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="phm1","phm",loc)) %>%  
  mutate(v5=hydro) %>% 
  select(-hydro) %>%  
  merge(ldi_sites_500m) %>% 
  merge(ldi_sites_1k) %>% 
  merge(ldi_sites_2k) %>% 
  select(loc,v5)


range(final_scores_update$v5)

final_scores_update %>% 
  summarise(mean=mean(v5),
            se=sd(v5)/sqrt(78))



############################################################New V6 Calculation
#This variable was previously V7



v6_scores_complex <- newram_all %>%
  select(loc,wet_use) %>% 
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(complexes) %>% 
  group_by(complex) %>% 
  reframe(v6=mean(wet_use)) %>% 
  merge(veg_complex) %>% 
  merge(ldi_table_1k)


v6_scores_complex %>%
  filter(complex!="sw1") %>% 
  reframe(mean=mean(v6),
          se=sd(v6)/sqrt(78),
          var=var(v6),
          max=max(v6),
          min=min(v6))


v6_scores_update <- newram_all %>%
  select(loc,wet_use) %>% 
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="phm1","phm",loc)) %>% 
  merge(veg_summaries_new) %>% 
  merge(data_doc_mod) %>% 
  mutate(v6=wet_use) %>% 
  select(-wet_use) %>%  
  merge(ldi_table_500m) %>% 
  merge(ldi_table_1k) %>% 
  merge(ldi_table_2k)



range(final_scores_update$v6)

final_scores_update %>% 
  summarise(mean=mean(v6),
            se=sd(v6)/78)



############################################################New Final Calculation


final_scores_update <- v3_update %>%
  merge(v4_update_north) %>% 
  merge(v5_scores_update) %>% 
  merge(v6_scores_update) %>% 
  filter(loc!="sw1") %>% 
  mutate(v1=0.67,
         v2=0.56,
         final=v1+1.5*v2+v3+v4_north+2*v5+v6,
         wis_cat_update=ifelse(wis_categories== "wet","wet","dry"))