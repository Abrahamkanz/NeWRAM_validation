
library(tidyverse)
library(janitor)



##################################################################################################
#Proper Areas

buffer_area_categories <- waypoint_buff_tabulate_transfer %>% 
  clean_names() %>% 
  mutate(open_water = value_11,
         developed_open_space = value_21,
         developed_low_intensity = value_22,
         developed_medium_intensity = value_23,
         developed_high_intensity = value_24,
         barren_land = value_31,
         deciduous_forest = value_41,
         shrub_scrub = value_52,
         herbaceous = value_71,
         hay_pasture = value_81,
         cultivated_crops = value_82,
         woody_wetlands = value_90,
         emergent_herbaceous_wetlands = value_95) %>% 
  select(2,16:28) %>% 
  mutate(developed_low_intensity_new = developed_open_space+developed_low_intensity,
         natural = deciduous_forest+barren_land+shrub_scrub+
           herbaceous+woody_wetlands+emergent_herbaceous_wetlands,
         pasture = hay_pasture,
         row_crops = cultivated_crops,
         water = open_water) %>% 
  select(orig_fid,developed_low_intensity_new,developed_medium_intensity,developed_high_intensity,
         natural, pasture, row_crops,water) %>% 
  mutate(tot = as.numeric(rowSums(.,2:8)))



######################################################################################
#NeWRAM Scores

v1_scores <- study_area_tabulate_evt %>% 
  select(-OBJECTID,-TNMID) %>% 
  gather(key = "code",value = "cover") %>% 
  separate(code,into = c("text","VALUE")) %>% 
  select(-text) %>% 
  merge(LF20_EVT_220,by="VALUE") %>% 
  mutate(EVT_PHYS=ifelse(VALUE==7967,"Pasture",EVT_PHYS)) %>% 
  group_by(EVT_PHYS) %>% 
  reframe(cover=sum(cover)) %>% 
  ungroup() %>% 
  spread(EVT_PHYS,cover) %>% 
  mutate(natural=`Conifer`+`Conifer-Hardwood`+`Exotic Herbaceous`+`Exotic Tree-Shrub`+
           `Grassland`+`Hardwood`+`Riparian`+`Shrubland`+`Sparsely Vegetated`,
         row_crops=`Agricultural`,
         pasture=`Pasture`,
         natural_managed=`Developed`,
         developed_low=`Developed-Low Intensity`,
         developed_medium=`Developed-Medium Intensity`,
         developed_high=`Developed-High Intensity`,
         mines=`Quarries-Strip Mines-Gravel Pits-Well and Wind Pads`,
         water=`Open Water`,
         roads=`Developed-Roads`) %>% 
  select(natural,row_crops,pasture,natural_managed,developed_low,developed_medium,
         developed_high,mines,roads,water) %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         v1=((water/tot_cover)*10+(natural/tot_cover)*10+(pasture/tot_cover)*6.59+(row_crops/tot_cover)*3+
               (developed_low/tot_cover)*2.53+(developed_medium/tot_cover)*2.45+(developed_high/tot_cover)*0.58+
               (roads/tot_cover)*1.95+(mines/tot_cover)*1.68)/10)


v1_scores_complex <- veg_complex %>%
  mutate(v1=0.661)


v1_score_sherman <- Tabulat_WBDHU81_TableToExcel %>% 
  select(-OBJECTID,-TNMID) %>% 
  gather(key = "code",value = "cover") %>% 
  separate(code,into = c("text","VALUE")) %>% 
  select(-text) %>% 
  merge(LF20_EVT_220,by="VALUE") %>% 
  mutate(EVT_PHYS=ifelse(VALUE==7967,"Pasture",EVT_PHYS)) %>% 
  group_by(EVT_PHYS) %>% 
  reframe(cover=sum(cover)) %>% 
  ungroup() %>% 
  spread(EVT_PHYS,cover) %>% 
  mutate(natural=`Conifer-Hardwood`+`Exotic Herbaceous`+`Exotic Tree-Shrub`+
           `Grassland`+`Hardwood`+`Riparian`+`Shrubland`+`Sparsely Vegetated`,
         row_crops=`Agricultural`,
         pasture=`Pasture`,
         natural_managed=`Developed`,
         developed_low=`Developed-Low Intensity`,
         developed_medium=`Developed-Medium Intensity`,
         developed_high=`Developed-High Intensity`,
         mines=`Quarries-Strip Mines-Gravel Pits-Well and Wind Pads`,
         water=`Open Water`,
         roads=`Developed-Roads`) %>% 
  select(natural,row_crops,pasture,natural_managed,developed_low,developed_medium,
         developed_high,mines,roads,water) %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         v1=((water/tot_cover)*10+(natural/tot_cover)*10+(pasture/tot_cover)*6.59+(row_crops/tot_cover)*3+
               (developed_low/tot_cover)*2.53+(developed_medium/tot_cover)*2.45+(developed_high/tot_cover)*0.58+
               (roads/tot_cover)*1.95+(mines/tot_cover)*1.68)/10)



v1_scores_sites <- Tabulat_waypoint_1k %>% 
  merge(waypoint_corners_XYTableToPoint7_PointsToLine5_TableToExcel) %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="sw","sw1",loc)) %>% 
  select(-OBJECTID,-ORIG_FID,-Shape_Length) %>%  
  gather(key = "code",value = "cover",1:41) %>% 
  separate(code,into = c("text","VALUE")) %>% 
  select(-text) %>% 
  merge(LF20_EVT_220,by="VALUE") %>% 
  mutate(EVT_PHYS=ifelse(VALUE==7967,"Pasture",EVT_PHYS)) %>% 
  group_by(EVT_PHYS,loc) %>% 
  reframe(cover=sum(cover)) %>% 
  ungroup() %>% 
  spread(EVT_PHYS,cover) %>% 
  mutate(natural=`Conifer-Hardwood`+ `Exotic Herbaceous` +`Exotic Tree-Shrub`+
           `Grassland`+`Hardwood`+`Riparian`+`Shrubland`+`Sparsely Vegetated`,
         row_crops=`Agricultural`,
         pasture=`Pasture`,
         natural_managed=`Developed`,
         developed_low=`Developed-Low Intensity`,
         developed_medium=`Developed-Medium Intensity`,
         developed_high=`Developed-High Intensity`,
         mines=`Quarries-Strip Mines-Gravel Pits-Well and Wind Pads`,
         water=`Open Water`,
         roads=`Developed-Roads`) %>% 
  select(loc,natural,row_crops,pasture,natural_managed,developed_low,developed_medium,
         developed_high,mines,roads,water) %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         v1=((natural/tot_cover)*1.0+(row_crops/tot_cover)*7.00+(pasture/tot_cover)*3.41+(natural_managed/tot_cover)*4.54+
               (developed_low/tot_cover)*7.47+(developed_medium/tot_cover)*7.55+(developed_high/tot_cover)*9.42+
               (mines/tot_cover)*8.32+(water/tot_cover)*1.0+(roads/tot_cover)*8.05)/10) %>% 
  merge(veg_summaries_new) %>% 
  merge(data_doc_mod) %>%  
  merge(ldi_sites_500m) %>% 
  merge(ldi_sites_1k) %>% 
  merge(ldi_sites_2k)


ldi_table_1k <- Tabulat_complex1k_TableToExcel %>% 
  select(-OBJECTID) %>% 
  gather(key = "code",value = "cover",2:42) %>% 
  separate(code,into = c("text","VALUE")) %>% 
  select(-text) %>% 
  merge(LF20_EVT_220,by="VALUE") %>% 
  mutate(EVT_PHYS=ifelse(VALUE==7967,"Pasture",EVT_PHYS)) %>% 
  group_by(EVT_PHYS,COMPLEX) %>% 
  reframe(cover=sum(cover)) %>% 
  ungroup() %>% 
  spread(EVT_PHYS,cover) %>% 
  mutate(natural=`Conifer-Hardwood`+`Exotic Tree-Shrub`+
           `Grassland`+`Hardwood`+`Riparian`+`Shrubland`+`Sparsely Vegetated`,
         row_crops=`Agricultural`,
         pasture=`Pasture`,
         natural_managed=`Developed`,
         developed_low=`Developed-Low Intensity`,
         developed_medium=`Developed-Medium Intensity`,
         developed_high=`Developed-High Intensity`,
         mines=`Quarries-Strip Mines-Gravel Pits-Well and Wind Pads`,
         water=`Open Water`,
         roads=`Developed-Roads`,
         complex=COMPLEX) %>% 
  select(complex,natural,row_crops,pasture,natural_managed,developed_low,developed_medium,
         developed_high,mines,roads,water) %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         ldi_1k=((natural/tot_cover)*1.0+(row_crops/tot_cover)*7.00+(pasture/tot_cover)*3.41+(natural_managed/tot_cover)*4.54)+
           (developed_low/tot_cover)*7.47+(developed_medium/tot_cover)*7.55+(developed_high/tot_cover)*9.42+
           (mines/tot_cover)*8.32+(water/tot_cover)*1.0+(roads/tot_cover)*8.05) %>% 
  filter(complex!="mormon_se",
         complex!="mormon_sw")



ldi_table_1k %>% 
  summarise(mean=mean(ldi_1k),
            median=median(ldi_1k),
            min=min(ldi_1k),
            max=max(ldi_1k))


test <- ldi_table_1k %>% 
  merge(veg_complex)



ldi_table_2k <- Tabulat_complex2k_TableToExcel %>% 
  select(-OBJECTID) %>% 
  gather(key = "code",value = "cover",2:42) %>% 
  separate(code,into = c("text","VALUE")) %>% 
  select(-text) %>% 
  merge(LF20_EVT_220,by="VALUE") %>% 
  mutate(EVT_PHYS=ifelse(VALUE==7967,"Pasture",EVT_PHYS)) %>% 
  group_by(EVT_PHYS,COMPLEX) %>% 
  reframe(cover=sum(cover)) %>% 
  ungroup() %>% 
  spread(EVT_PHYS,cover) %>% 
  mutate(natural=`Conifer-Hardwood`+`Exotic Tree-Shrub`+
           `Grassland`+`Hardwood`+`Riparian`+`Shrubland`+`Sparsely Vegetated`,
         row_crops=`Agricultural`,
         pasture=`Pasture`,
         natural_managed=`Developed`,
         developed_low=`Developed-Low Intensity`,
         developed_medium=`Developed-Medium Intensity`,
         developed_high=`Developed-High Intensity`,
         mines=`Quarries-Strip Mines-Gravel Pits-Well and Wind Pads`,
         water=`Open Water`,
         roads=`Developed-Roads`,
         complex=COMPLEX) %>% 
  select(complex,natural,row_crops,pasture,natural_managed,developed_low,developed_medium,
         developed_high,mines,roads,water) %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         ldi_2k=((natural/tot_cover)*1.0+(row_crops/tot_cover)*7.00+(pasture/tot_cover)*3.41+(natural_managed/tot_cover)*4.54)+
           (developed_low/tot_cover)*7.47+(developed_medium/tot_cover)*7.55+(developed_high/tot_cover)*9.42+
           (mines/tot_cover)*8.32+(water/tot_cover)*1.0+(roads/tot_cover)*8.05) %>% 
  filter(complex!="mormon_se",
         complex!="mormon_sw")


ldi_table_2k %>% 
  summarise(mean=mean(ldi_2k),
            median=median(ldi_2k),
            min=min(ldi_2k),
            max=max(ldi_2k))



test <- ldi_table_2k %>% 
  merge(veg_complex)



ldi_table_500m <- Tabulat_complex500m_TableToExcel %>% 
  select(-OBJECTID) %>% 
  gather(key = "code",value = "cover",2:41) %>% 
  separate(code,into = c("text","VALUE")) %>% 
  select(-text) %>% 
  merge(LF20_EVT_220,by="VALUE") %>% 
  mutate(EVT_PHYS=ifelse(VALUE==7967,"Pasture",EVT_PHYS)) %>% 
  group_by(EVT_PHYS,COMPLEX) %>% 
  reframe(cover=sum(cover)) %>% 
  ungroup() %>% 
  spread(EVT_PHYS,cover) %>% 
  mutate(natural=`Conifer-Hardwood`+ `Exotic Herbaceous` +`Exotic Tree-Shrub`+
           `Grassland`+`Hardwood`+`Riparian`+`Shrubland`+`Sparsely Vegetated`,
         row_crops=`Agricultural`,
         pasture=`Pasture`,
         natural_managed=`Developed`,
         developed_low=`Developed-Low Intensity`,
         developed_medium=`Developed-Medium Intensity`,
         developed_high=`Developed-High Intensity`,
         mines=`Quarries-Strip Mines-Gravel Pits-Well and Wind Pads`,
         water=`Open Water`,
         roads=`Developed-Roads`,
         complex=COMPLEX) %>% 
  select(complex,natural,row_crops,pasture,natural_managed,developed_low,developed_medium,
         developed_high,mines,roads,water) %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         ldi_500m=((natural/tot_cover)*1.0+(row_crops/tot_cover)*7.00+(pasture/tot_cover)*3.41+(natural_managed/tot_cover)*4.54)+
           (developed_low/tot_cover)*7.47+(developed_medium/tot_cover)*7.55+(developed_high/tot_cover)*9.42+
           (mines/tot_cover)*8.32+(water/tot_cover)*1.0+(roads/tot_cover)*8.05) %>% 
  filter(complex!="mormon_se",
         complex!="mormon_sw")




ldi_table_500m %>% 
  summarise(mean=mean(ldi_500m),
            median=median(ldi_500m),
            min=min(ldi_500m),
            max=max(ldi_500m))


test <- ldi_table_500m %>% 
  merge(veg_complex)



#Individual sloughs

ldi_sites_1k <- Tabulat_waypoint_1k %>% 
  merge(waypoint_corners_XYTableToPoint7_PointsToLine5_TableToExcel) %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="sw","sw1",loc)) %>% 
  select(-OBJECTID,-ORIG_FID,-Shape_Length) %>%  
  gather(key = "code",value = "cover",1:41) %>% 
  separate(code,into = c("text","VALUE")) %>% 
  select(-text) %>% 
  merge(LF20_EVT_220,by="VALUE") %>% 
  mutate(EVT_PHYS=ifelse(VALUE==7967,"Pasture",EVT_PHYS)) %>% 
  group_by(EVT_PHYS,loc) %>% 
  reframe(cover=sum(cover)) %>% 
  ungroup() %>% 
  spread(EVT_PHYS,cover) %>% 
  mutate(natural=`Conifer-Hardwood`+ `Exotic Herbaceous` +`Exotic Tree-Shrub`+
           `Grassland`+`Hardwood`+`Riparian`+`Shrubland`+`Sparsely Vegetated`,
         row_crops=`Agricultural`,
         pasture=`Pasture`,
         natural_managed=`Developed`,
         developed_low=`Developed-Low Intensity`,
         developed_medium=`Developed-Medium Intensity`,
         developed_high=`Developed-High Intensity`,
         mines=`Quarries-Strip Mines-Gravel Pits-Well and Wind Pads`,
         water=`Open Water`,
         roads=`Developed-Roads`) %>% 
  select(loc,natural,row_crops,pasture,natural_managed,developed_low,developed_medium,
         developed_high,mines,roads,water) %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         ldi_1k=((natural/tot_cover)*1.0+(row_crops/tot_cover)*7.00+(pasture/tot_cover)*3.41+(natural_managed/tot_cover)*4.54)+
           (developed_low/tot_cover)*7.47+(developed_medium/tot_cover)*7.55+(developed_high/tot_cover)*9.42+
           (mines/tot_cover)*8.32+(water/tot_cover)*1.0+(roads/tot_cover)*8.05)  %>% 
  select(loc,ldi_1k)


ldi_sites_500m <- Tabulat_waypoint_500m %>% 
  merge(waypoint_corners_XYTableToPoint7_PointsToLine5_TableToExcel) %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="sw","sw1",loc)) %>% 
  select(-OBJECTID,-ORIG_FID,-Shape_Length) %>%  
  gather(key = "code",value = "cover",1:37) %>% 
  separate(code,into = c("text","VALUE")) %>% 
  select(-text) %>% 
  merge(LF20_EVT_220,by="VALUE") %>% 
  mutate(EVT_PHYS=ifelse(VALUE==7967,"Pasture",EVT_PHYS)) %>% 
  group_by(EVT_PHYS,loc) %>% 
  reframe(cover=sum(cover)) %>% 
  ungroup() %>% 
  spread(EVT_PHYS,cover) %>% 
  mutate(natural=`Conifer-Hardwood`+ `Exotic Herbaceous` +`Exotic Tree-Shrub`+
           `Grassland`+`Hardwood`+`Riparian`+`Shrubland`+`Sparsely Vegetated`,
         row_crops=`Agricultural`,
         pasture=`Pasture`,
         natural_managed=`Developed`,
         developed_low=`Developed-Low Intensity`,
         developed_medium=`Developed-Medium Intensity`,
         developed_high=`Developed-High Intensity`,
         mines=`Quarries-Strip Mines-Gravel Pits-Well and Wind Pads`,
         water=`Open Water`,
         roads=`Developed-Roads`) %>% 
  select(loc,natural,row_crops,pasture,natural_managed,developed_low,developed_medium,
         developed_high,mines,roads,water) %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         ldi_500m=((natural/tot_cover)*1.0+(row_crops/tot_cover)*7.00+(pasture/tot_cover)*3.41+(natural_managed/tot_cover)*4.54)+
           (developed_low/tot_cover)*7.47+(developed_medium/tot_cover)*7.55+(developed_high/tot_cover)*9.42+
           (mines/tot_cover)*8.32+(water/tot_cover)*1.0+(roads/tot_cover)*8.05) %>% 
  select(loc,ldi_500m)


ldi_sites_500m %>% 
  summarise(mean=mean(ldi_500m),
            median=median(ldi_500m),
            min=min(ldi_500m),
            max=max(ldi_500m))



ldi_sites_2k <- Tabulat_waypoint_2k %>% 
  merge(waypoint_corners_XYTableToPoint7_PointsToLine5_TableToExcel) %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="sw","sw1",loc)) %>% 
  select(-OBJECTID,-ORIG_FID,-Shape_Length) %>%  
  gather(key = "code",value = "cover",1:41) %>% 
  separate(code,into = c("text","VALUE")) %>% 
  select(-text) %>% 
  merge(LF20_EVT_220,by="VALUE") %>% 
  mutate(EVT_PHYS=ifelse(VALUE==7967,"Pasture",EVT_PHYS)) %>% 
  group_by(EVT_PHYS,loc) %>% 
  reframe(cover=sum(cover)) %>% 
  ungroup() %>% 
  spread(EVT_PHYS,cover) %>% 
  mutate(natural=`Conifer-Hardwood`+`Exotic Tree-Shrub`+
           `Grassland`+`Hardwood`+`Riparian`+`Shrubland`+`Sparsely Vegetated`,
         row_crops=`Agricultural`,
         pasture=`Pasture`,
         natural_managed=`Developed`,
         developed_low=`Developed-Low Intensity`,
         developed_medium=`Developed-Medium Intensity`,
         developed_high=`Developed-High Intensity`,
         mines=`Quarries-Strip Mines-Gravel Pits-Well and Wind Pads`,
         water=`Open Water`,
         roads=`Developed-Roads`) %>% 
  select(loc,natural,row_crops,pasture,natural_managed,developed_low,developed_medium,
         developed_high,mines,roads,water) %>% 
  mutate(tot_cover=rowSums(across(where(is.numeric)), na.rm=TRUE),
         ldi_2k=((natural/tot_cover)*1.0+(row_crops/tot_cover)*7.00+(pasture/tot_cover)*3.41+(natural_managed/tot_cover)*4.54)+
           (developed_low/tot_cover)*7.47+(developed_medium/tot_cover)*7.55+(developed_high/tot_cover)*9.42+
           (mines/tot_cover)*8.32+(water/tot_cover)*1.0+(roads/tot_cover)*8.05) %>% 
  select(loc,ldi_2k)

#############################################NeWRAM Variables 



v1_scores

write.csv(v1_scores, file = "v1_scores.csv")

v3_scores <- newram_all %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  select(loc,bcw) %>% 
  mutate(v3=bcw) %>% 
  select(-bcw) %>% 
  merge(veg_summaries_new) %>% 
  merge(data_doc_mod) %>%  
  merge(ldi_sites_500m) %>% 
  merge(ldi_sites_1k) %>% 
  merge(ldi_sites_2k)


v3_scores_complex <- v3_scores %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(complexes) %>% 
  group_by(complex) %>% 
  reframe(v3=mean(bcw),
          median=median(bcw),
          min=min(bcw),
          max=max(bcw)) %>% 
  merge(veg_complex) %>% 
  merge(ldi_table_1k)


v3_scores_complex %>% 
  filter(complex!="sw1") %>% 
  reframe(mean=mean(v3),
          se=sd(v3)/31,
          var=var(v3),
          min=min(min),
          max=max(max))


v4_scores <- newram_all %>%
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  select(loc,bci) %>% 
  mutate(v4=bci) %>% 
  select(-bci) %>% 
  merge(veg_summaries_new) %>% 
  merge(data_doc_mod) %>% 
  merge(ldi_sites_500m) %>% 
  merge(ldi_sites_1k) %>% 
  merge(ldi_sites_2k)


v4_scores_complex <- v4_scores %>% 
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc)) %>%
  merge(veg_summaries_new) %>% 
  filter(loc!="sw1") %>% 
  merge(complexes) %>% 
  group_by(complex) %>% 
  reframe(v4=mean(bci),
          median=median(bci),
          min=min(bci),
          max=max(bci),
          fqi=mean(fqi),
          c=mean(c)) %>% 
  merge(veg_complex) %>% 
  merge(ldi_table_1k)

v4_merge <- v4_scores_complex%>% 
  select(complex,v4)


v4_scores_complex %>%
  filter(complex!="sw1") %>% 
  reframe(mean=mean(v4),
          se=sd(v4)/31,
          var=var(v4),
          max=max(v4),
          min=min(v4))


v5_scores_complex <- v5_scores_mod2 %>%
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(complexes) %>% 
  group_by(complex) %>% 
  reframe(v5=mean(v5_score)) %>% 
  merge(veg_complex) %>% 
  merge(ldi_table_1k)


v5_scores_complex %>%
  filter(complex!="sw1") %>% 
  reframe(mean=mean(v5),
          se=sd(v5)/31,
          var=var(v5),
          max=max(v5),
          min=min(v5))


v5_scores_sites <- v5_scores_mod2 %>%
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(veg_summaries_new) %>% 
  merge(data_doc_mod) %>% 
  mutate(v5=v5_score) %>% 
  select(-v5_score) %>%  
  merge(ldi_sites_500m) %>% 
  merge(ldi_sites_1k) %>% 
  merge(ldi_sites_2k)




v5_scores_north_complex <- v5_scores_north %>%
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(complexes) %>% 
  group_by(complex) %>% 
  reframe(v5=mean(v5_score)) %>% 
  merge(veg_complex) %>% 
  merge(ldi_table_1k)


v5_scores_sites_north <- v5_scores_north %>%
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(veg_summaries_new) %>% 
  merge(data_doc_mod) %>% 
  mutate(v5_north=v5_score) %>% 
  select(-v5_score) %>%  
  merge(ldi_sites_500m) %>% 
  merge(ldi_sites_1k) %>% 
  merge(ldi_sites_2k)



v6_scores_complex <- newram_all %>% 
  select(loc,hydro) %>% 
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(complexes) %>% 
  group_by(complex) %>% 
  reframe(v6=mean(hydro)) %>% 
  merge(veg_complex) %>% 
  merge(ldi_table_1k)


v6_scores_complex %>%
  filter(complex!="sw1") %>% 
  reframe(mean=mean(v6),
          se=sd(v6)/31,
          var=var(v6),
          max=max(v6),
          min=min(v6))

v6_scores <- newram_all %>% 
  select(loc,hydro) %>% 
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="phm1","phm",loc)) %>%  
  merge(veg_summaries_new) %>% 
  merge(data_doc_mod) %>% 
  mutate(v6=hydro) %>% 
  select(-hydro) %>%  
  merge(ldi_sites_500m) %>% 
  merge(ldi_sites_1k) %>% 
  merge(ldi_sites_2k)

v5_scores_2018 <- v6_scores %>% 
  mutate(v6=ifelse(v6==0.75,0.7,v6),
         v6=ifelse(v6==0.5,0.3,v6),
         v6=ifelse(v6==0.25,0.1,v6))


v7_scores_complex <- newram_all %>%
  select(loc,wet_use) %>% 
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(complexes) %>% 
  group_by(complex) %>% 
  reframe(v7=mean(wet_use)) %>% 
  merge(veg_complex) %>% 
  merge(ldi_table_1k)


v7_scores_complex %>%
  filter(complex!="sw1") %>% 
  reframe(mean=mean(v7),
          se=sd(v7)/31,
          var=var(v7),
          max=max(v7),
          min=min(v7))


v7_scores <- newram_all %>%
  select(loc,wet_use) %>% 
  mutate(loc=ifelse(loc=="prs1","prs",loc),
         loc=ifelse(loc=="wrp1","wrpe",loc),
         loc=ifelse(loc=="si1","si",loc),
         loc=ifelse(loc=="phm1","phm",loc)) %>% 
  merge(veg_summaries_new) %>% 
  merge(data_doc_mod) %>% 
  mutate(v7=wet_use) %>% 
  select(-wet_use) %>%  
  merge(ldi_sites_500m) %>% 
  merge(ldi_sites_1k) %>% 
  merge(ldi_sites_2k)


final_scores_complex <- v1_scores_complex %>% 
  merge(v3_scores_complex) %>% 
  merge(v4_merge) %>% 
  merge(v5_scores_complex) %>% 
  merge(v6_scores_complex) %>% 
  merge(v7_scores_complex) %>% 
  mutate(final_score=v1+(v3*v4)+v5+v6+v7)


final_scores_complex %>%
  filter(complex!="sw1") %>% 
  reframe(mean=mean(final_score),
          se=sd(final_score)/31,
          var=var(final_score),
          max=max(final_score),
          min=min(final_score))


final_scores_sites <- v1_scores %>%
  merge(v3_scores) %>%
  mutate(v1=ifelse(loc=="sw1",0.452,v1)) %>% 
  merge(v4_scores) %>% 
  merge(v5_scores_sites) %>% 
  merge(v5_scores_sites_north) %>% 
  merge(v6_scores) %>% 
  merge(v7_scores) %>% 
  mutate(final=v1+(v3*v4)+v5_north+v6+v7) %>%  
  merge(ldi_sites_500m) %>% 
  merge(ldi_sites_1k) %>% 
  merge(ldi_sites_2k) %>% 
  mutate(newram_cat=case_when(final >= 0  & final <= 1.0 ~ 'poor',
                              final > 1.0  & final <= 2.0 ~ 'fair',
                              final > 2.0  & final <= 3.0 ~ 'good',
                              final > 3.0  & final <= 4.0 ~ 'very_good',
                              final > 4.0  & final <= 5.0 ~ 'excellent'))