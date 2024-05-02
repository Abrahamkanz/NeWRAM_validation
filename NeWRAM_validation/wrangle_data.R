library(tidyverse)



newram_all <- newram22 %>% 
  rbind(newram21) %>% 
  select(-comments) %>% 
  rbind(newram20) %>% 
  mutate(loc=site) %>% 
  select(-site)

write.csv(newram_all,file="newram_all.csv",row.names = FALSE)

newram_invasive_score <- newram_all %>% 
  select(loc,invasive)

write.csv(newram_invasive_score,file="newram_invasive_score.csv",row.names = FALSE)


waypoints2_update <- waypoints2 %>% 
  select(ident, Latitude,Longitude) %>% 
  separate(ident, c("loc","Corner"),-1) %>%
  mutate(loc = tolower(loc),
         lat = Latitude,
         lon = Longitude,
         corner = Corner) %>% 
  filter(Corner == "E"|Corner == "F") %>% 
  select(loc,corner,lat,lon)


waypoint_corners <- waypoints1 %>% 
  rbind(waypoints2) %>% 
  separate(ident, c("loc","corner"),-1) %>% 
  mutate(loc = tolower(loc),
         corner = tolower(corner)) %>% 
  select(loc,corner,Latitude,Longitude) %>% 
  filter(corner == "e"|corner == "f"|corner == "a"|corner == "b") %>%
  mutate(loc = ifelse(loc == "phm","phm1",loc)) %>% 
  mutate(loc = ifelse(loc == "urep1","urep",loc)) %>% 
  merge(site_type_all) %>% 
  select(-trt) %>% 
  rbind(coord_b) %>% 
  mutate(corner_mod=ifelse(corner=="a",1,corner),
         corner_mod=ifelse(corner_mod=="b",2,corner_mod),
         corner_mod=ifelse(corner_mod=="f",3,corner_mod),
         corner_mod=ifelse(corner_mod=="e",4,corner_mod))


write.csv(waypoint_corners,file="waypoint_corners.csv",row.names = FALSE)


corner_a <- waypoint_corners_mod %>% 
  filter(corner=="a") %>% 
  merge(site_type_all) %>% 
  select(-corner,-trt) %>% 
  merge(site_bearings)


test <- data.frame(unique(waypoint_corners_mod$loc))

############################################Calculating Final Scores 2023


v1_scores %>% 
  mutate(loc=ifelse(loc=="si","si1",loc),
         loc=ifelse(loc=="sw","sw1",loc)) %>% 
  merge(v3_scores) %>% 
  mutate(v3=bcw) %>% 
  select(-bcw) %>% 
  merge(v4_scores) %>% 
  mutate(v4=bci) %>% 
  select(-bci) %>% 
  merge(v5) %>% 
  mutate(v5=as.numeric(ifelse(v5=="Inf",0,v5))) %>% 
  merge(v6_scores) %>% 
  mutate(v6=hydro) %>% 
  select(-hydro) %>% 
  merge(v7_scores) %>% 
  mutate(v7=wet_use) %>% 
  select(-wet_use) %>% 
  mutate(final_score=v1+(v3*v4)+v5+v6+v7) %>%
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(data_doc) %>% 
  filter(wwis<=3.0) %>% 
  ggplot(aes(x=fqi,y=final_score))+
  geom_point()


data_doc_scores <- v3_scores_complex %>% 
  mutate(v1=0.66) %>% 
  merge(v4_merge) %>% 
  merge(v5_scores_complex) %>% 
  merge(v6_scores_complex) %>% 
  merge(v7_scores_complex) %>% 
  mutate(final_score=v1+(v3*v4)+v5+v6+v7) %>%
  merge(complexes) %>% 
  merge(veg_complex) %>% 
  group_by(complex) %>% 
  reframe(v1=mean(v1),
          v3=mean(v3),
          v4=mean(v4),
          v5=mean(v5),
          v6=mean(v6),
          v7=mean(v7),
          final_score=mean(final_score),
          fqi=mean(fqi),
          c=mean(wc),
          p_native=mean(p_native),
          p_exotic=mean(p_exotic),
          realtive_native=mean(relative_native),
          relative_exotic=mean(relative_exotic),
          wwis=mean(wwis),
          ldi=mean(ldi_1k))%>% 
  mutate(newram_cat=case_when(final_score >= 0  & final_score <= 1.0 ~ 'poor',
                              final_score > 1.0  & final_score <= 2.0 ~ 'fair',
                              final_score > 2.0  & final_score <= 3.0 ~ 'good',
                              final_score > 3.0  & final_score <= 4.0 ~ 'very_good',
                              final_score > 4.0  & final_score <= 5.0 ~ 'excellent'))



data_doc_scores_all <- v1_scores %>% 
  mutate(loc=ifelse(loc=="si","si1",loc),
         loc=ifelse(loc=="sw","sw1",loc)) %>% 
  merge(v3_scores) %>% 
  mutate(v3=bcw) %>% 
  select(-bcw) %>% 
  merge(v4_scores) %>% 
  mutate(v4=bci) %>% 
  select(-bci) %>% 
  merge(v5_scores) %>% 
  merge(v6_scores) %>% 
  mutate(v6=hydro) %>% 
  select(-hydro) %>% 
  merge(v7_scores) %>% 
  mutate(v7=wet_use) %>% 
  select(-wet_use) %>% 
  mutate(final_score=v1+(v3*v4)+v5+v6+v7) %>%
  mutate(loc=ifelse(loc=="phm1","phm",loc),
         loc=ifelse(loc=="si1","si",loc)) %>% 
  merge(data_doc_mod) %>% 
  mutate(gley=as.factor(gley),
         dtw_cm_min=as.numeric(dtw_cm_min),
         arth_pa_tipu=ifelse(arth_gm3_tipu==0,"a","p"),
         worm_pa_apor=ifelse(worm_gm3_apor==0,"a","p"),
         worm_pa_diplo=ifelse(worm_gm3_diplo==0,"a","p"),
         arth_pa_arma=ifelse(arth_gm3_arma==0,"a","p"),
         arth_pa_elat=ifelse(arth_gm3_elat==0,"a","p"),
         arth_pa_curc=ifelse(arth_gm3_curc==0,"a","p"),
         arth_pa_scara=ifelse(arth_gm3_scara==0,"a","p")) %>% 
  merge(complexes) %>% 
  mutate(newram_cat=case_when(final_score >= 0  & final_score <= 1.0 ~ 'poor',
                              final_score > 1.0  & final_score <= 2.0 ~ 'fair',
                              final_score > 2.0  & final_score <= 3.0 ~ 'good',
                              final_score > 3.0  & final_score <= 4.0 ~ 'very_good',
                              final_score > 4.0  & final_score <= 5.0 ~ 'excellent'))




#######################################Proportion of native

test <- data_doc_scores %>% 
  mutate(prop_n=n/(n+e)) %>% 
  merge(data_doc_scores)

veg_summaries_new <- veg_summaries_new %>% 
  select(loc,wis,wwis,c,wc,sdi,n,e,fqi,
         p_graminoid,p_forb,p_shrub,p_tree,p_vine,
         p_exotic,p_native,ave_cover,bg,ld)

veg_summaries_new <- veg_summaries_new %>% 
  mutate(loc=ifelse(loc=="prs1","prs",loc)) %>% 
  mutate(relative_native=p_native/ave_cover,
         relative_exotic=p_exotic/ave_cover)


data_doc_mod <- data_doc %>% 
  select(-c(wis,wwis,c,wc,sdi,n,e,fqi,
            p_graminoid,p_forb,p_shrub,p_tree,p_vine,
            p_exotic,ave_cover,bg,ld)) %>% 
  mutate() %>% 
  merge(veg_summaries_new)

data_doc_mod %>% 
  merge(complexes) %>% 
  
  
  
  veg_complex <- veg_summaries_new %>% 
  merge(complexes) %>% 
  gather(key="variable",value="value",2:19) %>% 
  group_by(complex,variable) %>% 
  reframe(mean=mean(value)) %>% 
  spread(key="variable",value="mean") %>% 
  mutate(relative_native=p_native/ave_cover,
         relative_exotic=p_exotic/ave_cover)