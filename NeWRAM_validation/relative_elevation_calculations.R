library(tidyverse)
library(janitor)

#Relative elevation calculations 

elevation_site <- ZonalSt_ft %>% 
  mutate(MEAN=MEAN*0.3048) %>% 
  rbind(ZonalSt_m) %>% 
  mutate(OBJECTID=OBJECTID_1) %>% 
  select(-OBJECTID_1) %>%
  merge(objectid_elevation) %>% 
  select(loc,MEAN) %>% 
  mutate(mean_site_elev=MEAN) %>% 
  select(-MEAN)




elevation_river <- lidar_elevations_main %>% 
  mutate(loc=ifelse(loc=="phm","phm1",loc)) %>% 
  merge(site_type_all) %>% 
  mutate(mean_river_m=((river1+river2+river3)/3)*0.3048) %>% 
  mutate(mean_river_m=ifelse(loc=="sw1",525.49,mean_river_m)) %>% 
  select(-river1,-river2,-river3)



elevation_relative <- elevation_site %>% 
  merge(elevation_river) %>% 
  mutate(rel_elev=mean_site_elev-mean_river_m) %>% 
  select(loc,rel_elev)