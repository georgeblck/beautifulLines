library(tidyverse)
library(lubridate)
library(patchwork)

europeDat <- read_csv("GEOSTAT-grid-POP-1K-2011-V2-0-1/Version 2_0_1/GEOSTAT_grid_POP_1K_2011_V2_0_1.csv") %>%
  rbind(read_csv("GEOSTAT-grid-POP-1K-2011-V2-0-1/Version 2_0_1/JRC-GHSL_AIT-grid-POP_1K_2011.csv") %>%
          mutate(TOT_P_CON_DT='')) %>%
  mutate(lat = as.numeric(gsub('.*N([0-9]+)[EW].*', '\\1', GRD_ID))/100,
         lng = as.numeric(gsub('.*[EW]([0-9]+)', '\\1', GRD_ID)) * ifelse(gsub('.*([EW]).*', '\\1', GRD_ID) == 'W', -1, 1) / 100) %>%
  filter(lng > 25, lng < 60) 

europe0 <- europeDat %>%
  group_by(lat=round(lat, 0), lng=round(lng, 2)) %>%
  summarize(value = sum(TOT_P, na.rm=TRUE))  %>%
  ungroup() %>%
  complete(lat, lng) %>%
  ggplot(aes(lng, lat + 5*(value/max(value, na.rm=TRUE)))) +
  geom_line(size=0.4, alpha=0.8, color='black', aes(group=lat), na.rm=TRUE) +
  ggthemes::theme_map() +
  coord_equal(0.9)
europe1 <- europeDat %>%
  group_by(lat=round(lat, 1), lng=round(lng, 2)) %>%
  summarize(value = sum(TOT_P, na.rm=TRUE))  %>%
  ungroup() %>%
  complete(lat, lng) %>%
  ggplot(aes(lng, lat + 5*(value/max(value, na.rm=TRUE)))) +
  geom_line(size=0.1, alpha=0.9, color='black', aes(group=lat), na.rm=TRUE) +
  ggthemes::theme_map() +
  coord_equal(0.9)

euPlot <- europe0 / europe1 

ggsave(filename = paste0("plots/europe_", gsub("[^[:alnum:]=\\.]", "", lubridate::now()), ".pdf"), 
       plot = euPlot, height = 1189, width = 841, units = "mm")

gerDat <- read_csv("GEOSTAT-grid-POP-1K-2011-V2-0-1/Version 2_0_1/GEOSTAT_grid_POP_1K_2011_V2_0_1.csv") %>%
  rbind(read_csv("GEOSTAT-grid-POP-1K-2011-V2-0-1/Version 2_0_1/JRC-GHSL_AIT-grid-POP_1K_2011.csv") %>%
          mutate(TOT_P_CON_DT='')) %>%
  mutate(lat = as.numeric(gsub('.*N([0-9]+)[EW].*', '\\1', GRD_ID))/100,
         lng = as.numeric(gsub('.*[EW]([0-9]+)', '\\1', GRD_ID)) * ifelse(gsub('.*([EW]).*', '\\1', GRD_ID) == 'W', -1, 1) / 100) %>%
  filter(CNTR_CODE == "DE")

ger0 <- gerDat %>%
  group_by(lat=round(lat, 1), lng=round(lng, 2)) %>%
  summarize(value = sum(TOT_P, na.rm=TRUE))  %>%
  ungroup() %>%
  complete(lat, lng) %>%
  ggplot(aes(lng, lat + 2*(value/max(value, na.rm=TRUE)))) +
  geom_line(size=0.4, alpha=0.8, color='black', aes(group=lat), na.rm=TRUE) +
  ggthemes::theme_map() +
  coord_equal(0.9)
ger1 <- gerDat %>%
  group_by(lat=round(lat, 2), lng=round(lng, 2)) %>%
  summarize(value = sum(TOT_P, na.rm=TRUE))  %>%
  ungroup() %>%
  complete(lat, lng) %>%
  ggplot(aes(lng, lat + 0.5*(value/max(value, na.rm=TRUE)))) +
  geom_line(size=0.1, alpha=0.9, color='black', aes(group=lat), na.rm=TRUE) +
  ggthemes::theme_map() +
  coord_equal(0.9)

gerPlot <- ger0 + ger1 

ggsave(filename = paste0("plots/ger_", gsub("[^[:alnum:]=\\.]", "", lubridate::now()), ".pdf"), 
       plot = gerPlot, width = 1189, height = 841, units = "mm")



allPlot <- ( europe0 | europe1 )/(ger0|ger1)
ggsave(filename = paste0("plots/all_", gsub("[^[:alnum:]=\\.]", "", lubridate::now()), ".pdf"), 
       plot = allPlot, width = 1189, height = 841, units = "mm")













gerPlot <- read_csv("GEOSTAT-grid-POP-1K-2011-V2-0-1/Version 2_0_1/GEOSTAT_grid_POP_1K_2011_V2_0_1.csv") %>%
  rbind(read_csv("GEOSTAT-grid-POP-1K-2011-V2-0-1/Version 2_0_1/JRC-GHSL_AIT-grid-POP_1K_2011.csv") %>%
          mutate(TOT_P_CON_DT='')) %>%
  mutate(lat = as.numeric(gsub('.*N([0-9]+)[EW].*', '\\1', GRD_ID))/100,
         lng = as.numeric(gsub('.*[EW]([0-9]+)', '\\1', GRD_ID)) * ifelse(gsub('.*([EW]).*', '\\1', GRD_ID) == 'W', -1, 1) / 100) %>%
  filter(DATA_SRC == "NO") %>%
  group_by(lat=round(lat, 2), lng=round(lng, 2)) %>%
  summarize(value = sum(TOT_P, na.rm=TRUE))  %>%
  ungroup() %>%
  complete(lat, lng, fill = list(value=NA)) %>%
  ggplot(aes(lng, lat + 5*(value/max(value, na.rm=TRUE)))) +
  geom_line(size=0.1, alpha=0.9, color="black", aes(group=lat), na.rm=TRUE) +
  ggthemes::theme_map() +coord_equal(0.9)
print(gerPlot)

ggsave(filename = paste0("plots/nor_", gsub("[^[:alnum:]=\\.]", "", lubridate::now()), ".pdf"), plot = gerPlot, width = 841, height = 1189, units = "mm")








# only leipzig
europePlot <- read_csv("GEOSTAT-grid-POP-1K-2011-V2-0-1/Version 2_0_1/GEOSTAT_grid_POP_1K_2011_V2_0_1.csv") %>%
  rbind(read_csv("GEOSTAT-grid-POP-1K-2011-V2-0-1/Version 2_0_1/JRC-GHSL_AIT-grid-POP_1K_2011.csv") %>%
          mutate(TOT_P_CON_DT='')) %>%
  mutate(lat = as.numeric(gsub('.*N([0-9]+)[EW].*', '\\1', GRD_ID))/100,
         lng = as.numeric(gsub('.*[EW]([0-9]+)', '\\1', GRD_ID)) * ifelse(gsub('.*([EW]).*', '\\1', GRD_ID) == 'W', -1, 1) / 100) %>%
  #filter(lng > 35, lng < 56) %>%
  filter(DATA_SRC == "DE") %>%
  filter(lng > 44.7, lng < 44.9)%>%
  filter(lat > 31.25, lat <31.5) %>%
  group_by(lat,lng) %>%
  summarize(value = sum(TOT_P, na.rm=TRUE))  %>%
  ungroup() %>%
  complete(lat, lng) %>%
  ggplot(aes(lng, lat + 0.01*(value/max(value, na.rm=TRUE)))) +
  geom_line(size=0.1, alpha=0.9, color="black", aes(group=lat), na.rm=TRUE) +
  ggthemes::theme_map() +
  coord_equal(0.9)
print(europePlot)