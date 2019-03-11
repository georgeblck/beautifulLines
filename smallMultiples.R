rm(list = ls())
dev.off()

# load packages
library(countrycode)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(gridExtra)
library(formatR)
library(MALDIquant)
library(formatR)


# old function
range01 <- function(x) {
    (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Read data
euDat <- read_csv("GEOSTAT-grid-POP-1K-2011-V2-0-1/Version 2_0_1/GEOSTAT_grid_POP_1K_2011_V2_0_1.csv") %>% 
    rbind(read_csv("GEOSTAT-grid-POP-1K-2011-V2-0-1/Version 2_0_1/JRC-GHSL_AIT-grid-POP_1K_2011.csv") %>% 
        mutate(TOT_P_CON_DT = "")) %>% mutate(lat = as.numeric(gsub(".*N([0-9]+)[EW].*", "\\1", GRD_ID))/100, 
    lng = as.numeric(gsub(".*[EW]([0-9]+)", "\\1", GRD_ID)) * ifelse(gsub(".*([EW]).*", "\\1", GRD_ID) == 
        "W", -1, 1)/100)

# Make country data
countryDF <- as.data.frame(table(euDat$CNTR_CODE))
countryDF$names <- countrycode(countryDF$Var1, "iso2c", "country.name.de")
countryDF[which(is.na(countryDF$names)), "names"] <- c("Greece", "UK", "Kosovo")
countryDF$eu <- countryDF$Var1 %in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "EL", "FI", 
    "FR", "UK", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", 
    "CH")

countryDF$lenLat <- sapply(countryDF$Var1, function(x) {
    length(unique(euDat$lat[euDat$CNTR_CODE == x]))
})
countryDF$lenLng <- sapply(countryDF$Var1, function(x) {
    length(unique(euDat$lng[euDat$CNTR_CODE == x]))
})

# exclude countries
outCountries <- c("AD", "LI", "MC", "VA", "SM", "IM", "IS", "MT", "XK*", "MK")
countryDF <- countryDF[!(countryDF$Var1 %in% outCountries), ]
set.seed(1990)
countryDF <- countryDF[sample(nrow(countryDF)), ]
solutions <- matrix(c(countryDF$names[1:2], "leer", "leer", countryDF$names[3:33]), nrow = 5, byrow = T)

# Initialize list to put all plots into
plotlist <- list()

special <- FALSE
# Loop over countries and make the plots
for (j in 1:nrow(countryDF)) {
    if (special) {
        plotlist[[j]] <- euDat %>% filter(CNTR_CODE == countryDF$Var1[j]) %>% mutate(lat = seq(min(lat), 
            max(lat), length.out = 300)[match.closest(lat, seq(min(lat), max(lat), length.out = 300))]) %>% 
            # mutate(lng = seq(min(lng),max(lng), length.out = 340)[match.closest(lng, seq(min(lng),max(lng),
        # length.out = 340))])%>%
        group_by(lat, lng) %>% summarize(value = sum(TOT_P, na.rm = TRUE)) %>% ungroup() %>% complete(lat, 
            lng, fill = list(value = NA)) %>% ggplot(aes(lng, lat + 1 * (value/max(value, na.rm = TRUE)))) + 
            geom_line(size = 0.3, alpha = 0.8, color = "black", aes(group = lat), na.rm = TRUE) + ggthemes::theme_map() + 
            # xlab(countryDF$names[j])+
        coord_equal(0.9)
    } else {
        plotlist[[j]] <- euDat %>% filter(CNTR_CODE == countryDF$Var1[j]) %>% group_by(lat = round(lat, 
            2), lng = round(lng, 2)) %>% summarize(value = sum(TOT_P, na.rm = TRUE)) %>% ungroup() %>% 
            complete(lat, lng) %>% ggplot(aes(lng, lat + 1 * (value/max(value, na.rm = TRUE)))) + geom_line(size = 0.3, 
            alpha = 0.8, color = "black", aes(group = lat), na.rm = TRUE) + ggthemes::theme_map() + coord_equal(0.9)
    }
}





p <- grid.arrange(grobs = plotlist, ncol = 7, layout_matrix = matrix(c(1:2, NA, NA, 3:33), nrow = 5, 
    byrow = T))

# Save for big poster
ggsave(filename = paste0("plots/multi_", gsub("[^[:alnum:]=\\.]", "", lubridate::now()), ".pdf"), plot = p, 
    device = cairo_pdf, width = 270, height = 135, units = "cm", limitsize = FALSE)
dev.off()



if (FALSE) {
    # Loop over countries and make the plots
    for (j in 1:nrow(countryDF)) {
        plotlist[[j]] <- euDat %>% filter(CNTR_CODE == countryDF$Var1[j]) %>% group_by(lat = round(lat, 
            2), lng = round(lng, 2)) %>% summarize(value = sum(TOT_P, na.rm = TRUE)) %>% ungroup() %>% 
            complete(lat, lng) %>% ggplot(aes(lng, lat + 1 * (value/max(value, na.rm = TRUE)))) + geom_line(size = 0.2, 
            alpha = 0.9, color = "black", aes(group = lat), na.rm = TRUE) + ggthemes::theme_map() + coord_equal(0.9)
    }
    
    
    # ggsave(filename = paste0('plots/multi_', gsub('[^[:alnum:]=\\.]', '', lubridate::now()), '.pdf'),
    # plot = p, width = 1189, height = 841, units = 'mm')
    dev.off()
}
