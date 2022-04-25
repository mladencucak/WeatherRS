################################################
#Libraries
################################################



list.of.packages <-
  c( 
    "dplyr",
    "tidyr",
    "tibble",
    "lubridate",
    "here",
    "conflicted",
    "zoo",
    "ggplot2",
    "leaflet"
  )

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  
  install.packages(new.packages)

if (length(new.packages))
  install.packages(new.packages, repos = c(CRAN="https://cran.r-project.org/"))

packages_load <-
  lapply(list.of.packages, require, character.only = TRUE)

#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load) == 0)) {
  warning(paste("Package/s", paste(list.of.packages[packages_load != TRUE]), "not loaded!"))
} else {
  print("All packages were successfully loaded.")
}




conflict_prefer("here", "here")
conflict_prefer("map", "purrr")
conflict_prefer("filter", "dplyr")


rm(packages_load, list.of.packages, new.packages)


################################################
# Map
################################################


my_lat <- 40.714
my_lon <- -77.9488
zoom_in <- 9

leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  setView(my_lon, my_lat, zoom = zoom_in) %>%
  addCircleMarkers(
    lng = my_lon,
    lat = my_lat,
    label = "Rock Springs"  ,
    fillColor = "goldenrod",
    fillOpacity = .8,
    stroke = F
  )

################################################
#Data
################################################

# Download the data from 
# Note that data has do be donloaded for each year
browseURL("https://newa.cornell.edu/all-weather-data-query")

#find all files in folder with weather data
(y.files <- list.files(here("dta/wth"), full.names = T))


  
 
# Make an empty list and load every file
wth.ls <- list()


for (i in seq(y.files)) {
  file <- y.files[[i]]
  (wth <-  read.csv(file))
  #Change names
 
  
  names(wth)[grepl("date", names(wth))] <- "datetime"
  names(wth)[grepl("Temp", names(wth))] <- "temp"
  names(wth)[grepl("Precipitation", names(wth))] <- "rain" # note inches! 
  
  names(wth)[grepl("Relative", names(wth))]  <- "rh"
  # Solar radiation is in langleys
  names(wth)[grepl("Leaf.Wetness", names(wth))] <- "lw"
  names(wth)[grepl("Solar.Radiation", names(wth))]  <- "solrad"
  names(wth)[grepl("Wind.Speed", names(wth))] <- "wdspd"
  names(wth)[grepl("Wind.Direction", names(wth))] <- "wddir"
  
  # Set correct class of each variable(some are character in one of files)
  wth$temp <- as.numeric(wth$temp)
  wth$rain <- as.numeric(wth$rain)
  wth$rh <- as.numeric(wth$rh)
  wth$wdspd <- as.numeric(wth$wdspd)
  wth$lw <- as.numeric(wth$lw)
  wth$solrad <- as.numeric(wth$solrad)
  wth$wdspd <- as.numeric(wth$wdspd)
  wth$wddir <- as.numeric(wth$wddir)
  
  
    wth.ls[[i]] <- wth
  str(wth)
}

# Bind the list into a single data frame
wth <-
wth.ls %>% 
  bind_rows() 

#normalize dates
wth$datetime <- lubridate::mdy_hm(wth$datetime)

# Hourly Data for Rock Springs, PA - [id=pa_rsp newa, lat=40.714, lon=-77.9488]

# wth$date <- as_date(wth$datetime)

# Add columns for year and day of year at specific location 
wth <- 
add_column(wth, date = as_date(wth$datetime), .after = "datetime")

wth <- 
  add_column(wth, yr = year(wth$date), .after = "date")
wth <- 
  add_column(wth, mn = month(wth$date), .after = "date")
wth <- 
  add_column(wth, doy = yday(wth$date), .after = "yr")

wth %>% group_by(yr)  %>%
  summarize(
    NA_rain = sum(is.na(rain)),
    NA_temp = sum(is.na(temp)),
    NA_rh = sum(is.na(rh))
  ) 


#infill some of the missing data 
infil_gap <- 12 #Maximum length of the infill gap
wth$temp <-
  round(na.spline(wth$temp, na.rm = FALSE, maxgap = infil_gap), 1)
wth$rh <-
  round(na.spline(wth$rh, na.rm = FALSE, maxgap = infil_gap), 0)
wth$rh  <- sapply(wth$rh, function(x)
  ifelse(x > 100, x <- 100, x))

#Check if the imputation worked
wth %>% group_by(yr)  %>%
  summarize(
    NA_rain = sum(is.na(rain)),
    NA_temp = sum(is.na(temp)),
    NA_rh = sum(is.na(rh))
  )


################################################
# Daily summaries
################################################

#Create daily summaries
wthls <- 
  split(wth, wth$date, drop = TRUE)

# Remove dates with less than 3 hours
lenghts <- sapply(wthls, nrow) %>% unlist()
lenghts[lenghts<24] %>% length()
ss <- lenghts>3
wthls <- wthls[ss]



system.time(
  wthd <- 
    lapply(wthls, function(x){
      # x <- wthls[[1]]
      x
      
      y<- data.frame(
        date= date(x$datetime[1]),
        yr= x$yr[1],
        mn = x$mn[1] 
      )
      
      y$temp <- mean(x$temp, na.rm = TRUE) %>% round(1)
      y$mintemp <- min(x$temp, na.rm = TRUE) %>% round(1)
      y$maxtemp <- max(x$temp, na.rm = TRUE) %>% round(1)
      
      y$rh <- mean(x$rh, na.rm = TRUE) %>% round(1)
      y$minrh <- min(x$rh, na.rm = TRUE) %>% round(1)
      y$maxrh <- max(x$rh, na.rm = TRUE, warning=FALSE) %>% round(1)
      
      
      y$wdspd <- mean(x$wdspd, na.rm = TRUE) %>% round(1)
      y$minwdspd <- min(x$wdspd, na.rm = TRUE) %>% round(1)
      y$maxwdspd <- max(x$wdspd, na.rm = TRUE, warning=FALSE) %>% round(1)
      
      y$wddir <- mean(x$wddir, na.rm = TRUE) %>% round(1)
      y$minwddir <- min(x$wddir, na.rm = TRUE) %>% round(1)
      y$maxwddir <- max(x$wddir, na.rm = TRUE, warning=FALSE) %>% round(1)
      
      #sum of wet minutes
      y$lw <- sum(x$lw, na.rm = TRUE) 
      #proportion of day with wet leaf
      y$lw_prop <- sum(x$lw, na.rm = TRUE)/(24*60) %>% round(1)
      
      # total solar 
      y$solrad <- sum(x$solrad, na.rm = TRUE)
      
      # sum of rain 
      y$rain <- sum(x$rain, na.rm = TRUE)
      
      return(y)
    }) %>% bind_rows() 
)

# Procedure would be very similar for any other perod such as month aor week

################################################
# Filtering periods of interest
################################################

# Filter specific periods 

wthd %>% 
  filter(date> "2021-05-01"&date<"2021-10-01")


wthsub <- 
wthd %>% 
  filter(mn> 4 & mn <10)




################################################
# Some visualisations
################################################

#can be any developmental threshold - here just 0Celsius
threshold_line <- 32

wth %>% 
  mutate(hour = hour(datetime)) %>% 
  filter(mn %in% c(3:8)) %>% 
  group_by(yr, mn, hour) %>%
  mutate(month_abb = month(date, label = T)) %>%
  ggplot(aes(factor(hour), temp)) +
  geom_vline(
    xintercept = seq(0, 24, 6),
    size = 0.1,
    color = "gray",
    linetype = "dotted"
  ) +
  geom_boxplot(width = 0.2, outlier.size = 0.2) +
  geom_line(aes(hour, threshold_line, color = "10 ˚C Line")) +
  scale_fill_manual(values = c("10 ˚C Line" = "red"),
                    aesthetics = c("colour", "fill")) +
  facet_grid(~ month_abb) +
  egg::theme_article() +
  theme(legend.position = "top") +
  scale_x_discrete(labels = seq(0, 24, 6), breaks = seq(0, 24, 6)) +
  labs(
    title = "Average temperatures per hour for months April to September",
    y = "Temperature",
    x = "Hours of the day",
    color = "10 ˚C Line"
  )



wth %>%
  filter(mn %in% c(3:8)) %>% 
  group_by(yr, mn) %>% 
  summarize(rain = sum(rain)
  ) %>% 
  mutate( month_abb = month(mn, label = T)) %>% 
  ggplot(aes(month_abb,rain,fill = factor(year_var)))+
  geom_bar( position="dodge", stat="identity", width = 0.7)+
  scale_fill_manual(values = colorRampPalette(brewer.pal(10, "Blues"))(10))+
  annotate("text",x = 1:6, y = 170, 
           label=as.character(OP_months$rain))+
  ggtitle( "Monthly rainfall variation")+
  xlab("Month")+
  ylab("Rainfall (mm/month)")+
  labs(fill = "Year",
       subtitle = "Monthly averages are presented at the top of bar chart. ")+
  theme_bw()







png(here::here("out",  "temp_bar.png"),width = 650, height = 500)

hist( wthsub$temp,
      breaks = 50,
      col = "peachpuff",
      main = "Histogram of Temperatures (Apr. - Sep.)",
      prob = TRUE) # prob = TRUE to show densities instead of frequencies
lines(density( wthsub$temp, na.rm = T), lwd = 2, col = "chocolate3")
abline(v = mean( wthsub$temp),col = "royalblue", lwd = 2)
abline(v = median( wthsub$temp, na.rm = T),col = "red", lwd = 2)
abline(v=quantile( wthsub$temp,0.25, na.rm = T),col="darkgreen",lty=2)
abline(v=quantile( wthsub$temp,0.75, na.rm = T),col="green",lty=2)

legend(x = "left", 
       bty = "n",
       c("Density plot", "Mean", "Median","Lower Quantile", "Upper Quantile"),
       col = c("chocolate3", "royalblue", "red", "darkgreen","green"),
       lwd = c(2, 2, 2))

dev.off()

png(here::here("out", "rh_bars.png"),width = 650, height = 500)

hist( wthsub$rh,
      breaks = 80,
      col = "lightblue",
      main = "Histogram of Rel. Humidity (Apr. - Sep.)",
      prob = TRUE)
lines(density( wthsub$rh, na.rm = T), lwd = 2, col = "chocolate3")
abline(v = mean( wthsub$rh, na.rm = T),col = "royalblue", lwd = 2)
abline(v=quantile( wthsub$rh,0.25, na.rm = T),col="darkgreen",lty=2)
abline(v=quantile( wthsub$rh,0.75, na.rm = T),col="green",lty=2)
abline(v = median( wthsub$rh, na.rm = T),col = "red", lwd = 2)
legend(x = "left", 
       bty = "n",
       c("Density plot", "Mean", "Median","Lower Quantile", "Upper Quantile"),
       col = c("chocolate3", "royalblue", "red", "darkgreen","green"),
       lwd = c(2, 2, 2))

dev.off()


hist( wthsub$rain[ wthsub$rain>0], 
      breaks = 60, 
      col = "blue",
      main = "Freq. of rain > 0 mm (Apr. - Sep.)",
      xlab = "Rain (mm/hour)")


# Ridgeplots
library(ggridges)
ttl <- "Temperatures in Rock Spring per month"
wthd %>% 
  # mutate(mon = factor(mon, levels = rev(c(9:12, 1:5)))) %>% 
  ggplot(  aes(x = temp, y = factor(mn, ordered = T)))+
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 2, size = 0.2
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Temp. (°C)"
  )+
  ggtitle(ttl)+
  xlab("Temperature (°C)")+
  ylab("Month")+
  theme_ridges(font_size = 13, grid = TRUE)

ggsave(here::here("out", paste0(ttl, ".png")),dpi = 500)


