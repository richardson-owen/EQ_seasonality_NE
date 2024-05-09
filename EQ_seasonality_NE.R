# load required packages
library(data.table)
library(lubridate)
library(tidyverse)
library(sf)
library(ggplot2)
# install.packages("devtools")
library(devtools)
# install_github("ropensci/rnaturalearthhires")
library(rnaturalearth)
library(sp)
library(spData)
library(dataRetrieval)
library(splus2R)
library(patchwork)
library(ggpubr)
library(grid)
library(gridExtra)
library(R.utils)
library(changepoint)
library(rnaturalearth)
library(rnaturalearthhires)
library(circular)

#### Create a table of all ghcnd prcp data for stations with more than 80% complete record from 1901-01-01 to 2023-12-31 ####

# #initiate a blank data table, each iteration of the loop will append the data from the given file to this variable
# complete_record_NE_1901 <- data.table()
# 
# # set parameters
# date_1<-as.Date("1901-01-01")
# date_2<-as.Date("2023-12-31")
# a = seq(from = date_1, to = date_2, by = 'day')
# numdays_1901 = length(a)-1 # there are 44924 days from 1901-01-01 to 2023-12-31
# 
# 
# # Filtering and compiling the ghcn daily prcp data
# for (i in 1:length(file_list)){
#   print(i) # to check on progress of loop
#   ghcnd_daily = fread(file_list[i]) #each file will be read in
#   ghcnd_daily = ghcnd_daily %>%
#     rename(Station_ID = 1, # rename the columns by index, to account for changes in naming convention
#            Date = 2,
#          Measurement = 3,
#            Value = 4)
#   ghcn_daily_prcp = ghcnd_daily[Measurement == "PRCP" ,] # select only PRCP data -- "Total Daily Precipitation" as per NCEI website
#   ghcn_daily_prcp = ghcn_daily_prcp[ , Date:= parse_date_time(ghcn_daily_prcp$Date, 'ymd')] # parse the date
#   select_date_1901 = ghcn_daily_prcp[Date %between% c("1901-01-01", "2023-12-31")] # select days btw given values
# 
#   percent_complete_1901 = nrow(select_date_1901)/numdays_1901 # calculate the percent complete from 1901 to 2023 for each station
# 
#   if(percent_complete_1901 >= 0.80){ # check if the record is >=80% complete over given time period
#     complete_record_NE_1901 = rbind(complete_record_NE_1901, select_date_1901) #for each iteration, bind the new data to the building dataset
#   }
# 
# }
# 
# saveRDS(complete_record_NE_1901, file = 'complete_record_NE_1901.Rds') # save the above file for easy access
# 
# reload here for future use
complete_record_NE_1901 = readRDS('complete_record_NE_1901.Rds')
setDT(complete_record_NE_1901)
# cutting the record such that only observations after 1950 are included
complete_record_NE_1901 = complete_record_NE_1901[, year_cut:=year(Date)]
complete_record_NE_1950 = complete_record_NE_1901[year_cut>=1950]
complete_record_NE_1950 = complete_record_NE_1950[ , year_cut:=NULL]

station_list_1901 = unique(complete_record_NE_1901, by = "Station_ID") # create a list of stations that have
# at least 80% complete record from 1901 to 2023. Ignore all columns except for Station_ID. There are 123


#### Analysis ####
## Create a list of GHCN-d stations in the NorthEast
# note: the "colClasses" command makes R keep the leading zeros of "site_no"
GHCNdStationTable = fread("ghcnd_stations.csv") 

# create new GHCNdStationsNE with only states of interest
GHCNdStationsNE = GHCNdStationTable[ State %in% c("ME", "NH", "VT", "MA", "CT", "RI",
                                                  "NJ","NY","PA", "MD","DE","DC","WV")]

# Get latitude and longitude of stations with sufficient records
GHCNdStationsNE1901 = GHCNdStationsNE[Station_ID %in% station_list_1901$Station_ID, ]
GHCNdStationsNE1901 = GHCNdStationsNE1901[ , ':='(V7 = NULL, V8 = NULL)]
GHCNdStationsNE1901 = data.frame(GHCNdStationsNE1901) # sf is compatible with the tidyverse
# I have had issues before trying to use sf with data.table(), so I will be safe and use df
# class(GHCNdStationsNE1901)

# using sf package to define a sfc column
GHCNdStationsNE1901sf = st_as_sf(GHCNdStationsNE1901, coords = c("Longitude", "Latitude"), 
                                 crs = 4326)

# checking to see if this makes sense
# looks good
# plotting in lat/long just to start (simple visualization)
us_states <- st_transform(us_states, "EPSG:4326")
ggplot() +
  geom_sf(data = us_states) +
  geom_sf(data = GHCNdStationsNE1901sf, col = "darkblue")+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'Gray85', colour = NA))+
  ggtitle('NE GHCNd Stations, >80% complete record 1901-2023')

## Now, constructing grid polygons for spatial averaging
# there were no stations in DC with sufficiently complete record
# first, creating a frame with only states of interest, so I can create a bounding box
NE_states = dplyr::filter(us_states, NAME %in% c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Connecticut",
                                                 "Rhode Island", "New Jersey","New York","Pennsylvania",
                                                 "Maryland","Delaware","West Virginia")) 

NE_grid = st_make_grid(
  NE_states,
  cellsize = c(1,1)
)

NE_grid_df = data.frame(NE_grid)
ORID_list = seq(1:176)
NE_grid_df = NE_grid_df %>%
  mutate(ORID = ORID_list, .before = geometry)
NE_grid_df = st_as_sf(NE_grid_df)

# checking grid by plotting
ggplot() +
  geom_sf(data = NE_grid_df$geometry) + 
  geom_sf(data = GHCNdStationsNE1901sf, col = 'darkblue') +
  geom_sf(data = NE_states$geometry, fill = 'NA', lwd = 1)+
  # geom_sf(data = NE_grid_df[4, 'geometry'], col = 'red')+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'Gray85', colour = NA)) +
  ggtitle('NE Grid (1x1 degree), 80% complete stations beginning 1901') # looking good
# ggsave(filename = 'prcp_grid.jpg')

# identifying which stations are in which grid number
# reprojecting for st_intersects (must be projected)

NE_grid_df = NE_grid_df %>% st_transform(3857)
GHCNdStationsNE1901sf = GHCNdStationsNE1901sf %>% st_transform(3857)
NE_states = NE_states %>% st_transform(3857)


GHCNdStationsNE1901_ORID = data.table()
for(i in ORID_list) {
  # print(i)
  grid_select = NE_grid_df[i, ]
  stations_grid_sel = GHCNdStationsNE1901sf[grid_select$geometry, ]
  stations_grid_sel = stations_grid_sel %>% mutate(ORID = i)
  stations_grid_sel = st_drop_geometry(stations_grid_sel) # drop geometry for speed
  if (dim(stations_grid_sel)[1]==0){
    next
  }else{
    GHCNdStationsNE1901_ORID = rbind(GHCNdStationsNE1901_ORID, stations_grid_sel)
  }
}
setDT(GHCNdStationsNE1901_ORID)
# we have identified which grid cell each station falls into


# now, finding the top 1% of prcp values for each station
TopPrcp = data.table()

for(i in station_list_1901$Station_ID){
  station_percent_ = data.table()
  station_percent_ = complete_record_NE_1901[Station_ID == i]
  ## select top n% of wet days here ##
  # select only wet days (> 0.254 mm, or 0.01in rain)
  station_percent_ = station_percent_[Value > 2.54] 
  n <- 1 # top 1% of wet days
  station_top_percent = station_percent_[station_percent_$Value > quantile(station_percent_$Value,prob=1-n/100),]
  TopPrcp = rbind(TopPrcp,station_top_percent)
  # print(i)
}

rm(complete_record_NE_1901)
rm(complete_record_NE_1950)
# saveRDS(TopPrcp, file = 'TopPrcp.Rds')

# summing the top prcp values by year and station
TopPrcp = TopPrcp[, Year:=year(Date)]
sumTopPrcp = data.table()
years1950 = c(1950:2023)
for(i in station_list_1901$Station_ID){
  #print(i)
  station_select = data.table()
  station_select = TopPrcp[Station_ID == i]
  year_sum_table = data.table()
  for(j in years1950){
    #print(j)
    year_select = station_select[Year == j]
    year_select = year_select[ , sumTopPrcp :=sum(Value)]
    year_select = unique(year_select, by = "sumTopPrcp")
    year_select = year_select[ , c("Value", "V5", "V6", "V7", "V8"):= NULL]
    year_sum_table = rbind(year_sum_table, year_select)
  }
  sumTopPrcp = rbind(sumTopPrcp, year_sum_table)  
}

# assigning ORID values to all summed top 1% values
sumTopPrcp_ORID = sumTopPrcp %>% left_join(GHCNdStationsNE1901_ORID, by = "Station_ID")

# finding average pcpt by year, by grid cell
relevantORID = unique(sumTopPrcp_ORID, by = "ORID")
meanAnnGridPrcp = data.table()
for(i in years1950){
  #print(i)
  year_select = data.table()
  year_select = sumTopPrcp_ORID[Year == i]
  grid_mean_table = data.table()
  for(j in relevantORID$ORID){
    #print(j)
    grid_select = year_select[ORID == j]
    grid_select = grid_select[ , meanGridPrcp :=mean(sumTopPrcp)]
    grid_select = unique(grid_select, by = "meanGridPrcp")
    grid_mean_table = rbind(grid_mean_table, grid_select)
  }
  meanAnnGridPrcp = rbind(meanAnnGridPrcp, grid_mean_table)  
}

# taking the average of all grid averages
year_vs_prcp = data.table()
for (i in years1950) {
  year_select = data.table()
  year_select = meanAnnGridPrcp[Year == i]
  year_select = year_select[ , meanTotalPrcp:=mean(meanGridPrcp)]
  year_select = unique(year_select, by = "Year")
  year_vs_prcp = rbind(year_vs_prcp, year_select)
}

# now plotting year vs total prcp
ggplot() + 
  geom_point(data = year_vs_prcp, aes(x =Year , y = meanTotalPrcp), color = 'darkblue', size = 3) +
  labs(x = "Year", y = "Total Precipitation (mm)")
# ggsave(filename = 'yrvsprcp.jpg')



# examining changepoints using changepoint package
chgPcpt <- cpt.mean(year_vs_prcp$meanTotalPrcp,penalty="MBIC",pen.value=0,method="AMOC",Q=5,test.stat="Normal",class=TRUE,
                    param.estimates=TRUE,minseglen=1)

plot(chgPcpt, type = "l", cpt.col = "darkblue", xlab = "Index", cpt.width = 4, ylab = "Extreme Precipitation (mm)")
cpts(chgPcpt)
# the changepoint is identified to be the year 1996
# this is the same as Huanping found
# no additional changepoints were found after 1996


# Making a better plot, incorporating changepoints
ggplot() + 
  geom_point(data = year_vs_prcp, aes(x =Year , y = meanTotalPrcp/10), color = 'black', pch = 1, lwd = 1.5) +
  geom_line(data = year_vs_prcp, aes(x =Year , y = meanTotalPrcp/10), color = 'darkgrey', lwd = 0.6) +
  geom_smooth(data = year_vs_prcp[Year %between% c(1901,1996)], aes(x = Year, y = meanTotalPrcp/10), method = lm, color = 'gold2') +
  geom_smooth(data = year_vs_prcp[Year %between% c(1996,2023)], aes(x = Year, y = meanTotalPrcp/10), method = lm, color = 'purple') +
  labs(title = "Extreme precipitation trends before and after 1996", x = "Year", y = "Extreme Precipitation (mm)")+
  theme_bw()

# test for significance
p1 = t.test(year_vs_prcp[Year<1996, meanTotalPrcp], year_vs_prcp[Year>=1996, meanTotalPrcp], 
            alternative = "two.sided", var.equal = FALSE)$p.value

## create cptTable (for future use)
cptTable = as.data.table(list(Station_ID = GHCNdStationsNE1901$Station_ID))
cptTable[ , change_year:= 1996]
setorder(cptTable, by = 'Station_ID')

# joining geographic data
cptTable_geom = cptTable %>% left_join(GHCNdStationsNE1901sf, by = "Station_ID")


#### Coast geometry ####

# get ocean data, define projection
oceans_sf = ne_coastline(scale = 10, returnclass = c("sf"))
plot(oceans_sf$geometry, col = "lightblue")
# crsmeta::crs_epsg(oceans_sf)
st_crs(oceans_sf) = 4326
oceans_sf = oceans_sf %>% st_set_crs(4326)
# st_crs(oceans_sf)
oceans_sf = oceans_sf %>% st_transform(3857) # reproject

# clip coastline to only the NE
sf_use_s2(FALSE) # need to turn off spherical geometry to do this operation
oceans_sf = st_intersection(oceans_sf, NE_grid_df)

# most 'ocean' shapefiles seem to include the st lawrence river as an ocean
# artifically makes inland parts of inland new england "coastal"
# removing parts of the dataset that include the st lawrence
oceans_sf1 = oceans_sf[!(oceans_sf$ORID %in% c(120, 121, 122, 137, 138, 139, 154, 155, 156, 157, 172, 173, 174, 176)), ]

plot(oceans_sf1$geometry, col = 'lightblue')
# plot coastline

# coastal buffer (100km)
# reprojecting everything to EPSG:3857--> so we can buffer in units of meters 
# I already did this above, but just here to ensure it happened
NE_grid_df = NE_grid_df %>% st_transform(3857)
GHCNdStationsNE1901sf = GHCNdStationsNE1901sf %>% st_transform(3857)
NE_states = NE_states %>% st_transform(3857)
oceans_sf1 = oceans_sf1 %>% st_transform(3857)

plot(oceans_sf1$geometry)
# plotting, now with the coastline
# looking good



#### Cox Lewis ####
# compute cox lewis statistic for each station
# Calculate Cox-Lewis statistic (Z-Score) for each station
# define midpoint year and total length in years

ztable1 = data.table()
for(i in GHCNdStationsNE1901$Station_ID){
  zcalc = data.table()
  zcalc = TopPrcp[Station_ID == i]
  tm = min(zcalc$Year) + ((max(zcalc$Year)-min(zcalc$Year))/2) # midpoint year
  tl = max(zcalc$Year) - min(zcalc$Year) # total length in years
  zcalc = zcalc[ , year_dif:= zcalc$Year - tm]
  zcalc = zcalc[ , numerator:= sum(zcalc$year_dif) / nrow(zcalc)]
  zcalc = zcalc[ , denominator:= tl/(sqrt(12*nrow(zcalc)))]
  zcalc = zcalc[ , cox_lewis:= zcalc$numerator / zcalc$denominator]
  zcalc = zcalc[ , p:= 2*pt(-abs(cox_lewis),df=nrow(zcalc)-1)]
  ztable1 = rbind(ztable1,zcalc)
}
cox_lewis_table1 = unique(ztable1, by = 'Station_ID')
cox_lewis_table_geom1 = cox_lewis_table1 %>% left_join(GHCNdStationsNE1901sf, by = "Station_ID")


ggplot() +
  # geom_raster(data = NE_elev, aes(x = x, y = y, fill = elevation), alpha = 0.5) +
  # scale_fill_gradientn(colours = c('lemonchiffon', 'khaki1', 'khaki2','lightsalmon1', 'lightsalmon2', 'tomato1', 'tomato2'))+
  geom_sf(data = NE_states$geometry, fill = 'white', lwd = 1)+
  geom_sf(data = cox_lewis_table_geom1, aes(geometry = geometry, col = cox_lewis, size = p<0.05)) +
  scale_size_manual(values=c(1.5,4))+
  scale_color_gradientn(colours = c('blue', 'red'),  
                        limits = c(-3, 3), oob = scales::squish)+
  labs(col = "Z")+ # add title
  ggtitle('Extreme Precipitation Cox-Lewis Z Score')+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'Gray85', colour = NA))

#### Get discharge data ####

# import the HCDN gage dataset
# note: the "colClasses" command makes R keep the leading zeros of "site_no"
hcdnGageTable = fread("HCDNGages.csv", keepLeadingZeros = getOption("datatable.keepLeadingZeros", TRUE)) 

# setDT
setDT(hcdnGageTable)

# create new hcdnGageTableNE with only states of interest
hcdnGageTableNE = hcdnGageTable[ state %in% c("ME", "NH", "VT", "MA", "CT", "RI",
                                              "NJ","NY","PA", "MD","DE","DC","WV")]

# create a new data table "hcdnSitesTable" with the station information for all of the HCDN stations in the states of interest
hcdnSitesTableNE=whatNWISdata(siteNumber=hcdnGageTableNE$site_no, sitetype="ST", parameterCd="00060", service="dv")
setDT(hcdnSitesTableNE)

# remove duplicates
hcdnSitesTableNE=unique(hcdnSitesTableNE, by = 'site_no')

# create to new columns in the hcdnGageTable to hold the latitude, longitude, and years of record for each station
hcdnGageTableNE[ , ':='(Latitude = NA_real_, Longitude = NA_real_, YearsOfRecord = NA_real_)]

# get the latitude, longitude, and the years of record (count_nu/365) from the hcdnGageTableNE and assign them the corresponding row in the hcdnSitesTable for which the 
# site_no == hcdnGageTableNE[index, site_no].

# sort
hcdnSitesTableNE = setorder(hcdnSitesTableNE, 'site_no')
hcdnGageTableNE = setorder(hcdnGageTableNE, 'site_no')
hcdnGageTableNE = hcdnGageTableNE[site_no %in% hcdnSitesTableNE$site_no]

# add more metadata info
for(i in 1:nrow(hcdnGageTableNE)) {
  # create site number variable equal to the site_no of hcdnSitesTable 
  SiteNum= hcdnGageTableNE[i, site_no]
  # create variables with the lat, long, and length of record info from hcdnSitesTable
  lat=hcdnSitesTableNE[i, dec_lat_va]
  long=hcdnSitesTableNE[i, dec_long_va]
  years=hcdnSitesTableNE[i, count_nu]/365
  # store the above variables in the appropriate row, corresponding to the appropriate site number
  hcdnGageTableNE[site_no==SiteNum]
  hcdnGageTableNE[site_no==SiteNum, Latitude:=lat]
  hcdnGageTableNE[site_no==SiteNum, Longitude:=long]
  hcdnGageTableNE[site_no==SiteNum, YearsOfRecord:=years]
}

# make a map of all the HCDN gage stations in the states of interest, coloring each point based on the years of record
# create Northeast and NortheastCodes variables
Northeast=c("maine", "new hampshire", "vermont", "massachusetts", "connecticut",
            "rhode island", "new jersey","new york","pennsylvania",
            "maryland","delaware","west virginia")
NortheastCodes=c("ME", "NH", "VT", "MA", "CT", "RI",
                 "NJ","NY","PA", "MD","DE","DC","WV")

## use readNWISsite to get additional site data
more_site_info = readNWISsite(siteNumbers = hcdnGageTableNE$site_no)
setDT(more_site_info)
more_site_info = setorder(more_site_info, 'site_no')
hcdnGageTableNE = setorder(hcdnGageTableNE, 'site_no')
hcdnSitesTableNE = setorder(hcdnSitesTableNE, 'site_no')

# add drain_area_va to hcdnSitesTableNE
hcdnSitesTableNE[ , drain_area_va := more_site_info$drain_area_va]

## DOWNLOAD DAILY DISCHARGE DATA FROM SELECTED SITES 
# Set parameter codes
parameterCd_sel <- c('00060') # Stream parameter code
paramCds_daily <- c(parameterCd_sel)
# Select site numbers from all the sites that were downloaded
site_no_sel <- unique(hcdnGageTableNE$site_no)
# Function to download daily data
getDailyUSGS <- function(site_no_sel, paramCds_daily){
  daily_import <- renameNWISColumns(data.table(readNWISdv(parameterCd=c(paramCds_daily), 
                                                          siteNumbers= site_no_sel
  )))
  return(daily_import)
}


# Download daily data from selected sites, and for selected parameters
# CAUTION: this could take as much as 10 seconds per site!
# (depending on the length of the records you are getting)
# From Evan Dethier
# For example, this took Evan about five minutes for 66 sites (and downloaded 2.3 million days of data!)
daily_usgs_data <- rbindlist(lapply(site_no_sel, getDailyUSGS, paramCds_daily = paramCds_daily), fill = T)

# setDT
setDT(daily_usgs_data)

# reload here for future use
daily_NEhcdn_DT = daily_usgs_data # rename
rm(daily_usgs_data)

## Getting peaks
# below is basic demonstration of the peaks function
# install.packages('splus2R')
library('splus2R')
peaks_ex <- data.table(values = c(1,2,3,7,4,5,6,9,6,5,4,3,2,4,
                                  5,3,1,2,4,5,6,7,5,9,7,6,5,4,
                                  3,2,3,4,6,8,7,6,7,8,9,7,6,5))
peaks_ex <- cbind(peaks_ex, data.table(dates = c(1:nrow(peaks_ex))))
peaks_tf <- peaks(peaks_ex$values, span = 7)
get_peaks <- peaks_ex[peaks_tf]

ggplot(peaks_ex, aes(x = dates, y = values)) + 
  geom_line() +
  geom_point() + 
  geom_point(data = get_peaks, color = 'red')

# get site numbers
setorder(daily_NEhcdn_DT, "site_no")
site_no_list = as.list(hcdnGageTableNE$site_no) # list of sites
# print(site_no_list)

## get peak flows for each site
site_no_vec = unlist(site_no_list)

peakTF = data.table()

for(index in site_no_vec){
  site_peaks_ = data.table()
  site_peaks_ = daily_NEhcdn_DT[site_no == index]
  # 7 day span, we don't want multiday events
  site_peaks_ = site_peaks_[ , peaks_tf:= peaks(site_peaks_$Flow, span = 7)]
  peakTF = rbind(peakTF,site_peaks_)
  # print(index)
}


all_peaks = data.table()
all_peaks = peakTF[peaks_tf=="TRUE"]
AllPeaks = all_peaks # set up AllPeaks data.table() for use later, rename
rm(all_peaks) # remove big tables
rm(daily_NEhcdn_DT)

# filtering sites, only sites that cover 1950-present
hcdnSitesTableNE[ , Start_year:= year(begin_date)]
hcdn_more_info = hcdnSitesTableNE #rename
rm(hcdnSitesTableNE)

hcdnGageTable = hcdnGageTableNE # rename
rm(hcdnGageTableNE)

# filtering sites, only sites that cover 1950-present (for comparison purposes)
setorder(hcdn_more_info, by = 'site_no')
hcdn_more_info[ , Start_year:= year(begin_date)]
# grab a few more sites. these records span into the 1950s
# and meet the >80% completeness criteria
sites_1950 = hcdn_more_info[Start_year <= 1960] 
sites_1950 = sites_1950[!(Start_year == 1960)]

sites_long_rec_vec = sites_1950$site_no
hcdn_more_info = hcdn_more_info[site_no %in% sites_long_rec_vec]
hcdnGageTable = hcdnGageTable[site_no %in% sites_long_rec_vec]
hcdnGageTableSF = st_as_sf(hcdnGageTable, coords = c("Longitude", "Latitude"), crs = 4326)
hcdnGageTableSF = hcdnGageTableSF %>% st_transform(3857)


# cut data to 1950-2023
AllPeaks = AllPeaks[site_no %in% sites_long_rec_vec]
AllPeaks = AllPeaks[, year:=year(Date)]
AllPeaks = AllPeaks[year>=1950]
AllPeaks = AllPeaks[year<2024]


## find top n% peaks
TopPeaks = data.table()

for(i in sites_long_rec_vec){
  site_percent_ = data.table()
  site_percent_ = AllPeaks[site_no == i]
  # select top n% of peaks here
  n <- 10
  site_top_percent = site_percent_[site_percent_$Flow > quantile(site_percent_$Flow,prob=1-n/100),]
  TopPeaks = rbind(TopPeaks,site_top_percent)
  # print(i)
}

## cox-lewis analysis
TopPeaks[ , Year:=year(Date)]

ztable_hcdn = data.table()
for(i in sites_long_rec_vec){
  zcalc = data.table()
  zcalc = TopPeaks[site_no == i]
  tm = min(zcalc$Year) + ((max(zcalc$Year)-min(zcalc$Year))/2) # midpoint year
  tl = max(zcalc$Year) - min(zcalc$Year) # total length in years
  zcalc = zcalc[ , year_dif:= zcalc$Year - tm]
  zcalc = zcalc[ , numerator:= sum(zcalc$year_dif) / nrow(zcalc)]
  zcalc = zcalc[ , denominator:= tl/(sqrt(12*nrow(zcalc)))]
  zcalc = zcalc[ , cox_lewis_q:= zcalc$numerator / zcalc$denominator]
  zcalc = zcalc[ , p_q:= 2*pt(-abs(cox_lewis_q),df=nrow(zcalc)-1)]
  ztable_hcdn = rbind(ztable_hcdn,zcalc)
}
cox_lewis_table_Q = unique(ztable_hcdn, by = 'site_no')
setorder(cox_lewis_table_Q, by = 'site_no')
cox_lewis_table_Q = subset(cox_lewis_table_Q, select = -c(Date, Flow, Flow_cd, 
                                                          peaks_tf,
                                                          Year, year_dif, numerator, denominator) )

hcdn_more_info = hcdn_more_info[ site_no %in% sites_long_rec_vec]
cox_lewis_Q_geom = cox_lewis_table_Q[ , ':='(Longitude=hcdn_more_info$dec_long_va, Latitude = hcdn_more_info$dec_lat_va)]
cox_lewis_Q_geom = st_as_sf(cox_lewis_Q_geom, coords = c("Longitude", "Latitude"), crs = 4326)
cox_lewis_Q_geom = cox_lewis_Q_geom %>% st_transform(3857)

# rough draft of plot
# final one later, with better basemaps
ggplot() +
  # geom_raster(data = NE_elev, aes(x = x, y = y, fill = elevation), alpha = 0.5) +
  # scale_fill_gradientn(colours = c('lemonchiffon', 'khaki1', 'khaki2','lightsalmon1', 'lightsalmon2', 'tomato1', 'tomato2'))+
  geom_sf(data = NE_states$geometry, fill = 'white', lwd = 1)+
  geom_sf(data = cox_lewis_Q_geom, aes(geometry = geometry, col = cox_lewis_q, size = p_q<0.05)) +
  scale_size_manual(values=c(1.5,4))+
  scale_color_gradientn(colours = c('blue', 'red'),  
                        limits = c(-3, 3), oob = scales::squish)+
  labs(col = "Z", size = "Statistically Significant (p < 0.05)")+ # add title
  ggtitle('Extreme Discharge Cox-Lewis Z Score')+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'Gray85', colour = NA))


# getting Q2 data (determined via Log-Pearson Type III, from Evan Dethier)
Qtable = fread("Qtable.csv")
setDT(Qtable)
Qtable[ , site_no:=paste0("0", Qtable$site_no)]

#### Circular stats ####
## Converting to circular space, finding mean date, R magnitude 
TopPrcp = TopPrcp
TopPrcp[ , Year:=year(Date)]

## Precipitation
# get day of year
TopPrcp[ , y_day:=yday(Date)]
TopPeaks[ , y_day:=yday(Date)]

# convert to degrees 
TopPrcp[ , angle:=360 * (y_day/365)]
TopPrcp[ , angle_rad:=angle * pi/180]
TopPeaks[ , angle:=360 * (y_day/365)]
TopPeaks[ , angle_rad:=angle * pi/180]

# find statistics of interest
prcp_circular = data.table()
for(i in unique(TopPrcp$Station_ID)){
  station_select = TopPrcp[Station_ID == i]
  station_select[ , sine:=sin(angle_rad)]
  station_select[ , cosine:=cos(angle_rad)]
  
  # for the entire record 1950-2021
  mean_dir=atan2(mean(station_select$sine), mean(station_select$cosine)) ## FIX HERE
  mean_dir_deg =mean_dir * (180/pi)
  if(mean_dir_deg<0) {mean_dir_deg = mean_dir_deg + (360)}
  station_select[ , mean_dir_rad:=mean_dir]
  station_select[ , mean_dir_deg:=mean_dir_deg]
  station_select[ , cosine_diff:= cos(angle_rad - mean_dir)]
  station_select[ , R_mean:=sqrt(((mean(cosine))^2)+((mean(sine))^2))]
  station_select = unique(station_select, by = 'Station_ID')
  prcp_circular = rbind(prcp_circular,station_select)
}

# getting rid of irrelevant columns
prcp_circular[ , c('Date', 'Value', 'V5', 'V6', 'V7', 'V8', 'Year', 'y_day', 'angle', 'angle_rad',
                   'sine', 'cosine', 'mean_dir_rad', 'cosine_diff'):= NULL]

# adding Lat and Long for QGIS stuff
prcp_circular = prcp_circular %>% left_join(GHCNdStationsNE1901, by = "Station_ID")

# compute the statistics separately pre and post changepoint

prcp_circular_cpt = data.table()
for(i in unique(cptTable$Station_ID)){
  station_select = TopPrcp[Station_ID == i]
  setDT(station_select)
  station_select[ , sine:=sin(angle_rad)]
  station_select[ , cosine:=cos(angle_rad)]
  cpt_year = cptTable[Station_ID == i, change_year] 
  
  # pre changepoint
  mean_dir_pre=atan2((mean(station_select[Year<=cpt_year, sine])),mean(station_select[Year<=cpt_year, cosine]))
  mean_dir_deg_pre =mean_dir_pre * (180/pi)
  if(mean_dir_deg_pre<0) {mean_dir_deg_pre = mean_dir_deg_pre + (360)}
  station_select[ , mean_dir_rad_pre:=mean_dir_pre]
  station_select[ , mean_dir_deg_pre:=mean_dir_deg_pre]
  station_select[ , cosine_diff_pre:= cos(angle_rad - mean_dir_pre)]
  station_select[ , R_mean_pre:=sqrt(((mean(station_select[Year<=cpt_year, cosine]))^2)+
                                       ((mean(station_select[Year<=cpt_year, sine]))^2))]
  
  # post changepoint
  mean_dir_post=atan2((mean(station_select[Year>cpt_year, sine])),mean(station_select[Year>cpt_year, cosine]))
  mean_dir_deg_post =mean_dir_post * (180/pi)
  if(mean_dir_deg_post<0) {mean_dir_deg_post = mean_dir_deg_post + (360)}
  station_select[ , mean_dir_rad_post:=mean_dir_post]
  station_select[ , mean_dir_deg_post:=mean_dir_deg_post]
  station_select[ , cosine_diff_post:= cos(angle_rad - mean_dir_post)]
  station_select[ , R_mean_post:=sqrt(((mean(station_select[Year>cpt_year, cosine]))^2)+
                                        ((mean(station_select[Year>cpt_year, sine]))^2))]
  
  station_select = unique(station_select, by = 'Station_ID')
  prcp_circular_cpt = rbind(prcp_circular_cpt,station_select)
}

# getting rid of irrelevant columns
prcp_circular_cpt[ , c('Date', 'Value', 'V5', 'V6', 'V7', 'V8', 'Year', 'y_day', 'angle', 'angle_rad',
                       'sine', 'cosine', 'mean_dir_rad_pre', 'cosine_diff_pre', 'mean_dir_rad_post', 
                       'cosine_diff_post'):= NULL]
prcp_circular_cpt[ , delta_R:=R_mean_post - R_mean_pre]
prcp_circular_cpt[ , delta_deg:=mean_dir_deg_post - mean_dir_deg_pre]

# adding lat/lon for GIS stuff
prcp_circular_cpt = prcp_circular_cpt %>% left_join(GHCNdStationsNE1901, by = "Station_ID")

## Discharge 
# find statistics of interest
q_circular = data.table()

# for the entire record 1950-2021
for(i in unique(TopPeaks$site_no)){
  site_select = TopPeaks[site_no == i]
  site_select[ , sine:=sin(angle_rad)]
  site_select[ , cosine:=cos(angle_rad)]
  
  # for the entire record 1950-2021
  mean_dir=atan2(mean(site_select$sine), mean(site_select$cosine)) ## FIX HERE
  mean_dir_deg =mean_dir * (180/pi)
  if(mean_dir_deg<0) {mean_dir_deg = mean_dir_deg + (360)}
  site_select[ , mean_dir_rad:=mean_dir]
  site_select[ , mean_dir_deg:=mean_dir_deg]
  site_select[ , cosine_diff:= cos(angle_rad - mean_dir)]
  site_select[ , R_mean:=sqrt(((mean(cosine))^2)+((mean(sine))^2))]   
  site_select = unique(site_select, by = 'site_no')
  q_circular = rbind(q_circular,site_select)
}

# getting rid of irrelevant columns
q_circular[ , c('Date', 'Flow', 'Flow_cd', 'peaks_tf', 'year', 'y_day', 'angle', 'angle_rad',
                'sine', 'cosine', 'mean_dir_rad', 'cosine_diff'):= NULL]

# adding Lat and Long for QGIS stuff
q_circular = q_circular %>% left_join(hcdnGageTable, by = "site_no")


# pre and post changepoint 


q_circular_cpt = data.table()
for(i in hcdnGageTable$site_no){
  # print(i)
  site_select = TopPeaks[site_no == i]
  setDT(site_select)
  site_select[ , sine:=sin(angle_rad)]
  site_select[ , cosine:=cos(angle_rad)]
  cpt_year = 1996 
  
  # pre changepoint
  mean_dir_pre=atan2((mean(site_select[year<=cpt_year, sine])),mean(site_select[year<=cpt_year, cosine]))
  mean_dir_deg_pre =mean_dir_pre * (180/pi)
  if(mean_dir_deg_pre<0) {mean_dir_deg_pre = mean_dir_deg_pre + (360)}
  site_select[ , mean_dir_rad_pre:=mean_dir_pre]
  site_select[ , mean_dir_deg_pre:=mean_dir_deg_pre]
  site_select[ , cosine_diff_pre:= cos(angle_rad - mean_dir_pre)]
  site_select[ , R_mean_pre:=sqrt(((mean(site_select[year<=cpt_year, cosine]))^2)+
                                    ((mean(site_select[year<=cpt_year, sine]))^2))]
  
  # post changepoint
  mean_dir_post=atan2((mean(site_select[year>cpt_year, sine])),mean(site_select[year>cpt_year, cosine]))
  mean_dir_deg_post =mean_dir_post * (180/pi)
  if(mean_dir_deg_post<0) {mean_dir_deg_post = mean_dir_deg_post + (360)}
  site_select[ , mean_dir_rad_post:=mean_dir_post]
  site_select[ , mean_dir_deg_post:=mean_dir_deg_post]
  site_select[ , cosine_diff_post:= cos(angle_rad - mean_dir_post)]
  site_select[ , R_mean_post:=sqrt(((mean(site_select[year>cpt_year, cosine]))^2)+
                                     ((mean(site_select[year>cpt_year, sine]))^2))]
  site_select = unique(site_select, by = 'site_no')
  q_circular_cpt = rbind(q_circular_cpt,site_select)
}

# getting rid of irrelevant columns
q_circular_cpt[ , c('Date', 'Flow', 'Flow_cd', 'peaks_tf', 'year', 'y_day', 'angle', 'angle_rad',
                    'sine', 'cosine', 'mean_dir_rad_pre', 'cosine_diff_pre', 'mean_dir_rad_post', 
                    'cosine_diff_post'):= NULL]
q_circular_cpt[ , delta_R:=R_mean_post - R_mean_pre]
q_circular_cpt[ , delta_deg:=mean_dir_deg_post - mean_dir_deg_pre]

# adding Lat and Long for QGIS stuff
q_circular_cpt = q_circular_cpt %>% left_join(hcdnGageTable, by = "site_no")
q_circular_cpt[ , c('YearsOfRecord'):=NULL]



# reproject so that we can find distances in terms of meters
prcp_circ = prcp_circular
prcp_circ = st_as_sf(prcp_circ, coords = c("Longitude", "Latitude"), 
                     crs = 4326)
prcp_circ = prcp_circ %>% st_transform(3857)

# prcp_cpt_circ = fread('prcp_circular_cpt.csv')
prcp_cpt_circ = prcp_circular_cpt
prcp_cpt_circ = st_as_sf(prcp_cpt_circ, coords = c("Longitude", "Latitude"), 
                         crs = 4326)
prcp_cpt_circ = prcp_cpt_circ %>% st_transform(3857)

# q_circ = fread('q_circular.csv')
q_circ = q_circular
# q_circ[ , site_no:=paste0("0", q_circ$site_no)]

q_circ = st_as_sf(q_circ, coords = c("Longitude", "Latitude"), 
                  crs = 4326)
q_circ = q_circ %>% st_transform(3857)

# q_cpt_circ = fread('q_circular_cpt.csv')
q_cpt_circ = q_circular_cpt
# q_cpt_circ[ , site_no:=paste0("0", q_cpt_circ$site_no)]
q_cpt_circ = st_as_sf(q_cpt_circ, coords = c("Longitude", "Latitude"), 
                      crs = 4326)
q_cpt_circ = q_cpt_circ %>% st_transform(3857)



# Things I need
# site_no/Station_ID, latitude, distance to coast, drainage area (for Q), altitude, R, vector mean
# create a single coastal polyline, to find min distance between stations/sites and 
coastline_polyline = st_union(oceans_sf1)
coastline_polyline = st_as_sf(coastline_polyline)

# for prcp_circ
# find distance to coastline
prcp_circ = prcp_circ %>% 
  mutate(dist_km = as.numeric((st_distance(prcp_circ, coastline_polyline))/1000))

# drop projected coords, add in Lat and Long
prcp_circ = st_drop_geometry(prcp_circ)
# adding Lat and Long
setorder(prcp_circ, by = 'Station_ID')
setorder(GHCNdStationsNE1901, by = 'Station_ID')
prcp_circ = prcp_circ %>% mutate(Latitude = GHCNdStationsNE1901$Latitude, Longitude = GHCNdStationsNE1901$Longitude)

# for prcp_cpt_circ
prcp_cpt_circ = prcp_cpt_circ %>% 
  mutate(dist_km = as.numeric((st_distance(prcp_cpt_circ, coastline_polyline))/1000))

# drop projected coords, add in Lat and Long
prcp_cpt_circ = st_drop_geometry(prcp_cpt_circ)
# adding Lat and Long
prcp_cpt_circ = prcp_cpt_circ %>%
  left_join(GHCNdStationsNE1901, by="Station_ID")

prcp_cpt_circ <- prcp_cpt_circ %>% 
  rename_at(
    vars(ends_with(".x")),
    ~str_replace(., "\\..$","")
  ) %>% 
  select_at(
    vars(-ends_with(".y"))
  )



# for q_circ
# find distance to coastline
q_circ = q_circ %>% 
  mutate(dist_km = as.numeric((st_distance(q_circ, coastline_polyline))/1000))

# drop projected coords, add in Lat and Long
q_circ = st_drop_geometry(q_circ)
# adding Lat and Long
setorder(q_circ, by = 'site_no')
q_circ = q_circ %>% mutate(Latitude = hcdn_more_info$dec_lat_va, Longitude = hcdn_more_info$dec_long_va
                           , Elevation = (hcdn_more_info$alt_va)/3.281, Drainage_area =(hcdn_more_info$drain_area_va)*2.59 )

# for q_cpt_circ
q_cpt_circ = q_cpt_circ %>% 
  mutate(dist_km = as.numeric((st_distance(q_cpt_circ, coastline_polyline))/1000))

# drop projected coords, add in Lat and Long
q_cpt_circ = st_drop_geometry(q_cpt_circ)
# adding Lat and Long
setorder(q_cpt_circ, by = 'site_no')
hcdn_more_info_circ_cpt = hcdn_more_info[site_no %in% q_cpt_circ$site_no]
setorder(hcdn_more_info_circ_cpt, by = 'site_no')
q_cpt_circ = q_cpt_circ %>% mutate(Latitude = hcdn_more_info_circ_cpt$dec_lat_va, 
                                   Longitude = hcdn_more_info_circ_cpt$dec_long_va,
                                   Elevation = (hcdn_more_info_circ_cpt$alt_va)/3.281, 
                                   Drainage_area =(hcdn_more_info_circ_cpt$drain_area_va)*2.59 )

# Set up for rose diagrams
TopPeaks[ , Month:=month(Date)]

# getting state info for TopPeaks and TopPrcp
TopPeaks_more = TopPeaks %>% left_join(sites_1950, by = 'site_no')
TopPeaks_more = TopPeaks_more %>% left_join(hcdnGageTable, by = 'site_no')

TopPrcp_more = TopPrcp %>% left_join(GHCNdStationsNE1901, by = 'Station_ID')

# testing interaction between elevation and drainage area
elev_area_interaction = lm(formula(q_circ$Drainage_area~q_circ$Elevation))
print(elev_area_interaction)
print(summary(elev_area_interaction))
# no interaction

## Multiple regressions
# all pcpt vs latitude, distance from coast, elevation
## Multiple Regression -- circular stats

## prcp
# R (mean resultant length)
multipleRegressionPrcp_R = lm(formula(prcp_circ$R_mean ~ prcp_circ$dist_km+ prcp_circ$Latitude + 
                                        prcp_circ$Elevation))
print(multipleRegressionPrcp_R)
print(summary(multipleRegressionPrcp_R))



# prcp_pct_circ
# pre
multipleRegressionPrcp_R_pre = lm(formula(prcp_cpt_circ$R_mean_pre ~ prcp_cpt_circ$dist_km+ prcp_cpt_circ$Latitude + 
                                            prcp_cpt_circ$Elevation))
print(multipleRegressionPrcp_R_pre)
print(summary(multipleRegressionPrcp_R_pre))

# post
multipleRegressionPrcp_R_post = lm(formula(prcp_cpt_circ$R_mean_post ~ prcp_cpt_circ$dist_km+ prcp_cpt_circ$Latitude + 
                                             prcp_cpt_circ$Elevation))
print(multipleRegressionPrcp_R_post)
print(summary(multipleRegressionPrcp_R_post))


# q_circ
# R (mean resultant length)
multipleRegressionQ_R = lm(formula(q_circ$R_mean ~ q_circ$dist_km+ q_circ$Latitude + q_circ$Drainage_area +
                                     q_circ$Elevation))
print(multipleRegressionQ_R)
print(summary(multipleRegressionQ_R))


# q_cpt_circ
# R (mean resultant length)
# pre
multipleRegressionQ_R_pre = lm(formula(q_cpt_circ$R_mean_pre ~ q_cpt_circ$dist_km+ q_cpt_circ$Latitude + 
                                         q_cpt_circ$Drainage_area + q_cpt_circ$Elevation))
print(multipleRegressionQ_R_pre)
print(summary(multipleRegressionQ_R_pre))

# post
multipleRegressionQ_R_post = lm(formula(q_cpt_circ$R_mean_post ~ q_cpt_circ$dist_km+ q_cpt_circ$Latitude + 
                                          q_cpt_circ$Drainage_area + q_cpt_circ$Elevation))
print(multipleRegressionQ_R_post)
print(summary(multipleRegressionQ_R_post))




#### Cox-Lewis seasonal breakdown ####
# add month column
TopPeaks[ , Month:=month(Date)]
TopPrcp[ , Month:=month(Date)]

## Prcp

zTablePrcp = data.table()
for(i in unique(TopPrcp$Station_ID)) {
  print(i)
  zcalc = data.table()
  zcalc = TopPrcp[Station_ID == i]
  tm = min(zcalc$Year) + ((max(zcalc$Year)-min(zcalc$Year))/2) # midpoint year
  tl = max(zcalc$Year) - min(zcalc$Year) # total length in years
  
  # entire year
  zcalc = zcalc[ , year_dif:= zcalc$Year - tm]
  zcalc = zcalc[ , numerator:= sum(zcalc$year_dif) / nrow(zcalc)]
  zcalc = zcalc[ , denominator:= tl/(sqrt(12*nrow(zcalc)))]
  zcalc = zcalc[ , cox_lewis:= zcalc$numerator / zcalc$denominator]
  zcalc = zcalc[ , p:= 2*pt(-abs(cox_lewis),df=nrow(zcalc)-1)]
  
  # first half of year
  zcalc_H1 = zcalc[(Month) < 6 | (Month) > 10] # Nov-May
  zcalc_H1[ , year_dif:= zcalc_H1$Year - tm]
  numerator_H1 = sum(zcalc_H1$year_dif) / nrow(zcalc_H1)
  denominator_H1 = tl/(sqrt(12*nrow(zcalc_H1))) 
  zcalc[ , cox_lewis_H1:= numerator_H1 / denominator_H1]
  zcalc = zcalc[ , p_H1:= 2*pt(-abs(cox_lewis_H1),df=nrow(zcalc_H1)-1)]
  
  # second half of year
  zcalc_H2 = zcalc[(Month) > 5 & (Month) < 11] # June-October
  zcalc_H2[ , year_dif:= zcalc_H2$Year - tm]
  numerator_H2 = sum(zcalc_H2$year_dif) / nrow(zcalc_H2)
  denominator_H2 = tl/(sqrt(12*nrow(zcalc_H2))) 
  zcalc[ , cox_lewis_H2:= numerator_H2 / denominator_H2]
  zcalc = zcalc[ , p_H2:= 2*pt(-abs(cox_lewis_H2),df=nrow(zcalc_H2)-1)]
  
  # DJF
  zcalcDJF = zcalc[(Month) < 3 | (Month) == 12]
  zcalcDJF[ , year_dif:= zcalcDJF$Year - tm]
  numeratorDJF = sum(zcalcDJF$year_dif) / nrow(zcalcDJF)
  denominatorDJF = tl/(sqrt(12*nrow(zcalcDJF))) 
  zcalc[ , cox_lewisDJF:= numeratorDJF / denominatorDJF]
  zcalc = zcalc[ , pDJF:= 2*pt(-abs(cox_lewisDJF),df=nrow(zcalcDJF)-1)]
  
  # MAM
  zcalcMAM = zcalc[(Month) > 2 & (Month) < 6]
  zcalcMAM[ , year_dif:= zcalcMAM$Year - tm]
  numeratorMAM = sum(zcalcMAM$year_dif) / nrow(zcalcMAM)
  denominatorMAM = tl/(sqrt(12*nrow(zcalcMAM))) 
  zcalc[ , cox_lewisMAM:= numeratorMAM / denominatorMAM]
  zcalc = zcalc[ , pMAM:= 2*pt(-abs(cox_lewisMAM),df=nrow(zcalcMAM)-1)]
  
  # JJA
  zcalcJJA = zcalc[(Month) > 5 & (Month) < 9]
  zcalcJJA[ , year_dif:= zcalcJJA$Year - tm]
  numeratorJJA = sum(zcalcJJA$year_dif) / nrow(zcalcJJA)
  denominatorJJA = tl/(sqrt(12*nrow(zcalcJJA))) 
  zcalc[ , cox_lewisJJA:= numeratorJJA / denominatorJJA]
  zcalc = zcalc[ , pJJA:= 2*pt(-abs(cox_lewisJJA),df=nrow(zcalcJJA)-1)]
  
  # SON
  zcalcSON = zcalc[(Month) > 8 & (Month) < 12]
  zcalcSON[ , year_dif:= zcalcSON$Year - tm]
  numeratorSON = sum(zcalcSON$year_dif) / nrow(zcalcSON)
  denominatorSON = tl/(sqrt(12*nrow(zcalcSON))) 
  zcalc[ , cox_lewisSON:= numeratorSON / denominatorSON]
  zcalc = zcalc[ , pSON:= 2*pt(-abs(cox_lewisSON),df=nrow(zcalcSON)-1)]
  
  # rbind
  zTablePrcp = rbind(zTablePrcp,zcalc)
}
cox_lewis_table_prcp = unique(zTablePrcp, by = 'Station_ID')
setorder(cox_lewis_table_prcp, by = 'Station_ID')
# prcp_circ = fread('prcp_circ.csv')
# prcp_circ[, V1:=NULL]
setorder(prcp_circ, by = 'Station_ID')

prcp_circ_cox = prcp_circ %>% mutate(cox_lewis = cox_lewis_table_prcp$cox_lewis, 
                                     cox_lewis_H1 = cox_lewis_table_prcp$cox_lewis_H1,
                                     cox_lewis_H2 = cox_lewis_table_prcp$cox_lewis_H2,
                                     cox_lewisDJF = cox_lewis_table_prcp$cox_lewisDJF,
                                     cox_lewisMAM = cox_lewis_table_prcp$cox_lewisMAM,
                                     cox_lewisJJA = cox_lewis_table_prcp$cox_lewisJJA,
                                     cox_lewisSON = cox_lewis_table_prcp$cox_lewisSON
)

## Q Peaks

zTableQ = data.table()
for(i in unique(TopPeaks$site_no)) {
  zcalc = data.table()
  zcalc = TopPeaks[site_no == i]
  tm = min(zcalc$year) + ((max(zcalc$year)-min(zcalc$year))/2) # midpoint year
  tl = max(zcalc$year) - min(zcalc$year) # total length in years
  
  # entire year
  zcalc = zcalc[ , year_dif:= zcalc$year - tm]
  zcalc = zcalc[ , numerator:= sum(zcalc$year_dif) / nrow(zcalc)]
  zcalc = zcalc[ , denominator:= tl/(sqrt(12*nrow(zcalc)))]
  zcalc = zcalc[ , cox_lewis:= zcalc$numerator / zcalc$denominator]
  zcalc = zcalc[ , p:= 2*pt(-abs(cox_lewis),df=nrow(zcalc)-1)]
  
  # first half of year
  zcalc_H1 = zcalc[(Month) < 6 | (Month) > 10] # Nov-May (wet season)
  zcalc_H1[ , year_dif:= zcalc_H1$year - tm]
  numerator_H1 = sum(zcalc_H1$year_dif) / nrow(zcalc_H1)
  denominator_H1 = tl/(sqrt(12*nrow(zcalc_H1))) 
  zcalc[ , cox_lewis_H1:= numerator_H1 / denominator_H1]
  zcalc = zcalc[ , p_H1:= 2*pt(-abs(cox_lewis_H1),df=nrow(zcalc_H1)-1)]
  
  # second half of year
  zcalc_H2 = zcalc[(Month) > 5 & (Month) < 11] # May-October
  zcalc_H2[ , year_dif:= zcalc_H2$year - tm]
  numerator_H2 = sum(zcalc_H2$year_dif) / nrow(zcalc_H2)
  denominator_H2 = tl/(sqrt(12*nrow(zcalc_H2))) 
  zcalc[ , cox_lewis_H2:= numerator_H2 / denominator_H2]
  zcalc = zcalc[ , p_H2:= 2*pt(-abs(cox_lewis_H2),df=nrow(zcalc_H2)-1)]
  
  # DJF
  zcalcDJF = zcalc[(Month) < 3 | (Month) == 12]
  zcalcDJF[ , year_dif:= zcalcDJF$year - tm]
  numeratorDJF = sum(zcalcDJF$year_dif) / nrow(zcalcDJF)
  denominatorDJF = tl/(sqrt(12*nrow(zcalcDJF))) 
  zcalc[ , cox_lewisDJF:= numeratorDJF / denominatorDJF]
  zcalc = zcalc[ , pDJF:= 2*pt(-abs(cox_lewisDJF),df=nrow(zcalcDJF)-1)]
  
  # MAM
  zcalcMAM = zcalc[(Month) > 2 & (Month) < 6]
  zcalcMAM[ , year_dif:= zcalcMAM$year - tm]
  numeratorMAM = sum(zcalcMAM$year_dif) / nrow(zcalcMAM)
  denominatorMAM = tl/(sqrt(12*nrow(zcalcMAM))) 
  zcalc[ , cox_lewisMAM:= numeratorMAM / denominatorMAM]
  zcalc = zcalc[ , pMAM:= 2*pt(-abs(cox_lewisMAM),df=nrow(zcalcMAM)-1)]
  
  # JJA
  zcalcJJA = zcalc[(Month) > 5 & (Month) < 9]
  zcalcJJA[ , year_dif:= zcalcJJA$year - tm]
  numeratorJJA = sum(zcalcJJA$year_dif) / nrow(zcalcJJA)
  denominatorJJA = tl/(sqrt(12*nrow(zcalcJJA))) 
  zcalc[ , cox_lewisJJA:= numeratorJJA / denominatorJJA]
  zcalc = zcalc[ , pJJA:= 2*pt(-abs(cox_lewisJJA),df=nrow(zcalcJJA)-1)]
  
  # SON
  zcalcSON = zcalc[(Month) > 8 & (Month) < 12]
  zcalcSON[ , year_dif:= zcalcSON$year - tm]
  numeratorSON = sum(zcalcSON$year_dif) / nrow(zcalcSON)
  denominatorSON = tl/(sqrt(12*nrow(zcalcSON))) 
  zcalc[ , cox_lewisSON:= numeratorSON / denominatorSON]
  zcalc = zcalc[ , pSON:= 2*pt(-abs(cox_lewisSON),df=nrow(zcalcSON)-1)]
  
  # rbind
  zTableQ = rbind(zTableQ,zcalc)
}
cox_lewis_table_Q = unique(zTableQ, by = 'site_no')
setorder(cox_lewis_table_Q, by = 'site_no')
setorder(q_circ, by = 'site_no')

q_circ_cox = q_circ %>% mutate(cox_lewis = cox_lewis_table_Q$cox_lewis, 
                               cox_lewis_H1 = cox_lewis_table_Q$cox_lewis_H1,
                               cox_lewis_H2 = cox_lewis_table_Q$cox_lewis_H2,
                               cox_lewisDJF = cox_lewis_table_Q$cox_lewisDJF,
                               cox_lewisMAM = cox_lewis_table_Q$cox_lewisMAM,
                               cox_lewisJJA = cox_lewis_table_Q$cox_lewisJJA,
                               cox_lewisSON = cox_lewis_table_Q$cox_lewisSON
)
q_circ_cox_sf = st_as_sf(q_circ_cox, coords = c("Longitude", "Latitude"), 
                         crs = 4326)


ggplot() +
  # geom_raster(data = NE_elev, aes(x = x, y = y, fill = elevation), alpha = 0.5) +
  # scale_fill_gradientn(colours = c('lemonchiffon', 'khaki1', 'khaki2','lightsalmon1', 'lightsalmon2', 'tomato1', 'tomato2'))+
  geom_sf(data = NE_states$geometry, fill = 'white', lwd = 1)+
  geom_sf(data = q_circ_cox_sf[q_circ_cox_sf$cox_lewis_H2 > 2, ], aes(geometry = geometry, col = cox_lewis_H2, size = Drainage_area)) +
  geom_sf(data = q_circ_cox_sf[q_circ_cox_sf$cox_lewis_H2 < -2, ], aes(geometry = geometry, col = cox_lewis_H2, size = Drainage_area)) +
  geom_sf(data = q_circ_cox_sf[q_circ_cox_sf$cox_lewis_H2 < 2, ], aes(geometry = geometry, col = cox_lewis_H2, size = Drainage_area), pch = 21, fill = NA)+
  # scale_size_manual(values=c(1.5,4))+
  scale_color_gradientn(colours = c('blue', 'red'),  
                        limits = c(-1, 3), oob = scales::squish)+
  labs(col = "Z", size = bquote('Drainage area '(km^2)))+ # add title
  ggtitle('Extreme Discharge Cox-Lewis Z Score, Warm Season (Jun-Oct)')+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'Gray85', colour = NA))
# ggsave(filename = 'H2_cox_Q.jpg')


## multiple regression-- Cox-Lewis
# Discharge
q_cox_all = lm(formula(q_circ_cox$cox_lewis ~ q_circ_cox$dist_km+ q_circ_cox$Latitude + q_circ_cox$Drainage_area + 
                         q_circ_cox$Elevation))
q_cox_H1 = lm(formula(q_circ_cox$cox_lewis_H1 ~ q_circ_cox$dist_km+ q_circ_cox$Latitude + q_circ_cox$Drainage_area + 
                        q_circ_cox$Elevation))
q_cox_H2 = lm(formula(q_circ_cox$cox_lewis_H2 ~ q_circ_cox$dist_km+ q_circ_cox$Latitude + q_circ_cox$Drainage_area + 
                        q_circ_cox$Elevation))


## results
print(summary(q_cox_all))
print(summary(q_cox_H1))
print(summary(q_cox_H2))

#### Quadrant Analysis ####

## Breaking stations into coastal vs inland
Coastal_stations = prcp_circ_cox[dist_km <=150]
Inland_stations = prcp_circ_cox[dist_km > 150]

## Breaking data into four parks for further Rose Diagram breakdown
## Coastal, North (<150km from Coast, North of 42 Latitude)
Coastal_N_sites = q_circ_cox[Latitude > 42 & dist_km <= 150]
Coastal_N_stations = prcp_circ_cox[Latitude > 42 & dist_km <= 150]

## Coastal, South
Coastal_S_sites = q_circ_cox[Latitude <= 42 & dist_km <= 150]
Coastal_S_stations = prcp_circ_cox[Latitude <= 42 & dist_km <= 150]

## Inland, North
Inland_N_sites = q_circ_cox[Latitude > 42 & dist_km > 150]
Inland_N_stations = prcp_circ_cox[Latitude > 42 & dist_km > 150]

## Inland, South
Inland_S_sites = q_circ_cox[Latitude <= 42 & dist_km > 150]
Inland_S_stations = prcp_circ_cox[Latitude <+ 42 & dist_km > 150]

# finding prcp changpoints for each of the 4 quadrants
# summing the top prcp values by year and station

quadrantCpt = function(quadrant){
  quadrant_stations = quadrant
  # find sum of EP per year per station
  sumTopPrcp = data.table()
  years1950 = c(1950:2021)
  for(i in quadrant_stations$Station_ID){ 
    #print(i)
    station_select = data.table()
    station_select = TopPrcp[Station_ID == i]
    year_sum_table = data.table()
    for(j in years1950){
      #print(j)
      year_select = station_select[Year == j]
      year_select = year_select[ , sumTopPrcp :=sum(Value)]
      year_select = unique(year_select, by = "sumTopPrcp")
      year_select = year_select[ , c("Value", "V5", "V6", "V7", "V8"):= NULL]
      year_sum_table = rbind(year_sum_table, year_select)
    }
    sumTopPrcp = rbind(sumTopPrcp, year_sum_table)  
  }
  
  
  sumTopPrcpORID =  merge(sumTopPrcp, GHCNdStationsNE1901_ORID[, .(Station_ID, ORID)], by = "Station_ID", all.x = TRUE)
  
  
  # finding average EP by year, by grid cell
  
  meanAnnGridPrcp = data.table()
  for(i in years1950){
    #print(i)
    year_select = data.table()
    year_select = sumTopPrcpORID[Year == i]
    grid_mean_table = data.table()
    for(j in unique(sumTopPrcpORID$ORID)){
      #print(j)
      grid_select = year_select[ORID == j]
      grid_select = grid_select[ , meanGridPrcp :=mean(sumTopPrcp)]
      grid_select = unique(grid_select, by = "meanGridPrcp")
      grid_mean_table = rbind(grid_mean_table, grid_select)
    }
    meanAnnGridPrcp = rbind(meanAnnGridPrcp, grid_mean_table)  
  }
  
  # taking the average of all grid averages
  year_vs_prcp = data.table()
  for (i in years1950) {
    year_select = data.table()
    year_select = meanAnnGridPrcp[Year == i]
    year_select = year_select[ , meanTotalPrcp:=mean(meanGridPrcp)]
    year_select = unique(year_select, by = "Year")
    year_vs_prcp = rbind(year_vs_prcp, year_select)
  }
  
  
  chgPcpt <- cpt.mean(year_vs_prcp$meanTotalPrcp,penalty="MBIC",pen.value=0,method="AMOC",Q=5,test.stat="Normal",class=TRUE,
                      param.estimates=TRUE,minseglen=1)
  cpt_year = cpts(chgPcpt) + 1950
  
  
  # Making a plot, incorporating changepoints (force study area 1996 cpt)
  cpt_plot = ggplot() + 
    geom_point(data = year_vs_prcp, aes(x =Year , y = meanTotalPrcp/10), color = 'black', pch = 1, lwd = 1.5) +
    geom_line(data = year_vs_prcp, aes(x =Year , y = meanTotalPrcp/10), color = 'darkgrey', lwd = 0.6) +
    # geom_smooth(data = year_vs_prcp[Year %between% c(1950,cpt_year)], aes(x = Year, y = meanTotalPrcp/10), method = lm, color = 'gold2') +
    # geom_smooth(data = year_vs_prcp[Year %between% c(cpt_year,2023)], aes(x = Year, y = meanTotalPrcp/10), method = lm, color = 'purple') +
    geom_smooth(data = year_vs_prcp[Year %between% c(1950,1996)], aes(x = Year, y = meanTotalPrcp/10), method = lm, color = 'gold2') +
    geom_smooth(data = year_vs_prcp[Year %between% c(1996,2023)], aes(x = Year, y = meanTotalPrcp/10), method = lm, color = 'purple') +
    labs(title = "Extreme precipitation trends before and after changepoint", x = "Year", y = "Extreme Precipitation (mm)")+
    theme_bw()+
    theme(axis.title = element_text(size = 8),
          plot.title = element_text(size = 10))
  
  
  p = t.test(year_vs_prcp[Year<1996, meanTotalPrcp], year_vs_prcp[Year>=2014, meanTotalPrcp], 
             alternative = "two.sided", var.equal = FALSE)$p.value
  
  regression = summary(lm(year_vs_prcp[Year>=1996, meanTotalPrcp]~year_vs_prcp[Year>=1996, Year]))
  
  
  return(cpt_plot)
  # return(p)
  # return(regression)
  
}

# # return p values
# p_IS = quadrantCpt(Inland_S_stations)
# p_IN = quadrantCpt(Inland_N_stations)
# p_CN = quadrantCpt(Coastal_N_stations)
# p_CS = quadrantCpt(Coastal_S_stations)
# 
# # see post-1996 regressions
# print(quadrantCpt(Inland_N_stations))
# print(quadrantCpt(Coastal_N_stations))
# print(quadrantCpt(Inland_S_stations))
# print(quadrantCpt(Coastal_S_stations))
# 
# # select from Coastal_N_stations, Coastal_S_stations, Inland_N_stations, Inland_S_stations
# cpt_IS = quadrantCpt(Inland_S_stations) + ggtitle("c) Inland South")
# cpt_IN = quadrantCpt(Inland_N_stations) + ggtitle("a) Inland North")
# cpt_CS = quadrantCpt(Coastal_S_stations) + ggtitle("d) Coastal South")
# cpt_CN = quadrantCpt(Coastal_N_stations) + ggtitle("b) Coastal North")
# 
# summary_quadrant_cpt <- arrangeGrob(cpt_IN, cpt_CN, cpt_IS, cpt_CS,
#                                    ncol = 2, nrow = 2,
#                                    layout_matrix = rbind(c(1, 2), c(3, 4)),
#                                    widths = c(1, 1))
# ggsave('summary_quadrant_cpt.png', plot = summary_quadrant_cpt)

## results of above cpt analysis
# Coastal_N_cpt: 1994 (before updating to 2023, was 1972)
# Coastal_S_cpt: 2004 (before updating to 2023, was 1971)
# Inland_N_cpt: 1995
# Inland_S_cpt: 1994 

# Coastal_N_stations, Coastal_N_sites, Coastal_S_stations, Costal_S_sites, Inland_N_stations, Inland_N_sites,
# Inland_S_stations, Inland_S_sites

## now, making rose diagrams pre and post changepoint for each of the four regions
# Coastal_N_stations, Coastal_N_sites, Coastal_S_stations, Costal_S_sites, Inland_N_stations, Inland_N_sites,
# Inland_S_stations, Inland_S_sites


## set up binning for normalized change pre and post changepoint
## assign a bin number to each observation
# q
TopPeaks_bin = data.table()
TopPeaks[angle>360, angle := 360] # fix dates from leap years for binning purposes
for (i in 1:24) {
  
  bin_select = data.table()
  bin_select = TopPeaks[angle >= (15*(i-1)) & angle <= (15 * i)]
  bin_select[ , Bin:=i]
  
  TopPeaks_bin = rbind(TopPeaks_bin, bin_select)
}
TopPeaks = TopPeaks_bin

# p
TopPrcp_bin = data.table()
TopPrcp[angle>360, angle := 360] # fix dates from leap years for binning purposes
for (i in 1:24) {
  
  bin_select = data.table()
  bin_select = TopPrcp[angle >= (15*(i-1)) & angle <= (15 * i)]
  bin_select[ , Bin:=i]
  
  TopPrcp_bin = rbind(TopPrcp_bin, bin_select)
}
TopPrcp = TopPrcp_bin


## find trig values for each quadrant, pre and post-changepoint, both p and q
TopPeaks[ , sine:=sin(angle_rad)]
TopPeaks[ , cosine:=cos(angle_rad)]
TopPrcp[ , sine:=sin(angle_rad)]
TopPrcp[ , cosine:=cos(angle_rad)]
TopPrcp[ , year:=Year]

# coastal south
QCSpost = TopPeaks[site_no %in% Coastal_S_sites$site_no & year > 1996]
QCSpre = TopPeaks[site_no %in% Coastal_S_sites$site_no & year <= 1996]
PCSpost = TopPrcp[Station_ID %in% Coastal_S_stations$Station_ID & Year > 1996]
PCSpre = TopPrcp[Station_ID %in% Coastal_S_stations$Station_ID & Year <= 1996]
PCS = TopPrcp[Station_ID %in% Coastal_S_stations$Station_ID]


# inland south
QISpost = TopPeaks[site_no %in% Inland_S_sites$site_no & year > 1996]
QISpre = TopPeaks[site_no %in% Inland_S_sites$site_no & year <= 1996]
PISpost = TopPrcp[Station_ID %in% Inland_S_stations$Station_ID & Year > 1996]
PISpre = TopPrcp[Station_ID %in% Inland_S_stations$Station_ID & Year <= 1996]
PIS = TopPrcp[Station_ID %in% Inland_S_stations$Station_ID]

# coastal north
QCNpost = TopPeaks[site_no %in% Coastal_N_sites$site_no & year > 1996]
QCNpre = TopPeaks[site_no %in% Coastal_N_sites$site_no & year <= 1996]
PCNpost = TopPrcp[Station_ID %in% Coastal_N_stations$Station_ID & Year > 1996]
PCNpre = TopPrcp[Station_ID %in% Coastal_N_stations$Station_ID & Year <= 1996]
PCN = TopPrcp[Station_ID %in% Coastal_N_stations$Station_ID]

# inland north
QINpost = TopPeaks[site_no %in% Inland_N_sites$site_no & year > 1996]
QINpre = TopPeaks[site_no %in% Inland_N_sites$site_no & year <= 1996]
PINpost = TopPrcp[Station_ID %in% Inland_N_stations$Station_ID & Year > 1996]
PINpre = TopPrcp[Station_ID %in% Inland_N_stations$Station_ID & Year <= 1996]
PIN = TopPrcp[Station_ID %in% Inland_N_stations$Station_ID]

# perform Rayleigh test using circular package
rayleigh_test = function(table_A){
  A_table = table_A
  rayleigh = rayleigh.test(A_table$angle_rad)
  return(rayleigh)
}
rayleigh_quadrant_cpt = rayleigh_test(QCSpost)

# make rose diagrams
RoseDiagram = function(table_A, table_B){
  A_Table = table_A
  R_A=round(sqrt(((mean(A_Table$cosine))^2)+((mean(A_Table$sine))^2)), digits = 3)
  
  mean_dir=atan2(mean(A_Table$sine), mean(A_Table$cosine))
  mean_dir_deg =mean_dir * (180/pi)
  if(mean_dir_deg<0) {mean_dir_deg = mean_dir_deg + (360)}
  theta_A = round(mean_dir_deg, digits = 0)
  
  labValues1 = c((R_A),(theta_A))
  theta = expression(theta)
  
  
  R_lab1 = (bquote(bar(R) ~'='~.(labValues1[1])~~~~~~bar(theta)~'='~.(labValues1[2])*'°'))
  
  num_years_pre = as.numeric(nrow(unique(A_Table, by = 'year')))
  
  Rose_A = ggplot() +
    geom_histogram(data=A_Table, aes(x = angle_rad, y = after_stat(count)/num_years_pre), binwidth = .3,
                   color = 'white', fill = 'blue4', linewidth = 0.1) +
    scale_x_continuous(breaks = seq(0, 360, 60)) +
    coord_polar() +
    ylab(NULL)+ xlab(R_lab1) + 
    ggtitle('Pre-changepoint')+
    theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
          plot.title = element_text(hjust = 0.5)) +
    # scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0,7)) # for QCS
    # scale_y_continuous(breaks = seq(0, 16, 4), limits = c(0,15)) # for QIS
    scale_y_continuous(breaks = seq(0, 16, 4), limits = c(0,13)) # for QIN
  # scale_y_continuous(breaks = seq(0, 16, 4), limits = c(0,9)) # for QCN
  
  
  B_Table = table_B
  R_B=round(sqrt(((mean(B_Table$cosine))^2)+((mean(B_Table$sine))^2)), digits = 3)
  
  mean_dir=atan2(mean(B_Table$sine), mean(B_Table$cosine))
  mean_dir_deg =mean_dir * (180/pi)
  if(mean_dir_deg<0) {mean_dir_deg = mean_dir_deg + (360)}
  theta_B = round(mean_dir_deg, digits = 0)
  
  labValues1 = c((R_B),(theta_B))
  theta = expression(theta)
  
  
  R_lab1 = (bquote(bar(R) ~'='~.(labValues1[1])~~~~~~bar(theta)~'='~.(labValues1[2])*'°'))
  
  num_years_post = as.numeric(nrow(unique(B_Table, by = 'year')))
  
  Rose_B = ggplot() +
    geom_histogram(data=B_Table, aes(x = angle_rad, y = after_stat(count)/num_years_post), binwidth = .3,
                   color = 'white', fill = 'blue4', linewidth = 0.1) +
    scale_x_continuous(breaks = seq(0, 360, 60)) +
    coord_polar() +
    ylab(NULL)+ xlab(R_lab1) + 
    ggtitle('Post-changepoint')+
    theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
          plot.title = element_text(hjust = 0.5))  +
    # scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0,7)) # for QCS
    # scale_y_continuous(breaks = seq(0, 16, 4), limits = c(0,15)) # for QIS
    scale_y_continuous(breaks = seq(0, 16, 4), limits = c(0,13)) # for QIN
  # scale_y_continuous(breaks = seq(0, 16, 4), limits = c(0,9)) # for QCN
  
  Rose_C = ggplot() +
    geom_histogram(data=A_Table, aes(x = angle_rad, y = after_stat(count)/num_years_post), binwidth = .3,
                   color = 'white', fill = 'blue4', linewidth = 0.1) +
    scale_x_continuous(breaks = seq(0, 360, 60)) +
    coord_polar() +
    ylab(NULL) + 
    xlab(R_lab1) + 
    theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(size = 10)
    ) +
    # scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0,7)) # for QCS
    # scale_y_continuous(breaks = seq(0, 16, 4), limits = c(0,15)) # for QIS
    scale_y_continuous(breaks = seq(0, 16, 4), limits = c(0,13)) # for QIN
  # scale_y_continuous(breaks = seq(0, 16, 4), limits = c(0,9)) # for QCN
  
  # for each bin, identify the percent of the total distribution it occupies in each region
  # pre and post-changepoint
  # also find the difference between these values
  bin_rose_q = data.table()
  for (i in 1:24) {
    pre_cpt_select = A_Table
    post_cpt_select = B_Table
    bin_select_pre = pre_cpt_select[Bin == i]
    bin_select_post = post_cpt_select[Bin == i]
    # find length of each bar pre and post changepoint, normalized by number of observations
    # then find change and percent change
    bin_stats = data.table()
    bin_stats[ ,Bin:=i]
    # bin_stats[ , freq_norm_pre := (nrow(bin_select_pre) / nrow(pre_cpt_select))*100] 
    # bin_stats[ , freq_norm_post := (nrow(bin_select_post) / nrow(pre_cpt_select))*100] 
    # bin_stats[ , freq_change := ((freq_norm_post/num_years_post) - (freq_norm_pre/num_years_pre))]
    
    bin_stats[ , freq_norm_pre := (nrow(bin_select_pre) / (num_years_pre))] 
    bin_stats[ , freq_norm_post := (nrow(bin_select_post) / (num_years_post))] 
    bin_stats[ , freq_change := (freq_norm_post - freq_norm_pre)]
    
    bin_rose_q = rbind(bin_rose_q, bin_stats)
  }
  
  bin_rose_q[freq_change > 0, sign := 'positive']
  bin_rose_q[freq_change < 0, sign := 'negative']
  
  Change = ggplot(bin_rose_q, aes(x = Bin, y = freq_change, fill = sign)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = c("positive" = "cadetblue3", "negative" = "firebrick1"))+
    coord_polar() +
    ggtitle('Change') +
    xlab(" ")+ylab(NULL)+
    guides(fill="none")+
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.ticks.x=element_blank()#remove x axis ticks
          #,axis.text.y = element_blank(),
          #axis.ticks.y = element_blank()
          , plot.margin = ggplot2::margin(0, 0, 2, 2, "pt"),
          plot.title = element_text(hjust = 0.5)
    ) +
    scale_y_continuous(breaks = seq(-10, 10, 1), limits = c(-3,3)) # for all others
  #scale_y_continuous(breaks = seq(-10, 10, 2), limits = c(-5,4)) # for IS
  
  
  Change1 = ggplot(bin_rose_q, aes(x = Bin, y = freq_change, fill = sign)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = c("positive" = "cadetblue3", "negative" = "firebrick1"))+
    coord_polar() +
    # ggtitle('Change') +
    xlab(" ")+ylab(NULL)+
    guides(fill="none")+
    theme(axis.text.x=element_blank(), #remove x axis labels
          axis.ticks.x=element_blank()#remove x axis ticks
          #,axis.text.y = element_blank(),
          #axis.ticks.y = element_blank()
          , plot.margin = ggplot2::margin(0, 0, 2, 2, "pt"),
          plot.title = element_text(hjust = 0.5)
    )
  #
  scale_y_continuous(breaks = seq(-6, 3, 2), limits = c(-5,4))
  
  
  #  Change <- Change +
  #    scale_y_continuous(breaks = seq(-4, 2, 2))
  # # 
  Rose_select = ggarrange(Rose_A, Rose_B, Change, ncol = 3)
  
  Rose_change = Change1
  
  return(Rose_select)
  # return(Rose_change)
  # return(Rose_C) # for pcpt plot
  
}

# select region here
Rose = RoseDiagram(QINpre, QINpost)
Rose

# may need to change scale limits depending on the quadrant
# annotate figure
annotation_text1 <- "a) Extreme Discharge, Inland North"
grob1 <- textGrob(annotation_text1, gp = gpar(col = "red3", fontface = "bold", fontsize = 12))

# Create the plot with annotations
Rose +
  annotation_custom(grob1, xmin = 0.5, xmax = 0.5, ymin = 1, ymax = .9) +
  # annotation_custom(grob2, xmin = 0.5, xmax = 0.5, ymin = 1, ymax = 1) +
  theme(plot.margin = margin(2, 1, 0, 1, "cm"))
ggsave('sample_rose_QIN.png')
# add month labels in powerpoint/illustrator


## summary plot (P)
Rose1 = RoseDiagram(PCN, PCN) + ggtitle("b) Coastal North") +
  theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"), plot.title = element_text(size = 10))

Rose2 = RoseDiagram(PIN, PIN) + ggtitle("a) Inland North") +
  theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"), plot.title = element_text(size = 10))

Rose3 = RoseDiagram(PIS, PIS) + ggtitle("c) Inland South") +
  theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"), plot.title = element_text(size = 10))

Rose4 = RoseDiagram(PCS, PCS) + ggtitle("d) Coastal South") +
  theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"), plot.title = element_text(size = 10))


summary_circ_P <- arrangeGrob(Rose2, Rose1, Rose3, Rose4,
                              ncol = 2, nrow = 2,
                              layout_matrix = rbind(c(1, 2), c(3, 4)),
                              widths = c(1, 1))

ggsave('summary_circ_P.png', plot = summary_circ_P)



## summary plot (Q, change)
Rose1 = RoseDiagram(QCNpre, QCNpost) + ggtitle(bquote("b) Coastal North, " ~ bar(R)[pre] == "0.431," ~ bar(R)[post] == 0.255)) +
  theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"), plot.title = element_text(size = 8))

Rose2 = RoseDiagram(QINpre, QINpost) + ggtitle(bquote("a) Inland North, " ~ bar(R)[pre] == "0.505," ~ bar(R)[post] == 0.326)) +
  theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"), plot.title = element_text(size = 8))

Rose3 = RoseDiagram(QISpre, QISpost) + ggtitle(bquote("c) Inland South, " ~ bar(R)[pre] == "0.455," ~ bar(R)[post] == 0.373)) +
  theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"), plot.title = element_text(size = 8))

Rose4 = RoseDiagram(QCSpre, QCSpost) + ggtitle(bquote("d) Coastal South, " ~ bar(R)[pre] == "0.376," ~ bar(R)[post] == 0.229)) +
  theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"), plot.title = element_text(size = 8))


summary_circ_change <- arrangeGrob(Rose2, Rose1, Rose3, Rose4,
                                   ncol = 2, nrow = 2,
                                   layout_matrix = rbind(c(1, 2), c(3, 4)),
                                   widths = c(1, 1))

ggsave('summary_circ_change.png', plot = summary_circ_change)

## More circular stats stuff below

## Calculate mean R, site by site, 
# pre and post changepoint for each of the four regions, see statistical significance.
# Q
# repeat this process for each of the 4 regions
circ_t_test = function(region){
  
  region_sites = region
  
  q_circ_fn_table = data.table()
  for (i in region_sites$site_no) {
    site_select = TopPeaks[site_no == i]
    mean_sine_pre = with(site_select, mean(sine[year <= 1996]))
    mean_cosine_pre = with(site_select, mean(cosine[year <= 1996]))
    
    mean_dir_pre=atan2(mean_sine_pre, mean_cosine_pre)
    mean_dir_deg_pre =mean_dir_pre * (180/pi)
    if(mean_dir_deg_pre<0) {mean_dir_deg_pre = mean_dir_deg_pre + (360)}
    site_select[ , mean_dir_rad_pre:=mean_dir_pre]
    site_select[ , mean_dir_deg_pre:=mean_dir_deg_pre]
    site_select[ , R_mean_pre:=sqrt(((mean_cosine_pre)^2)+((mean_sine_pre)^2))]
    
    
    mean_sine_post = with(site_select, mean(sine[year > 1996]))
    mean_cosine_post = with(site_select, mean(cosine[year > 1996]))
    
    mean_dir_post=atan2(mean_sine_post, mean_cosine_post)
    mean_dir_deg_post =mean_dir_post * (180/pi)
    if(mean_dir_deg_post<0) {mean_dir_deg_post = mean_dir_deg_post + (360)}
    site_select[ , mean_dir_rad_post:=mean_dir_post]
    site_select[ , mean_dir_deg_post:=mean_dir_deg_post]
    site_select[ , R_mean_post:=sqrt(((mean_cosine_post)^2)+((mean_sine_post)^2))]
    
    
    site_select = unique(site_select, by = 'site_no')
    q_circ_fn_table = rbind(q_circ_fn_table,site_select)
  }
  
  return(q_circ_fn_table)
  
}

q_circ_IN = circ_t_test(Inland_N_sites)
q_circ_IN[ , Region:="inland_north"]
q_circ_CN = circ_t_test(Coastal_N_sites)
q_circ_CN[ , Region:="coastal_north"]
q_circ_IS = circ_t_test(Inland_S_sites)
q_circ_IS[ , Region:="inland_south"]
q_circ_CS = circ_t_test(Coastal_S_sites)
q_circ_CS[ , Region:="coastal_south"]

q_circ_regions = data.table()

q_circ_regions = rbind(q_circ_regions,q_circ_IN)
q_circ_regions = rbind(q_circ_regions,q_circ_CN)
q_circ_regions = rbind(q_circ_regions,q_circ_IS)
q_circ_regions = rbind(q_circ_regions,q_circ_CS)

## perform T-tests for pre and post changepoint changes in R and vector mean date
# q
q_regions_test_table = data.table()
for (i in unique(q_circ_regions$Region)) {
  region_select = q_circ_regions[Region == i]
  # R
  region_select[ , R_t_test_p := t.test(region_select$R_mean_pre, region_select$R_mean_post, 
                                        alternative = "two.sided", var.equal = FALSE)$p.value]
  region_select[ , R_diff:= mean(region_select$R_mean_post) - mean(region_select$R_mean_pre)]
  # vector mean date
  region_select[ , date_t_test_p := t.test(region_select$mean_dir_deg_pre, region_select$mean_dir_deg_post, 
                                           alternative = "two.sided", var.equal = FALSE)$p.value]
  region_select[ , date_diff:= mean(region_select$mean_dir_deg_post) - mean(region_select$mean_dir_deg_pre)]
  
  region_select = unique(region_select, by = 'Region')
  q_regions_test_table = rbind(q_regions_test_table, region_select)
  
}


# the above analyses are only on the frequency of EQ events
# we need to normalize by Q2 to do any analysis on actual flow values

#### Magnitude ####
# normalize all peak flows by Q2
Qtable[ , Q2:=Qtable$`2 yr (cms)`]

TopPeaks_norm = data.table()
for (i in unique(Qtable$site_no)){
  site_select = TopPeaks[site_no == i]
  Q2_select = Qtable[site_no == i, Q2]
  site_select[ , Flow_cms:= Flow*0.028316832]
  site_select[ , Flow_norm:=Flow_cms / Q2_select]
  TopPeaks_norm = rbind(TopPeaks_norm, site_select)
}

## assign region and season columns to TopPeaks_norm table
# assigning seasons
TopPeaks_norm[ , Month:=month(Date)]
TopPeaks_norm[(Month) < 6 | (Month) > 10, Season:="Cold"]
TopPeaks_norm[(Month) > 5 & (Month) < 11, Season:="Warm"]
TopPeaks_norm = setorder(TopPeaks_norm, by = 'site_no')
q_circ_regions = setorder(q_circ_regions, by = 'site_no')

# assigning regions
TopPeaks_region = data.table()
for (i in unique(TopPeaks_norm$site_no)){
  site_select = TopPeaks_norm[site_no == i]
  region_select = q_circ_regions[site_no == i, 'Region']
  site_select[ , Region:=region_select$Region]
  TopPeaks_region = rbind(TopPeaks_region, site_select)
}


# testing to see what happens without WV
# TopPeaks_noWV = data.table()
# site_list_noWV = (q_circ[state != 'WV'])
# site_list_noWV[ , site_no:=paste0("0", site_list_noWV$site_no)]
# TopPeaks_noWV = TopPeaks[site_no%in%site_list_noWV$site_no]
# TopPeaks = TopPeaks_noWV
# 

TopPeaks = TopPeaks_region
rm(TopPeaks_region)
(TopPeaks_norm)

TopPeaksH1 = TopPeaks[Season == "Cold"]
TopPeaksH1[,Year:=paste0(TopPeaksH1$year, ".1")]
TopPeaksH2 = TopPeaks[Season == "Warm"]
TopPeaksH2[,Year:=paste0(TopPeaksH2$year, ".2")]

TopPeaks = rbind(TopPeaksH1, TopPeaksH2)
setorder(TopPeaks, by = 'site_no', "Date")
rm(TopPeaksH1)
rm(TopPeaksH2)

## Find med and max flow per region per year&season

Magnitude_table = data.table()
for (i in unique(TopPeaks$Region)){
  region_select = TopPeaks[Region == i]
  setorder(region_select, by = 'Year')
  summary_table = data.table()
  #print(i)
  for (j in unique(TopPeaks$Year)){
    
    #print(j)
    year_select = region_select[Year == j]
    year_select[ , Med_flow := median(Flow_norm)]
    year_select[ , Max_flow := max(Flow_norm)]
    year_select = unique(year_select, by = 'Max_flow')
    year_select = year_select[ ,c('Season', 'Region', 'year', 'Max_flow', 'Med_flow')]
    summary_table = rbind(summary_table, year_select)
    
  }
  Magnitude_table = rbind(Magnitude_table, summary_table)
}

## seeing significance of trend
magnitude_significance = function(region_select, season_select, outlier) {
  select_table = Magnitude_table[Region == region_select & Season == season_select & year !=outlier]
  test1 = lm(select_table$Max_flow~select_table$year)
  return (summary(test1))
}

magnitude_significance("inland_north", "Warm", 0)
## only inland_north, Warm is significant


cold_mag_IN = ggplot(data = Magnitude_table[Region == 'inland_north' & Season == 'Cold']) + 
  geom_point(aes(x =year , y = Max_flow), col = 'blue4', size = 2.9)+
  geom_smooth(aes(x =year , y = Max_flow), col = 'blue4', method=lm, alpha = 0.2) +
  theme_bw()+
  labs(x = "Year", y = "Discharge (normalized by Q2)", color = "")+
  # ylim(0,5)+
  ggtitle('a) Inland North, Nov-May')+
  theme(legend.position = "none")

warm_mag_IN = ggplot(data = Magnitude_table[Region == 'inland_north' & Season == 'Warm']) + 
  geom_point(aes(x =year , y = Max_flow), col = 'blue4', size = 2.9)+
  geom_smooth(aes(x =year , y = Max_flow), col = 'blue4', method=lm, alpha = 0.2) +
  theme_bw()+
  labs(x = "Year", y = "Discharge (normalized by Q2)", color = "")+
  # ylim(0,5)+
  ggtitle('b) Inland North, June-Oct')+
  theme(legend.position = "none")

ggarrange(cold_mag_IN, warm_mag_IN, ncol = 2)
ggsave(filename = 'magnitude_inland_north.png', width = 10, height = 3.2)



#### Other Figures (Graphs) ####

## spatially averaged year vs extreme Q
sumTopQ = data.table()
TopPeaks[ , Flow_cms := Flow*0.028316832]
years1950 = c(1950:2023)
for(i in hcdn_more_info$site_no){
  #print(i)
  site_select = data.table()
  # site_select = TopPeaks[site_no == i]
  site_select = TopPeaks_norm[site_no == i]
  year_sum_table = data.table()
  for(j in years1950){
    #print(j)
    year_select = site_select[year == j]
    year_select = year_select[ , sumTopPeaks :=sum(Flow_norm)]
    year_select = unique(year_select, by = "sumTopPeaks")
    year_select = year_select[ , c("agency_cd", "Date", "Flow_cms", "Flow_cd", "peaks_tf"):= NULL]
    year_sum_table = rbind(year_sum_table, year_select)
  }
  sumTopQ = rbind(sumTopQ, year_sum_table)  
}

# identify which grid cell the hcdn gages are located in
hcdnStationsNE_ORID = data.table()
for(i in ORID_list) {
  # print(i)
  grid_select = NE_grid_df[i, ]
  sites_grid_sel = hcdnGageTableSF[grid_select$geometry, ]
  sites_grid_sel = sites_grid_sel %>% mutate(ORID = i)
  sites_grid_sel = st_drop_geometry(sites_grid_sel) # drop geometry for speed
  if (dim(sites_grid_sel)[1]==0){
    next
  }else{
    hcdnStationsNE_ORID = rbind(hcdnStationsNE_ORID, sites_grid_sel)
  }
}
setDT(hcdnStationsNE_ORID)
# we have identified which grid cell each gage falls into

# assigning ORID values to all summed top 10% peak flows
sumTopPeaks_ORID = sumTopQ %>% left_join(hcdnStationsNE_ORID, by = "site_no")

# finding average extreme discharge by year, by grid cell
relevantORID_Q = unique(sumTopPeaks_ORID, by = "ORID")
meanAnnGridPeaks = data.table()
for(i in years1950){
  #print(i)
  year_select = data.table()
  year_select = sumTopPeaks_ORID[year == i]
  grid_mean_table = data.table()
  for(j in relevantORID_Q$ORID){
    #print(j)
    grid_select = year_select[ORID == j]
    grid_select = grid_select[ , meanGridPeaks :=mean(sumTopPeaks)]
    grid_select = unique(grid_select, by = "meanGridPeaks")
    grid_mean_table = rbind(grid_mean_table, grid_select)
  }
  meanAnnGridPeaks = rbind(meanAnnGridPeaks, grid_mean_table)  
}

# taking the average of all grid averages
year_vs_peakQ = data.table()
for (i in years1950) {
  year_select = data.table()
  year_select = meanAnnGridPeaks[year == i]
  year_select = year_select[ , meanTotalPeaks:=mean(meanGridPeaks)]
  year_select = unique(year_select, by = "year")
  year_vs_peakQ = rbind(year_vs_peakQ, year_select)
}

# now plotting year vs total extreme Q
ggplot() + 
  geom_point(data = year_vs_peakQ, aes(x =year , y = meanTotalPeaks), color = 'darkblue', size = 3) +
  labs(x = "Year", y = "Total Extreme Flow (cms)")

# year_vs_peakQ_cut = year_vs_peakQ[year>1972]

# examining changepoints using changepoint package
chgQ <- cpt.mean(year_vs_peakQ$meanTotalPeaks,penalty="MBIC",pen.value=0,method="AMOC",Q=5,test.stat="Normal",class=TRUE,
                 param.estimates=TRUE,minseglen=1)
plot(chgQ, type = "l", cpt.col = "darkblue", xlab = "Index", cpt.width = 4, ylab = "Normalized Extreme Discharge")
cpt_year_Q = 1950 + cpts(chgQ)
print(chgQ)
# the changepoint is identified to be the year 1972

# Making a better plot, incorporating changepoints
q_time_series = ggplot() + 
  geom_point(data = year_vs_peakQ, aes(x =year , y = meanTotalPeaks), color = 'black', pch = 1, lwd = 1.5) +
  geom_line(data = year_vs_peakQ, aes(x =year , y = meanTotalPeaks), color = 'darkgrey', lwd = 0.6) +
  geom_smooth(data = year_vs_peakQ, aes(x = year, y = meanTotalPeaks), method = lm, color = 'gold2') +
  # geom_smooth(data = year_vs_peakQ[year %between% c(1972,2023)], aes(x = year, y = meanTotalPeaks), method = lm, color = 'purple') +
  labs(title = "b) Extreme Discharge, 1950-2023", x = "Year", y = "Normalized Extreme Discharge")+
  theme_bw()

p_time_series = ggplot() + 
  geom_point(data = year_vs_prcp, aes(x =Year , y = meanTotalPrcp/10), color = 'black', pch = 1, lwd = 1.5) +
  geom_line(data = year_vs_prcp, aes(x =Year , y = meanTotalPrcp/10), color = 'darkgrey', lwd = 0.6) +
  geom_smooth(data = year_vs_prcp[Year %between% c(1950,1996)], aes(x = Year, y = meanTotalPrcp/10), method = lm, color = 'gold2') +
  geom_smooth(data = year_vs_prcp[Year %between% c(1996,2023)], aes(x = Year, y = meanTotalPrcp/10), method = lm, color = 'purple') +
  labs(title = "a) Extreme Precipitation, trends before and after 1996", x = "Year", y = "Extreme Precipitation (mm)")+
  theme_bw()

ggarrange(p_time_series, q_time_series, ncol = 2)

# adjust the size of the output device to control overall aspect ratio
ggsave("combined_time_series.png", width = 10, height = 3.2)

p_p_all = p1
p_q_all = t.test(year_vs_peakQ[Year<1972, meanTotalPeaks], year_vs_peakQ[Year>=1972, meanTotalPeaks], 
                 alternative = "two.sided", var.equal = FALSE)$p.value


## peaks plot
## first, I will get the peaks for one site for one year for testing
parameterCd_sel <- c('00060') # Stream parameter code
paramCds_daily <- c(parameterCd_sel)
# Select site numbers from all the sites that were downloaded
site_no_sel <- unique(hcdnGageTable$site_no, by = site_no)
# Function to download daily data
getDailyUSGS <- function(site_no_sel, paramCds_daily){
  daily_import <- renameNWISColumns(data.table(readNWISdv(parameterCd=c(paramCds_daily), 
                                                          siteNumbers= site_no_sel
  )))
  return(daily_import)
}

daily_WhiteRiver <- rbindlist(lapply('01144000', getDailyUSGS, paramCds_daily = paramCds_daily), fill = T)

# setDT
setDT(daily_WhiteRiver)

daily_WhiteRiver[ , year:= year(daily_WhiteRiver$Date)]

# create a table with only 2020 White River Discharge
daily_WhiteRiver_2020 = daily_WhiteRiver[year=="2020"]

daily_WhiteRiver_2020[ , peaks_tf:=peaks(daily_WhiteRiver_2020$Flow, span = 7)]
White_2020_peaks = daily_WhiteRiver_2020[peaks_tf=="TRUE"]
n <- 10
White_2020_TopPeaks =White_2020_peaks[White_2020_peaks$Flow > quantile(White_2020_peaks$Flow,prob=1-n/100),]


colors <- c("Peaks" = "skyblue2", "Top 10% Peaks" = "red")

ggplot(daily_WhiteRiver_2020, aes(x = Date, y = Flow*0.028316832)) +
  # geom_point(size = 1) +
  geom_col(col = 'blue4', fill = 'blue4')+
  # geom_line(col = 'blue4') +
  
  geom_point(data = White_2020_peaks, 
             aes(x = Date, y = Flow*0.028316832, color = 'Peaks'), size = 1.7)+
  geom_point(data = White_2020_TopPeaks, 
             aes(x = Date, y = Flow*0.028316832, color = 'Top 10% Peaks'), size = 1.7)+
  geom_hline(data=White_2020_peaks, aes(yintercept=min(Flow)*0.028316832), color = 'skyblue2', linetype = 'dotted', linewidth = .6)+
  geom_hline(data=White_2020_TopPeaks, aes(yintercept=min(Flow)*0.028316832), color = 'red', linetype = 'dotted', linewidth = .6)+
  scale_color_manual(values = colors)+
  labs(color = "", y = 'Discharge (cms)', x = 'Date')+
  theme_bw()+
  theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"))+
  ggtitle('White River, 2020')
ggsave(file = 'white_top_peaks.jpg')

#### Other Figures (Maps) #### 
## quadrant breakdown

prcp_circ = st_as_sf(prcp_circ, coords = c("Longitude", "Latitude"), 
                     crs = 4326)
prcp_circ = st_transform(prcp_circ, 3857)


q_circ = st_as_sf(q_circ, coords = c("Longitude", "Latitude"), 
                  crs = 4326)
q_circ = st_transform(q_circ, 3857)

oceans_sf1 = oceans_sf[!(oceans_sf$ORID %in% c(120, 121, 122, 137, 138, 139, 154, 155, 156, 157, 172, 173, 174, 176)), ]

NE_states = dplyr::filter(us_states, NAME %in% c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Connecticut",
                                                 "Rhode Island", "New Jersey","New York","Pennsylvania",
                                                 "Maryland","Delaware","West Virginia")) 



# Create a 150km buffer around the coastline
buffer_150km <- st_buffer(oceans_sf1, dist = 150000)

# Merge all polygons into one
buffer_150km_single <- st_union(buffer_150km)

# Convert to data frame for ggplot
buffer_df <- st_as_sf(buffer_150km_single)

# Plot the geometry column using ggplot
ggplot() +
  geom_sf(data = buffer_df, color = "blue", fill = "transparent") +
  theme_minimal() +
  labs(title = "150km Buffer around Atlantic Coast")

# Intersect NE_states with the 150km buffer
NE_states = st_transform(NE_states, 3857)

intersected_area <- st_intersection(NE_states, buffer_150km_single)

# Convert to data frame for ggplot
Coastal_NE <- st_as_sf(intersected_area)

# Find the area that doesn't intersect
non_intersected_area <- st_difference(NE_states, buffer_150km_single)

# Convert to data frame for ggplot
Inland_NE <- st_as_sf(non_intersected_area)

# Plot the intersected and non-intersected areas using ggplot
ggplot() +
  geom_sf(data = Coastal_NE, color = "transparent", fill = "blue4", alpha = 0.7) +
  geom_sf(data = Inland_NE, color = "transparent", fill = "green4", alpha = 0.7) +
  geom_sf(data = NE_states, color = "black", fill = "transparent") +
  geom_sf(data = q_circ, color = "lightblue") +
  geom_sf(data = prcp_circ, color = "white") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'Gray85', colour = NA))+
  labs(title = "Northeastern Quadrants")


df_above42 <- data.frame(
  lon=c(-60,-60,-100,-100), 
  lat=c(42,70,70,42))

polygon_above42 <- df_above42 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
# Convert to data frame for ggplot
polygon_df <- st_sf(geometry = polygon_above42)

# Plot using ggplot
ggplot() +
  geom_sf(data = polygon_df, fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Rectangular Polygon Above 42 Latitude Line")

polygon_above42 = st_transform(polygon_above42, 3857)


# Intersect Coastal_NE with the north_rectangle
intersected_area2 <- st_intersection(Coastal_NE, polygon_above42)

# Convert to data frame for ggplot
Coastal_N_sf <- st_as_sf(intersected_area2)

# Find the area that doesn't intersect
non_intersected_area2 <- st_difference(Coastal_NE, polygon_above42)

# Convert to data frame for ggplot
Coastal_S_sf <- st_as_sf(non_intersected_area2)




# Do the same with Inland_NE
intersected_area3 <- st_intersection(Inland_NE, polygon_above42)

# Convert to data frame for ggplot
Inland_N_sf <- st_as_sf(intersected_area3)

# Find the area that doesn't intersect
non_intersected_area3 <- st_difference(Inland_NE, polygon_above42)

# Convert to data frame for ggplot
Inland_S_sf <- st_as_sf(non_intersected_area3)



##  include (in background) all relevant political boundaries
# Load the world dataset
world <- ne_countries(scale = "medium", returnclass = "sf")

# Subset for the US and Canada
countries_US_CA <- world[world$iso_a3 %in% c("USA", "CAN"), ]

# Set the CRS to 4326
countries_US_CA <- st_transform(countries_US_CA, 3857)

# get great lakes
# Get world boundaries
great_lakes <- st_read('ne_10m_lakes.shp')

# Set CRS to 3857
great_lakes_sf <- st_transform(great_lakes, 3857) 

df_study_area <- data.frame(  
  lon=c(-66,-66,-84,-84), 
  lat=c(36.5,48,48,36.5))

study_area <- df_study_area %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Convert to data frame for ggplot
area_df <- st_sf(geometry = study_area)

area_df = st_transform(area_df, 3857)

# Intersect Coastal_NE with the north_rectangle
intersected_area5 <- st_intersection(great_lakes_sf, area_df)
intersected_area4 = st_intersection(countries_US_CA, area_df)

# Convert to data frame for ggplot
lakes_df <- st_as_sf(intersected_area5)
US_CA_df = st_as_sf(intersected_area4)


# make the quadrants union
Inland_N_sf = st_union(Inland_N_sf)
Inland_S_sf = st_union(Inland_S_sf)
Coastal_N_sf = st_union(Coastal_N_sf)
Coastal_S_sf = st_union(Coastal_S_sf)

##  final plot
study_area_q = ggplot() +
  geom_sf(data = US_CA_df, color = "grey70", fill = "grey85")+
  geom_sf(data = lakes_df, color = "grey70", fill = "grey92")+
  geom_sf(data = NE_states, color = "grey70", fill = "grey85") +
  geom_sf(data = Inland_N_sf, color = "transparent", fill = "#f0f9e8", alpha = 0.5) +
  geom_sf(data = Coastal_N_sf, color = "transparent", fill = "#bae4bc", alpha = 0.7) +
  geom_sf(data = Inland_S_sf, color = "transparent", fill = "#7bccc4", alpha = 0.7) +
  geom_sf(data = Coastal_S_sf, color = "transparent", fill = "#2b8cbe", alpha = 0.5) +
  geom_sf(data = NE_states, color = "grey40", fill = "transparent") +
  geom_sf(data = q_circ, color = "black", size = 1.2) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey92', colour = NA))+
  labs(title = "a) Stream Gages (n=80)")

study_area_p = ggplot() +
  geom_sf(data = US_CA_df, color = "grey70", fill = "grey85")+
  geom_sf(data = lakes_df, color = "grey70", fill = "grey92")+
  geom_sf(data = NE_states, color = "grey70", fill = "grey85") +
  geom_sf(data = Inland_N_sf, color = "transparent", fill = "#f0f9e8", alpha = 0.5) +
  geom_sf(data = Coastal_N_sf, color = "transparent", fill = "#bae4bc", alpha = 0.7) +
  geom_sf(data = Inland_S_sf, color = "transparent", fill = "#7bccc4", alpha = 0.7) +
  geom_sf(data = Coastal_S_sf, color = "transparent", fill = "#2b8cbe", alpha = 0.5) +
  geom_sf(data = NE_states, color = "grey40", fill = "transparent") +
  geom_sf(data = prcp_circ, color = "black", size = 1.2) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey92', colour = NA))+
  labs(title = "b) Precipitation Gages (n=123)")

ggarrange(study_area_q, study_area_p, ncol = 2)
ggsave('study_area_draft.jpg')




## Other maps, with geographic context

R_lab_map = (bquote(bar(R) ~'(mean resultant length)'))

# R maps
ggplot() +
  geom_sf(data = US_CA_df, color = "grey70", fill = "grey85")+
  geom_sf(data = lakes_df, color = "grey70", fill = "grey92")+
  geom_sf(data = NE_states, color = "grey70", fill = "grey85") +
  geom_sf(data = NE_states, color = "grey40", fill = "grey97") +
  geom_sf(data = Inland_N_sf, color = "grey40", fill = "transparent", linetype = '4F') +
  geom_sf(data = Coastal_N_sf, color = "grey40", fill = "transparent", linetype = '4F') +
  geom_sf(data = Inland_S_sf, color = "grey40", fill = "transparent", linetype = '4F') +
  geom_sf(data = Coastal_S_sf, color = "grey40", fill = "transparent", linetype = '4F') +
  geom_sf(data = NE_states, color = "grey40", fill = "transparent") +
  geom_sf(data = prcp_circ, aes(geometry = geometry, col = R_mean), size = 2.9) +
  scale_color_gradientn(colours = c('lemonchiffon', 'khaki1', 'khaki2','lightsalmon1', 'lightsalmon2', 'tomato1', 'tomato2')
                        , limits = c(0,0.8), oob = scales::squish)+
  # scale_color_gradientn(colours = c('darkblue', 'blue3', 'lightblue','beige'), limits = c(-.4, -.0), oob = scales::squish)+
  labs(col = R_lab_map, size = bquote('Drainage area '(km^2)))+ # add labels
  # ggtitle("R Discharge")+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey92', colour = NA))
ggsave(filename = 'R_P.jpg', dpi = 400)

ggplot() +
  geom_sf(data = US_CA_df, color = "grey70", fill = "grey85")+
  geom_sf(data = lakes_df, color = "grey70", fill = "grey92")+
  geom_sf(data = NE_states, color = "grey70", fill = "grey85") +
  geom_sf(data = NE_states, color = "grey40", fill = "grey97") +
  geom_sf(data = Inland_N_sf, color = "grey40", fill = "transparent", linetype = '4F') +
  geom_sf(data = Coastal_N_sf, color = "grey40", fill = "transparent", linetype = '4F') +
  geom_sf(data = Inland_S_sf, color = "grey40", fill = "transparent", linetype = '4F') +
  geom_sf(data = Coastal_S_sf, color = "grey40", fill = "transparent", linetype = '4F') +
  geom_sf(data = NE_states, color = "grey40", fill = "transparent") +
  geom_sf(data = q_circ, aes(geometry = geometry, col = R_mean, size = Drainage_area)) +
  scale_color_gradientn(colours = c('lemonchiffon', 'khaki1', 'khaki2','lightsalmon1', 'lightsalmon2', 'tomato1', 'tomato2')
                        , limits = c(0,0.8), oob = scales::squish)+
  # scale_color_gradientn(colours = c('darkblue', 'blue3', 'lightblue','beige'), limits = c(-.4, -.0), oob = scales::squish)+
  labs(col = R_lab_map, size = bquote('Drainage area '(km^2)))+ # add labels
  # ggtitle("R Discharge")+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey92', colour = NA))
ggsave(filename = 'R_Q.jpg', dpi = 400)


# cox-lewis map
ggplot() +
  geom_sf(data = US_CA_df, color = "grey70", fill = "grey85")+
  geom_sf(data = lakes_df, color = "grey70", fill = "grey92")+
  geom_sf(data = NE_states, color = "grey70", fill = "grey85") +
  geom_sf(data = NE_states, color = "grey40", fill = "grey97") +
  geom_sf(data = Inland_N_sf, color = "grey40", fill = "transparent", linetype = '4B') +
  # geom_sf(data = Coastal_N_sf, color = "grey40", fill = "transparent", linetype = '4B') +
  geom_sf(data = Inland_S_sf, color = "grey40", fill = "transparent", linetype = '4B') +
  geom_sf(data = Coastal_S_sf, color = "grey40", fill = "transparent", linetype = '4B') +
  geom_sf(data = NE_states, color = "grey40", fill = "transparent") +
  geom_sf(data = q_circ_cox_sf[q_circ_cox_sf$cox_lewis_H2 > 2, ], aes(geometry = geometry, col = cox_lewis_H2, size = Drainage_area)) +
  geom_sf(data = q_circ_cox_sf[q_circ_cox_sf$cox_lewis_H2 < -2, ], aes(geometry = geometry, col = cox_lewis_H2, size = Drainage_area)) +
  geom_sf(data = q_circ_cox_sf[q_circ_cox_sf$cox_lewis_H2 < 2, ], aes(geometry = geometry, col = cox_lewis_H2, size = Drainage_area), pch = 21, fill = NA)+
  scale_color_gradientn(colours = c('blue', 'red'),  
                        limits = c(-1, 3), oob = scales::squish)+
  labs(col = "Z", size = bquote('Drainage area '(km^2)))+ # add title
  ggtitle('Extreme Discharge Cox-Lewis Z Score, Warm Season (Jun-Oct)')+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey92', colour = NA))
ggsave(filename = 'H2_cox_Q.png', dpi = 400)


