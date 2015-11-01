setwd("~/Coding/Data-Incubator")

library(plyr)
library(ggplot2)
library(scales)
library(ggmap)
listing <- read.csv("listing.csv", header = T)
listing.all <- subset(listing, bedrooms == "All")
listing.all$Week.of.Day <- as.Date(listing.all$Week.of.Day)
listing.all$Year <- format(listing.all$Week.of.Day, "%Y")
listing.all$Week <- format(listing.all$Week.of.Day, "%W")
sum.time.state <- ddply(listing.all, .(Week.of.Day, State), summarise,
              Properties = sum(numberOfProperties, na.rm = T),
              Listing.Price = mean(averageListingPrice, na.rm = T))
sum.time.state$Week.of.Day <- as.Date(sum.time.state$Week.of.Day)
# StateColorVector <- palette()
# XAxisBreaks <- c("")
state.time <- ggplot(aes(x = Week.of.Day,y = Properties, group=State), data = sum.time.state)+
  geom_line(aes(color = State)) + 
  scale_x_date(labels = date_format("%m of %Y")) +
  labs(title = "Properties ~ Time of the year", x="Weeks", y="Properties")
state.time.price <- ggplot(aes(x = Week.of.Day,y = Listing.Price, group=State), data = sum.time.state)+
  geom_line(aes(color = State)) + 
  scale_x_date(labels = date_format("%m of %Y")) +
  labs(title = "Price ~ Time of the year", x="Weeks", y="Avg. Price")

# Let's process the traffic data

traffic <- read.csv("traffic.csv", header = T)
traffic$date <- as.Date(traffic$date)
traffic$Year <- format(traffic$date, "%Y")
traffic$Week <- format(traffic$date, "%W")
traffic.week <- ddply(traffic, .(City, State, Year, Week), summarise,
                        national.traffic = mean(percentNationalTraffic),
                        state.traffic = mean(percentStateTraffic),
                        latitude = mean(Latitude),
                        longitude = mean(Longitude))
# Here I want to link the listing price with traffic

traffic.total <- merge(traffic.week, listing.all, by = c("City", "State","Year", "Week"))

traffic.GA <- subset(traffic.total, State == 'GA', Year = "2014")

traffic.GA.simple <- ddply(traffic.GA, .(City), summarise,
                           NationalAvgT = mean(national.traffic),
                           StateAvgT = mean(state.traffic),
                           latitude = mean(latitude),
                           longitude = mean(longitude),
                           AvgListPrice = mean(averageListingPrice),
                           NumProperty = sum(numberOfProperties),
                           MedListPrice = mean(medianListingPrice))
traffic.GA.simple$PriceRatio = (traffic.GA.simple$AvgListPrice/
                                  traffic.GA.simple$MedListPrice)


center_lat <- mean(traffic.GA.simple$latitude)
center_lon <- mean(traffic.GA.simple$longitude)
map.GA <- get_map(location=c(center_lon, center_lat),zoom = 6)
GAMap <- ggmap(map.GA,legent="topleft")
GAMap + geom_point(aes(x=longitude,y=latitude, size = PriceRatio), data = traffic.GA.simple, color = "blue", alpha = 0.5)

# Let's read the weather data!
weather <- read.csv("GA-weather.csv", header = T)
weather$DATE <- strptime(weather$DATE, "%Y%m%d")
weather$Year <- format(weather$DATE, "%Y")
weather$Week <- format(weather$DATE, "%W")
weather[weather$LATITUDE == "unknown", ]$LATITUDE = NA
weather[weather$LONGITUDE == "unknown", ]$LONGITUDE = NA
weather$LATITUDE = as.numeric(as.character(weather$LATITUDE))
weather$LONGITUDE = as.numeric(as.character(weather$LONGITUDE))
weather[weather$TMAX == -9999, ]$TMAX = NA
weather[weather$TMIN == -9999, ]$TMIN = NA
weather$DATE <- as.character(weather$DATE)
weather.week <- ddply(weather, .(STATION_NAME, Year, Week),summarise,
                      latitude = mean(LATITUDE),
                      longitude = mean(LONGITUDE),
                      tmax = mean(TMAX, na.rm = T)/10,
                      tmin = mean(TMIN, na.rm = T)/10
                      )
low.t <- 0
high.t <- 40
for (i in 34:51) {
  GA.housing <- traffic.GA[traffic.GA$Week == i,]
  weather.plot <- weather.week[weather.week$Week == i,]
  GA.housing$state.traffic = GA.housing$national.traffic*100
  weather.plot$AvgTemp = weather.plot$tmax
  weather.plot2 <- weather.plot[,c(1,4,5,8)]
  ggmap(map.GA) + stat_summary2d(aes(x=longitude, y = latitude, z = AvgTemp),
                                         data = weather.plot2, fun = median, binwidth = c(0.3, 0.3), alpha = 0.5) +
    scale_fill_gradient(name="Temp", low = "green", high = "red", limit = c(low.t, high.t)) +
    geom_point(aes(x=longitude,y=latitude, size = state.traffic), color = "blue", alpha = 0.7, data = GA.housing) + scale_size(name="Traffic")
  
    
  file = paste("GA",as.character(i),".png",sep="")
  ggsave(file)
}

