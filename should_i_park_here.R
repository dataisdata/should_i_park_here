library(readr)
library(leaflet)
library(DT)
library(reshape2)
library(plotly)
library(ggthemes)
options( java.parameters = "-Xmx16g" )
setwd("D:/vancouver_crime")

# Read in our Vancouver crime file:
crime_data = read_csv("crime_csv_all_years.csv")

#### Fix the coordinate system ####
# So, this is one annoying thing gov't agencies do: almost never standardize on GIS coordinate systems.
# These are 'Spatial Points' with a zone of 'UTM 10'
# We want to map this easily, so let's get it into something more familiar and widely used: lats and lons
# We'll use the package 'rgdal' to handle the transformation to latitutde and longitude:

library(rgdal)
# Pull the SP coordinates out and get them ready to convert:
utmcoor = SpatialPoints(cbind(crime_data$X, crime_data$Y), proj4string=CRS("+proj=utm +zone=10"))
# The field X = UTM Easting and Y = UTM Northing
# I know the UTM zone because it's in the metadata the City of Vancouver provided along with the data: 
# Link: http://data.vancouver.ca/datacatalogue/crime-data-attributes.htm#X
longlatcoor = spTransform(utmcoor,CRS("+proj=longlat"))

# Take the latitude and longitude out from this new data structure and plug them back into the original dataset:
crime_data$lat = longlatcoor@coords[, 2]
crime_data$lon = longlatcoor@coords[, 1]

str(crime_data)

# Be a good Data Professional and check your work! Ensure your new lats and lons map out to the same places. 
# I used this site to validate that they do: 
# link: http://www.rcn.montana.edu/Resources/Converter.aspx
# One odd-looking thing is that the X values of 0 converted to a longitude of 127.4887. 
# This happens because a UTM zone has boundaries for latitudes. There are 60 of these UTM zones.
# A zero X value actually indicates the edge of the zone, which has an actual lat associated with it.

# We'll ignore these points when we map out individual incidents later.
# Now we have our latitudes and longitudes, so we're done with the not-so-friendly GIS part of our work. Phew!
# You can optionally toss these X and Y values after transforming and validating. We don't need them now.
crime_data = subset(crime_data, select = -c(X, Y))

# Split out all the "Theft of Vehicle" and "Theft from Vehicle" incidents:
auto_theft = subset(crime_data, grepl("Theft.*.Vehicle", TYPE) == TRUE)

# How has the rate of auto theft and theft from autos changed from 2003 to 2017?
library(sqldf)
theft_by_year = sqldf("SELECT YEAR,
                      COUNT(*) AS incidents
                      FROM auto_theft
                      WHERE YEAR < 2018
                      GROUP BY YEAR
                      ORDER BY YEAR;")

quick_yrly_crime_plot = ggplot(data = theft_by_year, aes(x = YEAR, y = incidents)) +
  geom_line() +
  ggtitle("Theft of or From Autos in Vancouver\n2003 - 2017")
ggplotly(quick_yrly_crime_plot)

# Question 1: Historically, which neighborhoods are safe places to park your car? Which are not?
# Let's just summarize the total # of incidents by neighborhood. I personally like to pull out 'sqldf' for things like this
# because I use various kinds of SQL a lot. The syntax here is SQLite, so, pretty vanilla SQL:

nbhd_summary = sqldf("SELECT NEIGHBOURHOOD,
                     COUNT(*) AS incidents
                     FROM auto_theft
                     GROUP BY NEIGHBOURHOOD;")

# Now sort it:
nbhd_summary = nbhd_summary[order(-nbhd_summary$incidents),]
# View it:
DT::datatable(nbhd_summary, style = "bootstrap")

# Question 1 - extended: Maybe some of these neighborhoods are just bigger. Having more crimes doesn't necessarily make a 
# large neighborhood less safe. How about an incidents per capita metric? I'd need population for each neighborhood to do that.

# I can grab some recent (2016) census data, luckily. I'll get it from Vancouver's open data repository:
# http://data.vancouver.ca/datacatalogue/
# VPD and Stats Canada both use (almost exactly) the same names for neighborhoods. This sort of thing is always a nice surprise.
# I looked up 'central business district' and it corresponds to Stats Canada's 'downtown' definition, so I renamed 'downtown'
# to match my original dataset's neighborhood names.
# Now we can just reshape it and then join it up. No manual renaming of anything involved.

census_data = na.omit(read_csv("CensusLocalAreaProfiles2016.csv"))

# Now reshape it so the neighborhood names are all in one column:
census_data_long = melt(census_data, id = c("Variable", "ID"))

# And rename 'variable so it doesn't clash with the existing column named that:
colnames(census_data_long) = c("Variable", "ID", "Neighbourhood", "value")

# That gives us a dataframe where the neighborhood names are in a column called 'variable'
# We're after the total population of the neighborhood, which is labelled with ID 1. 

# Let's do incidents per year per capita for 2016 now. Let's grab auto thefts for 2016 and join it with the total populations
# of each neighborhood for that same year:
autothefts_2016 = subset(auto_theft, YEAR == 2016)

nbhd_summary_2016 = sqldf("SELECT autothefts_2016.NEIGHBOURHOOD,
                     COUNT(*) AS total_incidents,
                     census_data_long.value AS population
                     FROM autothefts_2016
                      LEFT JOIN census_data_long ON census_data_long.Neighbourhood = autothefts_2016.NEIGHBOURHOOD
                     WHERE census_data_long.ID = 1
                     GROUP BY autothefts_2016.NEIGHBOURHOOD;")

# Now we have population right next to it. Let's create a calculated column for incidents per 1,000 residents.
nbhd_summary_2016$PerThousand = nbhd_summary_2016$total_incidents / (nbhd_summary_2016$population / 1000)

nbhd_summary_2016 = nbhd_summary_2016[order(nbhd_summary_2016$PerThousand), ]
nbhd_summary_2016$NEIGHBOURHOOD = factor(nbhd_summary_2016$NEIGHBOURHOOD, levels = nbhd_summary_2016$NEIGHBOURHOOD)

# And visualize
per_capita_viz_1 = ggplot(data = nbhd_summary_2016) +
  aes(x= NEIGHBOURHOOD, y = PerThousand, text = paste("Total Incidents: ", total_incidents)) +
  geom_bar(stat = "identity", fill = "#3c78d8") +
  ylab("Thefts of autos or from autos, per capita") +
  xlab("") +
  ggtitle("Vancouver Neighbourhoods: Auto Theft Stats (2016)") +
  coord_flip() +
  theme_hc()

ggplotly(per_capita_viz_1)

library(maps)
library(leaflet)

# I'm going to spice up my visual by making a little car thief icon to use as markers on my map:
autotheft_icon = makeIcon("autothefticon.png", iconWidth = 50, iconHeight = 50)

van_map = leaflet(data = autothefts_2016) %>%
 # Leaflet allows you to use various 3rd-party map styles. I like this one (Toner Lite) because it doesn't distract from the data:
  addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(opacity = 0.45)) %>%
  addMarkers(icon = autotheft_icon,
 # Now I'm going to create detailed tooltips. You can use HTML line breaks to create multi-line
 # tooltips with lots of information for users:
    popup = paste0("Description: ", autothefts_2016$TYPE, "<br>",
                  "Location: ", autothefts_2016$HUNDRED_BLOCK, "<br>",
                  "Date: ", autothefts_2016$YEAR, "-", autothefts_2016$MONTH, "-", autothefts_2016$DAY, "<br>",
                  "Time: ", autothefts_2016$HOUR, ":", autothefts_2016$MINUTE
                  ), 
 # Applying clustering to keep the map tidy without losing the ability to get high detail by zooming in:
    clusterOptions = markerClusterOptions(maxClusterRadius = 100))
  
van_map

# Now, are there any especially bad streets that should really be avoided? Let's remove the hundred block part of the street names
# and get total incidents by street. I'll remove anything from the hundred block names that has a number followed by 'XX' or 'XXX':

auto_theft$street = gsub("^\\d*X*\\s", "", auto_theft$HUNDRED_BLOCK)

thefts_by_street = sqldf("SELECT street AS \"Street\",
                         COUNT(*) AS \"Total_Incidents\"
                         FROM auto_theft
                         GROUP BY street;")

theft_by_street_hist = ggplot(data = thefts_by_street, aes(Total_Incidents)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(breaks = seq(100, max(thefts_by_street$Total_Incidents), by = 300))
  
ggplotly(theft_by_street_hist)

# Okay, many streets with very few auto thefts over this 15 year period, but look at that range! There are some streets with 
# unbelievable auto theft rates. Let's see the worst 25:
# Sorth the full streets dataframe:
thefts_by_street = thefts_by_street[order(-thefts_by_street$Total_Incidents),]

# And take the top 25:
mean_streets = thefts_by_street[1:25,]

#Change the sorting so I can create a horizontal bar chart:
mean_streets = mean_streets[order(mean_streets$Total_Incidents),]

mean_streets$Street = factor(mean_streets$Street, levels = mean_streets$Street)

mean_streets_viz_1 = ggplot(data = mean_streets) +
  aes(x= Street, y = Total_Incidents) +
  geom_bar(stat = "identity", fill = "#ff4d4d") +
  ylab("Thefts of autos or from autos") +
  xlab("") +
  ggtitle("Vancouver's Top 25 Worst Streets to Park on") +
  coord_flip() +
  theme(axis.text.y = element_text(angle = 30)) +
  theme_hc()

ggplotly(mean_streets_viz_1)

# Some of these streets are much longer than others, like Main street. Maybe someone can tack on a length variable and generate 
# something that evens things out a bit across streets of different lengths to get a better picture of how dangerous each street 
# really is. 

# I wonder if we can predict how many thefts will happen in an hour, day, week, or month on a given street or in a given neighborhood
# I also wonder if there are some meaningful patterns as to when these thefts happen. My initial guess is late in the evening,
# but there might be other things that come out. I'll also add weekday to see if there are patterns within weeks.

# Starting with seeing where in the day these thefts are distributed. We'll go back to our original dataset for the exploration and
# modeling:

thefts_by_hour = sqldf("SELECT HOUR AS \"Hour\",
                       COUNT(*) AS \"Incidents\"
                       FROM auto_theft
                       GROUP BY HOUR
                       ORDER BY HOUR;")
thefts_by_hour$Hour = as.integer(thefts_by_hour$Hour)

thefts_by_hour_vis = ggplot(data = thefts_by_hour, aes(y = Incidents, x = Hour)) +
  geom_line(stat = "identity", color = '#3c78d8', size = 1.5) +
  geom_point(color = '#3c78d8', size = 2.5) +
  theme_hc() +
  scale_color_tableau() +
  ggtitle("Vancouver: Thefts of or from Autos, by Hour of Day\n2003 - 2017")

ggplotly(thefts_by_hour_vis)

# Not totally surprising, but interesting to see the peak time is actually 6 PM, rather than something later.
# This could be due to thefts occurring throughout a workday and people are only able to report them after they leave work
# or school. The 12 PM peak might also be explained by the same thing - theft during the morning, only noticed when people get to
# their cars.

# Are there neighborhood where the time of day doesn't matter?

















