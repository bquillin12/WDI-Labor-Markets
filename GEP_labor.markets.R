#######################################################################################################
# Data and analysis for:                                                                              #
# World Bank Global Economic Prospects, 2015, June                                                    #
# Box 1.3 Recent Developments in Emerging and Developing Country Labor Markets, pps 49-54             #
#######################################################################################################

# Packages
library(reshape2)
library(kimisc)
library(dplyr)
library(plyr)


# Unemployment data loaded from WDI pull and cleaned -------------------------------------------

# Download raw data from file
UE <- read.csv("UE.csv", header = TRUE, sep = ",")

# Reshape data
UE_long <- melt(
  data = UE,
  id = "country",
  variable.name = "year",
  value.name = "UE",
  na.rm = TRUE
)
# Sort by country
UE_long <- dplyr::arrange(UE_long, country)
UE_long_t1 <- subset(UE_long, year == "X2000" | year == "X2001" | year == "X2002" | year == "X2003" | year == "X2004" | year == "X2005" | year == "X2006" | year == "X2007") # subset pre-crisis
UE_long_t2 <- subset(UE_long, year == "X2008" | year == "X2009" | year == "X2010" | year == "X2011"| year == "X2012"| year == "X2013" | year == "X2014") # subset post-crisis

# Calculate annual percentage point change in UE (t/t-1(-1))
UE_delta <- ddply(UE_long, "country", transform, Growth_rate = c(NA, kimisc::gdiff(UE, FUN = `-`))) # full sample
UE_delta.t1 <- ddply(UE_long_t1, "country", transform, Growth_rate = c(NA, kimisc::gdiff(UE, FUN = `-`))) # subset precrisis
UE_delta.t2 <- ddply(UE_long_t2, "country", transform, Growth_rate = c(NA, kimisc::gdiff(UE, FUN = `-`))) # subset postcrisis

# Calculate average annual growth rate
UE_delta_sum <- ddply(UE_delta, ~country, summarise, mean = mean(Growth_rate, na.rm = TRUE)) # full sample
UE_delta.sum.t1 <- ddply(UE_delta.t1, ~country, summarise, mean = mean(Growth_rate, na.rm = TRUE)) # subset precrisis
UE_delta.sum.t2 <- ddply(UE_delta.t2, ~country, summarise, mean = mean(Growth_rate, na.rm = TRUE)) # subset postcrisis

# Real GDP growth data loaded from WDI pull and cleaned -----------------------------

GDP_delta <- read.csv("GDP_delta.csv")

# Reshape data
GDPdelta_long <- melt(
  data = GDP_delta,
  id = "Country",
  variable.name = "year",
  value.name = "GDP_delta",
  na.rm = TRUE
)

# Sort by country
GDPdelta_long <- dplyr::arrange(GDPdelta_long, Country)
GDPdelta_long$GDP_delta <- as.numeric(as.character(GDPdelta_long$GDP_delta)) # convert $GDP_delta to num
GDP_delta_long_t1 <- subset(GDPdelta_long, year == "X2000" | year == "X2001" | year == "X2002" | year == "X2003" | year == "X2004" | year == "X2005" | year == "X2006" | year == "X2007") # subset pre-crisis
GDP_delta_long_t2 <- subset(GDPdelta_long, year == "X2008" | year == "X2009" | year == "X2010" | year == "X2011"| year == "X2012"| year == "X2013" | year == "X2014") # subset post-crisis
GDP_delta_long_t2 <- na.omit(GDP_delta_long_t2)

# Calculate average annual growth rate
GDPdelta_sum <- ddply(GDPdelta_long, ~Country, summarise, mean = mean(GDP_delta)) # full sample
GDPdelta_sum.t1 <- ddply(GDP_delta_long_t1, ~Country, summarise, mean = mean(GDP_delta)) # subset precrisis
GDPdelta_sum.t2 <- ddply(GDP_delta_long_t2, ~Country, summarise, mean = mean(GDP_delta)) # subset postcrisis

