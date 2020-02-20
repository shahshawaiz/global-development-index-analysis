## ----echo=FALSE, message=FALSE, results=FALSE, warning=FALSE-------------
# install packages, if not installed
list.of.packages <- c(
  "reshape2", 
  "plyr", 
  "PerformanceAnalytics",
  "corrplot",
  "Hmisc",
  "hexbin",
  "gapminder",
  "tidyverse",
  "ggplot2",
  "plotly",
  "gganimate"
)


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# import packages
lapply(list.of.packages, require, character.only = TRUE)

# read csv
dat.raw <- read.csv('wdi-f.csv')
dat.raw.d <- read.csv('wdi-d.csv')



## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%'----

#load new data
#dat.new.raw <- read.csv("wdi-e.csv")

# dat.new.raw <- subset(dat.new.raw, select = -c(X, X.2))
# dat.new <- renameIndicators(dat.new.raw)
# dat.newNationalYearlyAvg.w <- groupByCol(dat.new, c('country', 'year', 'indicator'))

# country_year_indicator<-dcast(dat.newNationalYearlyAvg.w, country + year ~ indicator, value.var="value")
# countries <- levels(unique(gapminder$country))
# countries_length <- length(countries)
 
# country_year_indicator$region=""
# for(index in 1:countries_length){
#   country_year_indicator[country_year_indicator$country==countries[index],]$region = as.character(gapminder[gapminder$country==countries[index],]$continent[1][1])
# }
 
# country_year_indicator<-country_year_indicator[country_year_indicator$region!="",]
# write.csv(country_year_indicator, file = "wdi-f.csv")


# label regions; 1=North America, 2=Scandavivan, 3=South Asia, 4=Middle East

labelRegions <- function(dat){
  dat$region[dat$country_code %in% c("USA", "CAN")] <- "North America"
  dat$region[dat$country_code %in% c("DNK", "NOR", "SWE")] <- "Scandinavia"
  dat$region[dat$country_code %in% c("IND", "PAK", "BGD")] <- "South Asia"
  dat$region[dat$country_code %in% c("QAT", "EGY", "ARE")] <- "Middle East"
  
  dat$region <- as.factor(dat$region)
  return(dat)
}

dat.raw.d <- labelRegions(dat.raw.d)



## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%'----

renameIndicators <- function(dat){
  
  # old_names
  old_names <- c(
    "GDP per capita (current US$)",
    "GDP growth (annual %)",
    "GDP per capita growth (annual %)",
    "Expense (% of GDP)",
    "Industry (including construction), value added (% of GDP)",
    "Net primary income (BoP, current US$)",
    "GNI per capita (constant 2010 US$)",
    # "GNI per capita (constant LCU)",
    
    "Pupil-teacher ratio, primary",
    "Government expenditure on education, total (% of government expenditure)",
    "Access to electricity (% of population)",
    "Birth rate, crude (per 1,000 people)",
    "Population growth (annual %)",
    "Life expectancy at birth, total (years)",
    
    "CO2 emissions (metric tons per capita)",
    "Total greenhouse gas emissions (kt of CO2 equivalent)",
    "Forest area (sq. km)",
    "Population density (people per sq. km of land area)"
  )
  
  new_names <- c(
    "gdp_capita", # GDP per capita (current US$)
    "gdp_growth", # GDP growth (annual %)
    "gdp_capita_growth", # GDP per capita growth (annual %)
    "expense_gdp", # Expense (% of GDP)
    "industry_gdp", # Industry (including construction), value added (% of GDP)
    "net_primary_income", # "Net primary income (BoP, current US$)"
    "gni_capita", # GNI per capita (constant 2010 US$)
    # "gni_capita_constant", # GNI per capita (constant LCU)
    
    "teacher_per_pupil", # Pupil-teacher ratio, primary
    "education_expense", # Government expenditure on education, total (% of government expenditure)
    "electricity_access", # Access to electricity (% of population)
    "birth_rate", # Birth rate, crude (per 1,000 people)
    "population_growth", # Population growth (annual %)
    "life_expectancy", # Life expectancy at birth, total (years)
    
    "co2_emission", # CO2 emissions (metric tons per capita)
    "greenhouse_emission", # Total greenhouse gas emissions (kt of CO2 equivalent)
    "forest_area", # Forest area (sq. km)
    "population_density" # Population density (people per sq. km of land area)
  )
  
  
  dat$indicator <- mapvalues(dat$indicator, from = old_names, to = new_names)
  
  return(dat)
}

dat.d <- renameIndicators(dat.raw.d)



## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%'----

# group by routine
groupByCol <- function(dat, col, filter = ""){
  groupColumns = col
  dataColumns = c('value')
  
  if(filter != "") {
    
  }
  
  groupedDat <- ddply(
    dat, groupColumns, function(x) colMeans(x[dataColumns])
  )
  return(groupedDat)
}

# add decade column and init gropued by var
dat.d <- mutate(dat.d, decade = year - (year %% 10))


datGlobalYearlyAvg.w <- groupByCol(dat.d, c('year', 'indicator'))
datNationalYearlyAvg.w <- groupByCol(dat.d, c('country', 'year', 'indicator'))
datRegionalYearlyAvg.w <- groupByCol(dat.d, c('region', 'year', 'indicator'))
datNationalDecadeAvg.w <- groupByCol(dat.d, c('country', 'decade', 'indicator'))
datRegionalDecadeAvg.w <- groupByCol(dat.d, c('region', 'decade', 'indicator'))
datRegionalNationalYearlyAvg.w <- groupByCol(dat.d, c('region', 'country', 'year', 'indicator')) 
datRegionalNationalDecadeAvg.w <- groupByCol(dat.d, c('region', 'country', 'decade', 'indicator')) 

datNationalAvg.w <- groupByCol(dat.d, c('country', 'indicator'))
datRegionalAvg.w <- groupByCol(dat.d, c('region', 'indicator'))





## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%'----

# normalize routine
normalize <- function(col) {
  return((col - min(col))/(diff(range(col))))
}




## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%'----

# Write CSV in R
#write.csv(dat.all, file = "Final_Data.csv")
change_year_columns <- function (dat){
      colnames(dat)[which(colnames(dat) %in% c("X1960","X1961","X1962","X1963","X1964","X1965","X1966","X1967","X1968","X1969","X1970","X1971","X1972","X1973","X1974","X1975","X1976","X1977","X1978","X1979","X1980","X1981","X1982","X1983","X1984","X1985","X1986","X1987","X1988","X1989","X1990","X1991","X1992","X1993","X1994","X1995","X1996","X1997","X1998","X1999","X2000","X2001","X2002","X2003","X2004","X2005","X2006","X2007","X2008","X2009","X2010","X2011","X2012","X2013","X2014","X2015","X2016","X2017","X2018") )] <- c("1960","1961","1962","1963","1964","1965","1966","1967","1968","1969","1970","1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")
    return (dat)
  }


wide_to_long <- function(data_wide)
{
  data_wide <- change_year_columns(grouped_comparison_wide)

  data_long<-melt(data_wide, id.vars=c("X.1","Country_Name","Country_Code","Indicator_Name","Indicator_Code","X"))
  
  colnames(data_long)[which(colnames(data_long) %in% c("variable") )] <- c("year")
  
  data_long <- data_long[setdiff(colnames(data_long), c('X.1', 'X', "Indicator_Code"))]
  colnames(data_long)[which(colnames(data_long) %in% c("Country_Name","Country_Code","Indicator_Name") )] <- c("country","country_code","indicator")
  return(data_long)
}

datGlobalYearlyAvg <- dcast(datGlobalYearlyAvg.w, year ~ indicator, value.var="value")
datNationalYearlyAvg <- dcast(datNationalYearlyAvg.w, year + country ~ indicator, value.var="value")
datNationalDecadeAvg <- dcast(datNationalDecadeAvg.w, decade + country ~ indicator, value.var="value")
datRegionalYearlyAvg <- dcast(datRegionalYearlyAvg.w, year + region ~ indicator, value.var="value")
datRegionalDecadeAvg <- dcast(datRegionalDecadeAvg.w, decade + region ~ indicator, value.var="value")
datRegionalNationalYearlyAvg <- dcast(datRegionalNationalYearlyAvg.w, year + region + country ~ indicator, value.var="value")
datRegionalNationalDecadeAvg <- dcast(datRegionalNationalDecadeAvg.w, decade + region + country ~ indicator, value.var="value")

datNationalAvg <- dcast(datNationalAvg.w, country ~ indicator, value.var="value")
datRegionalAvg <- dcast(datRegionalAvg.w, region ~ indicator, value.var="value")



## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%'----

fillMissing <- function(df){
  for(i in 1:ncol(df)){
    
    if(is.numeric(df[,i])){
      df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
    }
  }
    
  return(df)
}

datGlobalYearlyAvg <- fillMissing(datGlobalYearlyAvg)
datNationalYearlyAvg <- fillMissing(datNationalYearlyAvg)
datNationalDecadeAvg <- fillMissing(datNationalDecadeAvg)
datRegionalYearlyAvg <- fillMissing(datRegionalYearlyAvg)
datRegionalDecadeAvg <- fillMissing(datRegionalDecadeAvg)
datRegionalNationalYearlyAvg <- fillMissing(datRegionalNationalYearlyAvg)
datRegionalNationalDecadeAvg <- fillMissing(datRegionalNationalDecadeAvg)
datNationalAvg <- fillMissing(datNationalAvg)
datRegionalAvg <- fillMissing(datRegionalAvg)




## ----echo=FALSE, message=FALSE, results=FALSE, warning=FALSE, warning=FALSE----

getSocialProsperity <- function(df){
  social_prosperity <- log(df["education_expense"] * df["electricity_access"] * df["life_expectancy"])/ log(df["teacher_per_pupil"] * df["birth_rate"] * df["population_growth"])
  
  return(social_prosperity)
}

getEniviornmenFriendly <- function(df){
  enviornment_friendly <- log(df["population_density"])/ log(df["co2_emission"] * df["greenhouse_emission"]*df["forest_area"])
  
  return(enviornment_friendly)
}


# social prosperity
  datGlobalYearlyAvg['social_prosperity'] <- getSocialProsperity(datGlobalYearlyAvg)
  datRegionalYearlyAvg['social_prosperity'] <- getSocialProsperity(datRegionalYearlyAvg)
  datRegionalDecadeAvg['social_prosperity'] <- getSocialProsperity(datRegionalDecadeAvg)  
  datNationalYearlyAvg['social_prosperity'] <- getSocialProsperity(datNationalYearlyAvg)
  datNationalDecadeAvg['social_prosperity'] <- getSocialProsperity(datNationalYearlyAvg)  
  datNationalAvg['social_prosperity'] <- getSocialProsperity(datNationalAvg)
  datRegionalAvg['social_prosperity'] <- getSocialProsperity(datRegionalAvg)
  datRegionalNationalYearlyAvg['enviornment_friendly'] <- getSocialProsperity(datRegionalNationalYearlyAvg)
  
# enviornemt friendly
  datGlobalYearlyAvg['enviornment_friendly'] <- getEniviornmenFriendly(datGlobalYearlyAvg)  
  datRegionalYearlyAvg['enviornment_friendly'] <- getEniviornmenFriendly(datRegionalYearlyAvg)
  datRegionalDecadeAvg['enviornment_friendly'] <- getEniviornmenFriendly(datRegionalDecadeAvg)  
  datNationalYearlyAvg['enviornment_friendly'] <- getEniviornmenFriendly(datNationalYearlyAvg)
  datNationalDecadeAvg['enviornment_friendly'] <- getEniviornmenFriendly(datNationalDecadeAvg)  
  datNationalAvg['enviornment_friendly'] <- getEniviornmenFriendly(datNationalAvg)
  datRegionalAvg['enviornment_friendly'] <- getEniviornmenFriendly(datRegionalAvg)
  datRegionalNationalYearlyAvg['enviornment_friendly'] <- getEniviornmenFriendly(datRegionalNationalYearlyAvg)
  



## ----echo=FALSE, message=FALSE, warning=FALSE, fig.align="center", out.width = '100%'----

p <- ggplot(datRegionalYearlyAvg, aes(year, gdp_capita, color = region)) + 
  geom_line() + 
  geom_segment(aes(xstart=1960, xend = 2018, yend = gdp_capita), linetype = 2, colour = 'grey') +
  geom_point(size = 2) + 
  geom_text(aes(x = 2018, label = region), hjust = 0) + 
  transition_reveal(year) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'National GDP', x='Year', y = 'GDP per Capits') + 
  theme_minimal() + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5)) +
  ease_aes('linear', interval = 0.0001)

animate(p, nframes = 300, fps=10)



## ----echo=FALSE, message=FALSE, warning=FALSE, fig.align="center", out.width = '100%'----

p <- ggplot(datNationalYearlyAvg, aes(year, gdp_capita, color = country)) + 
  geom_line() + 
  geom_segment(aes(xstart=1960, xend = 2018, yend = gdp_capita), linetype = 2, colour = 'grey') +
  geom_point(size = 2) + 
  geom_text(aes(x = 2018, label = country), hjust = 0) + 
  transition_reveal(year) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'National GDP', x='Year', y = 'GDP per Capits') + 
  theme_minimal() + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5)) +
  ease_aes('linear', interval = 0.0001)

animate(p, nframes = 300, fps=10)



## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%'----

getCorrelation <- function(df){
# slice important cols
  cols <- c(
    "social_prosperity",
    "enviornment_friendly",

    "gdp_growth", # GDP growth (annual %)
    "gni_capita", # GNI per capita (constant 2010 US$)
    # "gni_capita_constant", # GNI per capita (constant LCU)
    
    "education_expense", # Government expenditure on education, total (% of government expenditure)
    "electricity_access", # Access to electricity (% of population)
    "population_growth", # Population growth (annual %)
    "life_expectancy", # Life expectancy at birth, total (years)
    
    "co2_emission", # CO2 emissions (metric tons per capita)
    "forest_area", # Forest area (sq. km)
    "population_density" # Population density (people per sq. km of land area)
  )
  df.slice <- df[,cols]
  
  
  df.slice.corr <- rcorr(as.matrix(df.slice))
  
  return(df.slice.corr)
}

par(mfrow=c(1,2))

# plot 
  # pre-2000
  datGlobalYearlyAvg.slice.corr.1960 <- getCorrelation(datNationalYearlyAvg[datNationalYearlyAvg$year<2000,])
    corrplot(datGlobalYearlyAvg.slice.corr.1960$r, type="upper",
         p.mat = datGlobalYearlyAvg.slice.corr.1960$P, sig.level = 0.01, insig = "blank")
  # 
  # post-2000
  datGlobalYearlyAvg.slice.corr.2018 <- getCorrelation(datNationalYearlyAvg[datNationalYearlyAvg$yea>=2000,])
  corrplot(datGlobalYearlyAvg.slice.corr.2018$r, type="upper",
         p.mat = datGlobalYearlyAvg.slice.corr.2018$P, sig.level = 0.01, insig = "blank")



## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----

  economic_indicators <- c(
    "gdp_capita", # GDP per capita (current US$)
    "gdp_growth", # GDP growth (annual %)
    "gdp_capita_growth", # GDP per capita growth (annual %)
    "expense_gdp", # Expense (% of GDP)
    "net_income" # GNI per capita (constant LCU)
  )

  social_indicators <- c(
    "teacher_per_pupil", # Pupil-teacher ratio, primary
    "education_expense", # Government expenditure on education, total (% of government expenditure)
    "electricity_access", # Access to electricity (% of population)
    "birth_rate", # Birth rate, crude (per 1,000 people)
    "population_growth" # Population growth (annual %)
  )
  
    environmental_indicators <- c(
    "co2_emission", # CO2 emissions (metric tons per capita)
    "greenhouse_emission", # Total greenhouse gas emissions (kt of CO2 equivalent)
    "forest_area", # Forest area (sq. km)
    "population_density", # Population density (people per sq. km of land area)
    "life_expectancy" #Life expectancy at birth, total (years)
  )
    
read_data <- read.csv('wdi-f.csv')

test <- read_data[FALSE,]

for(index in 1960:2020){
    test <- rbind(test, read_data[read_data$year==index,])
}

gg <- ggplot(test, aes(net_income, life_expectancy, color = region)) +
  geom_point(aes(size = population_density, frame = year, ids = country)) +
  scale_x_log10()+ labs(x = "GNI per capita")

ggplotly(gg)





## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%'----

# normalize
datRegionalYearlyAvg$population_growth <- normalize(datRegionalYearlyAvg$population_growth)

# extract region
region1 <- datRegionalYearlyAvg[datRegionalYearlyAvg$region=="North America",]
region2 <- datRegionalYearlyAvg[datRegionalYearlyAvg$region=="Scandinavia",]
region3 <- datRegionalYearlyAvg[datRegionalYearlyAvg$region=="South Asia",]
region4 <- datRegionalYearlyAvg[datRegionalYearlyAvg$region=="Middle East",]


# plot
plot(region1$year, region1$population_growth*100, ylim=c(0,100), type = "o", col = 100, pch=19, xlab="Year", ylab = "Growth %", main = "Regional Population Growth")
lines(region2$year, region2$population_growth*100, type = "o", col = 110, pch=20)
lines(region3$year, region3$population_growth*100, type = "o", col = 120, pch=21)
lines(region4$year, region4$population_growth*100, type = "o", col = 130, pch=22)

legend("topright",
  legend = c("North America", "Scandinavia", "South Asia", "Middle East"),
  col = c(100,110, 120,130),
  pch = c(19,20,21,22)
)


## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----


#Data has been merged before and saved it as a new csv for the sake of simplicity
grouped_comparison_wide <- read.csv('Group_comparison.csv')

grouped_comparison_long <- wide_to_long(grouped_comparison_wide)

grouped_comparison_long <- renameIndicators(grouped_comparison_long)

grouped_comparison_long<-grouped_comparison_long[!is.na(grouped_comparison_long$value),]

grouped_comparison_indicators_wide <- dcast(grouped_comparison_long, year+country ~ indicator, value.var="value")
#grouped_comparison_indicators_wide[, -c(1,2)] <- scale(grouped_comparison_indicators_wide[, -c(1,2)])


long<-melt(grouped_comparison_indicators_wide, id.vars=c("year","country"))

llong<-within(long, {
    group = ifelse(variable %in% social_indicators, "Social", "Environmental")
    group = ifelse(variable %in% environmental_indicators, "Environmental", "Economical")
})

check_val <- function(x) {
  #print(x)
  if(x%in% social_indicators) {
    "Social"
    #print("Social")
  }
  else if(x%in% environmental_indicators) {
    "Environmental"
    #print("Environmental")
  }
  else if(x%in% economic_indicators) {
    "Economical"
    #print("Economical")
  }
}

llong$group <- apply(array(long[["variable"]]),MARGIN=1, FUN=check_val)
llong <- llong[!is.na(llong$value),]
#llong<-llong[llong$year<-2008,]
llong$year <-as.numeric(as.character(llong[,1])) 




## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----

plot_circular_bar<-function(llong,var1,var2){
  
  # library
  library(tidyverse)
  llong<-llong[llong$group==var1,]
  llong<-llong[llong$country==var2,]
  #view(llong)
  llong$variable<-factor(llong$variable)
  #print( llong$variable) 
  # Create dataset
  if (var1=="Environmental")
  {
    for (i in 1:length (environmental_indicators)){
      #print(environmental_indicators[i])
      llong[llong$variable==environmental_indicators[i],]$value = normalize(llong[llong$variable==environmental_indicators[i],]$value)
      
    }
      data=data.frame(
        individual=llong$year,
        group=llong$variable ,
        value=llong$value*50)
  }
  else if (var1=="Economical"){
    
        for (i in 1:length (economic_indicators)){
      #print(environmental_indicators[i])
      llong[llong$variable==economic_indicators[i],]$value = normalize(llong[llong$variable==economic_indicators[i],]$value)
      
    }
    
      data=data.frame(
        individual=llong$year,
        group=llong$variable ,
        value=llong$value*50)
  }
  else{
            for (i in 1:length (social_indicators)){
      #print(environmental_indicators[i])
      llong[llong$variable==social_indicators[i],]$value = normalize(llong[llong$variable==social_indicators[i],]$value)
      
    }
    
      data=data.frame(
        individual=llong$year,
        group=llong$variable ,
        value=llong$value*50)
  }

  #data = data %>% arrange(group, value)    
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar=2
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$group=rep(levels(data$group), each=empty_bar)
  data=rbind(data, to_add)
  data=data %>% arrange(group)
  data$id=seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data=data %>% 
    group_by(group) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data = base_data
  grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  grid_data=grid_data[-1,]
  
  
  # Make the plot
  p = ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=1 , angle=0, fontface="bold", hjust=1) +
    
    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    ylim(-100,120) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=value+60, label=individual, hjust=hjust), color="black",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )
  
  #   if (var1=="Economical")
  #   {
  #       #print(var1)
  #       #print(var2)
  #       if(var2=="Low income"){
  #         geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0), colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)
  #       }
  #         else
  #       {
  #                   geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,1,0), colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)
  #       }
  #   }
  # else
  # {
  #         #print(base_data)
  #         geom_text(data=base_data[1:5,], aes(x = title, y = -18, label=group), hjust=c(1,1,1,0,1), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)
  # }
 if(var1=="Economical" && var2=="Low income")
 {
   #print(paste(var2,":",var1))
   #"#9ACD32", "#00CED1", "#BA55D3"
   p + scale_fill_manual(paste(var2,":",var1), values = c("gdp_growth" = "#9ACD32", "gdp_capita" = "#00CED1", "gdp_capita_growth" = "#BA55D3"))
 }
  else{
     p + scale_fill_discrete(name = paste(var2,":",var1))
  }
  
  
  #print(data$group)
}




## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----

plot_circular_bar(llong,"Social","High income")



## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----

plot_circular_bar(llong,"Social","Middle income")



## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----

plot_circular_bar(llong,"Social","Low income")



## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----

plot_circular_bar(llong,"Environmental","High income")



## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----

plot_circular_bar(llong,"Environmental","Middle income")



## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----

plot_circular_bar(llong,"Environmental","Low income")




## ----echo=FALSE, message=FALSE, results=FALSE, fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----

plot_circular_bar(llong,"Economical","High income")




## ----fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----

plot_circular_bar(llong,"Economical","Middle income")




## ----fig.align="center", out.width = '100%', message=FALSE, warning=FALSE----

plot_circular_bar(llong,"Economical","Low income")



## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------

ggplot(datRegionalDecadeAvg, aes(decade, social_prosperity, fill = social_prosperity)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  facet_wrap(~region) +
  labs(x = 'Decade', y = 'Social Prosperity Score') +
  transition_states(decade) +
  shadow_mark()

