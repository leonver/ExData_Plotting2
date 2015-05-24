## this script creates a graph from data downloaded from the internet.

# Conditional download data from the internet

    # Create a data folder for the data to download
        if (!file.exists("downloaddata")) 
        {
            dir.create("./downloaddata")
        }
    
    #name to give to the dataset to download
        dataset.zip <- "./downloaddata/dataset.zip"
    
    #check if the data is already present in the folder
        
        if (!file.exists(dataset.zip))
        {
            #location of the file to download
            downloadurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
            #execute download
            download.file(downloadurl, destfile=dataset.zip, mode="wb")
            #extract the data from the zipfile
            unzip(dataset.zip, exdir="./downloaddata")
        }
        
#read the data if it's not already available in memory
    if (!exists("NEI"))
    {        
        NEI <- readRDS("downloaddata/summarySCC_PM25.rds")
        if (nrow(NEI[!complete.cases(NEI),]) == 0 )
        {    
            message("No NA's in dataset NEI")
            #check duplicates
        }    
        else message("Dataset NEI with NA's, take further action")
        SCC <- readRDS("downloaddata/Source_Classification_Code.rds")
        if (nrow(SCC[!complete.cases(SCC),]) == 0)
        {    
            message("No NA's in dataset SCC")
        }    
        else 
        {
            message("Dataset SCC with NA's take further action")
            #variables Map.To (11358) and Last.Inventory.Year (8972) contains NA's
        }    
    }    
#6.Compare emissions from motor vehicle sources in Baltimore City with emissions
#from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

library(plyr)

#Select Road data for Balitmore and Los Angeles County
    OnRoadCities.data <- subset(NEI, ((fips == "24510") | (fips == "06037")) & (type == "ON-ROAD"))   
    colnames(OnRoadCities.data) <- c("city", "SCC", "Pollutant", "Emissions", "type", "year")  
    OnRoadCities.data$year <- factor(OnRoadCities.data$year, levels = c('1999', '2002', '2005', '2008'))    
    OnRoadCities.data$city[OnRoadCities.data$city == "24510"] <- "Baltimore"  
    OnRoadCities.data$city[OnRoadCities.data$city == "06037"] <- "Los Angeles"  
        
#the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year
# check the number of observation per source , type and year         
    UniquePoints <- count(OnRoadCities.data, c("SCC", "year")) 

#There are more than one obeservations per year, we take the average per source, type and  year)
        
    PointsStat <- ddply(OnRoadCities.data, c("city", "SCC",  "year"), 
                  summarize, EmmissionMean =mean(Emissions), 
                             NrObs = length(Emissions))
        
#Not every point has observation for every year only the points with an observation for every year 
#are taken as data for the plot.    
    PointsYearCount <- count(PointsStat, "SCC") 
#For Baltimore there are no measurments for each year in the period of 1999 upto 2008 
# so for this assignment I leave the filter away for a range of measurements for every year        
    

#Summarize amount for each year

plot.data <- ddply(PointsStat, c("city","year"), summarize, SumEmmission = sum(EmmissionMean))  
        
library(ggplot2)
        
png('plot6.png', width = 480, height= 480, units = "px")
g <- ggplot(data = plot.data, aes(x=year, y = SumEmmission)) 
g <- g + geom_bar(aes(fill= year), stat = "identity")
g <- g + guides(fill = FALSE) 
g <- g + ggtitle("PM 2.5 vehhicle emissions in Baltimore & LA") 
g <- g + ylab("Total PM 2.5 emission") 
g <- g + xlab("year")
g <- g + facet_grid(. ~ city) 
g <- g + geom_text(aes(label = round(SumEmmission), size = 1, hjust = 0.5, vjust = -1)) 
print(g)        
dev.off()        

        
       

        
        
      