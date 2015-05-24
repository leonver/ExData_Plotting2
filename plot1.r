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
#1.Have total emissions from PM2.5 decreased in the United States from 
# 1999 to 2008? Using the base plotting system, make a plot showing the
#total PM2.5 emission from all sources for each of the years 1999, 2002, 
#2005, and 2008

library(plyr)

#Check the number of observation per year
        
    NumberOfObservationsPerYear <- count(NEI, c("year"))    
    #        Var1    Freq
    #        1 1999 1108469
    #        2 2002 1698677
    #        3 2005 1713850
    #        4 2008 1976655    
        
#Check the number of observation per year
    NumberOfPollutant <- count(NEI, "Pollutant")    
    #        Var1    Freq
    #       1 PM25-PRI 6497651# PM25-PRI 6497651        

#the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year
# check the number of observation per source , type and year         
    UniquePoints <- count(NEI, c("SCC", "type", "year")) 
    #        SCC  type year freq
    #        1 10100101 POINT 1999   23
    #        2 10100101 POINT 2002   16
    #        3 10100101 POINT 2005   18
    #        4 10100101 POINT 2008   39
    #        5 10100102 POINT 1999    2
    #        6 10100102 POINT 2002    1        

#There are more than one obeservations per year, we take the average per source, type and  year)
        
    PointsStat <- ddply(NEI, c("SCC", "type", "year"), 
                  summarize, EmmissionMean =mean(Emissions), 
                             NrObs = length(Emissions))
        
#Not every point has observation for every year only the points with an observation for every year 
#are taken as data for the plot.    
    PointsYearCount <- count(PointsStat, c("SCC", "type"))    
        
    EveryYearPoints <- subset(PointsYearCount, freq == 4)
        
#Select the data with values for every year
        
    toplot.data <- merge(x = PointsStat, y= EveryYearPoints, by= c("SCC", "type"))      
    
#Summarize amount for each year
        
    plot.data <- ddply(toplot.data, "year", summarize, SumEmmission = sum(EmmissionMean))   
        
png('plot1.png', width = 480, height= 480, units = "px")
plot(plot.data, type = "l", xlab = "year", ylab = "Emmision in tons", main = "Totaal emmission PM2.5")       
dev.off()        
        
      