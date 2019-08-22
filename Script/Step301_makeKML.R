#Ben messing with John's original code (makeKML_v3)
##### STEP 3.1: output KML to visualize in google earth
# Created: Jan 2019
# Wenjing messing with Ben's adapted code (Step301_CreateKML)

# input: PAPO_Mig_Cycle.csv  // JMH_mig_Cycle.csv
# output: all the KML in analysis/gis

library(rgdal)
library(sp)
library(plotKML)

setwd("/Users/Mushy/Google Drive (wenjing.xu@berkeley.edu)/RESEARCH/Pronghorn/Analysis/GIS/KML")

makeKML <- function(x,# Spatial Points object
                    id.col = "ID",
                    long.col = "Longitude",
                    lat.col = "Latitude",
                    timestamp = "time",
                    layer.name){
      
  if(all(c("rgdal","plotKML") %in% installed.packages()[,1])==FALSE)
    stop("You will need to install the following packages: rgdal, plotKML")
  x@data <- as.data.frame(x@data) # tibbles will break the code
      #Check input class
      stopifnot(class(x) == "SpatialPointsDataFrame")
      stopifnot(any(class(x@data[,timestamp]) == c('POSIXct','POSIXt')))
      #Extract extra field names. #Ben - 06232017 I don't want this
      # extra <- NULL
      # if(length(names(x[[1]]))>6){
      #       extra <- names(x[[1]])[4:(length(names(x[[1]]))-3)]
      # }
      
      #Specify and convert colors for icons/lines and balloons
      require(plotKML)
      color <- rainbow(length(unique(x@data[,id.col])))
      color <- col2kml(color)
      bcolor <- rainbow(length(x), s=0.4)
      bcolor <- col2kml(bcolor)
      
      #Create file
      kml <- file(paste(layer.name, ".kml", sep=""), "w")
      
      #Write kml head
      writeLines(c('<?xml version="1.0" encoding="utf-8" ?>', '<kml xmlns="http://www.opengis.net/kml/2.2">', '<Document id="root_doc">'), kml)
      
      #Write Styles
      writeLines(c('<Style id="mark">','<IconStyle><scale>0.5</scale><Icon><href>http://maps.google.com/mapfiles/kml/pal2/icon18.png</href></Icon></IconStyle>', '<LabelStyle><scale>0</scale></LabelStyle>', '<BalloonStyle><text><center><table border="1" style="font-weight:bold">', '<tr><td>ID</td><td>$[/ID]</td></tr>', '<tr><td>Time</td><td>$[/Time]</td></tr>', '<tr><td>Latitude</td><td>$[/Latitude]</td></tr>', '<tr><td>Longitude</td><td>$[/Longitude]</td></tr>'), kml)
      
      # #Write extra styles #Ben 06232017 I don't want this either
      # if(!is.null(extra)){
      #       for(p in 1:length(extra)){
      #             writeLines(c(paste('<tr><td>', extra[p], '</td><td>$[/', extra[p], ']</td></tr>', sep="")), kml)
      #       }
      # }
      
      #End styles
      writeLines(c('</table></center></text></BalloonStyle>', '</Style>'), kml)
      
      #Write line styles to hide balloons
      writeLines(c('<Style id="lines"><LineStyle><width>3</width></LineStyle>', '<BalloonStyle><displayMode>hide</displayMode></BalloonStyle></Style>'), kml)
      
      #Write schema
      writeLines(c('<Schema name="Turt" id="Turtle">', '<SimpleField name="ID" type="string"></SimpleField>', '<SimpleField name="Time" type="string"><displayName>Time</displayName></SimpleField>', '<SimpleField name="Longitude" type="string"></SimpleField>', '<SimpleField name="Latitude" type="string"></SimpleField>'), kml)
      
      #Write extra schemas
      # if(!is.null(extra)){
      #       for(n in 1:length(extra)){
      #             writeLines(c(paste('<SimpleField name="', extra[n], '" type="float"></SimpleField>', sep="")), kml)
      #       }
      # }
      
      #End Schema
      writeLines('</Schema>', kml)
      
      #Ben add on 06232017
      ids <- unique(x@data[,id.col])
      
      # coords <- x@data[,c(long.col,lat.col)]
      
      dates <- x@data[,timestamp]
      #For each turtle... #ha! Cause John studied twutles. 
      for(i in 1:length(ids)){
            sub <- x@data[x@data[,id.col] == ids[i],]
            # sub <-subset(x@data,)
            #Make a folder for the turtle
            writeLines(c("<Folder><name>", ids[i], "</name>"), kml)
            
            #Make a folder for the points
            writeLines(c("<Folder><name>Points</name>"), kml)
            coords <- sub[,c(long.col,lat.col)]
            #And for each data point for said turtle make a point
            for(j in 1:dim(coords)[1]){
                  
                  #Make placemark named by time
                  # writeLines(c("<Placemark><name>", as.character(x[[i]]@.Data[[1]][j]), "</name>"), kml)
                   writeLines(c("<Placemark><name>", as.character(sub[,timestamp][j]), "</name>"), kml)
                  
                  #Reference styles
                  writeLines('<styleUrl>#mark</styleUrl>', kml)
                  
                  #Write TimeStamp
                  # writeLines(c("<TimeStamp><when>", gsub(" ", "T", gsub(" EST", "", x[[i]]@.Data[[1]][j])), "</when></TimeStamp>"), kml)
                  writeLines(c("<TimeStamp><when>", gsub(" ", "T", gsub(" EST", "", sub[,timestamp][j])), "</when></TimeStamp>"), kml)
                  
                  #Set icon and balloon color
                  writeLines(c("<Style><IconStyle><color>", color[i],'</color></IconStyle>', '<BalloonStyle><bgColor>', bcolor[i], '</bgColor></BalloonStyle>', '</Style>'), kml)
                  
                  #Reference schema
                  writeLines(c('<ExtendedData><SchemaData schemaUrl="#Turt">'), kml)
                  
                  #Write simple data
                  writeLines(paste('<SimpleData name="ID">', ids[i], '</SimpleData>', sep=""), kml)
                  writeLines(paste('<SimpleData name="Time">', as.character(sub[,timestamp][j]), '</SimpleData>', sep=""), kml)
                  writeLines(paste('<SimpleData name="Latitude">', as.character(coords[j,2]), '</SimpleData>', sep=""), kml)
                  writeLines(paste('<SimpleData name="Longitude">', as.character(coords[j,1]), '</SimpleData>', sep=""), kml)
                  
                  #Write extra simple data
                  # if(!is.null(extra)){
                  #       for(o in 1:length(extra)){
                  #             writeLines(c(paste('<SimpleData name="', extra[o], '">', sep=""), x[[i]]@.Data[[3+o]][j], '</SimpleData>'), kml)
                  #       }
                  #       
                  # }
                  
                  #End schema
                  writeLines('</SchemaData></ExtendedData>', kml)
                  
                  #Write coordinates
                  writeLines(c('<Point><coordinates>', paste(coords[j,1], coords[j,2]),'</coordinates>', '</Point>'), kml)
                  
                  #End placemark
                  writeLines("</Placemark>", kml)
                  
            } # End nested for loop j
            
            #Close "Points" folder
            writeLines("</Folder>", kml)
            
            #If there are at least 2 points to connect...
            if(dim(coords)[1]>1){
                  
                  #Make a folder for linestrings
                  writeLines('<Folder><name>Lines</name>', kml)
                  
                  #Connect every 2 data points with a line
                  # for(k in 1:(length(x[[i]]@.Data[[1]])-1)){
              for(k in 1:(dim(coords)[1]-1)){
                        
                        writeLines('<Placemark>', kml)
                        
                        #Reference style
                        writeLines('<styleUrl>#lines</styleUrl>', kml)
                        
                        #Write timestamp
                        writeLines(c("<TimeStamp><when>", gsub(" ", "T", gsub(" EST", "", sub[,timestamp][k+1])), "</when></TimeStamp>"), kml)
                        
                        #Set line color
                        writeLines(c("<Style><LineStyle><color>", color[i],'</color></LineStyle></Style>'), kml)
                        
                        #Write coordinates
                        # writeLines(c('<LineString><coordinates>', paste(x[[i]]@.Data[[2]][k], x[[i]]@.Data[[3]][k], 0, sep=","), paste(x[[i]]@.Data[[2]][k+1], x[[i]]@.Data[[3]][k+1], 0, sep=","), '</coordinates></LineString>', '</Placemark>'), kml)
                        writeLines(c('<LineString><coordinates>', paste(coords[,1][k], coords[,2][k], 0, sep=","), paste(coords[,1][k+1], coords[,2][k+1], 0, sep=","), '</coordinates></LineString>', '</Placemark>'), kml)
                        
                  }
                  
                  #Close "Lines" Folder
                  writeLines('</Folder>', kml)
            
            }
            
            #Close turtle folder
            writeLines('</Folder>', kml)
            
      }
      
      #Close kml
      writeLines("</Document></kml>", kml)
      
      #Close connection
      close(kml)
      
}


prongs <- read.csv("PAPO_Mig_Cycle.csv")
prongs$time <- as.POSIXct(strptime(as.character(prongs$time), "%m/%d/%y %H:%M",tz = "America/Denver"))
prongs <- prongs[!is.na(prongs$Easting),]
prongs$ID <- paste0("PAPO_", prongs$Location.ID)

geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
prongs <- spTransform(prongs, CRS(geo.prj)) 
proj4string(prongs)
prongs$Longitude <- coordinates(prongs.1)[,1]
prongs$Latitude <- coordinates(prongs.1)[,2]
  
coordinates(prongs) <- c("Easting", "Northing")
proj4string(prongs) <- CRS("+init=epsg:32612")

for(i in unique(prongs$ID)) {
  x <- prongs[prongs$ID == i, ]
  makeKML(x, layer.name = i )
}

# for JMH ---
prongs <- read.csv("JMH_Mig_Cycle.csv")
prongs$time <- as.POSIXct(strptime(as.character(prongs$time), "%m/%d/%y %H:%M",tz = "America/Denver"))
prongs <- prongs[!is.na(prongs$Easting),]
prongs$ID <- paste0("PAPO_", prongs$Location.ID)

coordinates(prongs) <- c("Easting", "Northing")
proj4string(prongs) <- CRS("+init=epsg:32612")

geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
prongs <- spTransform(prongs, CRS(geo.prj)) 
proj4string(prongs)
prongs$Longitude <- coordinates(prongs)[,1]
prongs$Latitude <- coordinates(prongs)[,2]


for(i in unique(prongs$ID)) {
  x <- prongs[prongs$ID == i, ]
  makeKML(x, layer.name = i )
}
