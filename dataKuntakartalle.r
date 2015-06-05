library(sp)
library(leaflet)

#' Load Geo-data file through gisfin-library
#'
#' @param  mapID       String  Map ID to be loaded, default "Yleiskartta-4500"
#' @param  dataID      String  Data ID to be loaded, default "HallintoAlue"
#' @param  setGlobal    Boolean default TRUE - sets sp as global object, 
#'                              if another name to be assigned use FALSE to return the object
#' @return              sets global sp named SparialPolygonsDataFrame -object (if used fOpenGov-datas) in longlat-format, 
#'                      if setGlobal set to FALSE returns the same object
#' @examples            
#' loadGeoData()
#' map <- loadGeoData(FALSE)
#' 
loadGeoData <- function(setGlobal=T, mapID = "Yleiskartta-4500", dataID="HallintoAlue"){
  require(gisfin)
  
  sp <- get_mml(map.id="Yleiskartta-4500", data.id="HallintoAlue")
  
  sp <- spTransform(sp, CRS("+proj=longlat +datum=WGS84"))
  
  if(setGlobal) {
    sp <<- sp
  } else {
    return(sp)
  }
}

#' Open Geo-data file and create data-object.
#'
#' @param  workDir      String  path to where data-file is, default is current working direcotry (using getwd())
#' @param  fileName     String  filename, without extension, 
#' @param  setGlobal    Boolean default TRUE - sets sp as global object, 
#'                              if another name to be assigned use FALSE to return the object
#' @return              sets global sp named SparialPolygonsDataFrame -object (if used fOpenGov-datas) in longlat-format, 
#'                      if setGlobal set to FALSE returns the same object
#' @examples            
#' openLocalGeoRData('C:/maps','finmap')
#' map <- openLocalGeoRData('C:/maps','finmap', FALSE)
#'                      
openLocalGeoRData <- function(workDir = getwd(), fileName, setGlobal=T) {
  
  load(paste(workDir, "/", fileName, ".RData", sep=""))
  
  sp <- spTransform(sp, CRS("+proj=longlat +datum=WGS84"))
  
  if(setGlobal) {
    sp <<- sp
  } else {
    return(sp)
  }
}

#' Merge data into map
#'
#' @param  data      data.frame  dataset containing data for merging
#' @param  dataKey   String      variable name in data to make merge on
#' @param  geoData   sp-object   geospatial dataset in longlat-format, default sp-named
#' @param  geoKey    String      variable name in geospatial data to make merge on, default 'Kunta'            
#' @param  setGlobal Boolean default TRUE - sets sp as global object, 
#'                              if another name to be assigned use FALSE to return the object
#' @return              sets global spMerged named SparialPolygonsDataFrame -object, 
#'                      if setGlobal set to FALSE returns the same object
#' @examples            
#' mergeDataIntoMap(dt, "Kuntakoodi")
#' mergeDataIntoMap(dt, "Kuntakoodi", map, "Kunta_ni1", FALSE)
#'                      
mergeDataIntoMap <- function(data, dataKey, geoData = sp, geoKey = "Kunta", setGlobal=T) {
  
  spMerge <- merge(geoData, data, by.x=geoKey, by.y=dataKey, all.x = TRUE)
  
  if(setGlobal) {
    spMerged <<- spMerge
  } else {
    return(spMerge)
  }
}

#' Vizualize data onto map
#'
#' @param  data      data.frame  dataset containing variable for vizualization
#' @param  varToMap  String      variable name for vizualization on map
#' @param  varText   String      description text for showing in popups on map
#' @param  setGlobal Boolean default TRUE - sets sp as global object, 
#'                              if another name to be assigned use FALSE to return the object
#' @return              sets global readyMap named leaflet -object, 
#'                      if setGlobal set to FALSE returns the same object
#' @examples            
#' vizualizeDataOnMap("varhemaksu", "Varhe-maksu vuonna 2012")
#' leafletMap <- vizualizeDataOnMap("varhemaksu", "Varhe-maksu vuonna 2012", dt, FALSE, colorNumeric(c("yellow", "blue"), NULL, na.color = "#FFFFFF"), "#FFFFFF")
#'                      
vizualizeDataOnMap <- function(varToMap, varText, mergedData = spMerged, setGlobal=T, palette = colorNumeric(c("red", "yellow", "green"), NULL, na.color = ""), borderColor="#000000") {
  
  state_popup <- paste0("<strong>Kunta: </strong>", mergedData$Kunta_ni1, 
                        "<br><strong>", varText, ": </strong>", 
                        round(mergedData@data[,c(varToMap)], digits=2))
  
  k = leaflet(data = mergedData) %>% addTiles() %>% 
    addPolygons(fillColor = ~palette(get(varToMap)), 
                fillOpacity = 0.7, 
                color = borderColor, 
                weight = 1,
                popup = state_popup)
  
  if(setGlobal) {
    readyMap <<- k
  } else {
    return(k)
  }
}