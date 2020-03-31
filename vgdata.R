## Getting data from VG.no ##

get_vgdata <- function(){
  ## Big numbers and particular cases ##
  vgraw0 <- GET(url="https://redutv-api.vg.no",path="/corona/v1/sheets/norway-region-data")
  vgraw1 <- rawToChar(vgraw0$content)
  vgraw2 <- fromJSON(vgraw1)
  cases <- do.call("rbind",vgraw2$casesList)
  
  vgraw0 <- GET(url="https://redutv-api.vg.no",path="/corona/v1/sheets/fhi")
  vgraw1 <- rawToChar(vgraw0$content)
  vgraw2 <- fromJSON(vgraw1)
  tested.ts <- vgraw2$tested$timeseries
  infsource.ts <- vgraw2$infectionSources$timeseries
  reportssource.ts <- vgraw2$reports$timeseries
  
  vgraw0 <- GET(url="https://redutv-api.vg.no",path="/corona/v1/areas/country/reports")
  vgraw1 <- rawToChar(vgraw0$content)
  vgraw2 <- fromJSON(vgraw1)
  hospitals <- vgraw2$hospitals$hospitals
  
  return(list(cases=cases,tested.ts =tested.ts,infsource.ts=infsource.ts,reportssource.ts=reportssource.ts,hospitals=hospitals))
}

vgdata <- get_vgdata()