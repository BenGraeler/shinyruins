library(ncdf4)

# netcdf location
nc_file <- "~/Downloads/monthly_cordex_coast.nc"

# data folder
data_folder <- "data/"

nc_con <- ncdf4::nc_open(nc_file)
days_since <- ncdf4::ncvar_get(nc_con, "time")
ncatt_get(nc_con, "time")$units
time <- as.POSIXct("2006-01-31") + days_since*24*3600
varValNames <- nc_con$dim$vars$vals

# export to csv
for (setName in names(nc_con$var)) {
  df <- t(ncdf4::ncvar_get(nc_con, setName))
  colnames(df) <- varValNames
  df <- cbind(time, df)
  write.table(df, file=paste0(data_folder,"/",setName, ".csv"), row.names = FALSE, dec=".", sep=";")
}

# export to json
library(rjson)

timeInfo <- as.character(time)
dec <- paste0(substr(timeInfo,1,3),0)
mon <- substr(timeInfo,6,7)

varNames <- names(nc_con$var)

# ACHTUNG: Längen der Bezeichner variieren
gcmMod <- sapply(strsplit(varNames, ".", fixed = T), 
                 function(x) x[length(x)-3])
rcmMod <- sapply(strsplit(varNames, ".", fixed = T), 
                 function(x) x[3])
rcpMod <- sapply(strsplit(varNames, ".", fixed = T), 
                 function(x) x[length(x)])

unMod <- unique(sapply(strsplit(varNames, ".", fixed = T), 
                       function(x) paste0(x[c(length(x)-3,3,length(x))], collapse = ".")))

for (mod in unMod) { # mod <- unMod[41]
  modCom <- strsplit(mod, ".", fixed=T)[[1]]
  
  relSets <- varNames[gcmMod %in% modCom[1] & rcmMod %in% modCom[2] & rcpMod %in% modCom[3]]
  
  df <- NULL
  for (setName in relSets) { # setName <- relVar[1]
    dfRows <- as.data.frame(t(ncdf4::ncvar_get(nc_con, setName)))
    colnames(dfRows) <- varValNames
    
    dfRows$dec <- as.numeric(dec)
    dfRows$mon <- as.numeric(mon)
    df <- rbind(df, dfRows)
  }
  
  dir.create(mod)
  
  for (i in 1:(ncol(df)-2)) {
    writeLines(toJSON(df[,c(17,18,i)]), paste0(mod, "/", mod, ".", varValNames[i], ".json"))
  }
}
