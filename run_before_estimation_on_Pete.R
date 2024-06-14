# Set up root directory
user <- Sys.info()["user"]
root_dir <- file.path("/home", user, "AgMIP-Calibration-Phase-IV")


for(file in c("FR.SOL")){
  file.copy(file.path(root_dir, "DSSAT-files", file), file.path("/home", user, "DSSAT48/Soil", file), overwrite = TRUE)
}

for(file in c("STATION1.WTH", "STATION2.WTH", "STATION3.WTH", "STATION4.WTH", "STATION5.WTH", "STATION6.WTH", "STATION7.WTH")){
  file.copy(file.path(root_dir, "DSSAT-files", file), file.path("/home", user, "DSSAT48/Weather", file), overwrite = TRUE)
}

for(file in c("WHCER048.CUL", "WHCER048.ECO", "WHCRP048.CUL", "WHCRP048.ECO", "WHAPS048.CUL", "WHAPS048.ECO")){
  file.copy(file.path(root_dir, "DSSAT-files", file), file.path("/home", user, "DSSAT48/Genotype", file), overwrite = TRUE)
}

for(file in c("FRWHCER1.WHA", "FRWHCER1.WHT", "FRWHCER1.WHX",
              "FRWHCRP1.WHA", "FRWHCRP1.WHT", "FRWHCRP1.WHX",
              "FRWHAPS1.WHA", "FRWHAPS1.WHT", "FRWHAPS1.WHX")){
  file.copy(file.path(root_dir, "DSSAT-files", file), file.path("/home", user, "DSSAT48/Wheat", file), overwrite = TRUE)
}