# Load packages
library(tidyverse)
library(DSSAT)

# Set up working directory
user <- Sys.info()["user"]
root_dir <- paste0("/carl-data/shared/users/", user, "/AgMIP-Calibration-Phase-IV")
run_dir <- paste0(root_dir, "/DSSAT-files")
dir.create(run_dir, recursive = TRUE)
setwd(run_dir)

################################################################################

obs_data <-
  read_table("/carl-data/shared/users/mmehmoo/AgMIP-Calibration-Phase-IV/data/cal_4_obs_French_A.txt", col_names = TRUE) %>% 
  mutate(TNAME = paste0(Site, "_", HarvestYear) %>% str_replace_all(., c("_([0-9])_" = "_0\\1_", "_" = "")),
         DATE = as.POSIXct(Date, tz = "UTC", format = "%d/%m/%Y")) %>%
  arrange(TNAME)

input_data <-
  read_table("/carl-data/shared/users/mmehmoo/AgMIP-Calibration-Phase-IV/data/cal_4_input_data_French_A.txt", col_names = TRUE) %>% 
  mutate(TNAME = paste0(Site, "_", HarvestYear) %>% str_replace_all(., c("_([0-9])_" = "_0\\1_", "_" = ""))) %>% 
  arrange(TNAME) %>% 
  mutate(TRNO = 1:n())

treatment_use <-
  input_data %>%
  select(TRNO, TNAME) %>%
  mutate(use = ifelse(TNAME %in% unique(obs_data$TNAME), 
                      "calibration", 
                      "validation"))

obs_data <-
  treatment_use %>%
  select(TRNO, TNAME) %>% 
  right_join(., obs_data)

################################################################################

# Weather files
read_station <- function(sheet){
  weather_data <-
    readxl::read_xlsx("/carl-data/shared/users/mmehmoo/AgMIP-Calibration-Phase-IV/data/cal_4_weather.xlsx",
                      sheet = paste0(sheet), skip = 4) %>% 
    select(5:8,14) %>%
    rename(DATE = ...14) %>% 
    mutate(DATE = as.POSIXct(DATE, tz = "UTC", format = "%d/%m/%Y")) %>% 
    select(DATE, TMIN, TMAX, RAIN, SRAD)
  
  return(weather_data)
}

# STATION1
read_station("station_1") %>% 
  write_wth("STATION1.WTH", 
            location = "AgMIP Phase-IV",
            INSI = "ARBA",
            LAT = 49.816667,
            ELEV = 73,
            TAV = calc_TAV(.),
            AMP = calc_AMP(.))

# STATION2
read_station("station_2") %>% 
  write_wth("STATION2.WTH", 
            location = "AgMIP Phase-IV",
            INSI = "ARBA",
            LAT = 48.328333,
            ELEV = 87,
            TAV = calc_TAV(.),
            AMP = calc_AMP(.))

# STATION3
read_station("station_3") %>% 
  write_wth("STATION3.WTH", 
            location = "AgMIP Phase-IV",
            INSI = "ARBA",
            LAT = 47.275,
            ELEV = 206,
            TAV = calc_TAV(.),
            AMP = calc_AMP(.))

# STATION4
read_station("station_4") %>% 
  write_wth("STATION4.WTH", 
            location = "AgMIP Phase-IV",
            INSI = "ARBA",
            LAT = 49.148611,
            ELEV = 153,
            TAV = calc_TAV(.),
            AMP = calc_AMP(.))

# STATION5
read_station("station_5") %>% 
  write_wth("STATION5.WTH", 
            location = "AgMIP Phase-IV",
            INSI = "ARBA",
            LAT = 47.904167,
            ELEV = 129,
            TAV = calc_TAV(.),
            AMP = calc_AMP(.))

# STATION6
read_station("station_6") %>% 
  write_wth("STATION6.WTH", 
            location = "AgMIP Phase-IV",
            INSI = "ARBA",
            LAT = 47.884722,
            ELEV = 93,
            TAV = calc_TAV(.),
            AMP = calc_AMP(.))

# STATION7
read_station("station_7") %>% 
  write_wth("STATION7.WTH", 
            location = "AgMIP Phase-IV",
            INSI = "ARBA",
            LAT = 48.325278,
            ELEV = 109,
            TAV = calc_TAV(.),
            AMP = calc_AMP(.))

################################################################################

# File_sol

sol_data <- 
  input_data %>% 
  rename_with(\(.x) str_replace(.x, "layer_", "H")) %>% 
  select(TRNO, TNAME, matches("H[0-9]")) %>% 
  select(TRNO, TNAME, where(is.numeric)) %>% 
  pivot_longer(-c(TRNO, TNAME)) %>% 
  mutate(horizon = str_extract(name, "H[0-9]$"),
         vname = str_remove(name, "_H[0-9]$")) %>% 
  select(-name) %>% 
  pivot_wider(names_from = "vname") %>% 
  arrange(TRNO, TNAME, horizon) %>% 
  group_by(TRNO, TNAME) %>% 
  mutate(SLB = cumsum(Thickness)) %>% 
  ungroup() %>% 
  filter(!is.na(SLB)) %>% 
  mutate(PEDON = str_replace_all(TNAME, "Site", "FRST"),
         SLMH = NA_character_,
         SLLL = Wilting_point/100,
         SDUL = Field_capacity/100,
         SSAT = (1-Apparent_density/2.65)*0.95,
         SRGF = 1,
         SSKS = NA_real_,
         SBDM = Apparent_density,
         SLOC = Organic_matter,
         SLCL = Clay,
         SLSI = Fine_silt + Coarse_silt,
         SLCF = Stones,
         SLNI = NA_real_,
         SLHW = Water_pH,
         SLHB = NA_real_,
         SCEC = NA_real_,
         SADC = NA_real_) %>% 
  select(TRNO, TNAME, PEDON, horizon, SLB, SLLL, SDUL, SLCF, Available_water, everything()) %>% 
  group_by(PEDON) %>% 
  summarize(DEPTH = max(SLB),
            across(matches("^S[LDSRBCA]"),
                   list)) %>% 
  mutate(SOURCE = NA,
         TEXTURE = NA,
         DESCRIPTION = "AgMIP Phase-IV",
         SITE = NA,
         COUNTRY = "FRANCE",
         LAT = NA_real_,
         LONG = NA_real_,
         `SCS FAMILY` = NA,
         SCOM = NA,
         SALB = 0.14,
         SLU1 = 6.0,
         SLDR = 0.60,
         SLRO = 73.0,
         SLNF = 1.00,
         SLPF = 1.00,
         SMHB = "IB001",
         SMPX = "IB001",
         SMKE = "IB001")

sol_data %>%
  write_sol("FR.SOL")

################################################################################

# File_a
obs_data %>%
  mutate(Grain_Yield = Grain_Yield*10,
         SowingDate = as.POSIXct(SowingDate, tz = "UTC", format = "%d/%m/%Y"),
         Date_BBCH30 = as.POSIXct(Date_BBCH30, tz = "UTC", format = "%d/%m/%Y"),
         Date_BBCH55 = as.POSIXct(Date_BBCH55, tz = "UTC", format = "%d/%m/%Y"),
         Date_BBCH90 = as.POSIXct(Date_BBCH90, tz = "UTC", format = "%d/%m/%Y"),
         BBCH30 = as.numeric(Date_BBCH30 - SowingDate),
         BBCH55 = as.numeric(Date_BBCH55 - SowingDate),
         BBCH90 = as.numeric(Date_BBCH90 - SowingDate),
         HDAT = as.POSIXct(HarvestDate, tz = "UTC", format = "%d/%m/%Y")) %>%
  rename(`T#AM` = EarsPerSqM, 
         `H#AM` = Grain_Number,
         `HP%M`  = ProteinContentGrain, 
         `VN%M`  = N_in_biomassHarvest, 
         HWAM  = Grain_Yield) %>%
  select(TRNO, HDAT, `T#AM`, `H#AM`, `HP%M`, `VN%M`, HWAM, BBCH30, BBCH55, BBCH90) %>% 
  filter(!is.na(`T#AM`|`H#AM`|`HP%M`|`VN%M`|HWAM)) %>% 
## Create file_a for CERES
  as_DSSAT_tbl(v_fmt = c(TRNO = "%6.0f",
                         HDAT = "%6s",
                         `T#AM` = "%6.0f",
                         `H#AM` = "%6.0f",
                         `HP%M` = "%6.1f",
                         `VN%M` = "%6.2f",
                         HWAM = "%6.0f",
                         BBCH30 = "%6.0f",
                         BBCH55 = "%6.0f",
                         BBCH90 = "%6.0f")
               ) %>%
  select(TRNO, HDAT, `T#AM`, `H#AM`, `HP%M`, `VN%M`, HWAM, BBCH30, BBCH55, BBCH90) %>%
  `attr<-`("experiment", "AgMIP Phase-IV") %>% 
  write_filea("FRWHCER1.WHA")

## Create file_a for CROPSIM
file.copy("FRWHCER1.WHA", "FRWHCRP1.WHA", overwrite = TRUE)

# Create file_a for NWHEAT
file.copy("FRWHCER1.WHA", "FRWHAPS1.WHA", overwrite = TRUE)

################################################################################

# File_t
obs_data %>%
  mutate(CWAD = Biomass*10) %>% 
  select(TRNO, DATE, CWAD) %>% 
  drop_na() %>% 
## Create file_t for CERES
  as_DSSAT_tbl(v_fmt = c(TRNO = "%6i",
                         DATE = "%6s",
                         CWAD = "%6.0f")
               ) %>%
  select(TRNO, DATE, CWAD) %>%
  `attr<-`("experiment", "AgMIP Phase-IV") %>% 
  write_filet("FRWHCER1.WHT")

## Create file_t for CROPSIM
file.copy("FRWHCER1.WHT", "FRWHCRP1.WHT", overwrite = TRUE)


## Create file_t for NWHEAT
file.copy("FRWHCER1.WHT", "FRWHAPS1.WHT", overwrite = TRUE)

################################################################################

# File_x

file_x_data <- 
  input_data %>% 
  mutate(PDATE = as.POSIXct(SowingDate, tz = "UTC", format = "%d/%m/%Y"),
         WSTA = paste0("STATION", Station, ".WTH"),
         PPOP = Sowing_density) %>% 
  select(TRNO, TNAME, PDATE, WSTA, PPOP) %>% 
  arrange(TRNO) %>% 
  left_join(., obs_data %>% 
              mutate(HDATE = as.POSIXct(HarvestDate, tz = "UTC", format = "%d/%m/%Y")) %>% 
              select(TRNO, TNAME, HDATE) %>% 
              drop_na()) %>% 
  arrange(TRNO)

fdate <-
  input_data %>% 
  select(TRNO, TNAME, matches("fertil")) %>% 
  select(-matches("Amount|Product|Number")) %>% 
  pivot_longer(matches("Date_")) %>% 
  rename(FDATE = value) %>% 
  mutate(app_no = str_extract(name, "[0-9]+$")) %>% 
  select(-name)

famn <-
  input_data %>% 
  select(TRNO, TNAME, matches("fertil")) %>% 
  select(-matches("Date|Product|Number|Total")) %>% 
  pivot_longer(matches("Amount_")) %>% 
  rename(FAMN = value) %>% 
  mutate(app_no = str_extract(name, "[0-9]+$")) %>% 
  select(-name)

fsowing <- 
  input_data %>% 
  rename(FDATE = SowingDate) %>% 
  select(TRNO, TNAME, FDATE) %>% 
  mutate(app_no = "0",
         FAMN = 35)

fertilizer <-
  fdate %>% 
  full_join(famn) %>%
  full_join(fsowing) %>%
  arrange(TRNO, app_no) %>% 
  drop_na() %>%
  mutate(FACD = if_else(app_no == 0, 
                        "AP002",
                        "AP001"),
         FDEP = if_else(app_no == 0, 
                        15,
                        1),
         FAMN = FAMN/0.46,
         FDATE = as.POSIXct(FDATE, tz = "UTC", format = "%d/%m/%Y")) %>%
  select(TRNO, TNAME, FDATE, FACD, FDEP, FAMN) %>%
  group_by(TRNO, TNAME) %>%
  summarise(FDATE = list(FDATE),
            FACD = list(FACD),
            FDEP = list(FDEP),
            FAMN = list(FAMN)) %>% 
  ungroup()

file_x_data <-
  file_x_data %>%
  full_join(fertilizer)

## File_x GENERAL
file_x <-
  DSSAT:::filex_template(PEOPLE = "Muhammad Zeeshan Mehmood and Phillip D. Alderman",
                         ADDRESS = "Department of Plant and Soil Sciences, Oklahoma State University, Stillwater, OK, 74078, USA",
                         SITE = "French Wheat AgMIP Phase - IV")

## File_x TREATMENTS
file_x$TREATMENTS <-
  file_x_data %>%
  mutate(N = TRNO, 
         R = 1,
         O = 1,
         C = 0,
         CU = 1,
         FL = 1:n(),
         SA = 0,
         IC = 1:n(),
         MP = 1:n(),
         MI = 0,
         MF = 1:n(),
         MR = 0,
         MC = 0,
         MT = 0,
         ME = 0,
         MH = 0,
         SM = 1:n()) %>%
  select(N, R, O, C, TNAME, CU, FL, SA, IC, MP, MI, MF, MR, MC, MT, ME, MH, SM)

## File_x CULTIVARS
file_x$CULTIVARS <- 
  file_x$CULTIVARS %>%
  mutate(C = 1,
         CR = "WH",
         INGENO = "FR0001",
         CNAME = "Apache")

## File_x FIELDS
file_x$FIELDS <-
  file_x_data %>%
  mutate(L = 1:n(),
         ID_FIELD = str_replace_all(TNAME, "Site", "ST"),
         WSTA = WSTA,
         FLSA = NA,
         FLOB = NA,
         FLDT = NA,
         FLDD = NA,
         FLDS = NA,
         FLST = NA,
         SLTX = NA,
         SLDP = NA,
         ID_SOIL = str_replace_all(TNAME, "Site", "FRST"),
         FLNAME = TNAME,
         XCRD = NA,
         YCRD = NA,
         ELEV = NA,
         AREA = NA,
         SLEN = NA,
         FLWR = NA,
         SLAS = NA,
         FLHST = NA,
         FHDUR = NA) %>%
  select(L, ID_FIELD, WSTA, FLSA, FLOB, FLDT, FLDD, FLDS, FLST, SLTX, SLDP, ID_SOIL, FLNAME, 
         XCRD, YCRD, ELEV, AREA, SLEN, FLWR, SLAS, FLHST, FHDUR) %>%
  {attr(., "tier_info") <- list(c("L", "ID_FIELD", "WSTA", "FLSA", "FLOB", "FLDT", "FLDD", "FLDS", "FLST", "SLTX", "SLDP", "ID_SOIL", "FLNAME"),
                                c("L", "XCRD", "YCRD", "ELEV", "AREA", "SLEN", "FLWR", "SLAS", "FLHST", "FHDUR"))
  .}

## File_x PLANTING DETAILS
file_x$`PLANTING DETAILS` <-
  file_x_data %>%
  mutate(P = TRNO,
         EDATE = NA,
         PPOE = NA,
         PLME = "S",
         PLDS = "R",
         PLRS = 17,
         PLRD = NA,
         PLDP = 3,
         PLWT = NA,
         PAGE = NA,
         PENV = NA,
         PLPH = NA,
         SPRL = NA,
         PLNAME = TNAME) %>%
  select(P,PDATE,EDATE,PPOP,PPOE,PLME,PLDS,PLRS,PLRD,PLDP,PLWT,PAGE,PENV,PLPH,SPRL,PLNAME)

## File_x SIMULATION CONTROLS
file_x$`SIMULATION CONTROLS` <-
  file_x$`SIMULATION CONTROLS` %>%
  mutate(START = "S",
         SMODEL = "CSCER",
         WATER = "Y",
         NITRO = "Y",
         PHOTO = "C",
         MESOL = 3,
         FERTI = "R",
         HARVS = "M",
         GROUT = "Y",
         WAOUT = "Y") %>% 
  select(-SNAME, -SDATE, -N) %>% 
  cross_join(file_x$`PLANTING DETAILS` %>% select(P, PDATE, PLNAME)) %>% 
  rename(N = P,
         SDATE = PDATE,
         SNAME = PLNAME) %>% 
  mutate(SDATE = as.POSIXct(paste0(year(SDATE), "-08-01"), tz = "UTC"))

## File_x FERTILIZERS
file_x$FERTILIZERS <-
  file_x_data %>%
  unnest(c(FDATE, FACD, FDEP, FAMN)) %>% 
  mutate(F = TRNO,
         FMCD = "FE005",
         FAMP = NA_real_,
         FAMK = NA_real_,
         FAMC = NA_real_,
         FAMO =NA_real_,
         FOCD = NA_real_,
         FERNAME = TNAME) %>% 
  select(F, FDATE, FMCD, FACD, FDEP, FAMN, FAMP, FAMK, FAMC, FAMO, FOCD, FERNAME)

## File_x INITIAL CONDITIONS
file_x$`INITIAL CONDITIONS` <-
  file_x$FIELDS %>% 
  full_join(sol_data, by = c(ID_SOIL = "PEDON")) %>% 
  select(L, SLB, SLLL) %>% 
  unnest(c(SLB, SLLL)) %>% 
  rename(C = L, ICBL = SLB, SH2O = SLLL) %>% 
  group_by(C) %>% 
  mutate(SH2O = ifelse(ICBL == min(ICBL), 0, SH2O)) %>% 
  summarize(ICBL = list(ICBL),
            SNO3 = list(rep(NA_real_, length(SH2O))),
            SNH4 = list(rep(NA_real_, length(SH2O))),
            SH2O = list(SH2O)) %>% 
  ungroup() %>%
  mutate(PCR = "WH",
         ICRT = NA,
         ICND = NA,
         ICRN = NA,
         ICRE = NA,
         ICWD = NA,
         ICRES = NA,
         ICREN = NA,
         ICREP = NA,
         ICRIP = NA,
         ICRID = NA) %>%
  full_join(file_x$TREATMENTS, by = c("C" = "N")) %>% 
  full_join(file_x$`PLANTING DETAILS`, by = c("C" = "P")) %>% 
  mutate(ICDAT = as.POSIXct(paste0(year(PDATE), "-08-01"), tz = "UTC")) %>% 
  rename(ICNAME = TNAME) %>% 
  select(C, PCR, ICDAT, ICRT, ICND, ICRN, ICRE, ICWD, ICRES, ICREN, ICREP, ICRIP, ICRID, ICNAME, ICBL, SH2O, SNH4, SNO3) %>%
  {attr(., "tier_info") <- list(c("C","PCR","ICDAT","ICRT","ICND","ICRN","ICRE","ICWD","ICRES","ICREN","ICREP","ICRIP","ICRID","ICNAME"),
                                c("C","ICBL","SH20","SNH4","SNO3"))
  .}

file_x <- file_x[c("GENERAL", "TREATMENTS", "CULTIVARS", "FIELDS", "INITIAL CONDITIONS",
                   "PLANTING DETAILS", "FERTILIZERS", "SIMULATION CONTROLS")]

## Create file_x for CERES
write_filex(file_x, "FRWHCER1.WHX")

## Create file_x for CROPSIM
readLines("FRWHCER1.WHX") %>% 
  str_replace_all(c("CSCER" = "CSCRP")) %>% 
  write("FRWHCRP1.WHX")

## Create file_x for NWHEAT
readLines("FRWHCER1.WHX") %>% 
  str_replace_all(c("CSCER" = "WHAPS")) %>% 
  write("FRWHAPS1.WHX")

################################################################################

# Cultivar file (.CUL)
options(DSSAT.CSM = "/carl-data/shared/carl/DSSAT48/dscsm048")

## CERES
cul_orig <- options()$DSSAT.CSM %>%
            dirname() %>%
            str_c("/Genotype/WHCER048.CUL") %>%
            DSSAT::read_cul()

cul_orig %>%
  filter(`VAR#` == "IB1015") %>%
  mutate(`VAR#` = "FR0001",
         `VAR-NAME` = "Apache",
         `ECO#` = "FRWH01",
         `EXP#` = ".") %>%
  DSSAT::write_cul(file.path(run_dir, "WHCER048.CUL"))

## CROPSIM
cul_orig <- options()$DSSAT.CSM %>%
            dirname() %>%
            str_c("/Genotype/WHCRP048.CUL") %>%
            DSSAT::read_cul()

cul_orig %>%
  filter(`VAR#` == "IB1015") %>%
  mutate(`VAR#` = "FR0001",
         `VAR-NAME` = "Apache",
         `ECO#` = "FRWH01",
         `EXP#` = ".") %>%
  DSSAT::write_cul(file.path(run_dir, "WHCRP048.CUL"))

## NWHEAT
cul_orig <- options()$DSSAT.CSM %>%
            dirname() %>%
            str_c("/Genotype/WHAPS048.CUL") %>%
            DSSAT::read_cul()

cul_orig %>%
  filter(`VAR#` == "IB1015") %>%
  mutate(`VAR#` = "FR0001",
         `VRNAME` = "Apache",
         `ECO#` = "FRWH01",
         `EXPNO` = ".") %>%
  DSSAT::write_cul(file.path(run_dir, "WHAPS048.CUL"))

################################################################################

# Ecotype file (.ECO)
options(DSSAT.CSM = "/carl-data/shared/carl/DSSAT48/dscsm048")

## CERES
eco_orig <- options()$DSSAT.CSM %>%
            dirname() %>%
            str_c("/Genotype/WHCER048.ECO") %>%
            DSSAT::read_eco()

eco_orig %>%
  filter(`ECO#` == "UKWH01") %>%
  mutate(`ECO#` = "FRWH01") %>%
  DSSAT::write_eco(file.path(run_dir, "WHCER048.ECO"))

## CROPSIM
eco_orig <- options()$DSSAT.CSM %>%
            dirname() %>%
            str_c("/Genotype/WHCRP048.ECO") %>%
            DSSAT::read_eco()

eco_orig %>%
  filter(`ECO#` == "UK0001") %>%
  mutate(`ECO#` = "FRWH01",
         ECONAME = "FRWHEAT") %>%
  DSSAT::write_eco(file.path(run_dir, "WHCRP048.ECO"))

## NWHEAT
eco_orig <- options()$DSSAT.CSM %>%
            dirname() %>%
            str_c("/Genotype/WHAPS048.ECO") %>%
            DSSAT::read_eco()

eco_orig %>%
  filter(`ECO#` == "DFAULT") %>%
  mutate(`ECO#` = "FRWH01",
         ECONAME = "FRWHEAT") %>%
  DSSAT::write_eco(file.path(run_dir, "WHAPS048.ECO"))

################################################################################

# Run DSSAT to test model files

# Set up working directory
user <- Sys.info()["user"]
root_dir <- paste0("/carl-data/shared/users/", user, "/AgMIP-Calibration-Phase-IV")
run_dir <- paste0(root_dir, "/DSSAT48/Wheat")
setwd(run_dir)

################################################################################

# Run these lines in terminal if SCP DSSAT48 from CARL
#scp -r /carl-data/shared/carl/DSSAT48 /carl-data/shared/users/mmehmoo/AgMIP-Calibration-Phase-IV
#cd DSSAT48
#sed -i 's/\/carl\//\/users\/mmehmoo\/AgMIP-Calibration-Phase-IV\//g' DSSATPRO.L48
#sed -i 's/\/users\/mmehmoo\/AgMIP-Calibration-Phase-IV\/DSSAT48/\/carl\/mmehmoo_DSSAT48/p' DSSATPRO.L48

################################################################################

for(file in c("FR.SOL")){
  file.copy(file.path(root_dir, "DSSAT-files", file), file.path(root_dir, "DSSAT48/Soil", file), overwrite = TRUE)
}

for(file in c("STATION1.WTH", "STATION2.WTH", "STATION3.WTH", "STATION4.WTH", "STATION5.WTH", "STATION6.WTH", "STATION7.WTH")){
  file.copy(file.path(root_dir, "DSSAT-files", file), file.path(root_dir, "DSSAT48/Weather", file), overwrite = TRUE)
}

for(file in c("WHCER048.CUL", "WHCER048.ECO", "WHCRP048.CUL", "WHCRP048.ECO", "WHAPS048.CUL", "WHAPS048.ECO")){
  file.copy(file.path(root_dir, "DSSAT-files", file), file.path(root_dir, "DSSAT48/Genotype", file), overwrite = TRUE)
}

for(file in c("FRWHCER1.WHA", "FRWHCER1.WHT", "FRWHCER1.WHX",
              "FRWHCRP1.WHA", "FRWHCRP1.WHT", "FRWHCRP1.WHX",
              "FRWHAPS1.WHA", "FRWHAPS1.WHT", "FRWHAPS1.WHX")){
  file.copy(file.path(root_dir, "DSSAT-files", file), file.path(root_dir, "DSSAT48/Wheat", file), overwrite = TRUE)
}

################################################################################

options(DSSAT.CSM = "/carl-data/shared/users/mmehmoo/AgMIP-Calibration-Phase-IV/DSSAT48/dscsm048_annex6")

write_dssbatch("FRWHCER1.WHX", trtno = 1:22)
run_dssat()

write_dssbatch("FRWHCRP1.WHX", trtno = 1:22)
run_dssat()

write_dssbatch("FRWHAPS1.WHX", trtno = 1:22)
run_dssat()
