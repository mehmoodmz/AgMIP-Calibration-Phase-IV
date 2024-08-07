################################################################################
#
# Main script to apply AgMIP Calibration phase IV protocol using CroptimizR and 
# CroPlotR R packages in the context of the AgMIP Phase IV exercise.
#
# This script has been specifically implemented for the AgMIP Phase IV exercise
# conducted in early 2024.
#
# Full integration of the corresponding protocol into the CroptimizR package will 
# be carried out in future versions of the package.
# 
# If you use this script (and more generally the packages CroptimizR and CroPlotR),
# please cite the references given by executing the following lines:
#  citation("CroptimizR")
#  citation("CroPlotR")
#
# Please refer to the "Initialization step" section in the following lines of 
# this script to adapt it to your own case.
#
# Author: Samuel Buis
################################################################################

# DEBUG mode (set to TRUE to test the protocol with limited number of situations, repetitions and evaluations, 
#                    FALSE to run phaseIV exercise)
debug <- FALSE

# Checkpoint-restart mode (set to TRUE will save temporary results in Rdata files 
# so that in case of crash the script can be re-run from the last successful step,
# see Details in https://github.com/sbuis/AgMIP-Calibration-Phase-IV description)
# However, this may result in large files (up to several hundreds MBytes) being stored.
checkpoint_restart  <- FALSE
if (debug) {
  checkpoint_restart <- TRUE
}			

# Define the test case ("French" or "Australian") and variety
test_case <- "French"
variety <- "A"  # "A" or "B"
# test_case <- "Australian"
# variety <- "Janz"

################################################################################

# Model code
#model_name <- "CERES"
model_name <- "CROPSIM"
#model_name <- "NWHEAT"

# Read environment variables
user <- Sys.info()["user"]
node_name <- Sys.info()["nodename"]

# Set up root_dir and dssat_path
if(node_name == "hpc-cloud-carl2"){
  root_dir <- file.path("/carl-data/shared/users", user, "AgMIP-Calibration-Phase-IV")
  DSSAT_path <- file.path("/carl-data/shared/users", user, "AgMIP-Calibration-Phase-IV/DSSAT48")
  
}else{
  root_dir <- file.path("/home", user, "AgMIP-Calibration-Phase-IV")
  DSSAT_path <- file.path(Sys.getenv("TDIR"), "DSSAT48")
}

# Set up out directory
if(node_name == "hpc-cloud-carl2"){
  dir.create(file.path(root_dir, "results"), recursive = TRUE)
  dir.create(file.path(root_dir, "results", test_case), recursive = TRUE)
  dir.create(file.path(root_dir, "results", test_case, variety), recursive = TRUE)
  dir.create(file.path(root_dir, "results", test_case, variety, model_name), recursive = TRUE)
}else{
  dir.create(file.path("/scratch", user, "results"), recursive = TRUE)
  dir.create(file.path("/scratch", user, "results", test_case), recursive = TRUE)
  dir.create(file.path("/scratch", user, "results", test_case, variety), recursive = TRUE)
  dir.create(file.path("/scratch", user, "results", test_case, variety, model_name), recursive = TRUE)
}


# Set-up output results folder (OPTIONAL, set to results/test_case/variety by default)
# the following lines will set it to project_path/results/test_case/variety
# can be changed if needed
out_dir <-
  if(node_name == "hpc-cloud-carl2"){
    file.path(root_dir, "results", test_case, variety, model_name)
  }else{
    file.path("/scratch", user, "results", test_case, variety, model_name)
  }

################################################################################

# Install and load the needed libraries
# if(!require("here")){
#   install.packages("here")
#   library("here")
# }
source(file.path(root_dir,"R/install_load.r"))
install_load()

options(warn=1)

################################################################################
###### Initialization step => to be adapted to your case #######################
###### please read the comments and give the required information
################################################################################

# Set the path to the protocol description file to use (EXCEL file that describe the tables)
## replace path_to_protocol_description_file in the following line by the actual path to 
## the protocol description file (including its name).  
xls_path <-
  if (model_name == "CERES") {
    file.path(root_dir, "data/CERES_Mehmood_v6.xlsx")
  } else if (model_name == "CROPSIM") {
    file.path(root_dir, "data/CROPSIM_Mehmood_v6.xlsx")
  } else {
    file.path(root_dir, "data/NWHEAT_Mehmood_v6.xlsx")
  }
  
# Set-up your model wrapper
## Source the file including your model wrapper: 
## replace path_to_my_model_wrapper in the following line by the actual path to 
## the file including the code of your model wrapper  
## Or load the package including your model wrapper
source(file.path(root_dir, "DSSAT-wrapper/R/DSSAT_wrapper.R"))

## Give here the link to your model wrapper function: replace my_model_wrapper_function
## by the actual name of the function defining your model wrapper.
model_wrapper <- DSSAT_wrapper

# Define model_options depending on your model wrapper
model_options <- vector("list")
model_options$DSSAT_path <- DSSAT_path
model_options$DSSAT_exe <-  "dscsm048_annex6"
model_options$Crop <- "Wheat"
model_options$cultivar_filename <- 
  if (model_name == "CERES") {
    "WHCER048.CUL"
  } else if (model_name == "CROPSIM") {
    "WHCRP048.CUL"
  } else {
    "WHAPS048.CUL"
  }
  
model_options$ecotype_filename <- 
  if (model_name == "CERES") {
    "WHCER048.ECO"
  } else if (model_name == "CROPSIM") {
    "WHCRP048.ECO"
  } else {
    "WHAPS048.ECO"
  }
  
model_options$ecotype <-  "FRWH01"
model_options$cultivar <- "FR0001"

# Give here the type of reference date used for computing julian days for phenological stages
# should be equal to "SowingYear" if julian days are computed from the beginning of the sowing year
# or "SowingDate" if julian days are computed from the sowing date.
descr_ref_date <- "SowingYear"

# Define model output transformation(s) if necessary (OPTIONAL)
# Useful if one (or several) observed variable is not directly comparable to a 
# simulated one but if one can compute an equivalent from the simulated variables.
# In the following example, N_in_biomassHarvest (observed in French dataset) is not 
# directly computed by the model.
# The model simulates a variable, called QNplante, which is the N in biomass in kg ha-1, 
# and the biomass, called masec_n, in t ha-1.
# N_in_biomassHarvest (in %) can be computed from these simulated variables,
# by dividing QNplante by (masec_n*10).
# To do that, we created a transform_sim function that will be automatically run after each 
# model simulations within the parameter estimation process.
# If you define a transform_sim function, list in transform_outputs the variables 
# generated by transform_sim, and in transform_inputs those required in input of 
# transform_sim to compute them.
# In case you use transform_sim, add the name(s) of the computed variables (listed in transform_outputs)
# to the protocol description EXCEL file (tab Variables).
# If no transformation required, let transform_sim, transform_outputs and transform_inputs 
# to NULL
transform_sim <- NULL
transform_outputs <- NULL
transform_inputs <- NULL

# if (test_case=="French") {
#   transform_sim <- function(model_results, ...) {
#     
#     # Create the new variable for each situation included in model_results$sim_list
#     for (sit in names(model_results$sim_list)) {
#       model_results$sim_list[[sit]]$N_in_biomassHarvest <- 
#         model_results$sim_list[[sit]]$QNplante / (10 * model_results$sim_list[[sit]]$masec_n)
#     }
#     return(model_results)  
#   }
#   
#   transform_outputs <- c("N_in_biomassHarvest")
#   transform_inputs <- c("QNplante")
#   
# }

if (test_case=="French") {
   transform_sim <- function(model_results, ...) {
     
     # Create the new variable for each situation included in model_results$sim_list
     for (sit in names(model_results$sim_list)) {
       model_results$sim_list[[sit]]$`HP%M1` <- 
         model_results$sim_list[[sit]]$`HN%M` * 6.25 
     }
     return(model_results)  
   }
   
   transform_outputs <- c("HP%M1")
   transform_inputs <- c("HN%M")
   
 }

# Synthetic observation mode (set to TRUE to test the protocol using synthetic observations 
#                                    FALSE to apply phaseIV protocol to real data)
# The following lines should not be changed if you want to run phaseIV exercise.
use_obs_synth <- FALSE
beta <- 0.3 # level of perturbation of parameters values : 0.3 or 0.6
noise_sd <- 0 # sd of gaussian noise added to synthetic observations (in percentage, set 0 or 0.1)
seed <- 1234 # seed for random number generation. Set it to a constant value for an exact replicate of the experiment. Change its value to change random number generation and thus synthetic observations and default parameters values.

flag_eos <- TRUE # TRUE to compare simulated and observed final values of biomass and Yield on 31/12/harvestYear, 
# FALSE to compare them at observed harvest date
# Set TRUE to remove Minnipa observations from Australian dataset, FALSE otherwise
data_without_Minnipa <- TRUE


################################################################################
###### end of initialization step => the following should not be changed #######
################################################################################



set.seed(seed)

# check descr_ref_date
if (!(descr_ref_date %in% c("SowingYear","SowingDate"))) {
  stop("descr_ref_date must be equal to \"SowingYear\" or to \"SowingDate\", please correct its definition.")
}

# check path to the protocol description file
if (!file.exists(xls_path)) {
  stop("The path given for the protocol description file does not correspond to an existing file. Please check it.")
}

# Load the protocol description file (xls file) 
protocol_descr <- load_protocol(xls_path, transform_outputs, use_obs_synth, beta)
sitNames_corresp <- protocol_descr$sitNames_corresp 
varNames_corresp <- protocol_descr$varNames_corresp 
simVar_units <- protocol_descr$simVar_units 
param_info <- protocol_descr$param_info
forced_param_values <- protocol_descr$default_param_values 
param_group <- protocol_descr$param_group # list of params to estimate per group
obsVar_group <- protocol_descr$obsVar_group # groups of observed variables used in the calibration
converted_obsVar_group <- setNames(obsVar_group,nm=varNames_corresp[names(obsVar_group)])													  
true_param_values <- protocol_descr$true_param_values 		   

# Load the observations
suffix <- NULL
if (test_case=="French") suffix <- paste0("_",variety)
if (test_case=="Australian" & data_without_Minnipa) {
  obs_data_folder <- "data_without_Minnipa"
} else {
  obs_data_folder <- "data"
}
obs_data_path <- file.path(root_dir,obs_data_folder,paste0("cal_4_obs_",test_case,suffix,".txt"))
obs_unit_path <- file.path(root_dir,obs_data_folder,paste0("cal_4_obs_",test_case,"_units.csv"))
## Get the reference date for each situation
template_path <- file.path(root_dir, "data",paste0("simulations_template.txt"))
ref_date <- get_reference_date(descr_ref_date, template_path)

obs <- load_obs(obs_data_path, obs_unit_path, varNames_corresp,
                sitNames_corresp, simVar_units, obsVar_group, flag_eos, ref_date)

obs_list <- obs$obs_list  # list of observation as defined in the observation file
obsVar_names <- obs$obsVar_names # Names of the observed variables as defined in the observation file
obsVar_units <- obs$obsVar_units # Units of the observed variables as defined in the observation file
obsVar_used <- obs$obsVar_used # NAmes of the observed variables used in the current protocol application
converted_obs_list <- obs$converted_obs_list # list of observation in th emodel space (i.e. with name of situation and variables as in the model_wrapper, and units as defined in model outputs)
harvest_year <- obs$harvest_year

# Get the list of variables for which the user must provide 
# results in the cal_4_results_***.txt file)
template_df <- read.table(template_path,
                          header = TRUE, stringsAsFactors = FALSE)
resVar_names <- setdiff(names(template_df),c("Number","Site","HarvestYear",
                                             "Date_sowing", "SowingDate",
                                             "Variety","Date"))

## Check that the list of "observed and required variables" as defined in the protocol 
## description xls file is included in the union of observed and required variables
## as they are defined in the cal_4_obs and cal_4_reslts files.
if (!all(names(varNames_corresp) %in% unique(c(obsVar_names, resVar_names)))) 
  stop(paste0("Unknown variable(s) ",
              paste(setdiff(names(varNames_corresp), unique(c(obsVar_names, resVar_names))), collapse = ","), 
              "\nPlease modify sheet \"variables\", column \"Name of the observed or required variable\" of the file:\n",
              xls_path,
              "\nThe variables included in this column must be included in the list of variables defined in files:\n",
              template_path, "\nand\n",obs_data_path))

# Compute the list of required variables in output of the wrapper
# (list of variables for which there is a correspondance with observed and results variables + 
#  transform_inputs - transform_outputs)
reqVar_Wrapper <- setdiff(c(varNames_corresp,transform_inputs),transform_outputs)

# Generate synthetic observations if required
sim_true <- NULL				
if (use_obs_synth) {
  
  obs_synth <- generate_obs_synth(true_param_values=c(true_param_values), 
                                  model_wrapper, 
                                  model_options, sitNames_corresp, 
                                  reqVar_Wrapper, converted_obs_list, transform_sim,
                                  simVar_units, varNames_corresp, obsVar_units,  
                                  obs_list, obsVar_used, noise_sd, descr_ref_date,
                                  flag_eos)									
  obs_list <- obs_synth$obs_list
  converted_obs_list <- obs_synth$converted_obs_list
  sim_true <- obs_synth$sim_true								
  
}


# In debug mode reduce number of evaluations, situations, repetitions, candidate parameters, ...
if (debug) {
  cat("\nDebug mode ...\n")
  sit_list <- names(converted_obs_list)[1:6]
  converted_obs_list <- filter_obs(converted_obs_list,
                                       situation = sit_list,
                                       include=TRUE)
  obs_list <- filter_obs(obs_list,
                         situation = names(obs_list)[1:6],
                         include=TRUE)  
  for (gr in names(param_group)) { # only keep the 1st candidate
    if (!is.null(param_group[[gr]]$candidates)) 
      param_group[[gr]]$candidates <- param_group[[gr]]$candidates[1]
  }

}

# Initialize some local variables 
flag_checkpoint <- FALSE
igr <- 0
res_it1 <- list(); res_it1_tmp <- NULL; res_it2 <- NULL; 
weight_it2 <- NULL; 
complem_info <- list(it1=list(), it2=list())

if (file.exists(file.path(out_dir,"checkpoint.Rdata"))) {
											 
						 
  if (file.exists(file.path(out_dir,"config.Rdata"))) {
    load(file.path(out_dir,"config.Rdata"))
  }
  load(file.path(out_dir,"checkpoint.Rdata"))
  flag_checkpoint <- TRUE
  if (file.exists(file.path(out_dir,"complementary_info.Rdata"))) {
    load(file.path(out_dir,paste0("complementary_info.Rdata")))
  }
} else {
  # Save configuration
  if (checkpoint_restart) {
    save.image(file=file.path(out_dir,"config.Rdata"))
  }								  
  if (dir.exists(out_dir)) {
    unlink(out_dir, recursive = TRUE)
  } 
  dir.create(out_dir, recursive = TRUE)
}  

# Initialize transformation function
transform_var <- NULL
transform_var_converted <- NULL
if ("Biomass" %in% names(varNames_corresp)) {
  transform_var <- eval(parse(text=paste0("c(",varNames_corresp[["Biomass"]],
                                          "=function(x) log(x+.Machine$double.xmin))")))
  transform_var_converted <- transform_var
  names(transform_var_converted) <- 
    names(varNames_corresp)[match(names(transform_var),varNames_corresp)]
}
# Evaluate performances using default values of the parameters

sim_default <- run_wrapper(model_wrapper=model_wrapper,
                           model_options=model_options,
                           param_values=c(forced_param_values),
                           situation=sitNames_corresp, var=reqVar_Wrapper, 
                           obs_list=converted_obs_list,
                           transform_sim=transform_sim, transform_var=NULL)
check_run_wrapper(sim=sim_default, obs_list=converted_obs_list, protocol_path=xls_path,
                  out_dir)						  
sim_list_default_converted <- convert_and_rename(sim_default$sim_list, sitNames_corresp, simVar_units, 
                                           varNames_corresp, obsVar_units)
sim_default$sim_list_converted <- sim_list_default_converted
p <- plot(sim_list_default_converted, obs=obs_list, type="scatter")
CroPlotR::save_plot_pdf(p, out_dir, file_name = "scatterplots_default")


# Parameter Estimation, Step 6
cat("\n----- Parameter estimation Step 6\n")
cat("--------------------------------------\n")

crt_forced_param_values <- forced_param_values
while (igr < length(param_group)) {
  
  igr <- igr+1
  gr <- names(param_group)[igr]
  cat(paste("\n---------------- Group",gr,"\n"))
  
  ## Filter observations to use for the current group
  crt_var_list <- varNames_corresp[intersect(obsVar_used,
                                             names(obsVar_group)[grep(gr, obsVar_group)])]
  crt_obs_list <- filter_obs(obs_list=converted_obs_list, var=crt_var_list, include=TRUE)
  
  ## Filter information on the parameters to estimate for the current group
  crt_params <- c(param_group[[gr]]$obligatory, param_group[[gr]]$candidates)
  crt_param_info <- lapply(param_info,function(x) x[crt_params])

  ## Define parameters to force (estimated values for the parameters previously selected,
  ##                            default values for the others, exclude the current candidate 
  ##                            parameters)
  if (!is.null(res_it1_tmp)) {
    param_to_add <- setdiff(names(forced_param_values), names(crt_forced_param_values))
    crt_forced_param_values <- c(forced_param_values[param_to_add], crt_forced_param_values)
    crt_forced_param_values[names(res_it1_tmp$final_values)] <- res_it1_tmp$final_values
  }
  crt_forced_param_values[param_group[[gr]]$obligatory] <- NULL
  
  # Optimization options (depends on the number of obligatory parameters)
  # Define number of repetitions and evaluations
  nb_rep_it1 <- c(10,5)
  if (length(param_group[[gr]]$obligatory)>1) nb_rep_it1 <- c(20,5)
  maxeval <- 50000
  if (debug) {
    nb_rep_it1 <- c(1,1)
    maxeval=10
  }
  optim_options=list(nb_rep=nb_rep_it1, maxeval=maxeval, ranseed=seed, xtol_rel=1e-4, 
                     ftol_rel=1e-4, 
                     out_dir=file.path(out_dir,"step6",paste0("group_",gr)))
  
  crit_function <- function(sim_list, obs_list) {
    crit <- crit_ols(sim_list, obs_list)
    if ( !(crt_var_list[1] %in% names(transform_var)) ) {
      units(crit) <- paste(simVar_units[[crt_var_list[1]]],
                           simVar_units[[crt_var_list[1]]])
      units(crit) <- paste(obsVar_units[[convert_name(crt_var_list[1],varNames_corresp)]],
                           obsVar_units[[convert_name(crt_var_list[1],varNames_corresp)]])
      crit <- drop_units(crit)
    }
    return(crit)
  }
  res_it1_tmp <- estim_param(obs_list=crt_obs_list, 
                     crit_function = crit_function,
                     model_function=model_wrapper,
                     model_options=model_options,
                     optim_options=optim_options,
                     param_info=crt_param_info, candidate_param=param_group[[gr]]$candidates,
                     forced_param_values=crt_forced_param_values, 
                     transform_var=transform_var,
                     transform_sim=transform_sim, var=reqVar_Wrapper,
                     info_crit_func = list(CroptimizR::AICc, CroptimizR::BIC))
  
  # Run model wrapper using parameter values estimated at this step
  sim_it1_tmp <- run_wrapper(model_wrapper = model_wrapper,
                             model_options=model_options,
                             param_values=c(res_it1_tmp$final_values, 
                                            res_it1_tmp$forced_param_values),
                             situation=sitNames_corresp, var=reqVar_Wrapper, 
                             obs_list=crt_obs_list,
                             transform_sim=transform_sim, transform_var=NULL)
  generate_all_cal_results(sim_it1_tmp, obs_list, obsVar_units, obsVar_used, 
                           sitNames_corresp, template_path, out_dir, 
                           varNames_corresp, resVar_names, 
                           paste0("simulations_after","_",gr),
                           use_obs_synth=use_obs_synth, sim_true=sim_true, 
                           descr_ref_date=descr_ref_date, flag_obs_mat=TRUE, flag_eos=flag_eos)
						   
  # ScatterPlots simulations VS obs at this step
  sim_list_it1_tmp_converted <- convert_and_rename(sim_it1_tmp$sim_list, sitNames_corresp, simVar_units, 
                                                   varNames_corresp, obsVar_units)
  p <- plot(sim_list_it1_tmp_converted, obs=obs_list, type="scatter")
  CroPlotR::save_plot_pdf(p, out_dir, 
                          file_name = paste0("scatterplots_after_",gr))
  res_it1[[gr]] <- res_it1_tmp
  
  if (checkpoint_restart) {
    save(sim_default, res_it1_tmp, res_it1, igr, crt_forced_param_values, 
         transform_var,  
         file = file.path(out_dir,paste0("checkpoint_it1_gr",igr,".Rdata")))
  }
  
  complem_info$it1[[gr]] <- list(forced_param_values=unlist(crt_forced_param_values),
                                 obsVar_used=crt_var_list,
                                 crt_obs_list=crt_obs_list)

  if (checkpoint_restart) {
    save(complem_info, 
         file = file.path(out_dir,paste0("complementary_info.Rdata")))
  }
  
}

# List of parameters selected after step 6, estimated values and forced parameters
final_params <- unlist(lapply(names(param_group), function(x) names(res_it1[[x]]$final_values)))
res_it1$final_values <- setNames(object=unlist(lapply(names(param_group), 
                                                      function(x) res_it1[[x]]$final_values)),
                                 nm=final_params)
last_forced_param_values <- res_it1[[names(param_group)[length(param_group)]]]$forced_param_values
res_it1$forced_param_values <- last_forced_param_values[setdiff(names(last_forced_param_values),
                                                                final_params)]
# Run model wrapper using parameter values estimated in step 6
sim_it1 <- run_wrapper(model_wrapper = model_wrapper,
                       model_options=model_options,
                       param_values=c(res_it1$final_values, 
                                      res_it1$forced_param_values),
                       situation=sitNames_corresp, var=reqVar_Wrapper, 
                       obs_list=converted_obs_list,
                       transform_sim=transform_sim, transform_var=NULL)

# ScatterPlots simulations VS obs after it1
sim_list_it1_converted <- convert_and_rename(sim_it1$sim_list, sitNames_corresp, simVar_units, 
                                             varNames_corresp, obsVar_units)
sim_it1$sim_list_converted <- sim_list_it1_converted
p <- plot(sim_list_it1_converted, obs=obs_list, type="scatter")
CroPlotR::save_plot_pdf(p, out_dir, file_name = "scatterplots_after_step6")

if (checkpoint_restart) {
  save(sim_default, res_it1, sim_it1, igr, crt_forced_param_values, 
       file = file.path(out_dir,paste0("checkpoint_it1_final.Rdata")))
}


# Parameter Estimation, Step 7

cat("\n----- Parameter estimation step 7\n")
cat("---------------------------------\n")

nb_rep_it2 <- 20
maxeval <- 50000
if (debug) {
  nb_rep_it2 <- 1
  maxeval=10
}
optim_options=list(nb_rep=nb_rep_it2, maxeval=maxeval, ranseed=seed, xtol_rel=1e-4, ftol_rel=1e-4)

# List of parameters to estimate as selected in step 6
final_params <- names(res_it1$final_values)
final_param_info <- lapply(param_info,function(x) x[final_params])

# Use estimated values of step 6 as initial values for 1st repetition
final_param_info$init_values <- res_it1$final_values

## Define parameters to force (defaults values for all non-selected parameters)
final_forced_param_values <- forced_param_values[setdiff(names(forced_param_values),
                                                         final_params)]

if (is.null(res_it2)) {
 
  optim_options$out_dir <- file.path(out_dir,"step7")

  # Define the weight to use in the criterion to minimize
  ## apply transformation to sim_it1 first
  sim_it1_transformed <- apply_transform_var(sim_it1$sim_list, transform_var)				

  obs_converted_transformed <- apply_transform_var(converted_obs_list, transform_var)
  stats_tmp <- summary(sim_it1_transformed, obs=obs_converted_transformed, stats = c("n_obs","SS_res")) 
  groups<-converted_obsVar_group[stats_tmp$variable]
  stats_tmp <- mutate(stats_tmp, 
                  p = sapply(groups,function(x) length(res_it1[[x]]$final_values)))
  weight_it2 <- bind_rows(sqrt(stats_tmp$SS_res/(stats_tmp$n_obs-stats_tmp$p)))  # bind_rows transform the vector in tibble, useful for post_treat function
  weight <- function(obs, var) {
    return( weight_it2[[var]])
  }
  res_it2 <- estim_param(obs_list=converted_obs_list, 
                         crit_function = crit_wls,
                         model_function=model_wrapper,
                         model_options=model_options,
                         optim_options=optim_options,
                         param_info=final_param_info,
                         forced_param_values=final_forced_param_values,
                         transform_var=transform_var,
                         transform_sim=transform_sim, var=reqVar_Wrapper,
                         weight=weight)												 

  # Run model wrapper using parameter values estimated in step 7
  sim_it2 <- run_wrapper(model_wrapper = model_wrapper,
                         model_options=model_options,
                         param_values=c(res_it2$final_values, res_it2$forced_param_values),
                         situation=sitNames_corresp, var=reqVar_Wrapper, 
                         obs_list=converted_obs_list,
                         transform_sim=transform_sim, transform_var=NULL)
  
  # ScatterPlots simulations VS obs after it2
  sim_list_it2_converted <- convert_and_rename(sim_it2$sim_list, sitNames_corresp, simVar_units, 
                                               varNames_corresp, obsVar_units)
  sim_it2$sim_list_converted <- sim_list_it2_converted
  p <- plot(sim_list_it2_converted, obs=obs_list, type="scatter")
  CroPlotR::save_plot_pdf(p, out_dir, file_name = "scatterplots_after_step7")
  
  if (checkpoint_restart) {
    save(sim_default, res_it1, sim_it1, igr, res_it2, sim_it2, 
         file = file.path(out_dir,paste0("checkpoint_it2.Rdata")))
  }
  
  complem_info$it2 <- list(forced_param_values=unlist(final_forced_param_values),
                           obsVar_used=varNames_corresp[varNames_corresp %in% unlist(lapply(converted_obs_list,names))],
                           converted_obs_list=converted_obs_list,
                           weight=weight_it2, sim_it1=sim_it1)
  if (checkpoint_restart) {
    save(complem_info, 
       file = file.path(out_dir,paste0("complementary_info.Rdata")))
  }
  
}



# Generating diagnostics and results files using CroPlotR

generate_results_files(param_group, model_options,  
                       complem_info, res_it2, 
                       sitNames_corresp, 
                       sim_default, sim_it1, sim_it2, 
                       obs_list, converted_obs_list,
                       obsVar_units, obsVar_used, 
                       template_path, out_dir,
                       varNames_corresp, resVar_names, 
                       forced_param_values, use_obs_synth=use_obs_synth, 
                       sim_true=sim_true, 
                       descr_ref_date=descr_ref_date, flag_eos=flag_eos,
                       transform_var_converted)
# Copying script and protocol files in result folder
file.copy(from=xls_path, to=out_dir, overwrite = TRUE)
if(node_name == "hpc-cloud-carl2"){
  file.copy(from=file.path(root_dir, paste0("main_script.R")), to=out_dir, overwrite = TRUE)
}else{
  file.copy(from=file.path(root_dir, paste0("main_script", "_", model_name, ".R")), to=out_dir, overwrite = TRUE)
}

# Displaying Results
cat("\n----------------------\n")
cat("Final values of estimated parameters:\n")
print(res_it2$final_values)
cat("\nFixed values of the other parameters:\n")
print(res_it2$forced_param_values)

cat(paste("\nResults Tables and files required in Phase IV protocol as well as detailed additional results can be found in folder:",out_dir))

# Displaying Total time
cat("\nTotal time of parameter estimation process:\n")
cat(paste("    Step 6:", sum(sapply(names(param_group), function(gr) res_it1[[gr]]$total_time))/3600, "hours elapsed\n"))
cat(paste("    Step 7:", res_it2$total_time/3600, "hours elapsed\n"))
cat(paste("    Total:", 
          (sum(sapply(names(param_group), function(gr) res_it1[[gr]]$total_time)) + 
             res_it2$total_time)/3600, 
          "hours elapsed\n"))
cat("----------------------\n")


if (debug) {
  cat("\n----------------------\n")
  cat("WARNING: the protocol has been applied in DEBUG mode on a sublist of situations and with limited number of repetitions and evaluations.")
  cat("Set debug to FALSE in the main script to disable DEBUG mode.")
  cat("\n----------------------\n")
}

if (flag_checkpoint) {
  cat("\n----------------------\n")
  cat(paste("WARNING: the protocol has been applied in RESTART mode using the file:",
            file.path(out_dir,"checkpoint.Rdata")))
  cat("Change the name of this file to disable RESTART mode.")
  cat("\n----------------------\n")
}
