# run SS models, r4ss plots, and save a spreadsheet of model quantities.
# adapted from https://github.com/r4ss/testing/blob/master/modeltesting.R (still
# sourcing function from this location)

# Load packages, set options ---------------------------------------------------
library(devtools)
#install_github("r4ss/SSutils")
library(SSutils)
#install.github("r4ss/r4ss@development")
library(r4ss)
#install_github("DavisVaughan/furrr")
library(furrr) # parallelized versions of purrr functions.
options(stringsAsFactors = FALSE)
#https://github.com/r4ss/testing/blob/master/modeltesting.R
source(file.path("code", "modeltesting.R"))

# dynamic input to change -----------------------------------------------------
#will want to change these each time the script runs.
dir <- file.path("testing_models")
old_mod_folder <- file.path(dir, "v_3.30.14.00_July16")
new_mod_folder <- file.path(dir, "v_3.30.14.20_20200318") # need not exist yet
model <- "ss_3.30.14.20" # NOTE: this script assumes this is in the PATH. 
old_sum_name <- "summarytable.csv"
new_sum_name <- "summarytable_3.30.14.20.csv" # need not exist yet.
# copy and run models ----------------------------------------------------------
populate_multiple_folders(outerdir.old = old_mod_folder, 
                          outerdir.new = new_mod_folder,
                          overwrite = TRUE,
                          exe.file = NULL)

dirvec <- file.path(new_mod_folder, list.files(new_mod_folder))
# Note: running in parallel is probably not worth it unless there is a large
# set of models
plan(multiprocess)
run_results <- run_SS_models_parallel(dirvec,
                                      model = model,
                                      exe_in_path = TRUE)
# plot model output, add to csv ------------------------------------------------
mods.out <- SSgetoutput(dirvec = dirvec, verbose = FALSE)
# run plotting functions in parallel to make faster
plan(multiprocess) # change to plan(sequential) to not run in parallel
future_map(mods.out, ~SS_plots(replist = .x, 
                               printfolder = "plots", 
                               verbose = FALSE), 
           .progress = TRUE)
plan(sequential) # function fast so not worth running in parallel.
# look at file size to make sure ran: 
# check for successful completion of plots
# Look for 0s or small values. TODO: automate this.
sizes <- future_map(dirvec,
                    ~paste(.x, "size: ", 
                           file.info(file.path(.x, "plots/SS_output.html"))$size)
                    )
# read the output from the new runs and add it to the summary table
alloutput <- addtotable(dir        = dir,
                        oldtable   = old_sum_name,
                        newtable   = new_sum_name,
                        SSversions = basename(new_mod_folder))


