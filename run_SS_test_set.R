# run SS models, r4ss plots, and save a spreadsheet of model quantities.
# adapted from https://github.com/r4ss/testing/blob/master/modeltesting.R (still
# sourcing function from this location)

# Load packages, set options ----
#devtools::install_github("r4ss/SSutils")
library(SSutils)
#devtools::install.github("r4ss/r4ss@development")
library(r4ss)
#devtools::install_github("DavisVaughan/furrr")
library(furrr) # parallelized versions of purrr functions
options(stringsAsFactors = FALSE)
#https://github.com/r4ss/testing/blob/master/modeltesting.R
source("modeltesting.R")

#' Run a set of ss models, use r4ss to plot them, and create a summary table
#' of results
#' 
#' @param old_mod_folder Path to a folder containing folders of model files
#'  (can be prevously run)
#' @param new_mod_folder Path in which to run the new models. The folder does 
#'  not yet need to exist, as it will be created.
#' @param exe_name Name of the Stock Synthesis executable (no extension 
#'  required). Defaults to "ss".
#' @param dir_sum Directory containing the summary tables
#' @param old_sum_name Name of an existing summary table
#' @param new_sum_name Name of a new summary table to create
#' @details For now this function assumes that the SS exe exe_name is in the
#'  PATH, that 
#' @return A list of components: run_results indicating if the models ran or not,
#' 
#' @import r4ss SSutils future furrr 
run_ss_test_set <- function(old_mod_folder, new_mod_folder, exe_name = "ss",
                            dir_sum,
                            old_sum_name, new_sum_name, parallel = TRUE, 
                            exe_in_path = TRUE, verbose = FALSE) {
  # copy and run models ----
  if(verbose) message("Copying Models")
  copy_success <- SSutils::populate_multiple_folders(
    outerdir.old = old_mod_folder, 
    outerdir.new = new_mod_folder,
    overwrite = TRUE,
    exe.file = NULL,
    verbose = FALSE)
  dirvec <- file.path(new_mod_folder, list.files(new_mod_folder))
  if(verbose) message("Running Models")
  if(parallel) { 
    future::plan(multiprocess) # uses furrr
    run_results <- SSutils::run_SS_models_parallel(dirvec,
                                                   model = exe_name,
                                                   exe_in_path = exe_in_path,
                                                   intern = TRUE, 
                                                   verbose = FALSE)
  } else {
    run_results <- SSutils::run_SS_models(dirvec, 
                                          model = exe_name,
                                          exe_in_path = exe_in_path, 
                                          intern = TRUE, verbose = FALSE )
  }
  # add output to csv ----
  # read the output from the new runs and add it to the summary table
  if(verbose) message("Creating output table")
  alloutput <- addtotable(dir        = dir_sum,
                          oldtable   = old_sum_name,
                          newtable   = new_sum_name,
                          SSversions = basename(new_mod_folder))
  # plot model output ----
  if (verbose) message("Running SSgetoutput")
  mods_out <- r4ss::SSgetoutput(dirvec = dirvec, verbose = FALSE)
  # run plotting functions in parallel to make faster
  if(parallel) {
    future::plan(multiprocess)
  } else {
    future::plan(sequential)
  }
  if (verbose) message("Running SS_plots")
  #TODO: allow this function to work even if 1 model fails to be plotted.
  plot_return <- furrr::future_map(mods_out, ~SS_plots(replist = .x, 
                                                 printfolder = "plots", 
                                                 verbose = FALSE), 
                            .progress = TRUE)
  # return output ----
  return_list <- list(copy_success = copy_success,
                      run_results = run_results,
                      model_output = alloutput,
                      plot_return = plot_return)
  if(verbose) message("run_ss_test_set complete!")
  return_list
}

# call function
test_results <- run_ss_test_set(old_mod_folder = file.path("test_models", "old_mod_folder"), 
                new_mod_folder = file.path("test_models", "test_new_mod_folder"), 
                exe_name = "ss_3.30.15", dir_sum = "test_models",
                old_sum_name = "summarytable.csv", new_sum_name = "summarytable_new.csv", 
                parallel = FALSE)

