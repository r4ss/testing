# update examples

# vals to change ----
# note: within the old_outer_folder_name should be a folder named run_old 
# containing the model files
old_outer_folder_name <- "update_3.30.14_to_3.30.15"
new_outer_folder_name <- "update_3.30.15_to_3.30.16"
exe_name <- "ss_3.30.16.exe"

# Load pkgs, set options ----
library(r4ss)

# create new folder ----
dir.create(new_outer_folder_name)

# Copy over old files ---
run_new_mod_folder <- file.path(new_outer_folder_name, "run_old")
r4ss::populate_multiple_folders(
  outerdir.old = file.path(old_outer_folder_name, "updated_examples"), 
  outerdir.new = run_new_mod_folder,
  exe.file = NULL, verbose = FALSE)

dirvec <- file.path(run_new_mod_folder, list.files(run_new_mod_folder))
# Run model ---
run_results <- r4ss::run_SS_models(dirvec = dirvec, model = exe_name,
              exe_in_path = TRUE, verbose = FALSE, intern = TRUE)
if(!all(run_results$result == "ran model")) {
  stop("Not all models ran successfully.")
}
#check that the runs worked.
# copy over .ss_new files from model run ----
updated_mod_folder <- file.path(new_outer_folder_name, "updated_examples")
test_updated_folder <- file.path(new_outer_folder_name, "test_updated_examples")
r4ss::populate_multiple_folders(
  outerdir.old = run_new_mod_folder, 
  outerdir.new = updated_mod_folder,
  use_ss_new = TRUE,
  exe.file = NULL, 
  verbose = FALSE)
r4ss::populate_multiple_folders(
  outerdir.old = run_new_mod_folder, 
  outerdir.new = test_updated_folder,
  use_ss_new = TRUE,
  exe.file = NULL, 
  verbose = FALSE)


# check that new models will run ---
new_dirvec <- file.path(test_updated_folder, list.files(test_updated_folder))
updated_results <- run_SS_models(dirvec = new_dirvec, model = exe_name, exe_in_path = TRUE,
              verbose = FALSE, intern = TRUE)
if(!all(updated_results$result == "ran model")) {
  stop("Not all new models ran successfully.")
}
#TODO: figure out way to automate adding ss_summary.sso from the test run to the 
#updated_examples files.