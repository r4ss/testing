# tune comps demo

#install_github("r4ss/r4ss@issue-433") # need to use a particular branch of r4ss
# for now
library(r4ss)

dir <- "simple_tune"
# we will run this example in a different folder, called "simple_tune
copy_SS_inputs(dir.old =  system.file("extdata", "simple_3.30.13", package = "r4ss"), 
               dir.new = file.path(dir), create.dir = T)
# I will be using an exe that is in my PATH, but it is also possible to just
# use an exe in the simple_tune folder.

# I'll just run a model to start. If you have an exe called "ss.exe" available
# in the simple_tune folder, you can use model = "ss" and exe_in_path = FALSE.
run_SS_models(dirvec = dir, model = "ss_3.30.16", verbose = FALSE,
              exe_in_path = TRUE)
# just see the tunings, without rerunning the model.
tuning_vals <- SS_tune_comps(replist = NULL, fleets = "all", option = "Francis", 
                             niters_tuning = 0, dir = dir, verbose = FALSE)
tuning_vals

# Now, try rerunning the models with tuning. Note in this case the weights are
# all above 1. For this example, we will allow upweighting, but it is possible
# to also not allow the weights to be higher than 1.
used_francis_vals <- SS_tune_comps(replist = NULL, fleets = "all",
                                   option = "Francis", niters_tuning = 1,
                                   dir = dir, model = "ss_3.30.16",
                                   verbose = FALSE, allow_up_tuning = TRUE,
                                   exe_in_path = TRUE)
used_francis_vals

# Now, we want to use the same model, but with Dirichlet multinomial weighting.
# we will set up a new folder to do this.
dir_DM <- "simple_DM"
copy_SS_inputs(dir.old =  system.file("extdata", "simple_3.30.13", package = "r4ss"), 
               dir.new = file.path(dir_DM), create.dir = T)
file.copy(from = system.file("extdata", "simple_3.30.13", "Report.sso",
                             package = "r4ss"),
          to = file.path(dir_DM, "Report.sso"))
DM_pars <- SS_tune_comps(replist = NULL, fleets = "all",
                         option = "DM", dir = dir_DM, model = "ss_3.30.16",
                         niters_tuning = 1, verbose = FALSE, exe_in_path = TRUE)
DM_pars

# see that the DM pars were added
start <- SS_readstarter(file.path(dir_DM, "starter.ss"), verbose = FALSE)
dat <- SS_readdat(file.path(dir_DM, start$datfile), verbose = FALSE)
ctl <- SS_readctl(file.path(dir_DM, start$ctlfile),
                  use_datlist = TRUE, datlist = dat,
                  verbose = FALSE)
ctl[["dirichlet_parms"]]
dat$len_info
dat$age_info

# This function can also get rid of DM parameters from a model and rerun with
# Macallister Ianelli or Francis tuning
used_MI_vals <- SS_tune_comps(replist = NULL, fleets = "all",
                              option = "MI", niters_tuning = 1,
                              dir = dir_DM, model = "ss_3.30.16",
                              verbose = FALSE, allow_up_tuning = FALSE,
                              exe_in_path = TRUE)
used_MI_vals

# read in files again to confirm dm parameters removed
dat <- SS_readdat(file.path(dir_DM, start$datfile), verbose = FALSE)
ctl <- SS_readctl(file.path(dir_DM, start$ctlfile),
                  use_datlist = TRUE, datlist = dat,
                  verbose = FALSE)

ctl[["dirichlet_parms"]]
dat$len_info
dat$age_info
