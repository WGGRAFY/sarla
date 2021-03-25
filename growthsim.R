#install packages
remotes::install_github("r4ss/r4ss", ref = "development")
remotes::install_github("ss3sim/ss3sim", ref = "development")
remotes::install_github("nmfs-fish-tools/SSMSE")
require(SSMSE)

# create paths
run_SSMSE_dir <- file.path("run_SSMSE-ex")
dir.create(run_SSMSE_dir)
cod_mod_path <- system.file("extdata", "models", "cod", package = "SSMSE")


# develop_OMs will save a model called "cod_SR_BH_steep_1" in the out_dir
# specified
develop_OMs(OM_name = "cod", out_dir = run_SSMSE_dir, par_name = "SR_BH_steep",
            par_vals = 1, refit_OMs = FALSE, hess = FALSE)
# OM model for scenario 2
cod_1_path <- file.path(run_SSMSE_dir, "cod_SR_BH_steep_1")

#find dat file
datfile <- system.file("extdata", "models", "cod", "ss3.dat", package = "SSMSE")

#creates a sample structure for the datfile
sample_struct <- create_sample_struct(dat = datfile, nyrs = 6) # note warning

sample_struct$lencomp <- NULL # don't use length sampling

sample_struct_list <- list("h-ctl" = sample_struct, "h-1" = sample_struct)

run_res_path <- file.path(run_SSMSE_dir, "results")
dir.create(run_res_path)
run_SSMSE(scen_name_vec = c("h-ctl", "h-1"),# name of the scenario
          out_dir_scen_vec = run_res_path, # directory in which to run the scenario
          iter_vec = c(5,5), # run with 5 iterations each
          OM_name_vec = NULL, # specify directories instead
          OM_in_dir_vec = c(cod_mod_path, normalizePath(cod_1_path)), # OM files
          EM_name_vec = c("cod", "cod"), # cod is included in package data
          MS_vec = c("EM","EM"),       # The management strategy is specified in the EM
          use_SS_boot_vec = c(TRUE, TRUE), # use the SS bootstrap module for sampling
          nyrs_vec = c(6, 6),        # Years to project OM forward
          nyrs_assess_vec = c(3, 3), # Years between assessments
          rec_dev_pattern = "rand", # Use random recruitment devs
          scope = "2", # to use the same recruitment devs across scenarios.
          impl_error_pattern = "none", # Don't use implementation error
          run_EM_last_yr = FALSE, # Run the EM in 106
          run_parallel = FALSE, # Run iterations in parallel
          sample_struct_list = sample_struct_list, # How to sample data for running the EM.
          seed = 12345) #Set a fixed integer seed that allows replication
