# R script for testing a new SS executable

addtotable <- function(dir="\\\\nwcfs2\\assessment\\FramPublic\\StockSynthesisStuff\\modeltesting\\",
                       SSversions=c("Version_X","Version_Y"),
                       oldtable="summarytable.csv",
                       newtable="newsummarytable.csv")
{
  # read output from model runs and add to table of results
  cat("reading",paste(dir,oldtable,sep="\\"),"\n")
  summarytable <- read.csv(paste(dir,oldtable,sep="\\"))

  # stuff from existing table
  Model <- summarytable$Model
  Quant <- summarytable$Quantity
  nquants <- length(unique(Quant))

  alloutputs <- list()

  for(iversion in 1:length(SSversions)){ # loop over versions of SS
    newcolumn <- rep(NA,nrow(summarytable))

    cat("\nGetting info from folder:",SSversions[iversion],"\n")
    versionfolder <- paste(dir,SSversions[iversion],sep="\\")
    if(is.na(file.info(versionfolder)$isdir)) stop(versionfolder,"\n  is not a directory")

    # check for which are directories
    folderlist <- NULL
    stuff <- dir(versionfolder) # get list of subfolders within olddir
    for(i in 1:length(stuff)){ # loop over things within this directory
      # get full path of file
      foldername <- stuff[i]
      subfolder <- file.path(versionfolder, foldername)
      # check if it's a directory
      info <- file.info(subfolder)

      if(info$isdir){ # if it's a directory, then do stuff
        cat("checking",subfolder,"\n")
        # get starter and forecast, allowing for differences in capitalization
        starterfile <- dir(subfolder)[grep("^starter.ss$",tolower(dir(subfolder)))]

        if(length(starterfile)==1) folderlist <- c(folderlist,foldername)
      }
    }
    cat("\nGood folders are:",paste(folderlist,collapse="\n                 "),"\n")
    cat("Getting output from those folders...\n\n")

    nmodels <- length(folderlist)
    longfolderlist <- paste(versionfolder,folderlist,sep="\\")

    newoutput <- SSgetoutput(keyvec = NULL, dirvec = longfolderlist,
                              getcovar = TRUE, getcomp = TRUE,
                              forecast = FALSE, verbose = TRUE)
    names(newoutput) <- folderlist

    alloutputs[[SSversions[iversion]]] <- newoutput

    for(imodel in 1:nmodels){    # loop over test models within that version

      newreplist <- newoutput[[imodel]]
      if(is.list(newreplist) && "parameters" %in% names(newreplist)){
        # make new column
        newcolumn[Model==names(newoutput)[imodel] &
                  Quant=="TotalNLL"] <- newreplist$likelihoods_used$values[1]
        newcolumn[Model==names(newoutput)[imodel] &
                  Quant=="EndingDepl"] <- newreplist$current_depletion
        newcolumn[Model==names(newoutput)[imodel] &
                  Quant=="LogR0"] <- newreplist$parameters$Value[newreplist$parameters$Label %in% c("SR_LN(R0)","SR_R0")]
        newcolumn[Model==names(newoutput)[imodel] &
                  Quant=="LogR0_SD"] <- newreplist$parameters$Parm_StDev[newreplist$parameters$Label %in% c("SR_LN(R0)","SR_R0")]
        newcolumn[Model==names(newoutput)[imodel] &
                  Quant=="B0_SD"] <- newreplist$derived_quants$StdDev[newreplist$derived_quants$Label=="SSB_Virgin"]
        newcolumn[Model==names(newoutput)[imodel] &
                  Quant=="ForeCatch_last"] <- tail(newreplist$derived_quants$Value[grep("ForeCatch_",newreplist$derived_quants$Label)],1)
        newcolumn[Model==names(newoutput)[imodel] &
                  Quant=="ForeCatch_last_SD"] <- tail(newreplist$derived_quants$StdDev[grep("ForeCatch_",newreplist$derived_quants$Label)],1)
        newcolumn[Model==names(newoutput)[imodel] &
                  Quant=="Nwarnings"] <- newreplist$Nwarnings
        newcolumn[Model==names(newoutput)[imodel] &
                  Quant=="MaxGradient"] <- newreplist$maximum_gradient_component
      }
    }
    summarytable$newcolumn <- newcolumn
    names(summarytable)[names(summarytable)=="newcolumn"] <- SSversions[iversion]
  }


  cat("writing",paste(dir,newtable,sep="\\"),"\n")
  write.csv(summarytable,paste(dir,newtable,sep="\\"),row.names=FALSE)

  return(invisible(alloutputs))
}


if(FALSE){

  # new approach using SSutils
  library(SSutils)
  outerdir.old <- 'c:/SS/modeltesting/Version_3.30.13.09_July1'
  outerdir.new <- 'c:/SS/modeltesting/Version_3.30.14.00_July16'
  dir.info <- populate_multiple_folders(
      outerdir.old = outerdir.old,
      outerdir.new = outerdir.new,
      exe.dir = 'c:/SS/SSv3.30.14.00_July16', overwrite=FALSE)
  
  # run the models in each directory
  run_SS_models(dirvec = dir(outerdir.new, full.names=TRUE))

  # get output
  mods.dir <- dir(outerdir.new, full.names=TRUE)
  mods.out <- SSgetoutput(dirvec=mods.dir)
  
  # run plotting functions
  for(imod in 1:length(mods.out)){
    print(imod)
    print(mods.dir[imod])
    graphics.off()
    #SS_plots(mods.out[[imod]])
    SS_plots(mods.out[[imod]], printfolder = 'sexratio')
  }
  
  # check for successful completion of plots
  for(imod in 1:length(mods.dir)){
    #print(imod)
    print(mods.dir[imod])
    print(file.info(file.path(mods.dir[imod], "plots/SS_output.html"))$size)
  }


  # read the output from the new runs and add it to the summary table
  alloutput <-
    addtotable(dir = "c:/SS/modeltesting/",
               #dir = "\\\\nwcfs2\\assessment\\FramPublic\\StockSynthesisStuff\\modeltesting\\",
               oldtable = "summarytable.csv",
               newtable = "newsummarytable.csv",
               SSversions=c("Version_3.30.14.00_July16"))

  # end new approach



  #########################################################
  #### old stuff
  #########################################################

  
  ## this stuff should be pasted directly into R instead of run as a function
  source('c:/GitHub/testing/modeltesting.R')

  #### make directories and copy input files from one folder to the next

  # use ss_trans
  source('c:/GitHub/testing/modeltesting.R')
  folderinfo <- copyinputs(olddir="c:/SS/modeltesting/Version_3_24s_July24",
                           newdir="c:/SS/modeltesting/Version_3.30.08.02_trans")
  # now run using converted files
  source('c:/GitHub/testing/modeltesting.R')
  folderinfo <- copyinputs(olddir="c:/SS/modeltesting/Version_3.30.08.02_trans",
                           newdir="c:/SS/modeltesting/Version_3.30.08.02",
                           use_ss_new=TRUE)
  # now run using converted files
  source('c:/GitHub/testing/modeltesting.R')
  folderinfo <- copyinputs(olddir="c:/SS/modeltesting/Version_3.30.11.00_Mar29",
                           newdir="c:/SS/modeltesting/Version_3.30.12.00_June22",
                           use_ss_new=FALSE)

  # testing new function
  folderinfo <- populate_multiple_folders(
      outerdir.old="c:/SS/modeltesting/Version_3.30.11.00_Mar29",
      outerdir.new="c:/SS/modeltesting/Version_3.30.12.00_June22",
      use_ss_new=FALSE,
      exe.dir="c:/SS/SSv3.30.12_June22",
      exe.file="ss.exe")

    
  
  # starting after making directories
  source('c:/GitHub/testing/modeltesting.R')
  #setwd("c:/SS/modeltesting/Version_3.30.08.02_trans")
  setwd("c:/SS/modeltesting/Version_3.30.08.03")
  setwd("U:/SS/modeltesting/Version_3.30.08.03")
  folderinfo <- list(newdir=getwd(),
                     folderlist=dir())
  # on sysiphus
  folderinfo <- copyinputs(olddir="c:/SS/modeltesting/Version_3_20_Jan3",
                           newdir="y:/h_itaylor/SS/modeltesting/Version_3_20e_Mar15")

  # make copy using .ss_new files as sources to use as example files
  folderinfo <- copyinputs(olddir="c:/SS/modeltesting/Version_3_21e_June9_examples",
                           newdir="c:/SS/modeltesting/Version_3_21e_June9_examples_clean",
                           use_ss_new=TRUE)
  folderinfo <- copyinputs(olddir="c:/SS/modeltesting/Version_3_21e_June9_examples_clean",
                           newdir="c:/SS/modeltesting/Version_3_21e_June9_examples_test")

  # copy executables into subfolders where each new model will be run
  copyexe(sourcedir="C:/SS/SSv3.30.10.00_Dec8/",
          newdir=folderinfo$newdir,
          folderlist=folderinfo$folderlist,
          exe="ss_trans.exe")

  copyexe(sourcedir="C:/SS/SSv3.30.10.00_Dec8/",
          newdir=folderinfo$newdir,
          folderlist=folderinfo$folderlist,
          exe="ss.exe")
  
  ## # convert to SSv3.20
  ## setwd(folderinfo$newdir)
  ## for(i in 1:length(folderinfo$folderlist)){
  ##   model <- folderinfo$folderlist[i]
  ##   convert_to_v3.20(model,replace=T)
  ## }
  ## for(i in 1:length(folderinfo$folderlist)){
  ##   model <- folderinfo$folderlist[i]
  ##   (file.copy(paste(model,"forecast.ss",sep="/"),paste(model,"old_forecast.ss",sep="/")))
  ##   (file.copy("generic_forecast.ss",paste(model,"forecast.ss",sep="/"),overwrite=TRUE))
  ## }

  # run new SS executable for each example model without estimating anything
  runmodels(newdir=folderinfo$newdir,
            folderlist=folderinfo$folderlist,exe="ss_trans.exe",extras="-maxfn 0 -phase 20 -nohess -nox")

  # run new SS executable for each example model
  runmodels(newdir=folderinfo$newdir,
            folderlist=folderinfo$folderlist,exe="ss.exe",extras="-nox")

  # alternatively, run models in all subfolders
  #   if the folderinfo object is not available
  mydir <- "c:/SS/modeltesting/Version_3_24b_May15"
  runmodels(newdir=mydir, folderlist=dir(mydir),exe="SS3.exe",extras="-nox")
  runmodels(newdir=mydir, folderlist=dir(mydir),exe="SS3.exe",extras="-noest -nohess -nox")

  # on sysiphus
  source("http://r4ss.googlecode.com/svn/branches/testing/modeltesting.R")
  mydir <- "~/h_itaylor/SS/modeltesting/Version_3_20_Jan3"
  runmodels(newdir=mydir, folderlist=dir(mydir),exe="./SS3",extras="-nox")

  # get updated package files, including the SSgetoutput function
  devtools::install_github("r4ss/r4ss")
  library(r4ss)
  ## source('c:/GitHub/r4ss/R/update_r4ss_files.R')
  ## update_r4ss_files()

  #for(folder in dir(mydir)) file.remove(paste(folder,"SS3_safe.log",sep="/"))

  # add new models to newsummarytable
  # read table
  summarytable <- read.csv("c:/SS/modeltesting/summarytable.csv", stringsAsFactors=FALSE)
  # make list of models in it
  mods <- summarytable$Model
  ## # get list of quantities for each model
  ## quants <- summarytable$Quantity[summarytable$Model==mods[1]]
  # potentially bigger list of models
  mods.dir <- dir("c:/SS/modeltesting/Version_3.30.14.00_July16")
  mods.diff <- setdiff(mods.dir, mods)
  # rows for new model
  blankrows <- summarytable[summarytable$Model==mods[1],]
  blankrows[,-(1:2)] <- NA
  for(i in 1:length(mods.diff)){
    newrows <- blankrows
    newrows$Model <- mods.diff[i]
    summarytable <- rbind(summarytable, newrows)
  }
  write.csv(summarytable,file="c:/SS/modeltesting/expanded_summarytable.csv",
            row.names=FALSE)

  # read the output from the new runs and add it to the summary table
  alloutput <-
    addtotable(dir = "c:/SS/modeltesting/",
               #dir = "\\\\nwcfs2\\assessment\\FramPublic\\StockSynthesisStuff\\modeltesting\\",
               oldtable = "summarytable.csv",
               newtable = "newsummarytable.csv",
               SSversions=c("Version_3.30.14.00_July16"))

  # example on sysiphus
  alloutput <-
    addtotable(dir = "y:/h_itaylor/SS/modeltesting/",
               oldtable = "summarytable.csv",
               newtable = "newsummarytable.csv",
               SSversions=c("Version_3_20_Jan3"))

  # making plots
  for(i in length(alloutput):1){
    models <- alloutput[[i]]
    testvec <- rep(NA, length(models))
    for(j in 1:length(models)){
      if(is.na(models[[j]])){
        test <- 0
      }else{
        test <- SS_plots(models[[j]],pdf=T,verbose=F,forecast=F,datplot=T,aalresids=TRUE)
      }
      if(test==999){
        cat("!!!! plot code succeeded on model",j,"\n")
      }else{
        cat("!!!! plot code failed on model",j,"\n")
      }
      testvec[j] <- test
    }
    print(testvec)
  }


  # running on linux
  newdir <- "~/h_itaylor/SS/modeltesting/Version_3_11c_Oct30/"
  copyexe(sourcedir="~/h_itaylor/SS/SSv3.11c_Oct30",
          newdir=newdir,
          folderlist=dir(newdir),
          exe="SS3admb10")
  runmodels(newdir=newdir, folderlist=dir(newdir),exe="./SS3admb10")
}
