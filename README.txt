Researcher: Michael R. Larsen 
Project: Solving Common Reserving Problems with Bayesian MCMC   
This repository contains code and supporting materials developed by John Smith in connection with research conducted for the Casualty Actuarial Society. The repository is hosted by CAS pursuant to the applicable licensing agreement.  
Copyright and other ownership rights in the materials remain as specified in that agreement.

Project Name and Description
A group of programs were written to write a call paper, “Solving Common Reserving Problems with Bayesian MCMC.”  That group of programs is included to allow one to access the code for the different models shown in the call paper. 
The intent of the call paper is to give actuaries examples of how one can use currently available Bayesian MCMC open-source software (Stan and related programs) to solve reserve modeling problems and explain how this approach benefits the customers of the reserve analysis.
The programs and files that are furnished are intended to:
* Give those who read the call paper the opportunity to gain hands-on experience in creating the models that were summarized in the call paper.
* Provide coding examples for anyone who is looking for a place to start when building loss P&C reserve models.

There were two sets of R scripts created to build the examples. The first set of R scripts are simulation programs used to build the data sets for the examples. The second set of R scripts built the Bayesian MCMC models with the code from those R scripts put into Rmarkdown files in code chunks so as to insert a few comments between different steps in building the reserve models. 

This set of files (R scripts, Rmarkdown and EXCEL) was created to give actuaries who want to learn how to apply Bayesian MCMC to solve reserving problems some working examples where they can gain hands-on experience. That hands-on experience includes using R scripts for simulation code to create data sets that will be used in the Bayesian MCMC modeling examples along with Rmarkdown files that give users the ability to gain modeling experience with Bayesian MCMC in a structured environment.  
Installation Instructions
You will need to have Rstudio, R and the rtools installed as well as R the R packages that are listed in the Load_Packages.R script.
Install R4.6.1
Go to The Comprehensive R Archive Network (r-project.org) which is at https:// cran.r-project.org.  Download the precompiled version of R4.6.1 following the instructions furnished on the website and take the defaults. If you already have R installed, but it’s an earlier version please update to R4.6.1 and make sure Rstudio is pointing towards R4.6.1since the version of rstan we will use runs under R4.6.1.
Install rtools45
Go to RTools: Toolchains for building R and R packages from source on Windows (r-project.org) which is at https://cran.r-project.org/bin/windows/Rtools/. Install rtools45 since this version of rtools is compatible with R4.6.1.  Follow the default instructions. 
Install Rstudio
Go to RStudio Desktop - Posit which is at https:// posit.co/download/rstudio-desktop/. Install Rstudio by downloading the file for Rstudio and then take the defaults. 
R Packages
The following packages should be installed, and those packages should be current with R4.6.1: dplyr, readxl, brms, magittr, purr, forcats, tidyr, modelr, ggdist, tidybayes, ggplot2, cowplot, rstan, ggrepel, RColorBrewer, gganimate, broom, shinystan, writexl, knitr, loo, yaml, rlang, readr, stringr, gridExtra, rstudioapi.
In particular, install rstan version 2.32.7 using the install package option in Rstudio.

Create .Renviron File
At the prompt in the R console window in Rstudio, execute the following statement:
writeLines('PATH="${RTOOLS43_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Then restart R and verify that RTools is being found via executing at the prompt in the R console:
Sys.which("make")
## "C:\\rtools45\\usr\\bin\\make.exe"

Usage Instructions
You will need to run the programs in Rstudio and create a project in Rstudio that contains those files before starting. 
The Rmarkdown and R scripts will have a reference to one of the three cases in the call paper to allow one to find the code that is linked to a given example in the call paper:

* Underwriting
* Inflation
* Mixture
There are two sets of programs:
* Rmarkdown files that contain code chunks to create the models and model output with a few comments between the code chunks. You will need to have the EXCEL files in the same folder as the Rmarkdown program.
* R scripts that create the EXCEL data files via simulation that are used by the Rmarkdown files. The R script that creates the general inflation files has to be run before the files that build the EXCEL data sets used as input to the modeling work captured in the Rmarkdown files.
If you are using the Rmarkdown files, you can execute the code chunks one after the other that are in those files and read the comments to get a sense of what is taking place. 
If you are using the R scripts for the simulation routines used to build the data sets, you should run the following programs in the sequence listed first:
* Load_Packages.R
* Call_Paper_Functions.R
* Inflation_Loss_Cost_Trend_Call_Paper.R

 For the simulation scripts, you can outline a given set of code and use the “run” option shortcut to run the results. Please note that the R script code will generate data sets that will end up on your laptop in the same folder in which they are run.
If there are questions about what a given package or routine is doing, one should use the help options in Rstudio. There are some limited comments about the intended goal of a given section of code, but explaining how a package works or R coding in general is outside the scope of the support furnished. 

File Manifest
The Rmarkdown files that contain the Bayesian MCMC modeling code and code to translate the model results as well as read in the EXCEL files:
* Case_Inflation_Analysis.Rmd
* Case_Underwriting_Analysis.Rmd
* Case_Mixture_Analysis.Rmd
The EXCEL files used as input to the reserve analysis:
* Case_Inflation_9_train.xlsx
* Case_Inflation_ran_grp_9_train.xlsx
* Cal_Yr_Infl_Case_Inflation.xlsx
* Case_Underwriting_train.xlsx
* Cal_Yr_Infl_Case_Underwriting.xlsx
* Case_Underwriting_ran_grp_train.xlsx
* Case_Mixture_train.xlsx
* Cal_Yr_Infl_Case_Mixture.xlsx
* Case_Mixture_ran_grp_train.xlsx
The R scripts that generated the data sets used in the Rmarkdown files:
* Load_Packages.R
* Call_Paper_Functions.R
* Inflation_Loss_Cost_Trend_Call_Paper.R
* Data_Set_Inflation_Case_Experiment_9.R
* Data_Set_ Underwriting_Case_Experiment.R
* Data_Set_Mixture_Case_Experiment.R

As a side comment, the “_9” in the titles came about from taking nine attempts to get a final working data set for the inflation case.
System Requirements
The version numbers required are noted in the installation instructions. 
Licensing & Copyright
The programs and files included are intended for educational usage only. The code used in the programs uses open source software and the same copyright limitations attached to that software applies to the examples furnished. 
Contact Information
Furnishing the code behind the call paper, “Solving Common Reserving Problems with Bayesian MCMC” for educational purposes does not mean that I agree to support and update it as packages and future versions of R change what the packages can do. 
Michael Ray Larsen
Email: mikeplcact@gmail.com

