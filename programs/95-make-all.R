#######################################################################
# Master script
#######################################################################

## Clear out existing environment
gc()
rm(list = ls()) 
## Set master directory where all sub-directories are located

CPS_path <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_DataTable.csv"
CPS_all_path <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_DataTable_All.csv"
CPS_asian <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_DataTable_Asians.csv"
ACS_path <- "~/Dropbox/Research/My Research Data and Ideas/ACS-migration/ACS_DataTable.csv"
CPS_asian_mean  <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_Objective_Asian_Percentage_bystate_DataTable.csv"
Implicit_asian_Harvard <- "~/Dropbox/Research/My Research Data and Ideas/ProjectImplicit/asian_iat/Asian_IAT.public.2004-2021.csv"
Implicit_Race_Harvard <- "~/Documents/GiT/Project-Implicit-Data/data/datasets"

ANES_dir <- "/Users/hhadah/Dropbox/ANES/anes_timeseries_cdf_csv_20220916/anes_timeseries_cdf_csv_20220916.csv"

### GiT directories
git_mdir <- here::here()
datasets <- paste0(git_mdir,"/data/datasets")
raw <- paste0(git_mdir,"/data/raw")
tables_wd <- paste0(git_mdir,"/output/tables")
figures_wd <- paste0(git_mdir,"/output/figures")
programs <- paste0(git_mdir,"/programs")
thesis_tabs <- paste0(git_mdir,"/my_paper/tables")
thesis_plots <- paste0(git_mdir,"/my_paper/figure")

### run do files and scripts

# main scripts
source(file.path(programs,"01-packages-wds.r")) # set up package
source(file.path(programs,"02-clean-implicit-project-asian.r")) # set up package
source(file.path(programs,"03-merge-asian-iat-cps.r")) # set up package
source(file.path(programs,"04-table-one-summary-stats.r")) # set up package
source(file.path(programs,"05-table-two-observations-by-gen.r")) # set up package
source(file.path(programs,"06-figure-two-skin-iat-plots.r")) # set up package

### summary stats

# Send Message

textme(message = "👹 Back to work! You're not paid to run around and drink ☕ all day!")