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
ACS_path <- "/Users/hhadah/Library/CloudStorage/Dropbox/Research/My Research Data and Ideas/ACS-migration/ACS_DataTable_asians.csv"
CPS_asian_mean  <- "~/Dropbox/Research/My Research Data and Ideas/CPS_4GenData/CPS_Objective_Asian_Percentage_bystate_DataTable.csv"
Implicit_asian_Harvard <- "~/Dropbox/Research/My Research Data and Ideas/ProjectImplicit/asian_iat/Asian_IAT.public.2004-2021.csv"
Implicit_Race_Harvard <- "~/Documents/GiT/Project-Implicit-Data/data/datasets"
GSS_bystate_new_path <- "~/Dropbox/Research/GSS/Data/Datasets/Prejeduice_index_by_stateyear_new.csv"

ANES_dir <- "/Users/hhadah/Dropbox/ANES/anes_timeseries_cdf_csv_20220916/anes_timeseries_cdf_csv_20220916.csv"
hate_crime <- "/Users/hhadah/Documents/GiT/hate-crime-data/data/datasets/hate_crime_data_per_thousands.csv"
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
options(modelsummary_factory_latex = "kableExtra")

# main scripts
source(file.path(programs,"01-packages-wds.r")) # set up package
source(file.path(programs,"02-clean-implicit-project-asian.r")) # clean asian implicit data
source(file.path(programs,"03-merge-asian-iat-cps.r")) # merge iat and cps
source(file.path(programs,"04-table-one-summary-stats.r")) # summary stats of sample
source(file.path(programs,"05-table-two-observations-by-gen.r")) # ethnic attrition of sample
source(file.path(programs,"06-figure-two-skin-iat-plots.r")) # figure of iat and asian identity
source(file.path(programs,"07-figure-three-skin-map.r")) # asian iat maps
source(file.path(programs,"08-figure-five-skin-iat-regression-by-gen-plot.r")) # asian iat maps
source(file.path(programs,"09-figure-six-regressions-skin-iat-byparent-plot.r")) # asian iat maps
source(file.path(programs,"10-table-four-skin-iat-regression-by-gen.r")) # asian iat maps
source(file.path(programs,"11-table-five-regressions-skin-iat-byparent.r")) # asian iat by parent regression
source(file.path(programs,"12-figure-eight-skin-iat-regression-interaction-bygen-plot.r")) # interaction regression
source(file.path(programs,"13-table-six-regressions-skin-iat-thirdgens-grandparents.r")) # regression by grandparents type
source(file.path(programs,"14-regressions-cps-asian-iat.r")) # iat regression on asian identity
source(file.path(programs,"15-asian-iat-regression-by-gen.r")) # iat regression table by gen
source(file.path(programs,"16-regressions-asian-iat-byparent.r")) # iat regression table by parent
source(file.path(programs,"17-table-eight-endo-interethnic-reg.r")) # interethnic regression table
source(file.path(programs,"18-table-nine-proxy-main-effect-table.r")) # main effect table
source(file.path(programs,"19-prep-data-for-acs.R")) # 
source(file.path(programs,"20-acs-skin-iat-migration.R")) # 
source(file.path(programs,"21-cps-iat-county.R")) # 
source(file.path(programs,"22-cps-iat-msa.R")) # 
source(file.path(programs,"23-county-figure-five-skin-iat-regression-by-gen-plot.R")) # 
source(file.path(programs,"24-county-figure-six-regressions-skin-iat-byparent-plot.R")) # 
source(file.path(programs,"25-county-table-four-skin-iat-regression-by-gen.R")) # 
source(file.path(programs,"26-county-table-five-regressions-skin-iat-byparent.R")) # 
source(file.path(programs,"27-county-figure-eight-skin-iat-regression-interaction-bygen-plot.R")) # 
source(file.path(programs,"28-msa-figure-five-skin-iat-regression-by-gen-plot.R")) # 
source(file.path(programs,"29-msa-figure-six-regressions-skin-iat-byparent-plot.R")) # 
source(file.path(programs,"30-msa-table-four-skin-iat-regression-by-gen.R")) # 
source(file.path(programs,"31-msa-figure-eight-skin-iat-regression-interaction-bygen-plot.R")) # 
source(file.path(programs,"32-msa-table-five-regressions-skin-iat-byparent.R")) # 


### summary stats

# Send Message

textme(message = "👹 Back to work! You're not paid to run around and drink ☕ all day!")