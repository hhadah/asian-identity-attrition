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
# print all R scripts in programs folder
r_scripts <- list.files(programs, pattern = "\\.R$", full.names = TRUE)
print(r_scripts)
# main scripts
# main scripts
source(file.path(programs,"01-packages-wds.R")) # set up package
source(file.path(programs,"02-clean-implicit-project-asian.R")) # clean asian implicit data
source(file.path(programs,"03-merge-asian-iat-cps.R")) # merge iat and cps
source(file.path(programs,"04-table-one-summary-stats.R")) # summary stats of sample
source(file.path(programs,"05-table-two-observations-by-gen.R")) # ethnic attrition of sample
source(file.path(programs,"06-figure-two-skin-iat-plots.R")) # figure of iat and asian identity
source(file.path(programs,"07-figure-three-skin-map.R")) # asian iat maps
source(file.path(programs,"08-figure-five-skin-iat-regression-by-gen-plot.R")) # asian iat regression by gen plot
source(file.path(programs,"09-figure-six-regressions-skin-iat-byparent-plot.R")) # asian iat by parent plot
source(file.path(programs,"10-table-four-skin-iat-regression-by-gen.R")) # asian iat regression by gen table
source(file.path(programs,"11-table-five-regressions-skin-iat-byparent.R")) # asian iat by parent regression
source(file.path(programs,"12-figure-eight-skin-iat-regression-interaction-bygen-plot.R")) # interaction regression
source(file.path(programs,"13-table-six-regressions-skin-iat-thirdgens-grandparents.R")) # regression by grandparents type
source(file.path(programs,"14-regressions-cps-asian-iat.R")) # iat regression on asian identity
source(file.path(programs,"15-asian-iat-regression-by-gen.R")) # iat regression table by gen
source(file.path(programs,"16-regressions-asian-iat-byparent.R")) # iat regression table by parent
source(file.path(programs,"17-table-eight-endo-interethnic-reg.R")) # interethnic regression table
source(file.path(programs,"18-table-nine-proxy-main-effect-table.R")) # main effect table
source(file.path(programs,"19-prep-data-for-acs.R")) # prep ACS data
source(file.path(programs,"20-acs-skin-iat-migration.R")) # ACS migration analysis
source(file.path(programs,"21-cps-iat-county.R")) # county-level CPS IAT
source(file.path(programs,"22-cps-iat-msa.R")) # MSA-level CPS IAT
source(file.path(programs,"23-county-figure-five-skin-iat-regression-by-gen-plot.R")) # county regression by gen plot
source(file.path(programs,"24-county-figure-six-regressions-skin-iat-byparent-plot.R")) # county regression by parent plot
source(file.path(programs,"25-county-figure-eight-skin-iat-regression-interaction-bygen-plot.R")) # county interaction plot
source(file.path(programs,"26-msa-figure-five-skin-iat-regression-by-gen-plot.R")) # MSA regression by gen plot
source(file.path(programs,"27-msa-figure-six-regressions-skin-iat-byparent-plot.R")) # MSA regression by parent plot
source(file.path(programs,"28-msa-figure-eight-skin-iat-regression-interaction-bygen-plot.R")) # MSA interaction plot
source(file.path(programs,"29-merge-cps-with-hatecrime.R")) # merge CPS with hate crime data
source(file.path(programs,"30-hatecrime-figure-five-skin-iat-regression-by-gen-plot.R")) # hate crime regression by gen plot
source(file.path(programs,"31-hatecrime-figure-six-regressions-skin-iat-byparent-plot.R")) # hate crime regression by parent plot
source(file.path(programs,"32-figure-10-cps-hispanic-plot.R")) # Hispanic CPS plot
source(file.path(programs,"33-multi-logit-reg.R")) # multinomial logit regression
source(file.path(programs,"34-merge-asian-iat-cps-adults.R")) # merge IAT and CPS adults
source(file.path(programs,"35-table-adults-summary-stats.R")) # adults summary stats
source(file.path(programs,"36-table-adults-observations-by-gen.R")) # adults observations by gen
source(file.path(programs,"37-figure-five-skin-iat-regression-by-gen-plot-adults.R")) # adults regression by gen plot
source(file.path(programs,"38-figure-six-regressions-skin-iat-byparent-plot-adults.R")) # adults regression by parent plot
source(file.path(programs,"39-figure-histogram-identities.R")) # histogram of identities
source(file.path(programs,"40-interaction-iat-byparent-plot.R")) # interaction by parent plot
source(file.path(programs,"41-interaction-regressions-skin-iat-thirdgens-grandparents.R")) # interaction regression grandparents
source(file.path(programs,"44-bootstrap-run.R")) # bootstrap analysis
source(file.path(programs,"45-bootstrap-plot.R")) # bootstrap plot
source(file.path(programs,"46-secadult-bootstrap-run.R")) # second adult bootstrap
source(file.path(programs,"47-secadult-bootstrap-plot.R")) # second adult bootstrap plot
source(file.path(programs,"95-make-all.R")) # make all

### summary stats

# Send Message

# textme(message = "👹 Back to work! You're not paid to run around and drink ☕ all day!")
