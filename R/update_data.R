# Script to prompt Makefile to update data prior to each build
source(file.path(getwd(),"R/load_dependencies.R"))
# source each data-raw script
source(file.path(getwd(), "data-raw/import_STARS_data.R"))
source(file.path(getwd(), "data-raw/utils_fct_predict_tillotson_model.R"))
source(file.path(getwd(), "data-raw/import_jpe_annual_data.R"))
source(file.path(getwd(), "data-raw/import_winter_run_chinook_lad_loss_data.R"))
source(file.path(getwd(), "data-raw/import_winter_run_chinook_genetic_loss_data.R"))
source(file.path(getwd(), "data-raw/import_winter_run_chinook_hatchery_loss_data.R"))
source(file.path(getwd(), "data-raw/import_steelhead_rbdd_biweekly_passage_data.R"))
source(file.path(getwd(), "data-raw/import_steelhead_loss_data.R"))
source(file.path(getwd(), "data-raw/import_steelhead_daily_loss_export_data.R"))
