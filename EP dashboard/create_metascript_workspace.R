# This file uses RServe's "metascript" to generate various useful data frames
# that would normally be used to create reports in Copilot. Here we use the
# `save_workspace_only` parameter to skip all the report rendering and just
# save the useful data to .rds files in a mounted crypt.
#
# This is the first step in rendering the EP Dashboard RMD.
#
# ## Requirements
#
# * perts_crypt.vc mounted
# * the EP dashboard crypt mounted, containing exactly one folder named
#   "rserve_data"
# * working directory set to analysis repo on your local machine
# * internet connection that can reach our Cloud SQL databases (notably
#   Impact Hub's wifi cannot do this).
#
# ## Outline
#
# This script will, when run, download data exactly as RServe does, from
# Qualtrics and the Triton and Neptune databases. Then it will aggregate and
# impute responses in the normal way. It will skip all unnecessary functions
# like creating reports, saving summaries, or sending emails.
#
# When it is complete, the "rserve_data" folder you mounted should have .rds
# files in it that have been recently modified. The expected structure is:
#
# - rserve_data/
#   - rds/
#     - anonymization_input_df.rds
#     - metascript_args.rds
#     - metascript_output_workspace.rds

gymnast <- new.env()
source(
  "https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_legacy.R",
  local = gymnast
)

if (!grepl('/analysis/?$', getwd())) {
  stop("This file expects the working directory to be the root of the analysis repository.")
}

analysis_repo_wd <- getwd()
setwd(paste0(getwd(), "/rserve"))

gymnast$ensure_packages('jsonlite')
deps <- unlist(jsonlite::read_json('package.json')$rserve$dependencies)
gymnast$ensure_packages(deps)


ep_script <- modules::use('scripts/ep.R')
import_data <- modules::use('modules/import_data.R')$import_data
setwd(analysis_repo_wd)
qualtrics <- modules::use('common/qualtrics.R')


paths <- list(creds = 'rserve_credential_body.json')
creds_json <- gymnast$util.find_crypt_paths(paths)$creds
creds <- jsonlite::fromJSON(creds_json)

ep_script$main(
  auth_header = NA,
  platform_data = import_data(
    neptune_sql_credentials = creds$neptune_sql_credentials,
    triton_sql_credentials = creds$triton_sql_credentials
  ),
  qualtrics_service = qualtrics$create_service(creds$qualtrics_credentials$api_key),
  sheets_service = NULL,
  emailer = NULL,
  script_params = list(
    run_program = 'ep19',
    save_workspace_only = TRUE
  ),
  should_post = FALSE
)

