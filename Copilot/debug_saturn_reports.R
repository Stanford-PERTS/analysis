# This file uses RServe's "saturn_metascript" to generate various useful data
# frames that would normally be used to create reports in Copilot.
#
# ## Requirements
#
# * perts_crypt.vc mounted (required to connect to production data sources)
# * another "workspace" crypt mounted (required to save rds load points),
#   containing this folder structure:
#   - rserve_data/
#     - rds/
#     - csv/
# * working directory set to analysis repo on your local machine
# * internet connection that can reach our Cloud SQL databases (notably
#   Impact Hub's wifi cannot do this).
#
# ## Outline
#
# This script will, when run, download data exactly as RServe does, from
# Saturn and the Triton and Neptune databases. Then it will aggregate and
# impute responses in the normal way.
#
# When it is complete, the "rserve_data" folder you mounted should have .rds
# files in it that have been recently modified. See comments in
# rserve/scripts/copilot/saturn_metascript.R and create_report.R for where
# .rds files can be loaded. These allow analysts to jump to a specific line of
# code with all the necessary variables defined in order to debug.
#
# ## Configuration
#
# If the `save_workspace_only` parameter is TRUE, it will skip creating
# reports and just run the metascript. The goal is to quickly generate .rds
# files from the metascript so they can be used elsewhere, like a dashboard
# script.
#
# You can specify specific classrooms or teams, just like Copilot does, with
# the `reporting_units` parameter below.
#
# You can also specify the report date, which would be the Monday on which
# the report would be delivered, and which operates on the data recorded 7
# days prior.
#
# ## Line-by-line
#
# Follow these steps when transitioning to line-by-line execution.
#
# * clear your workspace to avoid conflicts
# * open rserve/scripts/copilot/saturn_metascript.R
# * change your working directory to rserve; from the analysis repo you
#   should be able to use `setwd('rserve')`
# * run all the importing and sourcing lines above the function
# * run the definitions having to do with paths to rds files (see
#   comments there)
# * run the appropriate `list2env` function to create your environment
# * check the setting `save_workspace_only`: TRUE if you want to skip reports
#   and just capture metascript calculations as .rds files, and FALSE
#   if you do want to create report-level data and plots.


if (!grepl('/analysis/?$', getwd())) {
  stop("Your working directory should be the root of the analysis repo.")
}

gymnast_base_path <- ifelse(
  dir.exists('../gymnast'),
  '../gymnast/R/',
  'https://raw.githubusercontent.com/PERTS/gymnast/master/R/'
)
gymnast <- new.env()
source(paste0(gymnast_base_path, 'util.R'), local = gymnast)

analysis_repo_wd <- getwd()

# Load RServe's dependencies.
# N.B. working directory changes temporarily.
setwd(paste0(getwd(), "/rserve"))
gymnast$ensure_packages('jsonlite')
deps <- unlist(jsonlite::read_json('package.json')$rserve$dependencies)
gymnast$ensure_packages(deps)

# Import RServe's modules. Note that some must be copied from
# analysis/common as they would be when RServe is built by Docker.
dir.create('common', showWarnings = FALSE)
file.copy(list.files("../common", full.names = TRUE), "common", overwrite = TRUE)
beleset_script <- modules::use('scripts/beleset.R')
cset_script <- modules::use('scripts/cset.R')
import_data <- modules::use('modules/import_data.R')$import_data
saturn <- modules::use('common/saturn.R')
# N.B. restoring working directory.
setwd(analysis_repo_wd)

paths <- list(creds = 'rserve_credential_body.json')
creds_json <- gymnast$util.find_crypt_paths(paths)$creds
creds <- jsonlite::fromJSON(creds_json)

# For report debugging, in "specified reporting units" mode. Note there is also
# "run_program" mode, which is used by the dashboard, and runs all teams,
# classrooms, and organizations for a given progrram.

# Choose one of the applicable programs to uncomment:
report_data_list <- cset_script$main(
  #report_data_list <- beleset_script$main(
  auth_header = NA,
  # get a real auth_header here (chris has to authorize your google)
  # https://copilot.perts.net/cron/rserve/reports/ep?really_send=false
  # it's after the word "authorization" and starts in the string "Bearer ",
  # and the "Bearer " (with a space) is part of it.
  # it's what's enclosed in the next pair of double quotes after authorization.
  # it's a super long string don't worry about it (Chris says it's opaque)
  platform_data = import_data(
    # For production data:
    neptune_sql_credentials = creds$neptune_sql_credentials,
    triton_sql_credentials = creds$triton_sql_credentials

    # # For local data:
    # neptune_sql_credentials = list(),
    # neptune_sql_ip = '127.0.0.1',
    # neptune_sql_password = 'neptune',
    # neptune_sql_user = 'neptune',
    # triton_sql_credentials = list(),
    # triton_sql_ip = '127.0.0.1',
    # triton_sql_password = 'triton',
    # triton_sql_user = 'triton'
  ),
  qualtrics_service = NULL,
  # For production data:
  saturn_service = saturn$create_service(creds$saturn_sql_credentials),
  # For local data:
  # (not done yet)
  sheets_service = NULL,
  emailer = function(...){},
  script_params = list(
    save_workspace_only = FALSE,
    # Analysts may configure the run here.
    report_date = '2020-02-24',  # or else it will assume next monday
    reporting_units = list(
      # Example of specifying a classroom report to run.
      # N.B. Script does NOT check if the team and classroom actually match.
      list(
        id = 'Classroom_FkeBU2FUEZ1743pS',
        classroom_id = 'Classroom_FkeBU2FUEZ1743pS',
        team_id = 'Team_Umz4ZP6X5Moqwqvs',
        organization_id = NULL,
        post_report_url = "https://copilot.perts.net/api/reports",
        post_url = "https://neptune.perts.net/api/datasets?parent_id=" %+%
          "Classroom_FkeBU2FUEZ1743pS"
      )
    )
  ),
  should_post = FALSE
)
