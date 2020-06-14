# This file uses RServe's "metascript" to generate various useful data frames
# that would normally be used to create reports in Copilot. Here we use the
# `save_workspace_only` parameter to skip all the report rendering and just
# save the useful data to .rds files in a mounted crypt.
#
# Analysts can follow up with this by running the metascript
# (rserve/scripts/copilot/metascript.R) line
# by line, restoring the captured environment from .rds files as they like,
# rather than running the whole thing from the beginning each time.
#
# ## Requirements
#
# * perts_crypt.vc mounted
# * another "workspace" crypt mounted, containing this folder structure:
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
# Qualtrics and the Triton and Neptune databases. Then it will aggregate and
# impute responses in the normal way. It will skip all unnecessary functions
# like creating reports, saving summaries to google sheets, or sending emails.
#
# When it is complete, the "rserve_data" folder you mounted should have .rds
# files in it that have been recently modified. See comments in
# rserve/scripts/copilot/metascript.R and create_report.R for where .rds files
# can be loaded.
#
# ## Configuration
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
# * open rserve/scripts/copilot/metascript.R
# * change your working directory to rserve; from the analysis repo you
#   should be able to use `setwd('rserve')`
# * run all the importing and sourcing lines above the function
# * run the definitions having to do with paths to rds files (see
#   comments there)
# * run the appropriate `list2env` function to create your environment
# * consider setting `save_workspace_only` to TRUE if you don't want
#   to shortcut rendering ggplot stuff.

# setwd("~/Sites/analysis/")

gymnast <- new.env()
source(
  "https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_legacy.R",
  local = gymnast
)

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
ep_script <- modules::use('scripts/ep.R')
import_data <- modules::use('modules/import_data.R')$import_data
qualtrics <- modules::use('common/qualtrics.R')
# N.B. restoring working directory.
setwd(analysis_repo_wd)

paths <- list(creds = 'rserve_credential_body.json')
creds_json <- gymnast$util.find_crypt_paths(paths)$creds
creds <- jsonlite::fromJSON(creds_json)

# For report debugging, in "specified reporting units" mode
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
    save_workspace_only = FALSE,
    # Analysts may configure the run here.
    report_date = '2019-11-04',  # or else it will assume next monday
    reporting_units = list(
      # Example of specifying a team report to run.
      list(
        id = 'Team_n2ZmivErsicDZHdR',
        classroom_id = NULL,
        team_id = 'Team_n2ZmivErsicDZHdR'
      ),
      # Example of specifying a classroom report to run.
      # N.B. Script does NOT check if the team and classroom actually match.
      list(
        id = 'Organization_8dzLI4pLAdvqxR77',
        organization_id = 'Organization_8dzLI4pLAdvqxR77'
        # classroom_id = 'Classroom_cIjCtITtFql6Ouw5',
        # team_id = 'Team_1QH8sWzPsSAsdF7Q'
      )
    )
  ),
  should_post = FALSE
)
