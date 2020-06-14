# API for report.perts.net

library(httr)
library(digest)
library(RJSONIO)


## Utility functions and global parameters:

report_api.SALT <- "0IUvj0ZECtVhQNGSMesT"
report_api.API_KEY <- "6Nzx4OH9AB1diSYD9udz"
report_api.DOMAIN <- "http://perts-report.appspot.com"

report_api.get_dataset_url <- function(template_name, entity_token) {
    url <- paste0(report_api.DOMAIN, "/dataset/", template_name, "/",
                  entity_token)
    return(url)
}

report_api.get_entity_token <- function(id) {
    salted <- paste(id, report_api.SALT, sep = "")
    # Without serialize=FALSE, output is different (wrong). Don't know why.
    # http://cran.r-project.org/web/packages/digest/digest.pdf
    token <- digest(salted, algo = "sha256", serialize = FALSE)
    return(token)
}

## Structuring data

# Download/upload data as lists with named components.

# # Creating
# l <- list(school_name = "Wabonsie Valley", num_students = 1942)

# # Accessing with literal
# l$num_students  # [1] 1942

# # Accessing with variable
# my_field <- "num_students"
# l[[my_field]]  # [1] 1942

## Upload: report_api.put_dataset()

report_api.put_dataset <- function(template_name, entity_token, data) {
    url <- report_api.get_dataset_url(template_name, entity_token)

    # The httr package use jsonlite, which translates all atomic vectors
    # into json lists, so `[1] 5` in R becomes "[5]" in json, but we want just
    # "5". So use RJSONIO as a stand-in because it correctly "unboxes" atomic
    # vectors.
    json_data <- RJSONIO::toJSON(data, nullValue = NA)

    # The body must be preceded by an API key.
    body <- report_api.API_KEY %+% json_data

    put_response <- PUT(url, body = body,
                        add_headers("Content-Type" = 'application/json'))
    return(status_code(put_response))  # Should be 204: No Content
}

# # Example use
# template_name <- "sample"  # Name of the kind of report the data is for.
# entity_token <- report_api.get_entity_token("School_XYZ")  # Use the id of the entity here.
# data <- list(putting = "data", from = "R")
# status_code <- report_api.put_dataset(template_name, entity_token, data)
# 
# print(paste(status_code, "'No Content', indicates successful save."))

## Download: report_api.get_dataset()

report_api.get_dataset <- function(template_name, entity_token) {
    url <- report_api.get_dataset_url(template_name, entity_token)
    dataset_response <- GET(url)
    return(content(dataset_response))
}

# # Example use
# template_name <- "sample"  # Name of the kind of report the data is for.
# entity_token <- report_api.get_entity_token("School_XYZ")  # Use the id of the entity here.
# dataset <- report_api.get_dataset(template_name, entity_token)
#
# print(dataset)
