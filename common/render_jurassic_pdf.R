render_jurassic_pdf <- function(
    html_report_path,
    pdf_generator_path = "~/Sites/pdf-generator/",
    production = FALSE,
    new_pdf_path = NULL
){
    orig_wd <- getwd()
    # pdf_generator_path is the path to the pdf generator repo (usually )
    tryCatch({
        #set working directory to pdf-generator repo so you can reach the .py script
        setwd(pdf_generator_path)
        
        #clean inbox from existing files with the same name
        # grep out the report name from the report path
        report_name <- gsub("([A-Za-z0-9/_~-]+\\/)([A-Za-z0-9_-]+\\.html$)", "\\2", html_report_path)
        report_base_path <- gsub("([A-Za-z0-9/_~]+\\/)([A-Za-z0-9_-]+\\.html)", "\\1", html_report_path)
        if(!dir.exists(report_base_path)){
            stop("render_jurassic_perts incorrectly identified the parent directory for the " %+%
                     "html file you are trying to render. " %+%
                     "html file path is " %+% html_report_path %+% ", incorrect parent directory " %+%
                     "is " %+% report_base_path)
        }
        file_name_basic <- gsub("\\.html", "", report_name)
        # account for possible white spaces in the file name
        file_name_basic <- gsub(" ", "\\\\ ",file_name_basic)
        # file path to pdf-generator inbox
        file_path_in <- paste0(pdf_generator_path, "inbox/", file_name_basic, ".html")
        if (file.exists(file_path_in)){
            paste0("rm ", file_path_in) %>% system()
        }
        #clean outbox from existing files with the same name
        file_path_out <- paste0(pdf_generator_path, "outbox/", file_name_basic, ".pdf")
        if (file.exists(file_path_out)){
            paste0("rm ", file_path_out) %>% system()
        }
        
        #copy html output to inbox
        report_path_escaped <- gsub(" ", "\\\\ ",report_path)
        paste0("cp ", report_path_escaped, " ", pdf_generator_path %+% "inbox") %>%
            system()
        
        if (production) {
            paste0("/usr/local/bin/python ",
                   pdf_generator_path,
                   "generate.py ",
                   "--production") %>%
                system()
        } else {
            paste0("/usr/local/bin/python ",
                   pdf_generator_path,
                   "generate.py ") %>%
                system()
        }
        
        if(is.null(new_pdf_path)){
            new_pdf_path <- report_base_path
        }
        
        #copy pdf from outbox to original place
        paste0("cp ", file_path_out, " ", new_pdf_path) %>%
            system()
        #clean outbox
        paste0("rm ", file_path_out) %>% system()
        # clean inbox
        paste0("rm ", file_path_in)  %>% system()
        
    },
    error = function(e){
        paste0("Error during PDF conversion.", e) %>%
            message
    }
)
    #return to the original working directory
    setwd(orig_wd)

}