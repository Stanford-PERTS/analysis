 
read_all_qualtrics <- function(complete_file_name, partial_file_name){
    
    complete_raw <- util.find_crypt_paths(list(f1 = complete_file_name)) %>%
        unlist %>%
        read.csv %>%
        qc.clean_qualtrics() %>%
        mutate(in_progress = NA) # note you will get a warning about duplicate columns, see (1) above
    
    partial_raw <- util.find_crypt_paths(list(p1 = partial_file_name)) %>%
        unlist %>%
        read.csv
    
    raw <- qc.rbind_inprogress(partial_raw, complete_raw)
    return(raw)
}

mark_qualtrics_dups <- function(clean_qdf, id_vars, min_secs = 120){
    # 1. Mark all rows where time on task < min_secs (remove_too_short)
    # 2. Identify id_vars that are still duplicated
    # 3. Mark all records corresponding to the still-duplicated IDs for removal (remove_duplicated)
    clean_qdf %>%
        mutate(remove_too_quick = difftime(EndDate, StartDate) < min_secs) %>%
        group_by_(.dots = id_vars) %>%
        mutate(
            id_instances = n(),
            remove_duplicated = id_instances > 1
        )
}

check_index_is_unique <- function(df, index_cols = NULL){
    # Checks whether the vector of column names index_cols define 
    # unique rows in df by comparing the number of rows in the whole df
    # to the number of rows defined by just the unique combinations of the 
    # index_cols. Returns boolean if throw_error is FALSE, otherwise simply
    # throws an error if the check fails.
    
    # if no index cols are specified, then check whether the whole df is unique
    if(is.null(index_cols)){
        index_cols <- names(df)
    }
    
    # Throw an error if df is missing any index_cols
    if(any(!index_cols %in% names(df))){
        stop("in check_index_is_unique, one of your index cols does not exist in df.")
    }
    
    nrows_index_cols <- df %>%
        select(one_of(index_cols)) %>%
        unique %>%
        nrow
    
    nrows_whole_df <- nrow(df)
    
    return(nrows_index_cols == nrows_whole_df)
}

print_item_text <- function(scale, scale_items_df){
    item_text <- scale_items_df[scale_items_df$scale %in% scale, "question_text"]
    paste0(item_text, collapse = "<br>")
}

in_good_range <- function(x, min_good, max_good){
    ifelse(x >= min_good & x <= max_good, 1, 0)
}

get_good_and_bad_responses <- function(response_options_string, min_good, max_good, delimiter = "; "){
    # takes a string of response options separated by a delimiter, splits them into a vector, 
    # and returns the elements of the vector that fall ordinally within min_range:max_range
    # (useful for extracting "good-range" and not "good-range" question choices)
    response_options <- strsplit(response_options_string, delimiter)[[1]] %>%
        util.trim
    good_response_options <- response_options[min_good:max_good]
    bad_response_options <- setdiff(response_options, good_response_options)
    good_and_bad_responses <- list(good = good_response_options, bad = bad_response_options)
    return(good_and_bad_responses)
}

#### copied from engagement_helpers.R

wrap_text <- function(text, width=35) {
    wtext <- paste( strwrap(text,width=width), collapse=" \n ")
    return(wtext)
}

p_aov <- function(dv, iv){
    # provided an iv and dv, return the omnibus p from an anova
    tryCatch({
        summary(aov(dv ~ iv))[[1]][["Pr(>F)"]][1]    
    }, error = function(e){
        return(1)
    })
}

se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

sentence_case <- function(str){
    # underscores to spaces
    # first letter capitalized
    str_spaces <- str %>%
        gsub("_"," ", .) %>%
        util.trim()
    substr(str_spaces,0,1) <- substr(str_spaces,0,1) %>%
        toupper()
    return(str_spaces)
}


mkdn_friendly_html <- function(str){
    # regex removes spaces after carriage returns 
    # (if a line starts with spaces, markdown renders as code block)  
    gsub("[\r\n]+ *", "", str)
}