
read_all_qualtrics <- function(complete_file_name, partial_file_name){

  complete_raw <- util.find_crypt_paths(list(f1 = complete_file_name)) %>%
    unlist %>%
    read.csv %>%
    qc.clean_qualtrics() # note you will get a warning about duplicate columns, see (1) above

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
    dplyr::select(one_of(index_cols)) %>%
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

rename_pdds <- function(in_df, pdd_pattern) {
  # in some cases, a raw qualtrics file could contain duplicated pdd tags
  # this function takes the raw qualtrics file, looks for a particular pdd patern
  # (you can get it from running qc.clean_qualtrics() first), and adds a
  # suffix to the pdd tag, so the tags will not be duplicated anymore
  # The pdd tags are always in the second row. The suffix will come from the
  # col name. For example  pdd__t1__demog__race__pdd for Q62_1 will become
  # pdd__t1__demog__race_Q62_1__pdd

  # Example call df1 <- rename_pdds(in_df, "pdd__t1__demog__race__pdd" )


  for (colname in colnames(in_df)) {
    hit <- grepl(pdd_pattern, in_df[1,colname])
    if (hit) {
      new_pdd_pattern <- paste("_", colname, "__pdd", sep = "")
      new_pdd <- gsub("__pdd", new_pdd_pattern, pdd_pattern)
      new_pattern <- gsub(pdd_pattern, new_pdd, in_df[1,colname])
      in_df[1,colname] <- new_pattern
    }
  }
  return(in_df)
}

alternative_or <- function(x,y) {
  # the normal or "|" returns NA when x is NA and y is FALSE
  # the current or will return FALSE in such cases
  if (!is.logical(x) | !is.logical(x)) {
    stop("Nonlogical vectors provided to alternative_or.")
  }
  if (length(x) != length(y)) {
    stop("Vectors of different lengths provided to alternative_or.")
  }
  x_or_y <- x | y
  at_least_one_false <- !(x) | !(y)
  x_or_y[is.na(x_or_y) & at_least_one_false ] <- FALSE
  return(x_or_y)

}


replace_missing <- function(in_df, shared_col, cols_to_fill_in, diverse_vals = FALSE) {
  # if there is deterministic dependecy between columns (e.g. the value
  # when the value of col1 is "white", the value of col2 is always "race")
  # we can use the values from the first column to fill in missing values in
  # the second

  # the assumption is that the rows either have missing values only in
  # cols_to_fill_in, or no missing values at all (in the same col)

  # if diverse_vals is set to TRUE, cols_to_fill_in might have different values (
  # instead of repeating the same value for each row)

  #Example call
  #grid_df <- replace_missing(
  #  in_df = grid_df,
  #  shared_col = "metric",
  #  cols_to_fill_in = c("question_with_dates_wrapped", "question_text")
  #)



  check_nrow <- nrow(in_df)
  in_df$rowindex <- as.numeric(rownames(in_df))
  # split in two dfs, one with missing values, the other one with
  # present values
  all_nas <- data.frame(in_df[,cols_to_fill_in]) %>%
    apply(., 1, function(x) all(is.na(x)))


  missing_df <- in_df[all_nas,]
  present_df <- in_df[!all_nas,]

  # create df which will have the missing information
  information_df <- present_df[,c(shared_col, cols_to_fill_in)]
  information_df <- information_df[!duplicated(information_df),]

  # if the rows did not have the same information, as expected, stop
  # the script
  test_length <- information_df[,shared_col] %>% duplicated %>% sum
  if (test_length > 0 & !diverse_vals) {
    stop("Error in the function replace_missing. The shared_col does not define unique rows, for the same value of shared_col, there are different values of cols_to_fill_in.")
  }



  missing_df[,cols_to_fill_in] <- NULL

  missing_df <- merge(
    missing_df,
    information_df,
    by = shared_col,
    all.x = TRUE,
    all.y = FALSE
  )
  in_df <- rbind(missing_df, present_df)
  if( nrow(in_df) != check_nrow) {
    stop("Error in the function replace_missing. Different number of rows for input and output!")
  }
  in_df <- in_df %>% arrange(rowindex)
  in_df$rowindex <- NULL
  return (in_df)
}

compute_multiple_races <- function(race_vect, criteria_vect) {
  # race vector is a vector with coded races. For example, subject one might hav
  # 3 codes such as "1,2,5". The criteria_vector describes those categories for
  # which we want a return a hit (meaning that it is present for that subject)
  #call: compute_multiple_races(s1_rn$t1__demog__race_1_to_18, criteria_vect)
  #race_vect <- s1_rn$t1__demog__race_1_to_18
  #creteria_vect <- criteria_vect
  output_vect <- rep(NA, length(race_vect))
  for (j in 1:length(race_vect)){
    curren_row <- strsplit(race_vect[j], ",") %>% unlist %>%  as.numeric
    output_vect[j] <- any(criteria_vect %in%  curren_row)
  }
  return(output_vect)
}

print_perc <- function(x){
  x <- x %>%  '*'(100.0) %>% round(., 0) %>% paste(.,"%", sep = "")
  return(x)
}
bold.somerows <-
  function(x) gsub('BOLD_(.*)',paste('\\<b>\\1','</b>'),x)



replace_missing <- function(in_df, shared_col, cols_to_fill_in, diverse_vals = FALSE) {
  # if there is deterministic dependecy between columns (e.g. the value
  # when the value of col1 is "white", the value of col2 is always "race")
  # we can use the values from the first column to fill in missing values in
  # the second

  # the assumption is that the rows either have missing values only in
  # cols_to_fill_in, or no missing values at all (in the same col)

  # if diverse_vals is set to TRUE, cols_to_fill_in might have different values (
  # instead of repeating the same value for each row)

  #Example call
  #grid_df <- replace_missing(
  #  in_df = grid_df,
  #  shared_col = "metric",
  #  cols_to_fill_in = c("question_with_dates_wrapped", "question_text")
  #)



  check_nrow <- nrow(in_df)
  in_df$rowindex <- as.numeric(rownames(in_df))
  # split in two dfs, one with missing values, the other one with
  # present values
  all_nas <- data.frame(in_df[,cols_to_fill_in]) %>%
    apply(., 1, function(x) all(is.na(x)))


  missing_df <- in_df[all_nas,]
  present_df <- in_df[!all_nas,]

  # create df which will have the missing information
  information_df <- present_df[,c(shared_col, cols_to_fill_in)]
  information_df <- information_df[!duplicated(information_df),]

  # if the rows did not have the same information, as expected, stop
  # the script
  test_length <- information_df[,shared_col] %>% duplicated %>% sum
  if (test_length > 0 & !diverse_vals) {
    stop("Error in the function replace_missing. The shared_col does not define unique rows, for the same value of shared_col, there are different values of cols_to_fill_in.")
  }



  missing_df[,cols_to_fill_in] <- NULL

  missing_df <- merge(
    missing_df,
    information_df,
    by = shared_col,
    all.x = TRUE,
    all.y = FALSE
  )
  in_df <- rbind(missing_df, present_df)
  if( nrow(in_df) != check_nrow) {
    stop("Error in the function replace_missing. Different number of rows for input and output!")
  }
  in_df <- in_df %>% arrange(rowindex)
  in_df$rowindex <- NULL
  return (in_df)
}


read_school_name_json <- function(x) {
  out_vect <- c()
  for (j in 1:length(x)) {
    tryCatch({
      json_x <- fromJSON(txt=x[j])
      school_name <- json_x$school_name

    }, error = function(e){
      school_name <- NA
    })
    if (is.na(x)){
      school_name <- NA
    }
    if (is.null(school_name)){
      school_name <- NA
    }
    out_vect <- c(out_vect, school_name)
  }
  return(out_vect)
}

