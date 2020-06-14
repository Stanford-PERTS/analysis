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

revised_ht <- theme(    #   remove the gray background
    panel.background    = element_blank() ,
    #   make the major gridlines light gray and thin
    panel.grid.major.y  = element_line( size=ug.line_size, colour="black" ) ,
    #   suppress the vertical grid lines
    panel.grid.major.x  = element_blank() ,
    #   suppress the minor grid lines
    panel.grid.minor    = element_blank() ,
    #   adjust the axis ticks
    axis.ticks    = element_line( size=ug.line_size , colour="black" ),
    #   move the y-axis over to the left
    axis.title.x  = element_text( vjust=-.5 , color="black", angle=0, size=ug.text_size ) ,
    axis.text     = element_text( vjust=0 , color="black", angle=0, size=ug.text_size ) ,
    axis.title.y 	= element_text( vjust=.2 , color="black", angle=90, size=ug.text_size ) ,
    plot.title		= element_text( color="black", angle=0, size=12 )
)

get_lmer_stats <- function(in_df, dv_var, iv_var, nesting_var = "survey_id") {
  # uses lmer, nesting variable is always organization
  # I will run random intercept only
  # Example call: get_lmer_stats(in_df = data_wide, dv_var = "gms", iv_var = "post_treatment")
  in_df$dv <- in_df[[dv_var]]
  in_df$iv <- in_df[[iv_var]]
  in_df$org_var <- in_df[[nesting_var]]
  smr <- lmerTest::lmer(dv ~ iv + (1|org_var), data=in_df) %>% summary
  output_df = list(
    dv_var = dv_var,
    est = smr$coefficients[2,1] %>% round(.,2),
    std_err = smr$coefficients[2,2] %>% round(.,2),
    df = smr$coefficients[2,3] %>% round(.,0),
    t = smr$coefficients[2,4] %>% round(.,2),
    p = smr$coefficients[2,5] %>% round(.,3)
  )
  if (output_df$p == 0) output_df$p <- "<.001"
  return(output_df)
}

get_perc_good <- function(in_vec, min_good, max_good) {
  # returns perc_good stats for two groups
  # for this projects, higher numbers are always better, so max_good should be a max
  tryCatch({
      if (max(in_vec, na.rm = TRUE) > max_good) {stop("max_good is not a maximum")}
      count_good <- (in_vec >= min_good ) %>% sum(., na.rm = TRUE)
      count_non_nas <- sum(!is.na(in_vec))
      perc_good <- ((count_good/count_non_nas)*100) %>% round(.,2)
      return_list = list(count_good = count_good,
                         perc_good = perc_good)

    }, error = function(e) {
      return_list = list(count_good = NA,
                         perc_good = NA)
    }
  )

  return(return_list)
}
p_to_text <- function(p_val, threshold = .001) {
  # takes numeric p_value, if less than threshold returns text
  if (util.is_blank(p_val) )return(NA)
  if (p_val < threshold) {
    p_val <- paste0("< ",threshold)
  } else {
    p_val <- paste0("= ",round(p_val, 3))
  }
  return(p_val)
}
p_to_text(0.01)


# make sure this will work even for schools with messed up participation codes / cohorts
sample_quotes <- function(in_df,
                          quote_col,
                          org_id,
                          max_quotes = 21,
                          min_quotes = 14,
                          min_len = 150 ) {
  # takes a sample of quotes from each organization
  # if the largest possible sample is smaller than a threshold,
  # no quotes are provided
  # Example call: quotes_df <- sample_quotes(d_initial, "Q4.51", "organization_id")
  #in_df <-  d_initial
  #quote_col = "Q4.51"
  #org_id = "organization_id"

  in_df$quotes <- in_df[[quote_col]]
  in_df$org_id <- in_df[[org_id]]

  # randomize order
  set.seed(1)
  in_df <- in_df[sample(1:nrow(in_df), replace = FALSE), ]

  quotes_df <- in_df %>%
    dplyr::filter(nchar(quotes) > min_len)%>%
    select(org_id, quotes) %>%
    group_by(org_id) %>%
    mutate(row_numb = row_number()) %>%
    mutate(max_row_numb = max(row_numb)) %>%
    as.data.frame() %>%
    dplyr::filter(row_numb <= max_quotes) %>%
    dplyr::filter(max_row_numb >= min_quotes)
  quotes_df$included <- 0
  quotes_df <- quotes_df %>%
    dplyr::rename(organization_id = org_id) %>%
    arrange(organization_id)
  return(quotes_df)
}


