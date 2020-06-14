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