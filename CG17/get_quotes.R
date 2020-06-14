source("~/Sites/gymnast/R/util.R")
source("~/Sites/gymnast/R/util_qualtrics_cleaning.R")

READ_DATE <- "2019-10-14"
s_raw_paths <- util.find_crypt_paths(
    list(
        # survey data come from Qualtrics. You have to update the year values each Fall.
        complete = "CG19_OFFICIAL_1_" %+% READ_DATE %+% ".csv"
    )
)
    
d0 <- s_raw_paths$complete %>% read.csv() %>% qc.clean_qualtrics()

select_n_elements <- function(x, n){
    # takes a vector and returns a "selected: vector of the same original length where n elements are TRUE and the rest false
    
    if(n > length(x)) {
        selected <- rep(TRUE, length(x))
    } else{
        selected <- rep(FALSE, length(x))
        selected[sample(x, n)] <- TRUE
    }
    return(selected)
}


d1 <- d0 %>%
    # filter out short responses
    mutate(
        nchars = nchar(Q4.51),
        row_number_index = 1:nrow(d0)
        ) %>%
    dplyr::filter(nchars > 100) %>% 
    # select relevant 20 items from each org
    group_by(organization_id) %>%
    mutate(
        selected = select_n_elements(1:n(), 7)
    ) %>%
    dplyr::filter(selected) %>%
    select(organization_id, row_number_index, survey_id, ResponseID, Q4.51) %>%
    rename(value = Q4.51)

d1$value_trimmed <- gsub("\\/", "", d1$value)
d1 <- select(d1, -value) %>% rename(value = value_trimmed)

write.csv(d1, "/Volumes/NO NAME/raw_quotes.csv", row.names = FALSE)
        
    