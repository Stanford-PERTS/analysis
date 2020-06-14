library(purrr)

impute <- function(x, y){
  if(!is.na(x) & is.na(y)){
    y <- x
  }
  return(y)
}

is_imputed <- function(x, y){
  if(!is.na(x) & is.na(y)){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

for(col in column_names){
  input_df[[col %+% "_imputed"]] <- accumulate(input_df[[col]], .f = impute)
  input_df[[col %+% "_is_imputed"]] <- accumulate(input_df[[col]], .f = is_imputed)
}


is_imputed(3,NA)

col_names_1 <- names(input_df)[grep("fg1_", names(input_df))]
col_names_1 <- c("id_col_comp",col_names_1)
input_df[, col_names_1]  %>% as.data.frame %>% View
