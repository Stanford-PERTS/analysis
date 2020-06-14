# this file contains the functions uses in the k-anonymization script for CB17

create_local_order <- function(
  in_df,
  category = "category",
  local_order = "local_order",
  global_order = "global_order")
{
  # creates a local order variable within, based on global order, where
  # local order is specific to a category
  # if the global variable for x is 12,15,21 then the local order is  1,2,3
  in_df$index <- row.names(in_df) %>% as.numeric

  in_df <- in_df   %>%
    arrange(category, global_order) %>%
    group_by_(category) %>%
    mutate(loc_ord = row_number()) %>%
    as.data.frame() %>%
    arrange(index) %>%
    select(-index) %>%
    rename("local_order" = loc_ord)

  in_df[is.na(in_df[, global_order]), local_order] <- NA
  return(in_df)
}

clean_NAs <- function(in_vect){
  #removes NA from a vectors
  in_vect <- in_vect[!is.na(in_vect)]
}

reorder_cols <- function(in_df, ordered_cols) {
  # orders alphabetically only some of the cols in data_frame
  unordered_cols <- setdiff(names(in_df), ordered_cols)
  ordered_col_df <- in_df[,ordered_cols] %>%
    as.data.frame() %>%
    setNames(ordered_cols)
  ordered_col_df <-  ordered_col_df[ , order(names(ordered_col_df))]
  unordered_col_df <- in_df[,unordered_cols] %>%
    as.data.frame() %>%
    setNames(unordered_cols)
  in_df <-  cbind(unordered_col_df, ordered_col_df)
  return(in_df)
}

created_indexed_categories <- function(in_df, tree_specs_df, key_vars) {
  # wrap up function for creating and populating categories with different resolution
  # (different number of levels)
  # in_df - the input data frame, which we will anonymize
  # key_vars - these are the predictors we want to anonymize for (e.g. gender, race, etc.)
  # tree_specs_df - strictly structured data frame, and we expect that it will
  # have all appropriate columns (this is checked earlier in the script)

  # create new variables, the same number as the merges
  # the index will go from high to low, indicating the number of levels
  #indexed_cat_name <- unique(tree_specs_df$category_indexed[!is.na(unique(tree_specs_df$category_indexed))])
  indexed_cat_name <- tree_specs_df$category_indexed %>% clean_NAs
  in_df[,indexed_cat_name] <- NA



  # use codes instead of labels, this helps to trace back errors and reduces
  # cognitive load (e.g. male, female becomes a, b)
  for (cat in key_vars) {
    old_vals <- tree_specs_df$category_level_label[tree_specs_df$category == cat]
    new_vals <- tree_specs_df$category_level_code[tree_specs_df$category == cat]
    in_df[,cat] <- in_df[,cat] %>% util.recode(., old_vals, new_vals)
  }

  for (cat in key_vars) {
    buff_df <- tree_specs_df[tree_specs_df$category == cat, ] %>%
      arrange(local_order)
    merge_order <- buff_df[ ,"local_order"] %>% sort
    in_df$current_cat <- in_df[,cat]
    category_vars <- buff_df[, "category_indexed"]
    category_vars <- category_vars[!is.na(category_vars)]

    for (i in merge_order) {
      if (i == 1) {
        in_df[,category_vars[i]] <- in_df[,"current_cat"]
      } else {
        in_df[,category_vars[i]] <- in_df[,category_vars[i-1]]
      }
      current_merge_level <- buff_df$category_level_code[buff_df$local_order %in% i]
      # above is the superordinate category
      levels_to_be_merged <- buff_df$category_level_code[buff_df$category_level_parent %in% current_merge_level]
      in_df[,category_vars[i]]  <- in_df[,category_vars[i]]  %>%
        util.recode (
          .,
          levels_to_be_merged,
          rep(current_merge_level, length(levels_to_be_merged))
        )
    }
  }

  in_df$current_cat <- NULL
  # add indexed categories which will be virtually unchanged original categories
  # with maximum resolution

  for (cat in key_vars) {
    current_max_resolution <- tree_specs_df$local_order[tree_specs_df$category %in% cat] %>%
      max(., na.rm = T) %>% '+'(.,1)
    new_name <- paste0(cat, "___", current_max_resolution)
    in_df[,new_name] <- in_df[,cat]
    indexed_cat_name <- c(indexed_cat_name, new_name)
  }

  # check if number of categories is correct for each level of resolution
  for (cat_ind in indexed_cat_name) {
    expected_n <- gsub("(^.+_)", "", cat_ind) %>% as.numeric()
    observed_n <- in_df[,cat_ind] %>% unique() %>% length()
    if (expected_n < observed_n) {
      print(in_df[,cat_ind] %>% unique())
      paste0("Script stopped working. Error in ", cat_ind,". Expected levels: ",
             expected_n, ", observed levels:", observed_n) %>%
        stop()
    } else {
      paste0(cat_ind, " passed n_levels test!\n") %>% cat()
    }
  }

  # switch back from codes to labels (e.g. a, b becomes male, female )
  cats_to_recode <- c(key_vars, indexed_cat_name)
  for (cat_ind in cats_to_recode) {
    orig_cat <- gsub("___.*", "", cat_ind)
    new_vals <- tree_specs_df$category_level_label[tree_specs_df$category == orig_cat] %>%
      unique
    old_vals <- tree_specs_df$category_level_code[tree_specs_df$category == orig_cat] %>%
      unique
    in_df[,cat_ind] <- in_df[,cat_ind] %>% util.recode(., old_vals, new_vals)
  }
  return(in_df)
}

compute_excluded_categories <- function(in_df,tree_specs_df, key_vars, min_cell, missing_value_code) {
  # computs which categories should be masked mased on the min_cell
  # tree_specs_df assumes that a number of columns are already present, which is checked elsewhere
  # indexed key vars are the different resolutions of the key vars
  control_masking_df <- tree_specs_df %>%
    arrange(global_order)
  control_masking_df <- control_masking_df[!is.na(control_masking_df$global_order),]
  control_masking_df[,key_vars] <- NA

  # initially set the categories in data to maximum resolution
  for (cat in key_vars) {
    buff_df <- control_masking_df[control_masking_df$category == cat,]
    highest_resolution_cat <-
      buff_df[buff_df$local_order == min(buff_df$local_order),"category_indexed_to_mask"]
    control_masking_df[,cat] <- highest_resolution_cat
  }

  #control_masking_df <- control_masking_df %>% arrange(global_order)
  steps <- control_masking_df$global_order
  steps <- steps[!is.na(steps)] %>% sort

  indexed_categories_to_mask <- c()

  for (step in steps) {
    current_category <- control_masking_df$category[step]
    current_indexed_category_to_mask <- control_masking_df$category_indexed_to_mask [step]
    current_categories <- control_masking_df[step, key_vars] %>% unlist%>% unname
    crosstab_df <- in_df %>%
      dplyr::group_by_(.dots = current_categories) %>%
      dplyr::summarise(count_ = n())
    # before checking for threshold violation, remove crosstabs with missing values,
    # since they cannot be used to infer identity (presumably)
    rows_with_nas <-  crosstab_df %>% apply(., 1, function(x) any(x %in% missing_value_code))
    crosstab_df <- crosstab_df[!rows_with_nas,]
    # check if min cell threshold is violated
    if(any(crosstab_df$count_ %in% 1:min_cell)) {
      indexed_categories_to_mask <- c(indexed_categories_to_mask, current_indexed_category_to_mask)
      control_masking_df[step:nrow(control_masking_df), current_category] <- control_masking_df[step, "category_indexed"]
    } else {
      control_masking_df[step:nrow(control_masking_df), key_vars] <-
        control_masking_df[step, key_vars]
      break
    }
  }
  return(indexed_categories_to_mask)
}

mask_demographics <- function(in_df, demogr_vars) {
  cross_tabbed_df <- in_df %>%
    group_by_(.dots = demogr_vars) %>%
    summarise(cell_count = n()) %>%
    as.data.frame()
  # remove rows containing NAs
  #print(demogr_vars)
  if (length(demogr_vars) > 1){
    na_rows <- cross_tabbed_df[,demogr_vars] %>%
      apply(., 1, function(x) any(is.na(x)))
  } else {
    na_rows <- is.na(cross_tabbed_df[,demogr_vars])
  }
  cross_tabbed_df <- cross_tabbed_df[!na_rows,]
  bad_range <- any(cross_tabbed_df$cell_count %in% c(1:4))
  if (!bad_range) {
    print(cross_tabbed_df)
    return(demogr_vars)
  } else {
    new_demogr_vars <- demogr_vars[-length(demogr_vars)]
    if (length(new_demogr_vars) > 0) {
      mask_demographics(in_df, new_demogr_vars)
    } else {
      return(NULL)
    }
  }
}

salt_n_hash <- function(x,salt){
  # a function which encrypts a vector of strings
  paste0(x,salt) %>%
    lapply(., digest, algo = "sha256") %>%
    unlist()
}