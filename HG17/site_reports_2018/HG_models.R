run_hg_models <- function(nested_df){
  # nested_pc_df is a data.frame that's indexed by project_cohort_id that can
  # serve as the basis for a tidyr model data.frame.
  # this function returns a nested data.frame with the same number of rows and
  # same project_cohort_ids as the nested_df input, but which contains fitted models.
  # (tidyr style)

  if(!"pc_being_tested" %in% names(nested_df)){
    stop("In run_hg_models, nested_df must contain a column labeled pc_being_tested")
  }

  if(!length(unique(nested_df$pc_being_tested)) == nrow(nested_df)){
    stop("In run_hg_models, pc_being_tested must contain unique values.")
  }

  glmer_custom <- function(df){
    possible_error <- tryCatch(
      {
        message("computing model") # to track progress
        glmer(value ~ session * is_pc + (1 | participant_id),
              data = df, family = "binomial")
      },
      error = function(e){
        paste0("Error in model: ", e) %>%
          message
      }
    )
  }

  # this model_df will have null values where there are supposed to be models,
  # in the case where computing models generates errors.
  model_df <- nested_df %>%
    mutate(
      model = map(data, glmer_custom)
    )

  tidy_df <- model_df %>%
    mutate(
      tidy = map(model, tidy)#,
      #         n_s1 = unlist(map(data, function(df) nrow(df[df$session %in% "s1" & df$is_pc %in% 1, ]))),
      #         n_s2 = unlist(map(data, function(df) nrow(df[df$session %in% "s2" & df$is_pc %in% 1, ]))),
      #         prop_gms_s1 = unlist(map(data, function(df) mean(df$value[df$session %in% "s1" & df$is_pc %in% 1]))),
      #         prop_gms_s2 = unlist(map(data, function(df) mean(df$value[df$session %in% "s2" & df$is_pc %in% 1])))
    )

  # can I do this all in one step?
  SIG_THRESHOLD <- .05/nrow(nested_df)

  # this df will now hae one row per combination of pc and model term. We filter
  # out some of the model terms (the random effects and intercept) to keep only
  # the non-intercept fixed effect coefficients.
  unnested_df_converged_models <- tidy_df %>%
    select(pc_being_tested, tidy
           #, n_s1, n_s2, prop_gms_s1, prop_gms_s2
    ) %>%
    unnest(cols = tidy) %>%
    dplyr::filter(
      group %in% "fixed",
      !term %in% "(Intercept)"
    ) %>%
    group_by(pc_being_tested) %>%
    mutate(
      sig_interaction = any(term %in% "sessions2:is_pc" & p.value < SIG_THRESHOLD),
      worse_effect = any(term %in% "sessions2:is_pc" & estimate < 0)
    )


  # so now we're going to create stats for the broken glms, and rbind them to the other df
  ##### Now we will handle the broken models by trying a glm function instead of glmer
  # note that we have to use an lmer because glm is too anti-conservative and makes
  # interactions look signficant when they're probably not. This is especially bad because
  # the p-values indicate something bad (deviance from overall trend)
  lmer_custom <- function(df){
    possible_error <- tryCatch(
      {
        message("computing model") # to track progress
        lmer(value ~ session * is_pc + (1 | participant_id),
             data = df)
      },
      error = function(e){
        paste0("Error in model: ", e) %>%
          message
      }
    )
  }

  broken_model_row_numbers <- lapply(model_df$model, is.null) %>% unlist %>% which()
  broken_model_pc_ids <- model_df$pc_being_tested[broken_model_row_numbers]

  broken_model_df <- nested_df[broken_model_row_numbers, ] %>%
    mutate(
      model = map(data, lmer_custom)
    )

  broken_tidy_df <- broken_model_df %>%
    mutate(
      tidy = map(model, tidy)
    )

  # for this, we'll use a p-value corresponding to the limit of t for the adjusted probability
  # in `SIG_THRESHOLD`. Assume an n of 100,000 to get an asymptotic crytical value.
  # I looked this up on the Internet.
  t_threshold <- 3.03

  # this df will now hae one row per combination of pc and model term. We filter
  # out some of the model terms (the random effects and intercept) to keep only
  # the non-intercept fixed effect coefficients.
  broken_unnested_df <- broken_tidy_df %>%
    select(pc_being_tested, tidy) %>%
    unnest(cols = tidy) %>%
    dplyr::filter(
      group %in% "fixed",
      !term %in% "(Intercept)"
    ) %>%
    group_by(pc_being_tested) %>%
    mutate(
      sig_interaction = any(term %in% "sessions2:is_pc" & estimate > t_threshold),
      worse_effect = any(term %in% "sessions2:is_pc" & estimate < 0)
    )

  unnested_df <- util.rbind_union(list(unnested_df_converged_models, broken_unnested_df))
}