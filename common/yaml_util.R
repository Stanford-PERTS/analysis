modules::import('stats', 'setNames')
modules::import('jsonlite', 'toJSON')
modules::import('yaml', 'as.yaml')

yaml_quote <- function (s) {
  return(paste0(
    "'",
    gsub("'", "''", s),
    "'"
  ))
}

js_vars <- function (variables) {
  # Create javascript variable delcarations in a <script> tag,
  # as a correctly-escaped, single-quoted YAML string ideal
  # for YAML blocks in Rmd scripts.
  #
  # Args:
  #   variables: list, where element names are the javascript
  #     variable names, and the values will be converted to
  #     JSON strings, which are valid javascript literals.
  #
  # Returns: a YAML string of a script tag with js code.
  #
  # Example:
  #
  # script_str <- js_vars(list(
  #   foo = "Xel'naga",
  #   bar = c(1, 2)
  # ))
  #
  # Result is a string with this value, _including_ the quotes:
  #
  # '<script>var foo = ["Xel''naga"]; var bar = [1, 2];</script>'
  #
  # To be used in YAML blocks:
  #
  # ---
  # script_tag: `r script_str`
  # ---

  script_str <- ""
  for (n in names(variables)) {
    # Create a "var" declaration for each variable,
    # using the element name as the variable name.
    script_str <- paste0(
      script_str,
      "var ",
      n,
      " = ",
      toJSON(variables[[n]]),
      ";"
    )
  }
  script_str <- paste0("<script>", script_str, "</script>")

  # Escape the whole thing and add single quotes.
  return(yaml_quote(script_str))
}

df_to_rows <- function (df) {
  # Convert a data frame to a list of rows, where each element is a one-row
  # data frame. This is the typical way to represent a table in many other
  # formats, like JSON and YAML.
  #
  # Example:
  # df <- data.frame(a = c(1,2,3), b = c(4,5,6))
  # df_to_row_lists(df)
  # # list(`1` = data.frame(a=1, b=4), `2` = data.frame(a=2, b=5), `3` = data.frame(a=3, b=6))
  split(df, seq(nrow(df)))
}

df_to_yaml <- function (df) {
  # Convert a data frame to a yaml string.
  #
  # Example:
  # df <- data.frame(a = c(1,2,3), b = c(4,5,6))
  # cat(df_to_yaml(df))
  # # - a: 1
  # #   b: 4
  # # - a: 2
  # #   b: 5
  # # - a: 3
  # #   b: 6
  as.yaml(setNames(df_to_row_lists(df), NULL))
}
