if (!exists("gymnast_install")) {
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_legacy.R")
}

script_tag <-
  '<!-- Chunk Toggle Script -->' %+%
  '<script>' %+%
    'var inputCollection = document.getElementsByClassName("chunk-toggle");' %+%
    # A "collection" isn\'t a real array, so turn it into one.
    'var inputArray = Array.prototype.slice.call(inputCollection);' %+%
    'inputArray.forEach(function (input) {' %+%
      'var placeholder = input.parentNode.nextElementSibling;' %+%
      'var chunk = input.parentNode.parentNode.nextElementSibling;' %+%
      'chunk.style.display = "none";' %+%
      'input.onchange = function () {' %+%
        'var isDisplayed = chunk.style.display != "none";' %+%
        'chunk.style.display = isDisplayed ? "none" : "block";' %+%
        'placeholder.style.display = isDisplayed ? "block" : "none";' %+%
      '}' %+%
    '});' %+%
  '</script>'
checkbox_markup <- '<label style="font-weight: normal">' %+%
    '<input class="chunk-toggle" type="checkbox">&nbsp;Expand code' %+%
  '</label>'

# When using the expandable chunk option, run this hook function to add the
# checkbox markup before the chunk.
checkbox_hook <- function(before, options, env, ...) {
  # options contains the keywords from the chunk, so if using this hook, you
  # would get options$expandable = TRUE
  if (before) {
    if (grepl('^unnamed-chunk', options$label) || util.is_blank(options$label)) {
      label <- ''
    } else {
      label <- '"' %+% options$label %+% '"'
    }
    checkbox_markup <- checkbox_markup %+%
      '<i class="hidden-chunk-placeholder"><br>' %+% label %+%
      ' source hidden</i>'
    return(checkbox_markup)
  }
}
knitr::knit_hooks$set(expandable = checkbox_hook)

# Attach the needed javascript once at the end of the
# document.
knitr::knit_hooks$set(document = function(x, options) {
  # x is a character vector, each element of which is some html which will be
  # concatenated into a document. Here we add the chunk show/hide script to the
  # end of the document. This hook is only called once for the whole document.
  return(c(x, script_tag))
})
