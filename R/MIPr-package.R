#' @keywords internal
"_PACKAGE"

#------------------------------------------------
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

# Silence the R CMD check notes on the where function
# This function comes from the tidyselect package but has not yet been exported
# causing R to throw notes at the user.
utils::globalVariables("where")
