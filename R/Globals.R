.formior_state <- new.env(parent = emptyenv())
.formior_state$Form_Info <- NULL

utils::globalVariables(c(".formior_state"))
utils::globalVariables(c("column", "max_distinct", "preview", "row_number", "slice" ))
