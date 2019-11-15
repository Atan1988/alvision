py_main  <- NULL
#get_document_bounds <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  py_main <<- reticulate::py_run_file('inst/py_scripts/extract info from resp.py')
  get_doc_contents <<- py_main$get_doc_content
  get_document_bounds <<- py_main$get_document_bounds
  assemble_word <<- py_main$assemble_word
  get_words_content <<- py_main$get_words_content
  FeatureType <<- py_main$FeatureType
}
