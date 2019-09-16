report.bootstrap=function(object,format="PDF",file,open=TRUE,...){
  #object=boot_object
  #format="Word"
  
  # Copy the report file to a temporary directory before processing it, in
  # case we don't have write permissions to the current working dir (which
  # can happen when deployed).
  tempReport <- file.path(tempdir(), "report.Rmd")
  #  file.copy(paste0(normalizePath( find.package("BDP2"),winslash = "/"),"/exdata/report.Rmd"), tempReport, overwrite = TRUE)
  file.copy(file.path(system.file("appdir", package = "challengeR"), "reportSingle.Rmd"), tempReport, overwrite = TRUE)
  
  # Set up parameters to pass to Rmd document
  params <- list(
    object=object
  )
  
  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  # render(tempReport, output_file = file,
  #   params = params,
  #   envir = new.env(parent = globalenv())
  # )
  out <- render(tempReport, switch(
    format,
    PDF = pdf_document(), HTML = html_document(), Word = word_document()
  ),params = params,
  envir = new.env(parent = globalenv())
  )
  
  if (!missing(file)) file.rename(out, file) else file=out
  if (open) system(paste0('open "', file, '"'))
  
}



report.bootstrap.list=function(object,consensus,format="PDF",file,open=TRUE,...){
  #object=boot_object
  #format="Word"
  
  # Copy the report file to a temporary directory before processing it, in
  # case we don't have write permissions to the current working dir (which
  # can happen when deployed).
  tempReport <- file.path(tempdir(), "report.Rmd")
  #  file.copy(paste0(normalizePath( find.package("BDP2"),winslash = "/"),"/exdata/report.Rmd"), tempReport, overwrite = TRUE)
  file.copy(file.path(system.file("appdir", package = "challengeR"), "reportMultiple.Rmd"), tempReport, overwrite = TRUE)
  
  # Set up parameters to pass to Rmd document
  params <- list(
    object=object,
    consensus=consensus
  )
  
  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  # render(tempReport, output_file = file,
  #   params = params,
  #   envir = new.env(parent = globalenv())
  # )
  out <- render(tempReport, switch(
    format,
    PDF = pdf_document(), HTML = html_document(), Word = word_document()
  ),params = params,
  envir = new.env(parent = globalenv())
  )
  
  if (!missing(file)) file.rename(out, file) else file=out
  if (open) system(paste0('open "', file, '"'))
  
}

