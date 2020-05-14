report <- function(object,...) UseMethod("report")
report.default <- function(object, ...) stop("not implemented for this class")

report.bootstrap=function(object,
                          file,
                          title="<Challenge name>",
                          colors=default_colors,
                          format="PDF",
                          latex_engine="pdflatex",
                          open=TRUE,...){

  # Copy the report file to a temporary directory before processing it, in
  # case we don't have write permissions to the current working dir (which
  # can happen when deployed).
  if (missing(file)) tempReport <- file.path(tempdir(), "report.Rmd")
  else {
    a=strsplit(file,"/")[[1]]
    path=paste0(a[-length(a)],collapse="/")
    if (path=="") tempReport=file.path(paste0(strsplit(a[length(a)],".",fixed=T)[[1]][1],".Rmd"))
    else tempReport=file.path(path,
                              paste0(strsplit(a[length(a)],".",fixed=T)[[1]][1],".Rmd"))
  }
  file.copy(file.path(system.file("appdir", package = "challengeR"),
                      "reportSingle.Rmd"),
            tempReport,
            overwrite = TRUE)

  # Set up parameters to pass to Rmd document
  params <- list(
    object=object,
    name=title,
    colors=colors
  )


  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
   out <- render(tempReport,
                 switch(
                   format,
                   PDF = pdf_document(number_sections=T,
                                      latex_engine=latex_engine),
                   HTML = html_document(number_sections=T),
                   Word = word_document()
                   ),
                 params = params,
                 envir = new.env(parent = globalenv()),
                 ...
  )

  if (!missing(file)){
    if (is.na(strsplit(file,".",fixed=T)[[1]][2])) file=paste0(file,
                                                               ".",
                                                               strsplit(out,".",fixed=T)[[1]][2])
    file.rename(out, file)
  } else file=out

  file.remove(tempReport)

  if (open) system(paste0('open "', file, '"'))

}



report.bootstrap.list=function(object,
                               consensus,
                               file,
                               title="<Challenge name>",
                               colors=default_colors,
                               format="PDF",
                               latex_engine="pdflatex",
                               open=TRUE,...){

  # Copy the report file to a temporary directory before processing it, in
  # case we don't have write permissions to the current working dir (which
  # can happen when deployed).
  if (missing(file)) tempReport <- file.path(tempdir(), "report.Rmd")
  else {
    a=strsplit(file,"/")[[1]]
    path=paste0(a[-length(a)],collapse="/")
    if (path=="") tempReport=file.path(paste0(strsplit(a[length(a)],
                                                       ".",
                                                       fixed=T)[[1]][1],".Rmd"))
    else tempReport=file.path(path,paste0(strsplit(a[length(a)],
                                                   ".",
                                                   fixed=T)[[1]][1],".Rmd"))
  }
  file.copy(file.path(system.file("appdir", package = "challengeR"),
                      "reportMultiple.Rmd"),
            tempReport,
            overwrite = TRUE)

  if (length(object$matlist) > 1) {
    consensus = consensus
    isMultiTask = TRUE
  }
  else {
    consensus = NULL
    isMultiTask = FALSE
  }

  # Set up parameters to pass to Rmd document
  params <- list(
    object=object,
    consensus=consensus,
    name=title,
    colors=colors,
    isMultiTask=isMultiTask,
    bootstrappingEnabled=TRUE
  )

  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  # render(tempReport, output_file = file,
  #   params = params,
  #   envir = new.env(parent = globalenv())
  # )
  out <- render(tempReport,
                switch(
                  format,
                  PDF = pdf_document(number_sections=T,
                                     latex_engine=latex_engine),
                  HTML = html_document(number_sections=T),
                  Word = word_document(df_print="kable")
                  ),
                params = params,
                envir = new.env(parent = globalenv()),
                ...
  )

  if (!missing(file)){
    if (is.na(strsplit(file,".",fixed=T)[[1]][2])) file=paste0(file,
                                                               ".",
                                                               strsplit(out,".",fixed=T)[[1]][2])
    file.rename(out, file)
  } else file=out

  file.remove(tempReport)

  if (open) system(paste0('open "', file, '"'))

}



########################


report.ranked=function(object,
                       file,
                       title="<Challenge name>",
                       colors=default_colors,
                       format="PDF",
                       latex_engine="pdflatex",
                       open=TRUE,...){

  # Copy the report file to a temporary directory before processing it, in
  # case we don't have write permissions to the current working dir (which
  # can happen when deployed).
  if (missing(file)) tempReport <- file.path(tempdir(), "report.Rmd")
  else {
    a=strsplit(file,"/")[[1]]
    path=paste0(a[-length(a)],collapse="/")
    if (path=="") tempReport=file.path(paste0(strsplit(a[length(a)],
                                                       ".",
                                                       fixed=T)[[1]][1],".Rmd"))
    else tempReport=file.path(path,
                              paste0(strsplit(a[length(a)],
                                              ".",
                                              fixed=T)[[1]][1],".Rmd"))
  }
  file.copy(file.path(system.file("appdir", package = "challengeR"),
                      "reportSingleShort.Rmd"),
            tempReport,
            overwrite = TRUE)

  # Set up parameters to pass to Rmd document
  params <- list(
    object=object,
    name=title,
    colors=colors
  )


  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  out <- render(tempReport,
                switch(
                  format,
                  PDF = pdf_document(number_sections=T,
                                     latex_engine=latex_engine),
                  HTML = html_document(number_sections=T),
                  Word = word_document()
                  ),
                params = params,
                envir = new.env(parent = globalenv()),...
  )

  if (!missing(file)){
    if (is.na(strsplit(file,".",fixed=T)[[1]][2])) file=paste0(file,
                                                               ".",
                                                               strsplit(out,".",fixed=T)[[1]][2])
    file.rename(out, file)
  } else file=out

  file.remove(tempReport)

  if (open) system(paste0('open "', file, '"'))

}



report.ranked.list=function(object,
                            consensus,
                            file,
                            title="<Challenge name>",
                            colors=default_colors,
                            format="PDF",
                            latex_engine="pdflatex",
                            open=TRUE,
                            ...){

  # Copy the report file to a temporary directory before processing it, in
  # case we don't have write permissions to the current working dir (which
  # can happen when deployed).
  if (missing(file)) tempReport <- file.path(tempdir(),
                                             "report.Rmd")
  else {
    a=strsplit(file,"/")[[1]]
    path=paste0(a[-length(a)],
                collapse="/")
    if (path=="") tempReport=file.path(paste0(strsplit(a[length(a)],
                                                       ".",
                                                       fixed=T)[[1]][1],
                                              ".Rmd"))
    else tempReport=file.path(path,
                              paste0(strsplit(a[length(a)],
                                              ".",
                                              fixed=T)[[1]][1],
                                     ".Rmd"))
  }
  file.copy(file.path(system.file("appdir", package = "challengeR"),
                      "reportMultiple.Rmd"),
            tempReport,
            overwrite = TRUE)

  if (length(object$matlist) > 1) {
    consensus = consensus
    isMultiTask = TRUE
  }
  else {
    consensus = NULL
    isMultiTask = FALSE
  }

  # Set up parameters to pass to Rmd document
  params <- list(
    object=object,
    consensus=consensus,
    name=title,
    colors=colors,
    isMultiTask=isMultiTask,
    bootstrappingEnabled=FALSE
  )

  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  out <- render(tempReport,
                switch(
                  format,
                  PDF = pdf_document(number_sections=T,
                                     latex_engine=latex_engine),
                  HTML = html_document(number_sections=T),
                  Word = word_document(df_print="kable")
                  ),
                params = params,
                envir = new.env(parent = globalenv()),
                ...
                )

  if (!missing(file)){
    if (is.na(strsplit(file,".",fixed=T)[[1]][2])) file=paste0(file,
                                                               ".",
                                                               strsplit(out,".",fixed=T)[[1]][2])
    file.rename(out, file)
  } else file=out

  file.remove(tempReport)

  if (open) system(paste0('open "', file, '"'))

}


