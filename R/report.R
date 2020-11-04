report <- function(object,...) UseMethod("report")
report.default <- function(object, ...) stop("not implemented for this class")

report.bootstrap.list=function(object,
                               consensus,
                               file,
                               title="<Challenge name>",
                               colors=default_colors,
                               format="PDF",
                               latex_engine="pdflatex",
                               fig.format = NULL, # file format of figures if clean=FALSE, can be vector, e.g. fig.format=c('jpeg','png', 'pdf')
                               dpi = 150, # DPI, relevant for bitmaps if clean==FALSE and fig.format specified
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

  bootstrappingEnabled = TRUE

  if (is(object, "ranked.list")) {
    bootstrappingEnabled = FALSE
  }

  # Set up parameters to pass to Rmd document
  if (!is.null(fig.format) & format=="PDF") fig.format=c("pdf",fig.format)
  if (!is.null(fig.format) && fig.format[1]=="pdf" && format=="Word") fig.format <- c(fig.format[-1], fig.format[1]) # in word avoid use of pdf to be embedded in document
  params <- list(
    object=object,
    consensus=consensus,
    name=title,
    colors=colors,
    isMultiTask=isMultiTask,
    bootstrappingEnabled=bootstrappingEnabled,
    fig.format = fig.format,
    dpi = dpi
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

report.ranked.list=function(object,
                            consensus,
                            file,
                            title="<Challenge name>",
                            colors=default_colors,
                            format="PDF",
                            latex_engine="pdflatex",
                            fig.format = NULL, # file format of figures if clean=FALSE, can be vector, e.g. fig.format=c('jpeg','png', 'pdf')
                            dpi = 150, # DPI, relevant for bitmaps if clean==FALSE and fig.format specified
                            open=TRUE,
                            ...){
  report.bootstrap.list(object, consensus, file, title, colors, format, latex_engine, fig.format, dpi, open, ...)
}
