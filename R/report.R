report <- function(object,...) UseMethod("report")
report.default <- function(object, ...) stop("not implemented for this class")


#' Generates a benchmarking report with bootstrapping results
#'
#' Generates a benchmarking report in PDF, HTML or Word format with bootstrapping results.
#' It contains the rankings, plots of the raw assessment data and plots of the ranking stability.
#' For multi-task challenges, it also contains plots of cross-task insights. If you are interested in
#' the individual plots as separate files, set argument \code{clean} to \code{FALSE} and specify \code{fig.format}.
#'
#' @param object The ranked (bootstrapped) assessment data set.
#' @param consensus The rank aggregation across tasks (consensus ranking). Only needed for a multi-task data set.
#' @param file A string specifying the file name of the report. It allows for specifying the output file path as well,
#'   otherwise the working directory is used. If \code{file} does not have a file extension, an extension will be automatically
#'   added according to the output format given in \code{format}. If the argument is omitted, the report is created in a
#'   temporary folder with file name "report".
#' @param title A string specifying the title of the report.
#' @param colors The color scheme that is applied to the plots.
#' @param format A string specifying the format of the report. The options are "PDF", "HTML" or "Word".
#' @param latex_engine A string specifying the LaTeX engine for producing PDF output. The Options are "pdflatex", "lualatex", and "xelatex".
#' @param clean A boolean indicating whether intermediate files (e.g. individual plots) should be kept. Using \code{TRUE} will clean
#'   intermediate files that are created during rendering.
#' @param fig.format A vector of strings containing the file format of the figures that are not removed if \code{clean} is set to \code{FALSE}.
#'   The options are "jpeg", "png" and "pdf",  e.g. \code{fig.format = c("jpeg", "png", "pdf")}.
#' @param dpi A positive integer specifying the resolution of the generated plot (\code{fig.format} "jpeg" or "png") in dots per inch (DPI).
#' @param open A boolean specifying whether the report should be opened with the default system viewer after generation.
#' @param ... Further arguments passed to or from other functions.
#'
#' @return
#'
#' @examples
#' @export
report.bootstrap.list=function(object,
                               consensus,
                               file,
                               title="<Challenge name>",
                               colors=default_colors,
                               format="PDF",
                               latex_engine="pdflatex",
                               clean=TRUE,
                               fig.format = NULL, # file format of figures if clean==FALSE, can be vector, e.g. fig.format=c('jpeg','png', 'pdf')
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
                clean = clean,
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

#' Generates a benchmarking report without bootstrapping results
#'
#' Generates a benchmarking report in PDF, HTML or Word format without bootstrapping results.
#' It contains the rankings, plots of the raw assessment data and plots of the ranking stability.
#' For multi-task challenges, it also contains plots of cross-task insights. If you are interested in
#' the individual plots as separate files, set argument \code{clean} to \code{FALSE} and specify \code{fig.format}.
#'
#' @param object The ranked assessment data set.
#' @param consensus The rank aggregation across tasks (consensus ranking). Only needed for a multi-task data set.
#' @param file A string specifying the file name of the report. It allows for specifying the output file path as well,
#'   otherwise the working directory is used. If \code{file} does not have a file extension, an extension will be automatically
#'   added according to the output format given in \code{format}. If the argument is omitted, the report is created in a
#'   temporary folder with file name "report".
#' @param title A string specifying the title of the report.
#' @param colors The color scheme that is applied to the plots.
#' @param format A string specifying the format of the report. The options are "PDF", "HTML" or "Word".
#' @param latex_engine A string specifying the LaTeX engine for producing PDF output. The Options are "pdflatex", "lualatex", and "xelatex".
#' @param clean A boolean indicating whether intermediate files (e.g. individual plots) should be kept. Using \code{TRUE} will clean
#'   intermediate files that are created during rendering.
#' @param fig.format A vector of strings containing the file format of the figures that are not removed if \code{clean} is set to \code{FALSE}.
#'   The options are "jpeg", "png" and "pdf",  e.g. \code{fig.format = c("jpeg", "png", "pdf")}.
#' @param dpi A positive integer specifying the resolution of the generated plot (\code{fig.format} "jpeg" or "png") in dots per inch (DPI).
#' @param open A boolean specifying whether the report should be opened with the default system viewer after generation.
#' @param ... Further arguments passed to or from other functions.
#'
#' @return
#'
#' @examples
#' @export
report.ranked.list=function(object,
                            consensus,
                            file,
                            title="<Challenge name>",
                            colors=default_colors,
                            format="PDF",
                            latex_engine="pdflatex",
                            clean=TRUE,
                            fig.format = NULL, # file format of figures if clean=FALSE, can be vector, e.g. fig.format=c('jpeg','png', 'pdf')
                            dpi = 150, # DPI, relevant for bitmaps if clean==FALSE and fig.format specified
                            open=TRUE,
                            ...){
  report.bootstrap.list(object, consensus, file, title, colors, format, latex_engine, clean, fig.format, dpi, open, ...)
}
