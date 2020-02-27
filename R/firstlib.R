.onAttach <- function (lib, pkg) {
  ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"),"Version")
  ver <- as.character(ver)
  packageStartupMessage("\nchallengeR ",
                        ver,
                        " loaded. \n\n",
                        #                       "Note: Layouting in case of many algorithms or tasks is not yet optimized. Please be patient, we are steadily working on improving the package",
                        "\n\nPlease cite as:\n   Wiesenfarth, M., Reinke, A., Landmann A.L., Cardoso, M.J., Maier-Hein, L. and Kopp-Schneider, A. (2019). Methods and open-source toolkit for analyzing and visualizing challenge results. arXiv preprint arXiv:1910.05121\n\n", 
                        domain = NULL,  
                        appendLF = TRUE)
}

.onLoad <- function(...) {
}

.onUnload <- function (libpath) {
}
