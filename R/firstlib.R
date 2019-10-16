.onAttach <- function (lib, pkg) {
  ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"),"Version")
  ver <- as.character(ver)
  packageStartupMessage("\nchallengeR ",ver," loaded. ",
                        "\nPlease cite as:\n   Wiesenfarth, M., Reinke, A., Landmann A.L., Cardoso, M.J., Maier-Hein, L. and Kopp-Schneider, A. (2019). Methods and open-source toolkit for analyzing and visualizing challenge results. arXiv preprint arXiv:1910.05121"                     
                         , domain = NULL,  appendLF = TRUE)
}
#Type 'help(\"AdaptFitOS-package\")' for an overview.\n
.onLoad <- function(...) {
}

.onUnload <- function (libpath) {
}