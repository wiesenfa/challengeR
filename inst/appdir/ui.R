library(shiny)
library(challengeR)

fluidPage(
    # oben  
      titlePanel("VFL Sampling calculator"),
      h3("Workflow to determine design parameters for a multi-stage single-arm phase II trial with binary endpoint. Declaration of efficacy and futility is based on Bayesian posterior distribution."),
      h4("Annette Kopp-Schneider, Manuel Wiesenfarth, Division of Biostatistics, German Cancer Research Center (DKFZ), Heidelberg, Germany"),
      br(),
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')) ,
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   'Comma'),
      
      radioButtons('dec', 'Desimal seperator',
                   c('komma'=",",
                     'punktum'="."), 'komma'),
      
      
      
numericInput("p0", "p0:", value=.12, min = 0, max = 1, step=.02),bsTooltip("p0", "Uninteresting (control) response rate", "right", options = list(container = "body")),
      br(),hr(),br(),#br(),

          textInput("furtherInterims.at", "Second and potential further interims at (separate by blank):", value = "20"),
           downloadButton("report", "Generate report"),
        radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word')
      ),
  h4("Reference"),
  h5("Kopp-Schneider, A., Wiesenfarth, M., Witt, R., Edelmann, D., Witt, O. and Abel, U. (2018). Monitoring futility and efficacy in phase II trials with Bayesian
posterior distributions - a calibration approach. 
Biometrical Journal, to appear.")
  )
