function(input, output) {
  mydata <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, header=input$header, sep=input$sep,  dec = input$dec)
    
    return(tbl)
  })
  
    cE.vs.PEcall= reactive({
       return(res)
  
    })
    
 
  output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
       filename = function() {
      paste('report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
      #  file.copy(paste0(normalizePath( find.package("BDP2"),winslash = "/"),"/exdata/report.Rmd"), tempReport, overwrite = TRUE)
        file.copy(file.path(system.file("appdir", package = "BDP2"), "report.Rmd"), tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
                        pF=input$pF,
                        pE=input$pE,
                        p0=input$p0,
                        p1=input$p1,
                        shape1F=input$shape1F,
                        shape2F=input$shape2F,
                        shape1E=input$shape1E,
                        shape2E=input$shape2E,
                        n.range_1=input$n.range_1,
                        n.range_2=input$n.range_2,
                        n.range_4=input$n.range_4,
                        cF=input$cF,
                        cE.range_1=input$cE.range_1,
                        cE=input$cE,
                        firstInterim.at=input$firstInterim.at,
                        furtherInterims.at=input$furtherInterims.at,
                        nfinal=input$nfinal,
                        nfinal.vec= input$nfinal.vec,
                        ptrue.range_1=input$ptrue.range_1
                      )

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        # render(tempReport, output_file = file,
        #   params = params,
        #   envir = new.env(parent = globalenv())
        # )
        out <- render(tempReport, switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ),params = params,
          envir = new.env(parent = globalenv())
      )
      file.rename(out, file)
      }
    )

}




