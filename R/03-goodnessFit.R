#' @export
#' @rdname shinyModule
goodnessOfFitUI <- function(id, title) {
  ns <- NS(id)

  tabPanel(title,
           id = id,
           value = id,
           fluidRow(
             sidebarPanel(
               style = "position:fixed; width:23%; max-width:500px; overflow-y:auto; height:88%",
               width = 3,
               tags$hr()
             ),
             mainPanel(width = 8,
                       fluidRow(
                         column(8, h4("Goodness of fit measures"))
                       ),
                       DTOutput(ns("styledTable")))
           ))
}

#' @export
#' @rdname shinyModule
#' @param savedData (reactive) list of saved data
goodnessOfFit <-
  function(input,
           output,
           session,
           savedData) {
    derivedMeasures <- reactiveVal(NULL)
    observe({
      req(names(savedData()))
      derivedMeasures(do.call("rbind", lapply(names(savedData()), function(x){
        llog = savedData()[x][[x]]$plotValues$modelData$modelOutput$llog
        nTotal = length(llog[,1])
        edf = savedData()[x][[x]]$plotValues$modelData$modelOutput$edf
        data.frame(plot = x,
                   AIC = round(-2 * sum(log(rowMeans(exp(llog)))) +2*edf,2),
                   BIC= round(-2 * sum(log(rowMeans(exp(llog)))) + edf * log(nTotal),2),
                   #edf = round(edf, 2),
                   n_obs = nTotal)
      })))
    })

    output$styledTable <- renderDT({
      req(derivedMeasures())
      derivedMeasures()
    })

}
