#' @export
#' @rdname shinyModule
multiplePredictionsUI <- function(id, title) {
  ns <- NS(id)

  tabPanel(title,
           id = id,
           value = id,
           fluidRow(
             sidebarPanel(
               style = "position:fixed; width:23%; max-width:500px; overflow-y:auto; height:88%",
               width = 3,
               selectInput(
                 ns("activePlots"),
                 label = "Select saved plots",
                 choices = c("Save or upload plots ..." = ""),
                 multiple = TRUE#,
                 #selectize = TRUE
               )
             ),
             mainPanel(
               width = 8,
               tabsetPanel(
                 type = "tabs",
                 id = ns("multiPlotsTabs"),
                 tabPanel(
                   "Correlation of Model Estimates",
                   value = ns("correlationOfGraps"),
                   # correlation of model estimates ####
                   fluidRow(
                     column(
                       6,
                       tags$br(),
                       tags$br(),
                       actionButton(ns("computeCorrelation"), "Compute Correlation")
                     ),
                     column(
                       6,
                       tags$br(),
                       sliderInput(
                         ns("xRangeCorr"),
                         label = "Underlying X Range",
                         min = 0,
                         max = 1,
                         value = c(0, 1),
                         dragRange = FALSE
                       ),
                     )
                   ),
                   tags$br(),
                   tableOutput(ns("correlationOfModels"))
                 ),
                 tabPanel(
                   "Predict Data",
                   value = ns("predictData"),
                   # predict response ####
                   fluidRow(
                     column(
                       8,
                       h4(
                         "Calculate Predictions or Derive Explanatory Variables for Multiple Graphs"
                       )
                     ),
                     column(
                       4,
                       align = "right",
                       dataExportButton(ns("exportPredictedY"),
                                        title = "Export Predicted Y"),
                       dataExportButton(ns("exportDerivedX"),
                                        title = "Export Derived X")
                     )
                   ),
                   checkboxInput(ns("useUploadedData"),
                                 label = "Use uploaded data",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.useUploadedData == false",
                     ns = ns,
                     fluidRow(
                       column(3,
                              h4("Predict Response"),
                              addMoreDataUI(ns("moreX"), label = "x")),
                       column(
                         2,
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         actionButton(ns("predictY"), "Predict Y")
                       ),
                       column(3,
                              h4("Derive Explanatory"),
                              addMoreDataUI(ns("moreY"), label = "y")),
                       column(
                         2,
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         actionButton(ns("deriveX"), "Derive X")
                       )
                     )
                   ),
                   conditionalPanel(
                     condition = "input.useUploadedData == true",
                     ns = ns,
                     selectInput(
                       ns("activeFile"),
                       label = "Select a file",
                       choices = c("Import a file ..." = "")
                     ),
                     tags$br(),
                     fluidRow(
                       column(
                         3,
                         h4("Predict Response"),
                         selectColumnsUI(
                           id = ns("moreXUploaded"),
                           label = "x",
                           emptyChoices = c("Select a file ..." = "")
                         ),
                         # no sampling here, we have a list of values
                       ),
                       column(
                         2,
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         actionButton(ns("predictYUploaded"), "Predict Y")
                       ),
                       column(
                         3,
                         offset = 1,
                         h4("Derive Explanatory"),
                         selectColumnsUI(
                           id = ns("moreYUploaded"),
                           label = "y",
                           emptyChoices = c("Select a file ..." = "")
                         ),
                         # no sampling here, we have a list of values
                       ),
                       column(
                         2,
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         actionButton(ns("deriveXUploaded"), "Derive X")
                       )
                     )
                   ),
                   tags$hr(),
                   fluidRow(
                     column(
                       12,
                       fluidRow(column(6, h4(
                         "Predicted Response"
                       )),
                       column(
                         6,
                         align = "right",
                         checkboxInput(
                           ns("aggPrediction"),
                           label = "If several points show mean over x per plot for all columns.",
                           value = TRUE,
                           width = "100%"
                         )
                       )),
                       DTOutput(ns("prediction")),
                       tags$hr(),
                       fluidRow(column(6, h4(
                         "Derived Explanatory"
                       )),
                       column(
                         6,
                         align = "right",
                         checkboxInput(
                           ns("aggExplanatory"),
                           label = "If several points show mean over y per plot for all columns.",
                           value = TRUE,
                           width = "100%"
                         )
                       )),
                       DTOutput(ns("derivedX"))
                     )
                   )

                 )
               )

             )
           ))
}

#' @export
#' @rdname shinyModule
#' @param savedData (reactive) list of saved data
#' @param loadedFiles (reactive) list of uploaded files
multiplePredictions <-
  function(input,
           output,
           session,
           savedData,
           loadedFiles) {
    activePlotsData <- reactiveVal(list())
    activeFile <- reactiveVal(NULL)

    commonPredictions <- reactiveVal(NULL)
    correlationOfGraphs <- reactiveVal(NULL)

    moreXUploaded <- reactiveVal(list())
    moreYUploaded <- reactiveVal(list())

    predictedEstimationList <- reactiveVal(list())
    predictedEstimation <- reactiveVal(NULL)

    derivedExplanatoryList <- reactiveVal(list())
    derivedExplanatory <- reactiveVal(NULL)

    observe({
      req(names(savedData()))
      updateSelectInput(session, "activePlots", choices = names(savedData()))

      shinyjs::disable("exportCombined")
    })

    observeEvent(input$activePlots, {
      req(input$activePlots)
      correlationOfGraphs(NULL)

      predictedEstimationList(list())
      predictedEstimation(NULL)

      derivedExplanatoryList(list())
      derivedExplanatory(NULL)

      activePlotsData(savedData()[input$activePlots])

      updateSliderInput(
        session,
        "xRangeCorr",
        min = getCommonX(activePlotsData(), what = "min"),
        max = getCommonX(activePlotsData(), what = "max"),
        value = c(
          getCommonX(activePlotsData(), what = "min"),
          getCommonX(activePlotsData(), what = "max")
        )
      )
    })

    # correlation of model estimates ####
    observeEvent(input$computeCorrelation, {
      req(names(activePlotsData()))
      commonPredictions(activePlotsData() %>%
                          getCommonPredictions(xRange = getCommonX(activePlotsData(),
                                                                   what = "range")))
    })

    observe({
      req(commonPredictions())
      correlationOfGraphs(commonPredictions() %>%
                            filterCommonXRange(xRange = input$xRangeCorr) %>%
                            cor())
    })

    output$correlationOfModels <- renderTable({
      req(correlationOfGraphs())
      correlationOfGraphs()
    },
    rownames = TRUE, bordered = TRUE)

    # predict response ####
    moreX <- callModule(addMoreData, id = "moreX")

    observeEvent(input$predictY, {
      checkReq(input$activePlots, label = "Please select saved plot(s).")
      req(input$activePlots)

      checkReq(moreX()$mean, label = "Please provide input values.")
      req(moreX()$mean)
      withProgress(
        message = "Predicting Data",
        detail = paste0("This may take some seconds ..."),
        value = 0,
        {
          predictedEstimationList(lapply(activePlotsData(), function(x) {
            incProgress(1 / length(activePlotsData()) * 0.9)
            predictAdditionalData(
              moreMean = moreX()$mean,
              moreSD = moreX()$sd,
              moreNSample = moreX()$nSample,
              plotValues = x$plotValues
            ) %>%
              tryCatchWithWarningsAndErrors(errorTitle = paste("Prediction failed for", x$plotName),
                                            alertStyle = "shinyalert")
          }))
        }
      )
    })

    # predict response uploaded ####
    observe({
      req(names(loadedFiles()))
      updateSelectInput(
        session,
        "activeFile",
        choices = names(loadedFiles()),
        selected = names(loadedFiles())[1]
      )
    })

    observe({
      req(names(loadedFiles()))
      activeFile(toNumericCols(loadedFiles()[[input$activeFile]]))
    }) %>%
      bindEvent(input$activeFile)

    moreXColumns <- callModule(
      selectColumns,
      id = "moreXUploaded",
      colNames = reactive(colnames(toNumericCols(activeFile()))),
      datSettings = reactive(defaultColSelection(colnames(activeFile())))
    )

    observeEvent(input$predictYUploaded, {
      checkReq(input$activePlots, label = "Please select saved plot(s).")
      req(input$activePlots)

      checkReq(activeFile(), label = "Please select data from a file.")
      req(activeFile())

      xSelection <- getSelection(moreXColumns())

      preparedData <- activeFile()[, unlist(c(xSelection$colNames))] %>%
        asNumericWithoutNA() %>%                   # removes NA
        getPrepDataPart(xSelection, part = "X")    # adds new columns

      moreXUploaded(preparedData)

      checkReq(moreXUploaded()$X, label = "Please provide input values.")
      req(moreXUploaded()$X)

      withProgress(
        message = "Predicting Data",
        detail = paste0("This may take some seconds ..."),
        value = 0,
        {
          predictedEstimationList(lapply(activePlotsData(), function(x) {
            incProgress(1 / length(activePlotsData()) * 0.9)
            predictAdditionalData(
              moreMean = moreXUploaded()$X,
              moreSD = moreXUploaded()$XUncertainty,
              plotValues = x$plotValues
            ) %>%
              tryCatchWithWarningsAndErrors(errorTitle = paste("Prediction failed for", x$plotName),
                                            alertStyle = "shinyalert")
          }))
        }
      )
    })

    observe({
      req(predictedEstimationList())
      predictedEstimation(lapply(predictedEstimationList(), function(x) {
        if (is.null(x)) return(x)
        if (input$aggPrediction)
          colMeans(x)
        else
          x
      }) %>%
        bind_rows(.id = "name"))
    })

    output$prediction <- renderDT({
      req(predictedEstimation())
      predictedEstimation()
    }, options = list(scrollX = T))

    # derive explanatory ####
    moreY <- callModule(addMoreData, id = "moreY")

    observeEvent(input$deriveX, {
      checkReq(input$activePlots, label = "Please select saved plot(s).")
      req(input$activePlots)

      checkReq(moreY()$mean, label = "Please provide input values.")
      req(moreY()$mean)

      derivedExplanatoryList(lapply(activePlotsData(), function(x) {
        deriveExplanatory(
          moreMean = moreY()$mean,
          moreSD = moreY()$sd,
          moreNSample = moreY()$nSample,
          plotValues = x$plotValues,
          graphName = x$plotName
        ) %>%
          tryCatchWithWarningsAndErrors(errorTitle = paste("Calculation failed for", x$plotName),
                                        alertStyle = "shinyalert")
      }))
    })

    # derive explanatory  uploaded ####

    moreYColumns <- callModule(
      selectColumns,
      id = "moreYUploaded",
      colNames = reactive(colnames(activeFile())),
      datSettings = reactiveVal(defaultColSelection(colnames(
        toNumericCols(activeFile())
      )))
    )

    observeEvent(input$deriveXUploaded, {
      checkReq(input$activePlots, label = "Please select saved plot(s).")
      req(input$activePlots)

      checkReq(activeFile(), label = "Please select data from a file.")
      req(activeFile())

      moreYUploaded(getPrepDataPart(activeFile(), getSelection(moreYColumns()), part = "Y"))

      checkReq(moreYUploaded()$Y, label = "Please provide input values.")
      req(moreYUploaded()$Y)

      derivedExplanatoryList(lapply(activePlotsData(), function(x) {
        deriveExplanatory(
          moreMean = moreYUploaded()$Y,
          moreSD = moreYUploaded()$YUncertainty,
          plotValues = x$plotValues,
          graphName = x$plotName
        ) %>%
          tryCatchWithWarningsAndErrors(errorTitle = paste("Calculation failed for", x$plotName),
                                        alertStyle = "shinyalert")
      }))
    })

    observe({
      req(derivedExplanatoryList())
      derivedExplanatory(lapply(derivedExplanatoryList(), function(x) {
        if (is.null(x)) return(x)
        if (input$aggExplanatory)
          colMeans(x)
        else
          x
      }) %>%
        bind_rows(.id = "name"))
    })

    output$derivedX <- renderDT({
      req(derivedExplanatory())
      derivedExplanatory()
    }, options = list(scrollX = T))

    # export data ####
    dataPredictedY <- reactive({
      req(predictedEstimation())
      function() {
        predictedEstimation()
      }
    })

    callModule(
      dataExport,
      "exportPredictedY",
      dat = dataPredictedY,
      filename = "predictedYData",
      useXOptions = FALSE
    )

    dataDerivedX <- reactive({
      req(derivedExplanatory())
      function() {
        derivedExplanatory()
      }
    })

    callModule(
      dataExport,
      "exportDerivedX",
      dat = dataDerivedX,
      filename = "derivedXData",
      useXOptions = FALSE
    )
  }

#' Pop up a Warning if Req is missing.
#'
#' @param x reactive object to be checked with req()
#' @param label (character) Warning that pops up if x is missing
checkReq <- function(x, label) {
  if (is.null(x)) {
    shinyalert("Missing input!", paste(label),
               type = "error")
  }
}
