#' @export
#' @rdname shinyModule
multiplePlotsUI <- function(id, title) {
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
               ),
               tags$br(),
               sliderInput(
                 ns("previewWidth"),
                 label = "Width of preview",
                 min = 400,
                 max = 2000,
                 value = 1100,
                 step = 10
               ),
               sliderInput(
                 ns("previewHeight"),
                 label = "Height of preview",
                 min = 400,
                 max = 2000,
                 value = 800,
                 step = 10
               ),
               tags$br(),
               fluidRow(
                 column(
                   4,
                   selectInput(
                     ns("combiType"),
                     label = "Combine plots",
                     choices = c("all-in-one" = "joinedPlot",
                                 "grid" = "fullGrid"),
                     selected = "fullGrid"
                   )
                 ),
                 column(
                   8,
                   conditionalPanel(
                     condition = "input.combiType == 'fullGrid'",
                     ns = ns,
                     numericInput(
                       inputId = ns("nGridCols"),
                       label = "Number of columns",
                       min = 1,
                       max = 8,
                       value = 1,
                       step = 1
                     )
                   ),
                   conditionalPanel(condition = "input.combiType == 'joinedPlot'",
                                    ns = ns,
                                    selectMarginUI(ns("margins")),
                                    addLegendUI(ns("joinedPlot"))
                   )
                 )
               ),
               tags$br(),
               fluidRow(column(
                 6,
                 selectInput(
                   ns("xAxisToHide"),
                   label = "Hide x axis of plots",
                   choices = NULL,
                   multiple = TRUE
                 )
               ),
               column(
                 6,
                 selectInput(
                   ns("yAxisToHide"),
                   label = "Hide y axis of plots",
                   choices = NULL,
                   multiple = TRUE
                 )
               )),
               tags$br(),
               conditionalPanel(
                 condition = "output.showSignifStatus == true",
                 ns = ns,
                 checkboxInput(ns("showSignif"), label = "Show significant differences in red"),
                 conditionalPanel(
                   condition = "input.showSignif == true",
                   ns = ns,
                   fluidRow(column(
                     6,
                     selectInput(
                       ns("referencePlot"),
                       label = "Reference plot",
                       choices = NULL,
                       multiple = FALSE
                     )
                   ),
                   column(
                     6,
                     sliderInput(
                       ns("sigLevel"),
                       label = "Significance level",
                       min = 0.5,
                       max = 0.999,
                       value = 0.95,
                       step = 0.001
                     )
                   ))
                 )
               )
             ),
             mainPanel(
               width = 8,
               fluidRow(column(9, h4(
                 "View Multiple Plots"
               )),
               column(3,
                      align = "right",
                      plotExportButton(ns(
                        "export"
                      )))),
               tags$br(),
               p(
                 strong("Notes:"),
                 "Adjust the format of a single plot in the tab Style Plot.",
                 "Increase the width/height if figure margins become to large when selecting many plots."
               ),
               conditionalPanel(
                 condition = "input.combiType == 'joinedPlot'",
                 ns = ns,
                 p(
                   "The background color of an",
                   em("all-in-one"),
                   "plot is determined by the first selected single plot. ",
                   "The axes of the first selected plot will be furthest inside."
                 )
               ),
               plotOutput(
                 ns("multiPlot"),
                 height = "800px",
                 width = "100%",
                 inline = TRUE
               )
             )
           ))
}

selectMarginUI <- function(id) {
  ns <- NS(id)
  div(
    selectInput(
      ns("side"),
      label = "Adjust margin",
      choices = c(
        "bottom" = "1",
        "left" = "2",
        "top" = "3",
        "right" = "4"
      ),
      selected = "1"
    ),
    conditionalPanel(
      condition = "input.side == '1'",
      ns = ns,
      numericInput(
        ns("nMarginLineBottom"),
        label = "Number of margin lines",
        min = 0,
        max = 1,
        value = 1,
        step = 1
      ),
    ),
    conditionalPanel(
      condition = "input.side == '2'",
      ns = ns,
      numericInput(
        ns("nMarginLineLeft"),
        label = "Number of margin lines",
        min = 0,
        max = 1,
        value = 1,
        step = 1
      ),
    ),
    conditionalPanel(
      condition = "input.side == '3'",
      ns = ns,
      numericInput(
        ns("nMarginLineTop"),
        label = "Number of margin lines",
        min = 0,
        max = 1,
        value = 1,
        step = 1
      ),
    ),
    conditionalPanel(
      condition = "input.side == '4'",
      ns = ns,
      numericInput(
        ns("nMarginLineRight"),
        label = "Number of margin lines",
        min = 0,
        max = 1,
        value = 1,
        step = 1
      ),
    )
  )
}

#' @export
#' @rdname shinyModule
#' @param savedData (reactive) list of saved data
multiplePlots <- function(input, output, session, savedData) {
  activePlotsNames <- reactiveVal(NULL)
  activePlotsData <- reactiveVal(list())
  values <- reactiveValues(plot = NULL)
  nActivePlots <- reactiveVal(NULL)

  nMarginLines <-
    callModule(selectMargin, id = "margins", nData = nActivePlots)

  observe({
    req(names(savedData()))
    updateSelectInput(session,
                      "activePlots",
                      choices = names(savedData()),
                      selected = activePlotsNames())
    #shinyjs::disable("exportCombined")
  })

  observeEvent(input$activePlots, {
    req(input$activePlots)

    updateSelectInput(session, "xAxisToHide", choices = input$activePlots)
    # only use if nMarginLines AND position of an axis will be reactive
    #selected = getNamesToDrop(input$activePlots, input$combiType))
    updateSelectInput(session, "yAxisToHide", choices = input$activePlots)
    #selected = getNamesToDrop(input$activePlots, input$combiType))
    updateSelectInput(session, "referencePlot", choices = input$activePlots)


    activePlotsNames(input$activePlots)
    nActivePlots(length(input$activePlots))
    activePlotsData(savedData()[input$activePlots])
  })

  output$showSignifStatus <- reactive({
    nActivePlots() >= 2
  })
  outputOptions(output, "showSignifStatus", suspendWhenHidden = FALSE)
  output$multiPlot <- renderPlot({
    validate(
      need(names(activePlotsData()), "Select plots ...")
    )
    tryCatchWithMessage(
      makeMultiPlot(
        activePlotsData(),
        nMarginLines = nMarginLines(),
        combiType = input$combiType,
        nGridCols = input$nGridCols,
        xAxisToHide = input$xAxisToHide,
        yAxisToHide = input$yAxisToHide,
        showSig = input$showSignif,
        referencePlot = input$referencePlot,
        sigLevel = input$sigLevel,
        legendPosition = input[["joinedPlot-legend"]]
      )
    )

    values$plot <- recordPlot()
  },
  width = reactive(input$previewWidth),
  height = reactive(input$previewHeight))

  callModule(
    plotExport,
    "export",
    reactive(values$plot),
    plotWidth = reactive(input$previewWidth),
    plotHeight = reactive(input$previewHeight)
  )
}

selectMargin <- function(input, output, session, nData) {
  observe({
    req(nData())
    updateNumericInput(session,
                       "nMarginLineBottom",
                       max = nData(),
                       value = nData())
    updateNumericInput(session,
                       "nMarginLineLeft",
                       max = nData(),
                       value = nData())
    updateNumericInput(session,
                       "nMarginLineTop",
                       max = nData(),
                       value = nData())
    updateNumericInput(session,
                       "nMarginLineRight",
                       max = nData(),
                       value = nData())
  })

  reactive(
    list(
      bottom = input$nMarginLineBottom,
      left = input$nMarginLineLeft,
      top = input$nMarginLineTop,
      right = input$nMarginLineRight
    )
  )
}

getNamesToDrop <- function(names, combiType, from = 4) {
  if (length(names) < from | combiType == "fullGrid")
    return(NULL)
  names[from:length(names)]
}
