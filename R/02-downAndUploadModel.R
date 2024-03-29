# Download a plot ####

#' @rdname shinyModule
downloadModelUI <- function(id, label) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    tags$hr(),
    tags$h4(label),
    # pickerInput(ns("selectedModels"), "Select saved models",
    #             choices = NULL,
    #             options = list(`actions-box` = TRUE),
    #             multiple = T),
    selectInput(ns("selectedModels"), label = NULL,
                choices = c("Save or upload plots ..." = ""),
                multiple = T),
    textAreaInput(ns("notes"), "Add notes"),
    checkboxInput(ns("onlyInputs"), "Store only data and model options"),
    downloadButton(ns("downloadModelButton"), "Download")
  )
}

#' @rdname shinyModule
#' @param uploadedNotes (reactive) variable that stores content for README.txt
downloadModel <- function(input, output, session, savedData, uploadedNotes){

  # disable the downdload button on page load
  shinyjs::disable("downloadModelButton")

  observe({
    updateSelectInput(session, "selectedModels", choices = names(savedData()),
                      selected = names(savedData())[length(savedData())])

    updateTextAreaInput(session, "notes", value = uploadedNotes())

    if (length(savedData())) {
      # enable the download button
      shinyjs::enable("downloadModelButton")
      return(NULL)
    }

  })

  output$downloadModelButton <- downloadHandler(
    filename = function() {
      gsub("[ ]", "_", paste0(Sys.time(), "_PlotR.zip"))
    },
    content = function(file) {
      zipdir <- tempdir()
      modelfile <- file.path(zipdir, "model.Rdata")
      notesfile <- file.path(zipdir, "README.txt")

      req(savedData(), input$selectedModels)
      model <- savedData()[input$selectedModels]

      if (input$onlyInputs) {
        model <- model %>%
          removeModelOutputs()
      }

      save(model, file = modelfile)
      writeLines(input$notes %>% addPackageVersionNo(),
                 notesfile)
      zipr(file, c(modelfile, notesfile))
    }
  )

}

addPackageVersionNo <- function(txt){
  versionNo <- packageVersion("PlotR") %>%
    as.character()

  paste0(txt, "\n\n", "PlotR version ", versionNo, " .")
}

#' Remove Model Outputs
#'
#' @param models list of model objects to be saved
removeModelOutputs <- function(models) {
  lapply(models, function(model) {
    model$plotValues$modelData <- NULL
    model$plotValues$predictedData <- NULL
    model
  })
}

# Upload a plot ####

#' @rdname shinyModule
uploadModelUI <- function(id, label) {
  ns <- NS(id)

  tagList(
    tags$hr(),
    tags$h4(label),
    fileInput(ns("uploadModel"), label = NULL, accept = ".zip"),
    remoteModelsUI(ns("remoteModels"))
  )
}

#' @rdname shinyModule
#' @param uploadedNotes (reactive) variable that stores content for README.txt
uploadModel <- function(input,
                        output,
                        session,
                        loadedFiles,
                        savedData,
                        #reset,
                        uploadedNotes){
  pathToModel <- reactiveVal(NULL)

  observeEvent(input$uploadModel, {
    logDebug("Entering (%s) observeEvent(input$uploadModel)", session$ns(""))
    pathToModel(input$uploadModel$datapath)
  })

  # observeEvent(reset(), {
  #   logDebug("Entering (%s) observeEvent(reset())", session$ns(""))
  #   req(reset())
  #   updateSelectInput(session, "remoteModel", selected = list())
  #   pathToModel(NULL)
  # })

  pathToRemote <- remoteModelsServer("remoteModels",
                                     githubRepo = "plotr",
                                     rPackageName = "PlotR",
                                     rPackageVersion = "PlotR" %>%
                                       packageVersion() %>%
                                       as.character())

  observeEvent(pathToRemote(), {
    logDebug("Entering (%s) observeEvent(pathToRemote())", session$ns(""))
    pathToModel(pathToRemote())
  })

  observeEvent(pathToModel(), {
    logDebug("Entering (%s) observeEvent(pathToModel())", session$ns(""))
    model <- NULL

    res <- try({
      zip::unzip(pathToModel())
      load("model.Rdata")
    })

    if (inherits(res, "try-error") || !exists("model")) {
      shinyalert("Could not read model from file", type = "error")
    } else if (is.null(model)) {
      shinyalert("Model object is empty.", type = "error")
    } else if (all(names(model) %in% c("loadedFiles", "savedData"))) {
      shinyalert(
        title = "Depricated format",
        text = "Could not read file. Please use downloaded files from PlotR version larger or equal 22.02.1",
        type = "error"
      )
    } else {
      if (any(names(model) %in% names(savedData()))) {
        nameExists <- which(names(model) %in% names(savedData()))
        shinyalert(
          title = "Duplicated plot names",
          text = paste(
            "Plot name\n",
            paste(names(model)[nameExists], collapse = ", "),
            "\n already exist and was updated."
          ),
          type = "warning"
        )

        # rename duplicated plot names
        newNames <- names(model)
        while (any(newNames %in% names(savedData()))) {
          nameExists <- which(newNames %in% names(savedData()))
          newNames[nameExists] <- lapply(newNames[nameExists], incIndexOfName)
          names(model) <- newNames
        }
      }

      savedData(c(savedData(), model))
      updateSelectInput(session, "activePlot", choices = names(savedData()),
                        selected = names(savedData())[length(savedData())])

      uploadedFileNames <- lapply(names(savedData()), function(plot){
        newFileName <- savedData()[[plot]]$plotValues$activeFile

        # rename duplicated files
        while (any(newFileName == names(loadedFiles()))) {
          newFileName <- incIndexOfFile(newFileName)
          tmpSavedData <- savedData()
          tmpSavedData[[plot]]$plotValues$activeFile <- newFileName
          savedData(tmpSavedData)
        }

        newFileName
      })

      uploadedFiles <- lapply(names(savedData()), function(plot){
        savedData()[[plot]]$plotValues$activeFileData
      })
      uploadedFiles <- setNames(uploadedFiles, uploadedFileNames)

      loadedFiles(c(loadedFiles(), uploadedFiles[unique(unlist(uploadedFileNames))]))
      updateSelectInput(session, "activeFile", choices = names(loadedFiles()),
                        selected = savedData()[[names(savedData())[length(savedData())]]]$plotValues$activeFile)

      uploadedNotes(readLines("README.txt")[[1]])

      shinyalert("Upload finished", type = "success")
    }

    # clean up
    if (file.exists("model.Rdata")) file.remove("model.Rdata")
    if (file.exists("README.txt")) file.remove("README.txt")
    if (file.exists("help.html")) file.remove("help.html")
  })
}

#' Inc Index Of Name
#'
#' If the name has no index, add a new index: "(1)". If an index already exists, increase it by one.
#'
#' @param name (character) name
incIndexOfName <- function(name) {
  # extract index
  currentIndex <-
    regmatches(name, regexpr("\\([[:digit:]]+\\)$", name))

  # inc index
  if (length(currentIndex) == 0) {
    paste0(name, "(1)")
  } else {
    # get new index
    newIndex <- currentIndex %>%
      gsub(pattern = "\\(|\\)",
           replacement = "") %>%
      as.numeric() + 1

    # replace with new index
    gsub("\\([[:digit:]]+\\)$" ,
         paste0("(", newIndex, ")") ,
         name)
  }
}
