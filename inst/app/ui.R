library("PlotR")
library("shiny")

tagList(
    navbarPage(
        header = includeCSS("www/custom.css"),
        title = paste("PlotR", packageVersion("PlotR")),
        theme = shinythemes::shinytheme("flatly"),
        position = "fixed-top",
        collapsible = TRUE,
        id = "tab",
        uploadFilesUI("files", "File Import(s)"),
        runModelUI("model", "Plot Data & Run Model"),
        postProcessingUI("post", "Post Processing"),
        stylePlotUI("style", "Style Plot"),
        addMorePointsUI("addPoints", "Add Points"),
        downUploadsUI("downUpload", "Down-/Upload"),
        multiplePlotsUI("multiple", "Multiple Plots"),
        multiplePredictionsUI("multiplePreds", "Multiple Predictions"),
        goodnessOfFitUI("goodness", "Goodness of Fit")
    ),
    shinyTools::headerButtonsUI(id = "header", help_link = "https://pandora-isomemo.github.io/PlotR/"),
    shinyjs::useShinyjs()
)

