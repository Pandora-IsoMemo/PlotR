# Server and UI Functions for Shiny Module

Server and UI Functions for Shiny Module

## Usage

``` r
addMoreDataUI(id, label)

addMoreData(input, output, session)

savePlotUI(id, label)

savePlot(input, output, session, savedData, currentPlot)

deletePlotUI(id, label)

deletePlot(input, output, session, savedData)

addMorePointsUI(id, title)

addMorePoints(input, output, session, savedData)

downUploadsUI(id, title)

downUploads(input, output, session, savedData, loadedFiles)

goodnessOfFitUI(id, title)

goodnessOfFit(input, output, session, savedData)

multiplePlotsUI(id, title)

multiplePlots(input, output, session, savedData)

multiplePredictionsUI(id, title)

multiplePredictions(input, output, session, savedData, loadedFiles)

postProcessingUI(id, title)

postProcessing(input, output, session, savedData)

runModelUI(id, title)

runModel(input, output, session, loadedFiles)

stylePlotUI(id, title)

stylePlot(input, output, session, savedData)

uploadFilesUI(id, title)

uploadFiles(input, output, session)
```

## Arguments

- id:

  namespace id

- label:

  (character) label used in UI

- input:

  shiny input object

- output:

  shiny output object

- session:

  shiny session

- savedData:

  (reactive) list of saved data

- currentPlot:

  plot object to be saved

- title:

  title of tab in tabset panel

- loadedFiles:

  (reactive) list of uploaded files
