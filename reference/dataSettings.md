# Data settings module

UI function to define settings for imported data

Backend for data settings module

UI function to define settings for the model

## Usage

``` r
dataSettingsUI(id, label = "Data Settings")

dataSettings(input, output, session, colNames, inputSettings)

modelSettingsUI(id, label = "Model Settings")
```

## Arguments

- id:

  id of module

- label:

  label of settings

- input:

  shiny input object

- output:

  shiny output object

- session:

  shiny session

- colNames:

  (reactive) colnames of dataframe of the selected loaded file

- inputSettings:

  (list) list of reactive settings for input fields
