#  PlotR 24.12.0

## Bug Fixes
- require most recent version of `DataTools` which contains a fix for missing filenames of imported
  files (#47)

#  PlotR 24.04.0

## New Features
- add pkgdown documentation via gh-actions
- add r-cmd check via gh-actions
- rm rgpt3 from remotes package
- Add Pandora drat Repo to .Rprofile

## Bug Fixes
- Fix test in test-downAndUploadModel.R: remove year

# PlotR 23.12.0

## New Features
- _Import of models_: display of "About" information that is associated to a selected Pandora 
  Repository

# PlotR 23.09.0

## New Features
- _Import of models_:
  - option to import models from Pandora platform
  
# PlotR 23.08.1

## New Features
- New UI tab with AIC and BIC information on models + plots

# PlotR 23.08.0

## Bug Fixes
- fixes of "Predict Data" in the tab "Multible Predictions" (#40)
  - fixing the update of the column choices when using uploaded data
  - removing NA values for uploaded data

# PlotR 23.05.2

## New Features
- data export contains quantile columns, the value of the selected quantile was added to those column names (#2)

# PlotR 23.05.1

## New Features
- option to add bands between prediction uncertainty lines (#1)

# PlotR 23.05.0

## New Features
- adds a legend to the all-in-one plot (#3)

# PlotR 23.03.4

## Updates
- warning after modeling if the sample size is below 8 (#24)

# PlotR 23.03.3

## New Features
- option to load remote models from the github folder `inst/app/predefinedModels` of the respective 
repository
- if there is no internet connection remote models are taken from the models that were saved with
  the last deployed app version
- option to save only user inputs and data without the model output
- option to plot data before/without running the model
  
## Updates
- update sidebar widths

## Bug fixes
- if the name of an uploaded plot already exists the name of the new plot is updated, thus rendering
of plots is triggered correctly

# PlotR 23.03.2

## Updates
- tighten prior for sigma (#24)

# PlotR 23.03.1

## Bug fixes
- add remote package to enable gpt3 in the _Import Data_ module

# PlotR 23.01.2

## New Features
- the _Import Data_ module is now imported from the new package DataTools (#22, PR #23)
  - additionally to file import, now import from _URL_ or from _Pandora Platform_ is possible
  - all redundant code was removed
  - use "file" as default source in _Import Data_
- now, sidebars are fixed with auto scroll in all tabs (iso-app #4)

# PlotR 23.01.1

## New Features
- show significant differences of the mean between a reference plot and one (or more) other 
plots (#19)

# PlotR 22.12.1

## Updates
- added placeholders to several select inputs as long as data is missing (#14)
- rename tab "File Upload(s)" to "File Import(s)" for more consistency with the placeholders
- tab "Style plot": add numeric inputs to sliders for the plot ranges (#13)

# PlotR 22.11.1

## New Features
- button to export the plot in "Run Model" tab (#9) and in "Post Processing" tab

## Updates
- in the "Run Model" tab:
  - change the default value of "Select a file" to the last uploaded file (not the first upload) (#6)
  - add a load button to load a selected file (#11)
- update the UI:
  - remove right sidebar, 
  - integrate export buttons into main panel,
  - in "Run model tab" load plot inputs directly above the plot view

## Bug Fixes
- reset column selection and model parameters after selecting a new file (#6)
- transform columns to numeric columns when running a model (#8), all NA's are removed automatically
- fix wrong default values for width and height when exporting a plot (#9, #10)
- fix `tryCatch` when fitting a model, error message is now forwarded correctly to a pop up (#9)
- fix import/upload of files with the same name (#11)
