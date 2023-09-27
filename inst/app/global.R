# load global variables
configFile <- system.file("config.yaml", package = "PlotR")
appConfig <- yaml::read_yaml(configFile)
