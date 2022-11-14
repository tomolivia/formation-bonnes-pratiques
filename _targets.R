# _targets.R file
library(targets)
source("R/functions_checkpoint.R")
tar_option_set(packages = c("arrow", "dplyr", "ggplot2","rlang","forcats","MASS","gt","aws.s3","readr","yaml"))
list(
  tar_target(path_data, "individu_reg.parquet", format = "file"),
  tar_target(path_apikey, "secrets.yaml", format = "file"),
  tar_target(survey_sample_24, read_from_parquet(path_data),format="parquet"),
  tar_target(pwd_api, read_yaml_secret(path_apikey))
)
