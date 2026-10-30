source("R/etl.R")
source("R/db.R")
source("R/pipeline.R")

print(as.data.frame(build_database()))
