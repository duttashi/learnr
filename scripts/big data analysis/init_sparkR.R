library(sparklyr)
sparklyr::spark_install()
sc <- spark_connect(master = "local") # Warning message:In create_hive_context_v1(sc) :   Failed to create Hive context, falling back to SQL. Some operations, like window-functions, will not work

config <- spark_config()
config[["spark.sql.warehouse.dir"]] <- file.path(sparklyr::spark_home_dir(), "tmp", "hive")
spark_connect(master = "local", config = config)
sc
