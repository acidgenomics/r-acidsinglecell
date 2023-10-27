lst <- AcidDevTools::cacheTestFiles(
    pkg = "AcidSingleCell",
    files = "sce_lanesplit.rds"
)
cacheDir <- lst[["cacheDir"]]
rm(lst)
