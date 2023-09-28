lst <- AcidDevTools::cacheTestFiles(
    pkg = .pkgName,
    files = "sce_lanesplit.rds"
)
cacheDir <- lst[["cacheDir"]]
rm(lst)
