# #### CHECKING PLANTR DEPENDENCIES ####
#
# ## which are the R base packages
# all.pckg <- installed.packages()
# base.pckg <- as.data.frame(all.pckg[ all.pckg[,"Priority"] %in% c("base","recommended"), c("Package", "Priority")])
# base.pckgs <- base.pckg[base.pckg$Priority %in% "base","Package"]
#
# ##Creating the list of plantR dependencies from package DESCRIPTION
# plantr.description <- desc::desc(package = "plantR")$get_deps()
# import <- plantr.description[plantr.description$type %in% "Imports", "package"]
# suggest <- plantr.description[plantr.description$type %in% "Suggests", "package"]
#
# # Getting the list of non-base dependencies
# deps.import <- vector("list", length(import))
# names(deps.import) <- import
# deps.suggest <- vector("list", length(suggest))
# names(deps.suggest) <- suggest
#
# for(i in 1:length(import)) {
#   #lista <- deps.import[[i]] <- sort(miniCRAN::pkgDep(import[i]))
#   lista <- sort(tools::package_dependencies(package= import[i], recursive=TRUE)[[import[i]]])
#   lista <- lista[!lista %in% base.pckgs]
#   deps.import[[i]] <- lista
# }
#
# for(i in 1:length(suggest)) {
#   #lista <- sort(miniCRAN::pkgDep(suggest[i]))
#   lista <- sort(tools::package_dependencies(package= suggest[i], recursive=TRUE)[[suggest[i]]])
#   lista <- lista[!lista %in% base.pckgs]
#   deps.suggest[[i]] <- lista
# }
#
# ## Inpsecting the package direct and recursive dependencies
# deps.import
# deps.suggest
#
# ## Checking the minimum R versions necessary for each dependency
# ap <- available.packages()
# any(!import %in% rownames(ap))
# ap[import, "Depends"]
# ap[c("devtools","sp","testthat"), "Depends"]
#
# strsplit(Sys.getenv("PATH"), ";")
#
#
# ## Localizando em quais funções estão as dependências a remover
# library(mvbutils)
# library(plantR)
# foodweb( find.funs("package:plantR"), prune="separate")
# foodweb( find.funs("package:plantR"), prune="haversine")
# foodweb( find.funs("package:plantR"), prune="kable")
# foodweb( find.funs("package:plantR"), prune="fixCase")
