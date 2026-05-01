#### CHECKING, TESTING AND SPELLING ####

#Checking
devtools::run_examples()
devtools::check()

#Testing
#ROpenSci: Test coverage below 75% will likely require additional tests or
#explanation before being sent for review.
devtools::test()

#Spelling
# spelling::update_wordlist(pkg = ".", vignettes = FALSE, confirm = TRUE)
# spelling::get_wordlist(pkg = ".")
toto <- spelling::spell_check_package(vignettes = FALSE)
toto1 <- spelling::spell_check_package(vignettes = TRUE)

#Checking by word
toto1 <- lengths(toto$found)
names(toto1) <- toto$word
head(tail(sort(toto1), 40))

palavras <- c("microrganisms", "miliseconds",
              "servico", "servicos", "gustavobio", "irrespectively",
              "misidentifications", "modificators", "nomenclatural",
              "parallelization", "parallelize",
              "reproducibility", "unabbreviated", "uppercasing")

toto1 <- toto$found[match(palavras, toto$word)]
names(toto1) <- palavras
toto1

#Checking by file
# tail(sort(table(unlist(toto$found))), 10)
# spelling::spell_check_files("man/validateCoord.Rd")
spelling::spell_check_files("./vignettes/plantr_introduction.Rmd",
                                     lang = "en_US")
spelling::spell_check_files("./vignettes/articles/plantr_tutorial.Rmd",
                                     lang = "en_US")
spelling::spell_check_files("./vignettes/articles/atualiza_duplicatas.Rmd",
                            lang = "pt_BR")
spelling::spell_check_files("./vignettes/articles/valida_taxonomia.Rmd",
                            lang = "en_US")

#Good practices
goodpractice::gp()
goodpractice::gp(".", checks = c("rcmdcheck_tests_pass"))

# In 03/10/2021: 57% of code lines were covered by test cases
# Larger functions pending tests: checkList, formatDwc, prepDup,
#readData, rgibif2, rspeciesLink, saveData, summaryData, summaryFlags

plantr_cov <- covr::package_coverage()
# covr::to_cobertura(plantr_cov)

devtools::test_coverage_active_file("tests/testthat/test-fixSpecies.R")
devtools::test_coverage_active_file("tests/testthat/test-prepSpecies.R")
devtools::test_coverage_active_file("tests/testthat/test-formatTax.R")
devtools::test_coverage_active_file("tests/testthat/test-checkInverted.R")

covr::codecov()
covr::file_coverage(source_files = "R/fixSpecies.R",
                    test_files = "tests/testthat/test-fixSpecies.R")
covr::file_coverage(source_files = "R/cleanName.R",
                    test_files = "tests/testthat/test-cleanName.R")
covr::file_coverage(source_files = "R/prepSpecies.R",
                    test_files = "tests/testthat/test-prepSpecies.R")

## Checking non-ascii characters in the code
arquivos <- list.files("R/", full.names = TRUE)
arquivos <- arquivos[!arquivos %in% "R/sysdata.rda"]
for (i in seq_along(arquivos)) {
  cat(arquivos[i], "\n")
  non.ascii <- tools::showNonASCIIfile(arquivos[i])
}


