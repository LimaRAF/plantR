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

#Checking by word
toto1 <- lengths(toto$found)
names(toto1) <- toto$word
head(tail(sort(toto1), 40))

palavras <- c("coden","Curitiba’","numb'and"
)
toto2 <- toto$found[match(palavras, toto$word)]
names(toto2) <- palavras
toto2

#Checking by file
# tail(sort(table(unlist(toto$found))), 10)
# spelling::spell_check_files("man/validateCoord.Rd")


#Good practices
goodpractice::gp()
