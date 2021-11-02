#### EXAMPLES TO TEST FUNCTIONS FORMATTING NAMES ####
# names <- c("Al Gentry", "Alwyn Howard Gentry",
#            "Gert G. Hatschbach", "HATSCHBACH, G.G.", "HATSCHBACH, G. G.", "G. G. HATSCHBACH","Gert G. Hatschbach, et al.",
#           "Carl Friedrich Philipp von Martius", "Alphonse Louis Pierre Pyrame de Candolle",
#           "Simon Jan van Ooststroom", "Jan van der Hoeven", "Maria da Silva",
#           "Silva, M. da", "da Silva, M.", "Hermogenes Leitao Filho", "Leitão F°, H.F.",
#           "Pe. Raulino Reitz", "Prof. Hermogenes de Freitas Leitao Filho",
#           "Leitao Filho, H.", "Leitao Filho, H.F.", "Leitao Filho, HF", "S.J. Filho Neto",
#           "Hermogenes de Freitas Leitao Filho", "H.F. Leitao Filho", "Leitao filho, H. de F.",
#           "Augustin Saint-hilaire", "Saint-Hilaire A.", "Augustin Saint Hilaire",
#           "Augustin Saint-Hilaire", "A. Saint-Hilaire", "Saint-Hilaire, Augustin",
#           "Auguste de Saint-Hilaire", "Auguste Saint-Hilaire",
#           "[D. Hugh-Jones]", "Cyl Farney Catarino de Sa", "Cyl Sa", "Sa, Cyl",
#           "L. McDade", "Cesar Sandro, Esteves, F", "Mendonca Filho, C.V.; Neto, F.C.C.",
#           "A. Alvarez, A. Zamora & V. Huaraca", "Karl Emrich & Balduino Rambo", '( Karl) Emrich ;(Balduino ) Rambo',
#           "R. Reitz; R.M. Klein", "Raulino Reitz; R.M. Klein","Alwyn H. Gentry|Elsa M. Zardini",
#           "Gert G. Hatschbach", "Leitão Filho, H.F.; Shepherd, G.J.", "Leitao Filho, HF",
#           "A. Ducke; Dárdano de Andrade-Lima", "A. Alvarez; A. Zamora & V. Huaraca",
#           "Cesar Sandro, Esteves, F", "Ducke, A.; Dárdano de Andrade-Lima", "Gentry", "Gentry, AH",
#           "GENTRY, A H", "Gentry AH", "GENTRY A.H.", "Silva, Maria A. Pereira da",
#           "Silva, Maria A. Pereira Da", "da Silva, Maria A. Pereira", "ter Braak, Hans",
#           "Braak, Hans ter", "Silva, Maria A. Pereirada", "Braak, Hanster", "Maria A. Pereira da Silva",
#           "Hans ter Braak", "Maria A. Pereirada Silva", "Hanster Braak", "da Silva",
#           "Silva", "DA SILVA", "HANS TER STEEGE", "SILVA, DA","TER STEEGE, HANS",
#           "Renato de la Barra", "Leitão Filho, H.F.;Shepherd, G.J.",
#           "Gentry A.H.", "Leitao filho, H. F.", "Saint-Hilaire, augustin", "H. F. Leitao filho",
#           "Filho Neto, S.J.", "Neto, F.C.C.", "Neto", "a.h. gentry", "a. h. gentry", "a. h. Gentry",
#           "A. H. GENTRY","saint-hilaire", "J.E.Q. Faria Jr.",
#           "F.daS.N.Thomé", 'F. da S.N. Thomé', 'Pedro L.R.de Moraes (30/4/1998)',
#           "Silva, DA", "Silva, Da", "Silva, da", "Silva, D.A.",
#           "Silva, D. A.", "Silva, Daniela A.", "DA Silva",
#           "Silva, D A", "Silva, D A M", "Silva, D", "D A M Silva",
#           "D.A. Silva", "D. A. Silva", "Daniela A. Silva",
#           "Filho, E L P", "Filho, E. L. P.", "Filho, E.L.P.",
#           "Filho, Elton P.", "Filho, Elton Pereira", "Filho, E. Pereira",
#           "E L P Filho", "E. L. P. Filho", "P Filho",
#           "Elton P. Filho", "E. P. Filho",
#           "Elton Pereira Filho", "E. Pereira Filho", "E.L. Pereira Filho",
#           "gentry", "Alwyn", "GENTRY", "Alwyn H. Gentry",  "A. Gentry",
#           "A. H. Gentry",  "A.H.Gentry", "A H Gentry",
#           "Gentry, Alwyn Howard", "gentry, a.h.", "gentry, a. h.",
#           "ALWYN HOWARD GENTRY", "GENTRY, ALWYN HOWARD",
#           "AHG",  "alwyn", "ALWYN",
#           "John MacDonald", "John McDonald", "John Mc Donald",
#           "John O'Brien", "John O' Brien", "John O'Reilly",
#           "AL", "Carl F. P. von Martius",  #takes name preposition as name
#           "AH gentry",  "Gentry, A.",  # ignores comma
#           "G., Alwyn",  "Ah. Gentry",  # problems
#           "M. Nadruz; ,J.F. Baumgratz, M. Bovini, D.S.P. Silva",
#           "M. A. Costa, J. Ribeiro, E. C. Pereira",
#           "M. A. Costa|J. Ribeiro|E. C. Pereira",
#           "M. A. Costa, J. Ribeiro & E. C. Pereira",
#           "M. A. Costa, Ribeiro, J., E. C. Pereira",
#           "M. A. Costa, Ribeiro, J. O., E. C. Pereira",
#           "M. A. Costa, Ribeiro, J.O., E. C. Pereira",
#           "Costa M. A., Ribeiro J., E. C. Pereira",
#           "Costa, M. A., Ribeiro J., E. C. Pereira",
#           "Costa, M. A., Ribeiro, J.",
#           "J. F. Morales Quirós; Michael H. Grayum ; Maria M. Chavarría",
#           "Tiritan, O; Paiva, M", "Costa, M. A., Ribeiro J., E. C. Pereira",
#           "Souza 35", "15/04/2015", "", NA)


# Comparing the outputs of the new and previous version
# prep.status <- FALSE
# init.status <- FALSE
# pretty.status <- TRUE
# tmp <- cbind(names, new = prepTDWG(names, get.prep = prep.status, get.initials = init.status))
# tmp1 <- cbind(tmp, old = plantR::prepTDWG(names, get.prep = prep.status, get.initials = init.status))
# rownames(tmp1) <- NULL
# tmp1[!tmp1[,2] == tmp1[,3],]

# Comparing the speed of the new and previous version
# x1 <- rep(unique(c(toto, name)),1000)
# prep.status <- TRUE
# init.status <- FALSE
# pretty.status <- FALSE
# system.time(prepTDWG(x1, get.prep = prep.status, get.initials = init.status, pretty = pretty.status))
# system.time(prepTDWG.old(x1, get.prep = prep.status, get.initials = init.status))


# tmp <- cbind(names, old = plantR::getInit(names))
# tmp1 <- cbind(tmp, new = getInit(names))
# rownames(tmp1) <- NULL
# tmp1[!tmp1[,2] == tmp1[,3],]
#
# tmp <- cbind(names, sapply(names, tdwgNames))
# tmp1 <- cbind(tmp, prepName(names, sep.out = " | "))
# rownames(tmp1) <- NULL
# tmp1[!tmp1[,2] == tmp1[,3],]
#
# names1 <- rep(names, 10000)
# #adding noise for testing
# names2 <- paste0(names1, stri_rand_strings(length(names1), 1, pattern = "[a-z]"))
