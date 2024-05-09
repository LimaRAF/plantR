#### GERANDO OS PDFs e HTMLs A PARTIR DAS VINHETAS E TUTORIAIS ####

#### VIGNETTE ALL FORMATS ####
rmarkdown::render('./vignettes/plantR.Rmd', "all")

#### PDFs ####
## Criando os PDFs a partir do Rmd
# rmarkdown::render('./vignettes/plantr_introduction.Rmd',
#                   rmarkdown::pdf_document(
#                     toc = TRUE, toc_depth = 3, number_sections = TRUE,
#                     dev = "pdf", df_print = "default", highlight = "default",
#                     template = "default", keep_md = FALSE,
#                     latex_engine = "pdflatex",
#                     citation_package = c("default", "natbib", "biblatex")),
#                   output_file = 'plantr_introduction', envir = new.env())
rmarkdown::render('./vignettes/articles/plantr_tutorial.Rmd',
                  rmarkdown::pdf_document(
                    toc = TRUE, toc_depth = 3, number_sections = TRUE,
                    dev = "pdf", df_print = "default", highlight = "default",
                    template = "default", keep_md = FALSE,
                    latex_engine = "pdflatex",
                    citation_package = c("default", "natbib", "biblatex")),
                  output_file = 'plantR_tutorial', envir = new.env())
rmarkdown::render('./vignettes/articles/update_duplicates_pt.Rmd',
                  rmarkdown::pdf_document(
                    toc = TRUE, toc_depth = 3, number_sections = TRUE,
                    dev = "pdf", df_print = "default", highlight = "default",
                    template = "default", keep_md = FALSE,
                    latex_engine = "pdflatex",
                    citation_package = c("default", "natbib", "biblatex")),
                  output_file = 'atualiza_duplicatas', envir = new.env())

#### HTMLs ####
## Criando os HTMLs a partir do Rmd
#por enquanto criado os HTMLs no próprio arquivo, para ter o template correto
# rmarkdown::render('./vignettes/articles/plantr_tutorial.Rmd',
#                   rmarkdown::html_document(
#                     toc = TRUE, toc_depth = 3, toc_float = FALSE,
#                     number_sections = TRUE, anchor_sections = FALSE,
#                     section_divs = TRUE,
#                     dev = "png", df_print = "default",
#                     code_folding = c("none", "show", "hide"),
#                     code_download = FALSE,  self_contained = TRUE,
#                     theme = "default", highlight = "default",
#                     mathjax = "default", template = "default",
#                     keep_md = FALSE),
#                   output_file = 'plantR_tutorial', envir = new.env())

#### PARA TESTAR OS ARGUMENTOS RAPIDAMENTE ####
# rmarkdown::render('./vignettes/articles/teste.Rmd',
#                   rmarkdown::pdf_document(
#                     toc = TRUE, toc_depth = 3, number_sections = TRUE,
#                     dev = "pdf", df_print = "default", highlight = "default",
#                     template = "default", keep_md = FALSE,
#                     latex_engine = "pdflatex",
#                     citation_package = c("default", "natbib", "biblatex")),
#                   output_file = 'teste', envir = new.env())
#
# rmarkdown::render('./vignettes/articles/teste.Rmd',
#                   rmarkdown::html_document(
#                     toc = TRUE, toc_depth = 3, toc_float = FALSE,
#                     number_sections = TRUE, anchor_sections = FALSE,
#                     section_divs = TRUE,
#                     dev = "png", df_print = "default",
#                     code_folding = c("none", "show", "hide"),
#                     code_download = FALSE,  self_contained = TRUE,
#                     theme = "default",
#                     highlight = "default",
#                     mathjax = "default",
#                     template = "default",
#                     keep_md = FALSE),
#                   output_file = 'teste', envir = new.env())
