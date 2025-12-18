
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plantR

<img src="https://raw.githubusercontent.com/liibre/plantR_logo/master/figs/plantR_logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R-CMD-check:
master](https://github.com/LimaRAF/plantR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LimaRAF/plantR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://github.com/LimaRAF/plantR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/LimaRAF/plantR/actions/workflows/test-coverage.yaml)
<!-- [![R-CMD-check: dev](https://github.com/LimaRAF/plantR/actions/workflows/R-CMD-check.yaml/badge.svg?branch=dev)](https://github.com/LimaRAF/plantR/actions/workflows/R-CMD-check.yaml?branch=dev) -->
<!-- badges: end  -->

An R Package for Managing Species Records from Biological Collections

### Description

The package plantR provides tools for downloading, processing, cleaning,
validating, summarizing and exporting records of species occurrences
from biological collections.

Please read the package
[Introduction](https://github.com/LimaRAF/plantR/blob/dev/vignettes/plantR.pdf)
and detailed
[Tutorial](https://github.com/LimaRAF/plantR/blob/dev/vignettes/articles/plantR_tutorial.pdf)
for more details.

#### Installation

The package can be installed in R from [github](https://github.com/)
with:

``` r
library("remotes")
install_github("LimaRAF/plantR")
library("plantR")
```

You can also download the development (and probably most up-to-date)
version of the package with:

``` r
install_github("LimaRAF/plantR", ref = "dev")
library("plantR")
```

If you run into errors while installing the package, please check the
detailed package introduction for alternatives.

##### Bug report and suggestions

The plantR project is hosted on
[GitHub](https://github.com/LimaRAF/plantR/). Please report any bugs and
suggestions of improvements for the package
[here](https://github.com/LimaRAF/plantR/issues).

The package gazetteer and the list of taxonomists are constantly being
improved. If you want to contribute with regional gazetteers or with
missing names of taxonomists, please e-mail <raflima@usp.br>.

### Authors and contributors

Renato A. F. de Lima, Sara R. Mortara, Andrea Sánchez-Tapia, Guilherme
S. Grittz, Mali Oz Salles, Hans ter Steege & Marinez F. de Siqueira

### Citation

Lima, R.A.F., Sánchez-Tapia, A., Mortara, S.R., ter Steege, H.,
Siqueira, M.F. (2023). *plantR*: An R package and workflow for managing
species records from biological collections. Methods in Ecology and
Evolution 14(2): 332-339. <https://doi.org/10.1111/2041-210X.13779>

### Funding

The development of this package was supported by the European Union’s
Horizon 2020 research and innovation program under the Marie
Skłodowska-Curie grant agreement No 795114, by the Coordination for the
Improvement of Higher Education Personnel (CAPES, process
88887.145924/2017-00), and by the ‘Instituto Nacional da Mata Atlântica’
(INMA). The improvement of the package internal dictionaries was
supported by the INCT Synthesis for Biodiversity in Amazonia
(CNPq/MCTIC/INCT-2022, process No 406767/2022-0).

### Acknowledgements

We thank Sidnei Souza from CRIA/speciesLink for his help with the web
API. We also thank the [CNCFlora](http://cncflora.jbrj.gov.br) and the
[TreeCo
database](http://labtrop.ib.usp.br/doku.php?id=projetos:treeco:start)
for providing many of the localities used to construct the package
gazetteer. We thank the Harvard University Herbarium, Brazilian Herbaria
Network, American Society of Plant Taxonomists and the Taxonomic
Catalogue of the Fauna of Brazil, who were main sources to compile the
current list of taxonomists. We also thank Vinícius C. Souza
(ESALQ/USP), who helped to validate and improve the list of plant
taxonomists used in the package, João Vieira for suggestions of code
fixes and updates, and André L. de Gasper and Leila Meyer, for their
valuable suggestions on how to make this package more useful and
flexible for collection managers, taxonomists and ecologists. We are
greatly in debt to Eduardo Pinto and André Montanari, who greatly
improved the quantity and quality of the dictionaries available in the
package.
