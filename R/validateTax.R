#' @title Validate Specimen Taxonomy
#'
#' @description This function ...
#'
#' @param x Character. Species name
#'
validateTax = function(x) {

  #Getting the dictionaries
  autores <- plantR:::autores
  families.apg <- plantR:::families_synonyms
  autores <- merge(autores,families.apg[,c("name","name.correct")],by.x="family",by.y="name",all.x=TRUE)
  autores <- autores[order(autores$order),]


  return(x)
}
