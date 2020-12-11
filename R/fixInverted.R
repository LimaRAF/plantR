#' Fixes inverted coordinates according to checkInverted
#'
#' @param x Data frame with species occurrences
#'
#' @importFrom dplyr left_join
#'
fixInverted <- function(x = occs) {
  x$decimalLatitude.new.new <- x$decimalLatitude.new
  x$decimalLongitude.new.new <- x$decimalLongitude.new
  fix_these <- x[!is.na(x$inv.check) & x$inv.check != "original",]
  fix_these$decimalLatitude.new.new[fix_these$inv.check %in% c("inverted_lat", "inverted_both")] <-
    fix_these$inv_lat[fix_these$inv.check %in% c("inverted_lat", "inverted_both")]

  fix_these$decimalLongitude.new.new[fix_these$inv.check %in% c("inverted_lon", "inverted_both")] <- fix_these$inv_lon[fix_these$inv.check %in% c("inverted_lon", "inverted_both")]

  fix_these$decimalLongitude.new.new[fix_these$inv.check %in% c("transposed")] <-
    fix_these$decimalLatitude.new[fix_these$inv.check %in% c("transposed")]
  fix_these$decimalLatitude.new.new[fix_these$inv.check %in% c("transposed")] <-
    fix_these$decimalLongitude.new[fix_these$inv.check %in% c("transposed")]

  fix_these$decimalLatitude.new.new[fix_these$inv.check %in% c("transposed_inv_lat")] <-
    fix_these$decimalLongitude.new[fix_these$inv.check %in% c("transposed_inv_lat")]
  fix_these$decimalLongitude.new.new[fix_these$inv.check %in% c("transposed_inv_lat")] <-
    fix_these$inv_lat[fix_these$inv.check %in% c("transposed_inv_lat")]

  fix_these$decimalLatitude.new.new[fix_these$inv.check %in% c("transposed_inv_lon")] <-
    fix_these$inv_lon[fix_these$inv.check %in% c("transposed_inv_lon")]
  fix_these$decimalLongitude.new.new[fix_these$inv.check %in% c("transposed_inv_lon")] <-
    fix_these$decimalLatitude.new[fix_these$inv.check %in% c("transposed_inv_lon")]

  fix_these$decimalLatitude.new.new[fix_these$inv.check %in% c("transposed_inv_both")] <-
    fix_these$inv_lon[fix_these$inv.check %in% c("transposed_inv_both")]
  fix_these$decimalLongitude.new.new[fix_these$inv.check %in% c("transposed_inv_both")] <-
    fix_these$inv_lat[fix_these$inv.check %in% c("transposed_inv_both")]
  y <- left_join(x, fix_these)
  return(y)
}
