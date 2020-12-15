#' Fixes inverted coordinates according to checkInverted
#'
#' @param x Data frame with species occurrences
#'
#' @importFrom dplyr left_join if_else
#'
#' @author Andrea Sánchez-Tapia & Sara Mortara
#'
#' @export
fixInverted <- function(x = occs) {
  fix_these <- x[!is.na(x$inv.check) & !x$inv.check %in% "original",]

  fix_these[fix_these$inv.check %in% c("inverted_lat", "inverted_both"), "decimalLatitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("inverted_lat", "inverted_both"), "inv_lat"]

  fix_these[fix_these$inv.check %in% c("inverted_lat"), "decimalLongitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("inverted_lat"), "decimalLongitude.new"]

  fix_these[fix_these$inv.check %in% c("inverted_lon", "inverted_both"),
            "decimalLongitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("inverted_lon", "inverted_both"), "inv_lon"]

  fix_these[fix_these$inv.check %in% c("inverted_lon"), "decimalLatitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("inverted_lon"), "decimalLatitude.new"]

  fix_these[fix_these$inv.check %in% c("transposed"), "decimalLongitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("transposed"), "decimalLatitude.new"]

  fix_these[fix_these$inv.check %in% c("transposed"), "decimalLatitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("transposed"), "decimalLongitude.new"]

  fix_these[fix_these$inv.check %in% c("transposed_inv_lat"), "decimalLatitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("transposed_inv_lat"), "decimalLongitude.new"]

  fix_these[fix_these$inv.check %in% c("transposed_inv_lat"), "decimalLongitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("transposed_inv_lat"), "inv_lat"]

  fix_these[fix_these$inv.check %in% c("transposed_inv_lon"), "decimalLatitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("transposed_inv_lon"), "inv_lon"]

  fix_these[fix_these$inv.check %in% c("transposed_inv_lon"), "decimalLongitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("transposed_inv_lon"), "decimalLatitude.new"]

  fix_these[fix_these$inv.check %in% c("transposed_inv_both"), "decimalLatitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("transposed_inv_both"), "inv_lon"]

  fix_these[fix_these$inv.check %in% c("transposed_inv_both"), "decimalLongitude.new.new"] <-
    fix_these[fix_these$inv.check %in% c("transposed_inv_both"), "inv_lat"]
  y <- left_join(x, fix_these)
  #recover original coordinates
  y$decimalLongitude.new.new <- if_else(is.na(y$decimalLongitude.new.new),
                                        y$decimalLongitude.new, y$decimalLongitude.new.new)
  y$decimalLatitude.new.new <- if_else(is.na(y$decimalLatitude.new.new),
                                       y$decimalLatitude.new, y$decimalLatitude.new.new)
  return(y)
}
