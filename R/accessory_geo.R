#' @title Distance between Coordinates
#'
#' @description Calculate the distances between geographical coordinates, taking
#'   into account that Earth is an oblate spheroid. By default, distances are
#'   returned in kilometers.
#'
#' @param lon numerical. Longitude in decimal degrees
#' @param lat numerical. Latitude in decimal degrees
#' @param radius numerical. Radius in kilometers to find spherical distances.
#'   Default to 6371 km. (If 1 distances are returned in radians)
#'
#' @return A distance matrix
#'
#' @author Renato A. F. de Lima
#'
#' @examples
#'
#' lon = c(-47, -46, -47)
#' lat = c(-23, -24, -23.5)
#'
#' geoDist(lon, lat)
#'
#' @keywords internal
#'
#' @export
geoDist <- function(lon, lat, radius = 6371) {

  if(length(lon) != length(lat))
    stop("Longitude and latitude must have the same length")

  coslat1 <- cos((lat * pi)/180)
  sinlat1 <- sin((lat * pi)/180)
  coslon1 <- cos((lon * pi)/180)
  sinlon1 <- sin((lon * pi)/180)

  pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*%
    t(cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1))

  dists <- radius * acos(ifelse(abs(pp) > 1, 1 * sign(pp), pp))
  diag(dists) <- 0

  return(dists)
}

#' @title Minimun Distance between Coordinates
#'
#' @description The function calculates the minimum distance between coordinates
#'   or the coordinates which are below a mininum threshold distance.
#'
#' @param lon numerical. Longitude in decimal degrees
#' @param lat numerical. Latitude in decimal degrees
#' @param min.dist numerical. Minimun threshold distance (in kilometers) to be
#'   used to detect duplicated coordinates. Default to 1 meter.
#' @param output character. The type of information that should be returned (see Details)
#'
#' @details The argument `output` controls the type of output that should be
#'   returned:
#'   - 'flag': a TRUE/FALSE vector indicating the coordinates which are below
#'   `min.dist` from other coordinates.
#'   - 'group': the position of the first coordinate representing a group of
#'   duplicated coordinates.
#'   - 'dist': the distance of each coordinate to the closest coordinated.
#'
#'
#' @return A vector of TRUE/FALSE or of minimun distances in kilometers.
#'
#' @author Renato A. F. de Lima
#'
#' @seealso
#'  \link[plantR]{geoDist}
#'
#' @examples
#'
#' lat = c(-23.475389, -23.475389, -23.475390, -23.475389)
#' lon = c(-47.123768, -47.123768, -47.123768, -47.123868)
#'
#' minDist(lon, lat, output = 'group')
#' minDist(lon, lat, output = 'flag')
#' minDist(lon, lat, output = 'dist')
#'
#' @keywords internal
#'
#' @export
minDist <- function(lon, lat, min.dist = 0.001, output = NULL) {

  if(length(lon) != length(lat))
    stop("Longitude and latitude must have the same length")

  if (is.null(output))
    stop("Please chose between one of the three types of output", call. = FALSE)

  dists <- geoDist(lon, lat)

  if (output == 'flag') {

    neighbors <- as.matrix(which(dists <= min.dist, arr.ind = TRUE))
    dups <- !duplicated(neighbors[, 2])
    return(duplicated(neighbors[dups, ][, 1]))

  }

  if (output == 'group') {

    neighbors <- as.matrix(which(dists <= min.dist, arr.ind = TRUE))
    dups <- !duplicated(neighbors[, 2])
    return(neighbors[dups, ][, 1])

  }

  if (output == 'dist') {

    diag(dists) <- NA
    return(apply(dists, 1, min, na.rm = TRUE))

  }
}
#'
#' @title Calculate Squared Mahalanobis Distances
#'
#' @param lon numerical. Longitude in decimal degrees
#' @param lat numerical. Latitude in decimal degrees
#' @param method character. Type of method desired: 'classic' and/or 'robust'
#'   (see Details)
#' @param center character. Which metric should be used to obtain he center of
#'   the distribution of coordinates: 'mean' or 'median'? Default to 'mean'.
#' @param n.min numerical. Minimun number of unique coordinates to be used in
#'   the calculations. Default to 10
#' @param digs numerical. Number of digits to be returned after the decimal point.
#' Default to 4
#'
#' @return the input data frame and a new column(s) with the distances obtained
#'   using the selected method(s)
#'
#' @details Two possible methods to calculate the Mahalanobis distances are
#'   available: the classic (`method`= 'classic') and the robust methods
#'   (`method`= 'robust'). The two methods take into account the geographical
#'   center of the coordinates distribution and the spatial covariance between the
#'   records. But they vary in the way the covariance matrix of the
#'   distribution is defined: the classic method uses an approach based on
#'   Pearson’s method, while the robust method uses a Minimum Covariance
#'   Determinant (MCD) estimator.
#'
#'   The argument `n.min` controls the minimum number of unique coordinates
#'   necessary to calculate the distances. The classic and robust methods needs
#'   at least 3 and 4 spatially unique coordinates to obtain the distances. But
#'   the MCD algorithm of the robust method can run into singularity issues
#'   depending on how close the coordinates are. This issue can result in
#'   the overestimation of the distances and thus in bad outlier flagging. A
#'   minimum of five and ideally 10 unique coordinates should avoid those
#'   problems.
#'
#'   If the MCD algorithm runs into singularity issues, the function silently
#'   add some random noise to both coordinates and re-run the MCD algorithm.
#'   This aims to deals with cases of few coordinates close to each other and
#'   in practice should not change the overall result of the detection of spatial
#'   outliers.
#'
#'   The function internally removes spatially duplicated coordinates previous
#'   to the calculation of the Mahalanobis distances. So, the value in `n.min`
#'   correspond to the number of coordinates after the removal of spatially
#'   duplicated coordinates.
#'
#' @importFrom dplyr left_join
#' @importFrom stats mahalanobis
#' @importFrom robustbase covMcd
#'
#' @examples
#' lon <- c(-42.2,-42.6,-45.3,-42.5,-42.3,-39.0,-12.2)
#' lat <- c(-44.6,-46.2,-45.4,-42.2,-43.7,-45.0,-8.0)
#'
#' mahalanobisDist(lon, lat, method = "classic", n.min = 1)
#' mahalanobisDist(lon, lat, method = "robust", n.min = 1)
#'
#' @seealso
#'  \link[plantR]{uniqueCoord}.
#'
#' @author Renato A. Ferreira de Lima
#'
#' @export
mahalanobisDist <- function(lon, lat, method = NULL, n.min = 5, digs = 4,
                            center = "mean") {

  if(length(lon) != length(lat))
    stop("Longitude and latitude must have the same length")

  if (is.null(method))
    stop("Please choose between classic and robust methods")

  if (!method %in% c("classic", "robust"))
    stop("Please choose between classic and robust methods")

  df <- cbind.data.frame(lon = as.double(lon),
                         lat = as.double(lat),
                         tmp.ordem = 1:length(lon))

  tmp <- suppressWarnings(uniqueCoord(df,
                                      lon = "lon", lat = "lat",
                                      type = c("exact"),
                                      output = c("group", "flag")))
  df$dup.coord.ID <- tmp$exact.ID
  df1 <- df[!tmp$exact, ]

  if (dim(df1)[1] < n.min) {
    res <- as.double(rep(NA_character_, dim(df)[1]))
  } else {

    if (method == "classic") {
      covar <- stats::cov(df1[,1:2])
      if (center == "mean")
        centro <- colMeans(df1[,1:2])
      if (center == "median")
        centro <- apply(df1[,1:2], 2, stats::median)

      res0 <- cbind.data.frame(dup.coord.ID = df1$dup.coord.ID,
                               res = sqrt(stats::mahalanobis(df1[,1:2],
                                                             center = centro,
                                                             cov = covar)))

      #Putting data back in the original order
      res1 <- dplyr::left_join(df, res0, by = "dup.coord.ID")
      res1 <- res1[order(res1$tmp.ordem), ]
      res <- as.double(res1$res)
    }

    if (method == "robust") {

      rob <- suppressWarnings(try(robustbase::covMcd(df1[, 1:2], alpha = 1 / 2), TRUE))
      if (class(rob) == "try-error") {
        df1$lon2 <- jitter(df1$lon, factor = 0.001)
        df1$lat2 <- jitter(df1$lat, factor = 0.001)
        rob <- robustbase::covMcd(df1[, c("lon2", "lat2")], alpha = 1 / 2, tol = 1e-20)
        res0 <- cbind.data.frame(dup.coord.ID = df1$dup.coord.ID,
                                 res = sqrt(stats::mahalanobis(df1[,c("lon2", "lat2")],
                                                               center = rob$center, cov = rob$cov, tol=1e-20)))
      } else {
        if (length(rob$singularity) > 0) {
          df1$lon2 = jitter(df1$lon, factor = 0.005)
          df1$lat2 = jitter(df1$lat, factor = 0.005)
          rob <- robustbase::covMcd(df1[, c("lon2", "lat2")], alpha = 1/2, tol=1e-20)
          res0 <- cbind.data.frame(dup.coord.ID = df1$dup.coord.ID,
                                   res = sqrt(stats::mahalanobis(df1[,c("lon2", "lat2")],
                                                                 center = rob$center, cov = rob$cov, tol=1e-20)))
        } else {
          res0 <- cbind.data.frame(dup.coord.ID = df1$dup.coord.ID,
                                   res = sqrt(stats::mahalanobis(df1[,c("lon", "lat")],
                                                                 center = rob$center, cov = rob$cov, tol=1e-20)))
        }
      }

      #Putting data back in the original order
      res1 <- dplyr::left_join(df, res0, by = "dup.coord.ID")
      res1 <- res1[order(res1$tmp.ordem), ]
      res <- as.double(res1$res)
    }
  }
  return(round(res, digits = digs))
}
#'
#' @title Outlier Mahalanobis Distances
#'
#' @param lon numerical. Longitude in decimal degrees
#' @param lat numerical. Latitude in decimal degrees
#' @param method character. Type of method desired: 'classic' or 'robust'
#' @param n.min numerical. Minimun number of unique coordinates to be used in
#'   the calculations. Default to 10
#' @param digs numerical. Number of digits to be returned after the decimal point.
#' Default to 4
#' @param probs numerical. Vector of probabilities between 0 and 1 to calculate
#' the sample quantiles. Defaul to c(0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 100)
#'
#' @return the number of unique coordinates ('n'), the number of outliers
#'   detected ('n.out') and the sample quantiles ('qt') of the Mahalanobis
#'   distances.
#'
#' @details The function returns the quantiles of the Mahalanobis distances for
#' the spatial outliers detected automatically, which can be used in the decision making of
#' the more appropriated distance cutoffs to flag spatial outliers.
#'
#' The automatic detection of spatial outliers is based on an adjusted threshold
#' of the Mahalanobis distances using package `mvoutlier` and its defaults
#' (function `mvoutlier::arw()`).
#'
#' If the number of unique coordinates is below `n.min` or if the Minimum
#' Covariance Determinant (MCD) estimator has issues, the function returns NAs.
#' See the help of function `mahalanobisDist()` for details on the other
#' parameters.
#'
#' @importFrom stats mahalanobis
#' @importFrom robustbase covMcd
#' @importFrom mvoutlier arw
#'
#' @examples
#' set.seed(123)
#' lon <- runif(100, -45, -35)
#' lat <- runif(100, -35, -25)
#'
#' distOutlier(lon, lat, method = "classic") # no outliers found...
#' distOutlier(lon, lat, method = "robust")
#'
#' # adding some noise
#' lat[1:5] <- lat[1:5] + runif(5, 10, 20)
#' lon[96:100] <- lon[96:100] + runif(5, 10, 20)
#'
#' distOutlier(lon, lat, method = "classic") # quantiles for the 10 outliers found
#' distOutlier(lon, lat, method = "robust")
#'
#' @seealso
#'  \link[plantR]{mahalanobisDist}.
#'
#' @author Renato A. Ferreira de Lima
#'
#' @export
#'
distOutlier <- function(lon, lat, method = "robust",
                        n.min = 10, digs = 3,
                        probs = c(0.5, 0.75, 0.9, 0.95, 0.975, 0.99, 1)) {

  ## Check input
  if(length(lon) != length(lat))
    stop("Longitude and latitude must have the same length")

  if (is.null(method))
    stop("Please choose between classic and robust methods")

  if (any(!method %in% c("classic", "robust")))
    stop("Please choose between classic and robust methods")

  ## Preparing the data
  df <- cbind.data.frame(lon = as.double(lon),
                         lat = as.double(lat),
                         tmp.ordem = 1:length(lon))

  tmp <- suppressWarnings(uniqueCoord(df,
                                      lon = "lon", lat = "lat",
                                      type = c("exact"),
                                      output = c("group", "flag")))
  df$dup.coord.ID <- tmp$exact.ID
  df1 <- df[!tmp$exact, ]

  ## Calculating the distances
  if (dim(df1)[1] < n.min) {
    res <-
      c(dim(df1)[1], 0, rep(NA_character_, length(probs) + 1))
  } else {

    rob <- suppressWarnings(try(robustbase::covMcd(df1[, 1:2], alpha = 1 / 2), TRUE))
    if (class(rob) == "try-error") {
      res <-
        c(dim(df1)[1], 0, rep(NA_character_, length(probs) + 1))
    } else {
      if (length(rob$singularity) > 0) {
        res <-
          c(dim(df1)[1], 0, rep(NA_character_, length(probs) + 1))
      } else {
        distcla <- sqrt(stats::mahalanobis(df1[, 1:2],
                                           center = apply(df1[, 1:2], 2, mean), cov = stats::cov(df1[, 1:2])))
        distrob <- sqrt(stats::mahalanobis(df1[, 1:2],
                                           center = rob$center, cov = rob$cov))

        xarw <- mvoutlier::arw(df1[, 1:2], rob$center, rob$cov, alpha = 0.025)
        if (xarw$cn != Inf) {
          alpha <- sqrt(c(xarw$cn, stats::qchisq(c(0.75, 0.5, 0.25), ncol(df1))))
        } else {
          alpha <- sqrt(stats::qchisq(c(0.975, 0.75, 0.5, 0.25), ncol(df1)))
        }

        out <- (distrob > min(sqrt(xarw$cn),
                              sqrt(stats::qchisq(0.975, dim(df1[, 1:2] )[2]))))

        qs.rob <- stats::quantile(distrob[out], prob = probs)
        qs.cla <- stats::quantile(distcla[out], prob = probs)
        res.rob <- c(dim(df1)[1],
                     sum(out),
                     #round(alpha[1], digs),
                     round(qs.rob, digs))
        res.cla <- c(dim(df1)[1],
                     sum(out),
                     #round(alpha[1], digs),
                     round(qs.cla, digs))
        if (method == "classic")
          res <- res.cla
        if (method == "robust")
          res <- res.rob
      }
    }
  }
  names(res) <- c("n", "n.out", paste0("qt", 100 * probs))
  return(res)
}
