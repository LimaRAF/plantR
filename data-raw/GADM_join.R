#GADM join
library(dplyr)
library(tidyr)
gadm <- list.files("./data-raw/GADM", full.names = TRUE)
codes <- basename(gadm) %>% stringr::str_sub(1,3)

countrynames <- countrycode(sourcevar = codes, origin = "iso3c", destination = 'country.name')

df <- data.frame(pais = rep("A", length(codes)), code = "ISO", nivel = 0, n = 1)

gadm_shp <- list()

for (i in 1:length(codes)) {
  print(codes[i])
  df$pais[i] <- countrynames[i]
  df$code[i] <- codes[i]
  lev <- stringr::str_extract(string = gadm[i], pattern = "\\d+")
  df$nivel[i] <- lev
  a <- readRDS(gadm[[i]])
  names(a)
  gadm_shp[[i]] <- a
  df$n[i] <- nrow(a)
}
names(gadm_shp) <- codes
df %>% View()
write.csv(df, "./data-raw/best_GADM.csv")

#cheguei até aqui e deixei tudo anotado em
# https://docs.google.com/spreadsheets/d/1vD1k4gFn5uFOCP6dznP1153vbZe-osYJMbWbeJRK1nU/edit?usp=sharing
