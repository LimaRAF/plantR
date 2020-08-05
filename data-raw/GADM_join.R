#GADM join

gadm <- list.files("./data-raw/GADM", full.names = TRUE)
names <- basename(gadm) %>% stringr::str_sub(1,3)
df <- data.frame(pais = rep("A", length(names)), nivel = 0, n = 1)
i <- 1
gadm_shp <- list()

for (i in 1:length(names)) {
  print(names[i])
  df$pais[i] <- names[i]
  lev <- stringr::str_extract(string = gadm[i], pattern = "\\d+")
  df$nivel[i] <- lev
  a <- readRDS(gadm[[i]])
  gadm_shp[[i]] <- a
  df$n[i] <- nrow(a)
}
df %>% View()
