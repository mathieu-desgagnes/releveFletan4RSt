#' Lire la base de données des conditions d'hamecons remontés. Effectue une uniformisation des abréviations des espèces vers le francais.
#' Les abbréviations attendues sont a=appât, b=bait, c=cod, e=empty, f=flétan, h=halibut, i=???, m=morue, r=redfish, s=sébastes, t=turbot, v=vide, x=autres.
#' À FAIRE: ajouter les entrées pour 3Pn
#' À FAIRE: valider les "stations supplémentaires", soit des stations de pêche (non scientifique) avec couverture d'observateur similiare au relevé
#' (durant premières années)
#' À FAIRE: valider les entrées 'i'
#'
#' @param origine localisation et nom du fichier de base de données des stations échantillonnées
#' @param destination localisation et nom du fichier de travail local
#'
#' @importFrom utils read.csv2 write.csv2
#'
#' @return une list()
#'
lecture_cph <- function(origine, destination) {
  #   origine=file.path('S:','Flétan','Relevé 4RST','BD','donneesReleveFletan_cph.csv')
  cph.init <- read.csv2(origine, stringsAsFactors = FALSE, dec = '.') # dim(cph.init)
  ## table(unlist(cph.init[,-1]))
  cph.init <- cph.init[,
    !apply(cph.init, 2, function(x) {
      all(is.na(x))
    })
  ] # dim(cph.init)
  ## dimnames(cph.init)[[1]] <- paste(cph.init$cleStation, 1:nrow(cph.init), sep='-')
  cph.mat <- as.matrix(cph.init[, -which(names(cph.init) == 'cleStation')])
  ## table(cph.mat, useNA='ifany')
  cph.mat[which(cph.mat == 'b')] <- 'a'
  cph.mat[which(cph.mat == 'c')] <- 'm'
  cph.mat[which(cph.mat == 'e')] <- 'v'
  cph.mat[which(cph.mat == 'h')] <- 'f'
  cph.mat[which(cph.mat %in% c('r', 'R'))] <- 's'
  ## table(cph.mat, useNA='ifany')
  cph.init[, -which(names(cph.init) == 'cleStation')] <- cph.mat
  # destination=file.path('S:','Flétan','Relevé 4RST','BD','versionTravailTemporaire','vTrav')
  write.csv2(
    cph.init,
    file = paste0(destination, '_cph.csv'),
    row.names = FALSE
  )
}
