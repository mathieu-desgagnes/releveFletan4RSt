#' Lectures des bases de données et création d'une version de travail (locale) des données.
#'
#' Les données de stations, de longueur, et de captures par hamecons sont lues et organisées en objets utilisables dans les autres fonctions du package.
#' Ces objets sont enregistrés en format .csv dans le répertoire fourni
#'
#' @param annee integer de la dernière année a considérer
#' @param dir_bd position du dossier contenant la base de donnée
#'
#' @returns une liste des stations, longueurs et cph
#' @export
#'
lecture_versionTravail <- function(annee, dir_bd = NULL) {
  anneeCourante <- annee
  if (is.null(dir_bd)) {
    dir_bd <- file.path('S:', 'Flétan', 'Relevé 4RST', 'BD')
  }
  source(file.path(
    'S:',
    'Flétan',
    'Relevé 4RST',
    'Analyses',
    'relevePalangre',
    'fonctions.r'
  ))

  ## charger les données de stations
  file.info(file.path(dir_bd, 'donneesReleveFletan_stations.csv'))$mtime
  file.info(file.path(
    dir_bd,
    'versionTravailTemporaire',
    anneeCourante,
    'stations_vTrav.csv'
  ))$mtime
  stations.tt.init <- chargerStations(
    fichierBD = file.path(dir_bd, 'donneesReleveFletan_stations.csv'),
    fichierVersionTravail = file.path(
      dir_bd,
      'versionTravailTemporaire',
      anneeCourante,
      'stations_vTrav'
    ),
    recalc = TRUE
  ) #recalc=FALSE) #
  ##
  ## charger les données de longueurs
  file.info(file.path(dir_bd, 'donneesReleveFletan_longueur.csv'))$mtime
  file.info(file.path(
    dir_bd,
    'versionTravailTemporaire',
    anneeCourante,
    'longueurs_vTrav.csv'
  ))$mtime
  longueurs.tt <- chargerLongueurs(
    fichierBD = file.path(dir_bd, 'donneesReleveFletan_longueur.csv'),
    fichierVersionTravail = file.path(
      dir_bd,
      'versionTravailTemporaire',
      anneeCourante,
      'longueurs_vTrav.csv'
    ),
    recalc = TRUE
  ) #recalc=FALSE) #
  ##
  ## charger les données de cph
  file.info(file.path(dir_bd, 'donneesReleveFletan_cph.csv'))$mtime
  file.info(file.path(
    dir_bd,
    'versionTravailTemporaire',
    anneeCourante,
    'cph_vTrav.csv'
  ))$mtime
  cph.tt <- chargerCPH(
    fichierBD = file.path(dir_bd, 'donneesReleveFletan_cph.csv'),
    fichierVersionTravail = file.path(
      dir_bd,
      'versionTravailTemporaire',
      anneeCourante,
      'cph_vTrav.csv'
    ),
    recalc = TRUE
  ) #recalc=FALSE) #

  donnee <- list(
    stations.tt = stations.tt.init,
    longueurs.tt = longueurs.tt,
    cph.tt = cph.tt
  )
  return(donnee)
}
