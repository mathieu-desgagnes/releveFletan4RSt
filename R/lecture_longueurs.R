#' Lecture de la base de données de longueur du relevé à la palangre.
#'
#' Contient toutes les espèces mesurées durant le relevé.
#' Des données supplémentaires ont aussi récoltées durant la pêche scientifique où des flétan sont conservés pour financement.
#'
#' À faire: considérer 3Pn
#'
#' @param origine chemin d'accès de la base de données
#' @param lpFletan_alpha `numeric` alpha de la relation longueur-poids du flétan de l'Atlantique: `poids=alpha*longueur^beta`
#' @param lpFletan_beta `numeric` beta de la relation longueur-poids du flétan de l'Atlantique: `poids=alpha*longueur^beta`
#' @param lpMorue_alpha `numeric` alpha de la relation longueur-poids de la morue: `poids=alpha*longueur^beta`
#' @param lpMorue_beta `numeric` beta de la relation longueur-poids de la morue: `poids=alpha*longueur^beta`
#' @param donneesSupplementaires Si les données recueillies dans les stations où le poisson est conservé doivent être inclues
#'
#' @returns `data.frame` de la base de données lue et mise en forme
#' @export
#'
#' @examples
lecture_longueurs <- function(
  origine,
  lpFletan_alpha = 0.000005262,
  lpFletan_beta = 3.167,
  lpMorue_alpha = 0.00000574,
  lpMorue_beta = 3.1160,
  donneesSupplementaires = FALSE
) {
  longueurs.init <- read.csv2(
    file = origine,
    stringsAsFactors = FALSE,
    dec = ',',
    na.strings = 'NA'
  )
  ##
  if (donneesSupplementaires) {
    longueurs.init.supp <- read.csv2(
      file = 'C:/gccode/releve4RST/Donnees/donneesReleveFletanDirigee_longueur.csv',
      stringsAsFactors = FALSE,
      dec = '.'
    )
    file.info(
      'C:/gccode/releve4RST/Donnees/donneesReleveFletanDirigee_longueur.csv'
    )$mtime
  }
  ##
  ## formatage
  longueurs.init$longueur_cm <- as.numeric(longueurs.init$longueur_cm)
  table(longueurs.init$espece)
  longueurs.init[
    longueurs.init$espece == 'fletan',
    'poidsEstime.kg'
  ] <- lpFletan_alpha *
    longueurs.init[
      longueurs.init$espece == 'fletan',
      'longueur_cm'
    ]^lpFletan_beta
  longueurs.init[
    longueurs.init$espece == 'morue',
    'poidsEstime.kg'
  ] <- lpMorue_alpha *
    longueurs.init[
      longueurs.init$espece == 'morue',
      'longueur_cm'
    ]^lpMorue_beta
  ##
  return(longueurs.init)
}
## longueurs.tt <- chargerLongueurs(fichierBD=file.path('S:','Flétan','Relevé 4RST','Donnees','2023','donneesReleveFletan_longueur.csv'), recalc=TRUE)
