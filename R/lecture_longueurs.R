# format :
#

lecture_longueurs <- function(
  origine,
  destination,
  lpFletan_alpha = 0.000005262,
  lpFletan_beta = 3.167,
  lpMorue_alpha = 0.00000574,
  lpMorue_beta = 3.1160
) {
  if (recalc | !file.exists(fichierVersionTravail)) {
    print("Création d'une version locale de travail du fichier des longueurs.")
  } else {
    print(
      "La version locale de travail du fichier des longueurs n'est pas remise à jour."
    )
    ## if(file.info(fichierVersionTravail)$mtime < file.info(fichierBD)$mtime){
    ##     print('Le fichier des longueurs a ete modifie. Envisager la mise à jour du fichier de travail.')
    ## }
  }
  if (recalc) {
    longueurs.init <- read.csv2(
      file = fichierBD,
      stringsAsFactors = FALSE,
      dec = ',',
      na.strings = 'NA'
    )
    ## file.info('C:/gccode/releve4RST/Donnees/donneesReleveFletan_longueur.csv')$mtime
    if (FALSE) {
      longueurs.init.3Pn <- read.csv2(
        file = 'C:/gccode/releve4RST/Donnees/donneesReleveFletan3Pn_longueur.csv',
        stringsAsFactors = FALSE,
        dec = '.'
      )
      file.info(
        'C:/gccode/releve4RST/Donnees/donneesReleveFletan3Pn_longueur.csv'
      )$mtime
      longueurs.init.supp <- read.csv2(
        file = 'C:/gccode/releve4RST/Donnees/donneesReleveFletanDirigee_longueur.csv',
        stringsAsFactors = FALSE,
        dec = '.'
      )
      file.info(
        'C:/gccode/releve4RST/Donnees/donneesReleveFletanDirigee_longueur.csv'
      )$mtime
      longueurs.init <- merge(
        longueurs.init.std,
        longueurs.init.3Pn,
        all = TRUE
      )
      longueurs.init <- merge(longueurs.init, longueurs.init.supp, all = TRUE)
    }
    ##
    longueurs.init$longueur_cm <- as.numeric(longueurs.init$longueur_cm)
    table(longueurs.init$espece)
    longueurs.init[
      longueurs.init$espece == 'fletan',
      'poidsFletan.kg'
    ] <- lpFletan_alpha *
      longueurs.init[
        longueurs.init$espece == 'fletan',
        'longueur_cm'
      ]^lpFletan_beta
    longueurs.init[
      longueurs.init$espece == 'morue',
      'poidsMorue.kg'
    ] <- lpMorue_alpha *
      longueurs.init[
        longueurs.init$espece == 'morue',
        'longueur_cm'
      ]^lpMorue_beta
    ##
    write.csv2(longueurs.init, file = fichierVersionTravail, row.names = FALSE)
    return(longueurs.init)
  } else {
    read.csv2(file = fichierVersionTravail, stringsAsFactors = FALSE, dec = ',')
  }
}
## longueurs.tt <- chargerLongueurs(fichierBD=file.path('S:','Flétan','Relevé 4RST','Donnees','2023','donneesReleveFletan_longueur.csv'), recalc=TRUE)
