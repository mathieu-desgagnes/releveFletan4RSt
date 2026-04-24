# dif_bd <- file.path('S:','Flétan','Relevé 4RST','BD')
# stations.tt.init <- chargerStations(source=file.path(dir_bd,'donneesReleveFletan_stations.csv'),
#                                     destination=file.path(dir_bd,'versionTravailTemporaire',anneeCourante,'stations_vTrav'),
#                                     recalc=TRUE)#recalc=FALSE) #

# format stations:
#
# cleStation: chr
# annee: num
# nbHamecon: num
# longitude, latitude: num
# dateHeureDescente: chr
# dateHeureRemontee: chr
#

#' Lit la base de données des stations
#'
#' @param origine chemin d'accès de la base de données
#' @param destination chemin d'accès où enregistrer la base de données de travail
#'
#' @returns `data.frame` mis en forme
#' @export
#'
#' @import lubridate suncalc utils
#'
#' @examples print('non disponible')
lecture_stations <- function(origine, destination) {
  stations.init <- read.csv2(
    origine,
    stringsAsFactors = FALSE,
    dec = '.',
    na.strings = 'NA'
  )
  #file.info(source)$mtime; nrow(stations.init)
  if (FALSE) {
    #a valider plus tard
    ## 3Pn
    stations.init.3Pn <- read.csv2(
      'C:/gccode/releve4RST/Donnees/donneesReleveFletan3Pn_stations.csv',
      stringsAsFactors = FALSE,
      dec = '.'
    )
    file.info(
      'C:/gccode/releve4RST/Donnees/donneesReleveFletan3Pn_stations.csv'
    )$mtime
    nrow(stations.init.3Pn)
    stations.init.3Pn$releve <- 'opano3Pn'
    ## pêche dirigée
    stations.init.supp <- read.csv2(
      'C:/gccode/releve4RST/Donnees/donneesReleveFletanDirigee_stations.csv',
      stringsAsFactors = FALSE,
      dec = '.'
    )
    stations.init.supp[
      which(stations.init.supp$profondeurFin == 'Na'),
      'profondeurFin'
    ] <- NA
    stations.init.supp$profondeurFin <- as.numeric(
      stations.init.supp$profondeurFin
    )
    file.info(
      'C:/gccode/releve4RST/Donnees/donneesReleveFletanDirigee_stations.csv'
    )$mtime
    nrow(stations.init.supp)
    stations.init.supp$releve <- 'stSupp'
    stations.init <- rbind(stations.init, stations.init.3Pn, stations.init.supp)
    nrow(stations.init)
  }
  ##
  lesquels <- which(
    is.na(as.numeric(stations.init$latDebDD)) |
      is.na(as.numeric(stations.init$longDebDD)) |
      is.na(as.numeric(stations.init$latFinDD)) |
      is.na(as.numeric(stations.init$longFinDD))
  )
  print(paste(
    'Problème avec la position de la station:',
    stations.init[lesquels, 'cleStation']
  ))
  if (FALSE) {
    stations.init[
      which(
        is.na(as.numeric(stations.init$latDebDD)) |
          is.na(as.numeric(stations.init$longDebDD)) |
          is.na(as.numeric(stations.init$latFinDD)) |
          is.na(as.numeric(stations.init$longFinDD))
      ),
      c(
        'noStation',
        'cleStation',
        'annee',
        'latDebDD',
        'longDebDD',
        'latFinDD',
        'longFinDD'
      )
    ]
  }
  ##
  stations.init$X <- apply(
    stations.init[, c('longDebDD', 'longFinDD')],
    1,
    mean,
    na.rm = TRUE
  )
  stations.init$Y <- apply(
    stations.init[, c('latDebDD', 'latFinDD')],
    1,
    mean,
    na.rm = TRUE
  )
  ##
  ## mettre les profondeurs sur la même unité
  table(stations.init$profondeurUnit, useNA = 'always')
  stations.init$profDeb <- NA
  stations.init$profDeb[which(
    stations.init$profondeurUnit == 'metre'
  )] <- stations.init$profondeurDebut[which(
    stations.init$profondeurUnit == 'metre'
  )]
  stations.init$profDeb[which(
    stations.init$profondeurUnit == 'brasse'
  )] <- 1.8288 *
    stations.init$profondeurDebut[which(
      stations.init$profondeurUnit == 'brasse'
    )]
  stations.init$profDeb[which(
    stations.init$profondeurUnit %in% c('pied')
  )] <- 0.3048 *
    stations.init$profondeurDebut[which(
      stations.init$profondeurUnit %in% c('pied')
    )]
  stations.init$profFin <- NA
  stations.init$profFin[which(
    stations.init$profondeurUnit == 'metre'
  )] <- stations.init$profondeurFin[which(
    stations.init$profondeurUnit == 'metre'
  )]
  stations.init$profFin[which(
    stations.init$profondeurUnit == 'brasse'
  )] <- 1.8288 *
    stations.init$profondeurFin[which(stations.init$profondeurUnit == 'brasse')]
  stations.init$profFin[which(
    stations.init$profondeurUnit %in% c('pied')
  )] <- 0.3048 *
    stations.init$profondeurFin[which(
      stations.init$profondeurUnit %in% c('pied')
    )]
  stations.init$profMoy <- apply(
    stations.init[, c('profDeb', 'profFin')],
    1,
    mean,
    na.rm = TRUE
  )
  ##
  ## ajouter les profondeurs théoriques des coordonnées
  if (FALSE) {
    profRaster <- function(st) {
      ## calculer la profondeur des stations en localisant les positions sur un raster de profondeur
      ## @st data.frame() qui contient c('X','Y')
      ##
      ## lire le raster pour associer une profondeurs aux stations
      rast20 <- raster(
        'C:/gccode/pbsmapping/input/bathy_DAISS/gulf20m_2020May26_ascii/gulf20m_mathieuDesgagnes.grd'
      )
      ## proj4string(rast20)
      ## transformation les coordonnées des stations dans la même projection que le raster (QcLambert)
      st.qcl <- st
      coordinates(st.qcl) <- c('X', 'Y')
      proj4string(st.qcl) <- CRS("+init=epsg:4326") #wgs84
      temp <- spTransform(st.qcl, CRS(proj4string(rast20))) #projection du raster
      st$Xqcl <- coordinates(temp)[, 'X']
      st$Yqcl <- coordinates(temp)[, 'Y']
      st.qcl <- st
      coordinates(st.qcl) <- c('Xqcl', 'Yqcl')
      proj4string(st.qcl) <- CRS(proj4string(rast20)) #projection du raster
      ##
      ## calculer la profondeur pour chaque station
      ## print(Sys.time());
      profs20 <- extnract(rast20, coordinates(st.qcl))
      ## print(Sys.time());  # ~1sec.
      return(profs20)
    }
    lesquels <- which(
      !is.na(as.numeric(stations.init$latDebDD)) &
        !is.na(as.numeric(stations.init$longDebDD))
    ) # peut pas calculer si manque une donnee
    stations.init[lesquels, 'profDebBathy'] <- -profRaster(
      st = as.data.frame(list(
        X = stations.init[lesquels, 'longDebDD'],
        Y = stations.init[lesquels, 'latDebDD']
      ))
    )
    lesquels <- which(
      !is.na(as.numeric(stations.init$latFinDD)) &
        !is.na(as.numeric(stations.init$longFinDD))
    ) # peut pas calculer si manque une donnee
    stations.init[lesquels, 'profFinBathy'] <- -profRaster(
      st = as.data.frame(list(
        X = stations.init[lesquels, 'longFinDD'],
        Y = stations.init[lesquels, 'latFinDD']
      ))
    )
    stations.init$profMoyBathy <- apply(
      stations.init[, c('profDebBathy', 'profFinBathy')],
      1,
      mean,
      na.rm = TRUE
    )
  }
  ##
  ##
  ## Estimation des fuseau horaires manquants
  stations.init[
    is.na(stations.init$fuseauHoraire) & stations.init$association == 'FFAW',
    'fuseauHoraire'
  ] <- "UTC-2.5"
  stations.init[
    is.na(stations.init$fuseauHoraire) & stations.init$association == 'ACPG',
    'fuseauHoraire'
  ] <- "UTC-4"
  stations.init[
    is.na(stations.init$fuseauHoraire) & stations.init$association == 'GNSFPB',
    'fuseauHoraire'
  ] <- "UTC-3"
  # stations.init[which(stations.init$fuseauHoraire=="UTC-4"), 'fuseauIANA'] <- "America/Toronto"
  # stations.init[which(stations.init$fuseauHoraire=="UTC-3"), 'fuseauIANA'] <- "America/Halifax"
  # stations.init[which(stations.init$fuseauHoraire=="UTC-2.5"), 'fuseauIANA'] <- "America/St_Johns"
  stations.init[
    which(stations.init$fuseauHoraire == "UTC-4"),
    'fuseauTxt'
  ] <- "-04:00"
  stations.init[
    which(stations.init$fuseauHoraire == "UTC-3"),
    'fuseauTxt'
  ] <- "-03:00"
  stations.init[
    which(stations.init$fuseauHoraire == "UTC-2.5"),
    'fuseauTxt'
  ] <- "-02:30"
  ##
  ## Standardisation des valeurs 24h00 vs 00h00
  stations.init[which(stations.init$heureRemonte == 24), 'heureRemonte'] <- 0
  ##
  ## Construction de la date pour descente
  ## Générer le vecteur desc_text selon heure/minute NA ou pas
  desc_text <- ifelse(
    is.na(stations.init$heureDescente) | is.na(stations.init$minuteDescente),
    sprintf(
      "%04d-%02d-%02d",
      stations.init$annee,
      stations.init$mois,
      stations.init$jour
    ),
    sprintf(
      "%04d-%02d-%02dT%02d:%02d:00%s",
      stations.init$annee,
      stations.init$mois,
      stations.init$jour,
      stations.init$heureDescente,
      stations.init$minuteDescente,
      stations.init$fuseauTxt
    )
  )
  ##
  ## Générer le vecteur desc_text selon heure/minute NA ou pas
  desc_text_date <- sprintf(
    "%04d-%02d-%02d",
    stations.init$annee,
    stations.init$mois,
    stations.init$jour
  )
  desc_text_datetime <- ifelse(
    !is.na(stations.init$heureDescente) &
      !is.na(stations.init$minuteDescente) &
      !is.na(stations.init$fuseauTxt),
    sprintf(
      "%04d-%02d-%02dT%02d:%02d:00%s",
      stations.init$annee,
      stations.init$mois,
      stations.init$jour,
      stations.init$heureDescente,
      stations.init$minuteDescente,
      stations.init$fuseauTxt
    ),
    NA
  )
  ##
  stations.init$dateDescente <- lubridate::ymd(desc_text_date)
  #
  stations.init$dateHeureDescente <- lubridate::ymd_hms(desc_text_datetime)
  stations.init$dateHeureDescente_qc <- stations.init$dateHeureDescente
  attr(stations.init$dateHeureDescente_qc, "tzone") <- "America/Montreal"
  stations.init$dateHeureDescente_qctxt <- format(
    stations.init$dateHeureDescente_qc,
    "%Y-%m-%d %H:%M:%S"
  )
  ##
  ##
  ## Calculer durée d'immersion
  stations.init$immersion <- lubridate::ddays(stations.init$nbrJourImmersion) +
    lubridate::dhours(stations.init$immersionHeure) +
    lubridate::dminutes(stations.init$immersionMinute)
  ##
  ## Ajout du dateHeure d'immersion pour dateHeureRemontee
  stations.init$dateHeureRemontee <- stations.init$dateHeureDescente +
    stations.init$immersion
  stations.init$dateHeureRemontee_qc <- stations.init$dateHeureRemontee
  attr(stations.init$dateHeureRemontee_qc, "tzone") <- "America/Montreal"
  stations.init$dateHeureRemontee_qctxt <- format(
    stations.init$dateHeureRemontee_qc,
    "%Y-%m-%d %H:%M:%S"
  )
  ##
  ##
  if (FALSE) {
    stations.init[
      1183:1193,
      c(
        'fuseauHoraire',
        'dateHeureDescenteLocale',
        'dateHeureDescente',
        'annee',
        'mois',
        'jour',
        'heureDescente',
        'minuteDescente'
      )
    ]
    stations.init[
      1183:1193,
      c(
        'fuseauHoraire',
        'dateHeureRemonteeLocale',
        'dateHeureRemontee',
        'heureRemonte',
        'minuteRemonte',
        'immersionHeure',
        'immersionMinute',
        'annee',
        'mois',
        'jour'
      )
    ]
    stations.init[
      1183:1193,
      c(
        'fuseauHoraire',
        'dateHeureDescenteLocale',
        'dateHeureDescente',
        'dateHeureRemonteeLocale',
        'dateHeureRemontee',
        'immersion',
        'immersionHeure',
        'immersionMinute',
        'annee',
        'mois',
        'jour'
      )
    ]
  }
  ##
  ## stations.init$date <- as.IDate(paste0(stations.init$annee, '-', stations.init$mois, '-', stations.init$jour))
  ## ##
  ## stations.init$heureDescente <- as.ITime(paste0(stations.init$heureDescente, ':', stations.init$minuteDescente, ':00'))
  ## stations.init$heureRemonte <- as.ITime(paste0(stations.init$heureRemonte, ':', stations.init$minuteRemonte, ':00'))
  ## stations.init$immersion <- apply(stations.init[,c('heureDescente','heureRemonte')], 1, diff)/3600 + stations.init$nbrJourImmersion*24
  ## stations.init$immersion <- stations.init$nbrJourImmersion*24 + stations.init$immersionHeure + stations.init$immersionMinute/60
  ## ## convertir heure en heure avancée de l'est
  ## ## table(stations.init$association, stations.init$fuseauHoraire, useNA='ifany')
  ## stations.init[which(stations.init$fuseauHoraire=='UTC-2.5'),c('heureDescente','heureRemonte')] <-
  ##     stations.init[which(stations.init$fuseauHoraire=='UTC-2.5'), c('heureDescente','heureRemonte')]-1.5*3600
  ## stations.init[which(stations.init$fuseauHoraire=='UTC-3'),c('heureDescente','heureRemonte')] <-
  ##     stations.init[which(stations.init$fuseauHoraire=='UTC-3'),c('heureDescente','heureRemonte')]-1*3600
  ##
  ##
  ## choix des variables d'intérêt
  ## stations <- stations.init[,c('noStation','cleStation','sZoneOpano','annee','capitaine','association','noStation','nbHamecon','latDebDD',
  ##                              'latFinDD','longDebDD','longFinDD','X','Y','profDeb','profFin','profMoy','profDebBathy','profFinBathy','profMoyBathy',
  ##                              'date','heureDescente','heureRemonte','immersion','fuseauHoraire','distance_avancon','appatType','appatQuant',
  ##                              'appatUnite','flottante_calante','long_avancon','observateur','releve')]
  ## stations.choix <- stations.init[,c('noStation','cleStation','sZoneOpano','annee','capitaine','association','noStation','nbHamecon','latDebDD',
  ##                                    'latFinDD','longDebDD','longFinDD','X','Y','profDeb','profFin','profMoy',
  ##                                    'date','heureDescente','heureRemonte','immersion','fuseauHoraire','distance_avancon','appatType','appatQuant',
  ##                                    'appatUnite','flottante_calante','long_avancon','observateur','releve','gardeOuiNon')]
  stations.choix <- stations.init[, c(
    'noStation',
    'cleStation',
    'sZoneOpano',
    'annee',
    'capitaine',
    'association',
    'nbHamecon',
    'latDebDD',
    'latFinDD',
    'longDebDD',
    'longFinDD',
    'X',
    'Y',
    'profDeb',
    'profFin',
    'profMoy',
    'dateDescente',
    'dateHeureDescente_qctxt',
    'dateHeureRemontee_qctxt',
    'immersion',
    'fuseauHoraire',
    'distance_avancon',
    'appatType',
    'appatQuant',
    'appatUnite',
    'flottante_calante',
    'long_avancon',
    'observateur'
  )]
  ## zone opano
  table(stations.choix$association, stations.choix$sZoneOpano, useNA = 'ifany')
  ##
  if (FALSE) {
    stations.choix$longueurTraitKm <- rep(NA, nrow(stations.choix))
    for (i in 1:nrow(stations.choix)) {
      if (
        all(!is.na(stations.choix[i, c('longFinDD', 'latFinDD')])) &
          all(!is.na(stations.choix[i, c('longDebDD', 'latDebDD')]))
      ) {
        stations.choix[i, 'longueurTraitKm'] <- spDistsN1(
          as.matrix(stations.choix[i, c('longFinDD', 'latFinDD')]),
          as.matrix(stations.choix[i, c('longDebDD', 'latDebDD')]),
          longlat = TRUE
        )
      }
    }
  }
  ##
  ## lever et coucher du soleil (ou plutot aube et crépuscule)
  stations.choix$heureNauticalDawn <- data.table::as.ITime(NA)
  stations.choix$heureDawn <- data.table::as.ITime(NA)
  stations.choix$heureSunrise <- data.table::as.ITime(NA)
  stations.choix$heureSunset <- data.table::as.ITime(NA)
  stations.choix$heureDusk <- data.table::as.ITime(NA)
  stations.choix$heureNauticalDusk <- data.table::as.ITime(NA)
  for (i in 1:nrow(stations.choix)) {
    ## temp <- getSunlightTimes(date=stations.choix[i,'date'], lat=stations.choix[i,'Y'], lon=stations.choix[i,'X'],tz="America/New_York",
    ##                          keep=c('nauticalDawn','nauticalDusk'))[c('nauticalDawn','nauticalDusk')]
    ## stations.choix[i,'heureNauticalDawn'] <- as.ITime(temp$nauticalDawn)
    ## stations.choix[i,'heureNauticalDusk'] <- as.ITime(temp$nauticalDusk)
    temp <- suncalc::getSunlightTimes(
      date = as.Date(sub(" .*", "", stations.choix[i, 'dateDescente'])),
      lat = stations.choix[i, 'Y'],
      lon = stations.choix[i, 'X'],
      tz = "America/New_York",
      keep = c(
        'nauticalDawn',
        'dawn',
        'sunrise',
        'sunset',
        'dusk',
        'nauticalDusk'
      )
    )
    stations.choix[i, 'heureNauticalDawn'] <- data.table::as.ITime(
      temp$nauticalDawn
    )
    stations.choix[i, 'heureDawn'] <- data.table::as.ITime(temp$dawn)
    stations.choix[i, 'heureSunrise'] <- data.table::as.ITime(temp$sunrise)
    stations.choix[i, 'heureSunset'] <- data.table::as.ITime(temp$sunset)
    stations.choix[i, 'heureDusk'] <- data.table::as.ITime(temp$dusk)
    stations.choix[i, 'heureNauticalDusk'] <- data.table::as.ITime(
      temp$nauticalDusk
    )
  }
  stations.toutes <- stations.choix
  write.csv2(
    stations.toutes,
    file = paste0(destination, '.csv'),
    row.names = FALSE
  )
  save(stations.toutes, file = paste0(destination, '.RData'))
  return(stations.toutes)
}
