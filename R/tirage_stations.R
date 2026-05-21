#' Tirage aléatoire des stations du relevé à la palangre
#'
#' @param annee année pour laquelle les stations sont tirées.
#' @param dir_releve chemin du répertoire où les stations tirées seront enregistrées
#' @param dir_shapefile chemin du répertoire où lire les shapefiles de la zone d'étude
#' @param fichier_nb_stations chemin du fichier .csv contenant le nombre de stations allouées à chaque strate
#' @param seed valeur utilisée pour initialiser le tirage aléatoire des stations. Si NULL, le paramètre `annee` est utilisé.#'
#'
#' @returns un data.frame des stations tirées
#'
#' @import sf
#'
#' @export
#'
#' @examples
tirage_stations <- function(
  annee,
  dir_releve,
  dir_shapefile,
  fichier_nb_stations,
  seed = NULL
) {
  ## 0) où enregistrer les stations
  dir_station <- file.path(dir_releve, as.character(annee), 'coordonnees')
  ## Créer le dossier s'il n'existe pas
  if (!dir.exists(dir_station)) {
    dir.create(dir_station, recursive = TRUE)
  }

  ##
  ## 1) lire les shapefiles de la zone d'étude
  cote.qcl <- sf::st_read(file.path(
    dir_shapefile,
    '20_50m',
    'habitat_20_50m_nettoye.shp'
  ))
  cote <- sf::st_transform(cote.qcl, sf::st_crs("epsg:4326")) #wgs84
  prof.qcl <- sf::st_read(file.path(
    dir_shapefile,
    '100_300m',
    'habitat_100_300m_nettoye.shp'
  ))
  prof <- sf::st_transform(prof.qcl, sf::st_crs("epsg:4326")) #wgs84

  ##
  ## 2) charger l'objet nb_stations, ou le construire si nécessaire
  if (file.exists(fichier_nb_stations)) {
    nb_stations <- read.csv2(file = fichier_nb_stations)
    print(file.info(fichier_nb_stations)$mtime)
  } else {
    # sinon construire l'objet nb_stations à partir de S:\Flétan\Relevé 4RST\choixStations\superficie_zones_2021.xls
    ## nb_stations.init <- read.csv2(file.path(dir.releve, 'choixStations', 'superficieEtNbStations.csv'), stringsAsFactors=FALSE) #nombre de stations par strate
    nb_stations1 <- as.data.frame(cbind(
      opano = c(
        paste('4RA', '4RB', sep = '-'),
        paste('4RC', '4RD', sep = '-'),
        paste('4SI', '4SS', '4SX', sep = '-'),
        paste('4SV', '4SW', '4SY', sep = '-'),
        '4TF',
        paste('4TG', '4TH', sep = '-'),
        '4TJ',
        '4TL',
        paste('4TM', '4TN', sep = '-'),
        paste('4SZ', '4TO', '4TP', '4TQ', sep = '-')
      ),
      aire = c(
        2194.51,
        2407.29,
        1481.54,
        2860.1,
        7040.23,
        6646.9,
        2406.23,
        3621.88,
        3441.71,
        1630.8
      ),
      ## nb_stationsOri=list(2,3,2,3,7,7,3,4,4,2),
      nb_stations = c(2, 2, 2, 3, 8, 7, 3, 4, 4, 2),
      nb_stations125 = c(2, 2, 2, 4, 8, 8, 3, 4, 4, 2),
      nomOpano = c(
        '4RA_',
        '4RC_',
        '4SS_',
        '4SV_',
        '4TF',
        '4TG_',
        '4TJ_',
        '4TL',
        '4TN_',
        '4TQ_'
      ),
      profondeur = 'cote'
    ))
    nb_stations2 <- as.data.frame(cbind(
      opano = c(
        paste('4RA', '4RB', sep = '-'),
        '4RC',
        '4RD',
        '4SI',
        '4SS',
        '4SV',
        '4SW',
        '4SX',
        '4SY',
        '4SZ',
        '4TO',
        paste('4TP', '4TQ', sep = '-'),
        paste('4TF', '4TG', sep = '-'),
        '4TH',
        '4TJ',
        '4TL',
        '4TM',
        paste('4TK', '4TN', sep = '-')
      ),
      aire = c(
        8419.32,
        9125.51,
        2399.83,
        3738.69,
        6575.7,
        9529.31,
        3038.29,
        10863.27,
        4642.11,
        4463.2,
        2735.33,
        4028.7,
        3326.94,
        0,
        0,
        0,
        0,
        4359.11
      ),
      nb_stations = c(9, 10, 3, 4, 7, 10, 3, 11, 5, 5, 3, 4, 4, 0, 0, 0, 0, 5),
      nb_stations125 = c(
        9,
        10,
        3,
        4,
        7,
        11,
        3,
        12,
        5,
        5,
        3,
        5,
        4,
        0,
        0,
        0,
        0,
        5
      ),
      nomOpano = c(
        '4RB_',
        '4RC',
        '4RD',
        '4SI',
        '4SS',
        '4SV',
        '4SW',
        '4SX',
        '4SY',
        '4SZ',
        '4TO',
        '4TP_',
        '4TF_',
        '4TH',
        '4TJ',
        '4TL',
        '4TM',
        '4TK_'
      ),
      profondeur = 'prof'
    ))
    nb_stations <- rbind(nb_stations1, nb_stations2)
    write.csv2(nb_stations, file = fichier_nb_stations, row.names = FALSE)
  }

  ##
  ## Tirer alétoirement des coordonnées de stations, une strate à la fois
  set.seed(annee^2) # Initialiser le générateur de nombres aléatoire, pour pouvoir reproduire le tirage
  taille_ech <- 5 # Multiplicateur de taille d'échantillon
  ##
  stations_choisies <- NULL
  ##
  for (i.poly in 1:nrow(nb_stations)) {
    if (nb_stations$nb_stations[i.poly] != 0) {
      ## selectionner le polygone
      switch(
        nb_stations[i.poly, 'profondeur'],
        'cote' = {
          poly.temp <- subset(
            cote.qcl,
            strate %in% nb_stations[i.poly, 'nomOpano']
          )
        },
        'prof' = {
          poly.temp <- subset(
            prof.qcl,
            strate %in% nb_stations[i.poly, 'nomOpano']
          )
        }
      )
      ##
      ## échantillonner les stations
      temp <- sf::st_sample(
        x = poly.temp,
        size = nb_stations[i.poly, 'nb_stations'] * taille_ech
      )
      ##
      ## créer le df() de stations
      stations.temp <- sf::st_as_sf(
        cbind(
          temp,
          data.frame(
            strateOpano = rep(nb_stations[i.poly, 'nomOpano'], length(temp)),
            strateProf = nb_stations[i.poly, 'profondeur'],
            priorite = 'autre',
            stringsAsFactors = FALSE
          )
        )
      )
      ##
      stations_choisies <- rbind(stations_choisies, stations.temp)
    }
  }

  ##########
  ##
  ## continuer ici
  ##
  ##########

  ## 4) Réordonner aléatoirement les statins et valider les profondeurs et distances entre stations
  ##
  diff.dist_min <- set_units(5, nautical_mile)
  diff.prof_min <- 100
  ## ATTENTION!!! valider la bathymetrie utilisée
  ##
  rast20 <- terra::rast(file.path(
    'S:',
    'Flétan',
    'bathy_DAISS',
    'gulf20m_2020May26_ascii',
    'gulf20m_mathieuDesgagnes.grd'
  ))
  stations_choisies$profondeur <- -extract(
    x = rast20,
    st_coordinates(stations_choisies)
  )$gulf20m_2020may26
  ##
  ## réordonner aléatoirement les stations
  ordreDesStations <- sample(
    1:nrow(stations_choisies),
    nrow(stations_choisies),
    replace = FALSE
  )
  stations_choisies <- stations_choisies[ordreDesStations, ]
  stations_choisies$idGlobal <- 1:nrow(stations_choisies)
  ##
  ## valider les distances
  dist.temp.qcl <- sf::st_distance(stations_choisies, stations_choisies) #distance avec autres points précédement choisis
  for (i in 2:nrow(dist.temp.qcl)) {
    lesquels <- which(dist.temp.qcl[i, 1:(i - 1)] < diff.dist_min) #pour une stations, déterminer la distance avec les stations précédentes
    lesquels <- lesquels[stations_choisies[lesquels, ]$priorite != 'tropProche']
    if (length(lesquels) > 0) {
      #si des stations précédentes sont à une distance plus petite
      if (
        !is.na(stations_choisies[i, ]$profondeur) &
          any(
            abs(
              stations_choisies[i, ]$profondeur -
                stations_choisies[lesquels, ]$profondeur
            ) <
              diff.prof_min
          )
      ) {
        stations_choisies[i, 'priorite'] <- 'tropProche'
      }
    }
  }
  temp <- cbind(
    as.data.frame(stations_choisies),
    st_coordinates(st_transform(stations_choisies, crs("epsg:4326"))),
    dist.temp.qcl
  )
  write.csv2(
    temp,
    file = file.path(dir.choix_station, 'distanceStationsProposees.csv')
  )
  ##
  ## ordonner les stations et traduire en wgs84
  stations_choisies <- stations_choisies[
    order(
      stations_choisies$strateProf,
      stations_choisies$strateOpano,
      stations_choisies$idGlobal
    ),
  ]
  stations_choisies[is.na(stations_choisies$profondeur), ]
  stations_choisies[
    which(
      stations_choisies$profondeur < 20 |
        (stations_choisies$profondeur < 100 &
          stations_choisies$profondeur > 50) |
        stations_choisies$profondeur > 300
    ),
  ]
  ## ajuster priorite à 'base' et à 'alternatives', en nombre égal
  for (i.opano in 1:nrow(nb_stations)) {
    strate_opano <- nb_stations[i.opano, 'nomOpano']
    prof
    nb <- nb_stations[i.opano, 'nb_stations']
    if (nb > 0) {
      lesquels <- which(
        stations_choisies$strateOpano == nb_stations[i.opano, 'nomOpano'] &
          stations_choisies$strateProf == nb_stations[i.opano, 'profondeur'] &
          stations_choisies$priorite == 'autre'
      )
      stations_choisies[lesquels[1:nb], 'priorite'] <- 'base'
      stations_choisies[lesquels[nb + (1:nb)], 'priorite'] <- 'alternative'
    }
  }
  ## table(stations_choisies$priorite)
  ##
  ##
  ## transformer en WGS84
  stations <- st_transform(stations_choisies, crs("epsg:4326")) #donc stations_choisies en qcl et stations en wgs84
  coord.temp <- cbind(
    as.data.frame(stations)[, c(
      'idGlobal',
      'strateOpano',
      'strateProf',
      'profondeur',
      'priorite'
    )],
    st_coordinates(stations)
  )
  write.csv2(
    coord.temp,
    file = file.path(dir.choix_station, 'stationsProposees.csv'),
    row.names = FALSE
  )

  ##
  transcrireFormat <- function(coord) {
    ## transcrire les stations en différents format (DMM, DMS, DD)
    ## coord['Latitude DMM'] <- sprintf("%d\u00B0%06.3f'",
    ##                                 floor(coord[,'Y']),
    ##                                 coord[,'Y']%%1*60)
    ## coord['Longitude DMM'] <- sprintf("-%d\u00B0%06.3f'",
    ##                                  floor(-coord[,'X']),
    ##                                  -coord[,'X']%%1*60)
    ## coord['Latitude DMS'] <- sprintf("%d\u00B0%02d'%02d\"",
    ##                                 floor(coord[,'Y']),
    ##                                 floor(coord[,'Y']%%1*60),
    ##                                 floor((coord[,'Y']%%1*60)%%1*60))
    ## coord['Longitude DMS'] <- sprintf("-%d\u00B0%02d'%02d\"",
    ##                                  floor(-coord[,'X']),
    ##                                  floor(-coord[,'X']%%1*60),
    ##                                  floor((-coord[,'X']%%1*60)%%1*60))
    ## coord['Latitude DD'] <- sprintf("%.5f\u00B0", coord[,'Y'])
    ## coord['Longitude DD'] <- sprintf("-%.5f\u00B0", -coord[,'X'])
    coord['Latitude DMM'] <- sprintf(
      "%ddeg%06.3f'",
      floor(coord[, 'Y']),
      coord[, 'Y'] %% 1 * 60
    )
    coord['Longitude DMM'] <- sprintf(
      "-%ddeg%06.3f'",
      floor(-coord[, 'X']),
      -coord[, 'X'] %% 1 * 60
    )
    coord['Latitude DMS'] <- sprintf(
      "%ddeg%02d'%02d\"",
      floor(coord[, 'Y']),
      floor(coord[, 'Y'] %% 1 * 60),
      floor((coord[, 'Y'] %% 1 * 60) %% 1 * 60)
    )
    coord['Longitude DMS'] <- sprintf(
      "-%ddeg%02d'%02d\"",
      floor(-coord[, 'X']),
      floor(-coord[, 'X'] %% 1 * 60),
      floor((-coord[, 'X'] %% 1 * 60) %% 1 * 60)
    )
    coord['Latitude DD'] <- sprintf("%.5fdeg", coord[, 'Y'])
    coord['Longitude DD'] <- sprintf("-%.5fdeg", -coord[, 'X'])
    coord
    ## temp['Latitude DMM'] <- paste0(floor(coord[,'Y']), '\u00B0',
    ##                                formatC(coord[,'Y']%%1*60, digits=3, width=6, flag='0', format='f'), '\u2032')
    ## temp['Longitude DMM'] <- paste0('-',floor(-coord[,'X']), '\u00B0',
    ##                                 formatC(-coord[,'X']%%1*60, digits=3, width=6, flag='0', format='f'), '\u2032')
    ## temp['Latitude DMS'] <- paste0(floor(coord[,'Y']), '\u00B0',
    ##                                formatC(floor(coord[,'Y']%%1*60), digits=0, width=2, flag='0', format='f'), '\u2032',
    ##                                formatC(floor((coord[,'Y']%%1*60)%%1*60), digits=0, width=2, flag='0', format='f'), '\u02BA')
    ## temp['Longitude DMS'] <- paste0('-',floor(-coord[,'X']), '\u00B0',
    ##                                 formatC(floor(-coord[,'X']%%1*60), digits=0, width=2, flag='0', format='f'), '\u2032',
    ##                                 formatC(floor((-coord[,'X']%%1*60)%%1*60), digits=0, width=2, flag='0', format='f'), '\u02BA')
    ## temp['Latitude DD'] <- paste0(formatC(round(coord[,'Y'],5), digits=7), '\u00B0')
    ## temp['Longitude DD'] <- paste0('-',formatC(round(-coord[,'X'],5), digits=7), '\u00B0')
    ## temp
  }
  temp <- transcrireFormat(coord = coord.temp)
  ##
  write.csv2(
    temp,
    file = file.path(dir.choix_station, 'stationsProposees_DMS.csv'),
    row.names = FALSE,
    fileEncoding = 'UTF-8',
    quote = FALSE
  )

  ########
  ##
  ## suite à la ligne 478 du fichier tirageAleatoireDeCoordonnees.r
  ##
  ########
}
