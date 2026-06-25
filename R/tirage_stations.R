#' Tirage aléatoire des stations du relevé à la palangre
#'
#' @param annee année pour laquelle les stations sont tirées.
#' @param dir_releve chemin du répertoire où les stations tirées seront enregistrées
#' @param dir_shapefile chemin du répertoire où lire les shapefiles de la zone d'étude
#' @param fichier_nb_stations chemin du fichier .csv contenant le nombre de stations allouées à chaque strate
#' @param fichier_raster chemin du fichier contenant les informations de profondeur
#' @param seed valeur utilisée pour initialiser le tirage aléatoire des stations. Si NULL, le paramètre `annee` est utilisé.#'
#'
#' @returns un data.frame des stations tirées
#'
#' @import sf
#' @import terra
#' @importFrom units set_units
#' @importFrom openxlsx write.xlsx
#'
#' @export
#'
#' @examples #à venir
tirage_stations <- function(
  annee,
  dir_releve,
  dir_shapefile,
  fichier_nb_stations,
  fichier_raster,
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
        2058.29,
        2012.07,
        1516.02,
        3084.98,
        7169.01,
        6888.15,
        2651.90,
        3573.89,
        3463.71,
        1971.18
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
        8377.67,
        8980.42,
        2554.41,
        3751.63,
        6588.86,
        9546.02,
        3019.60,
        10855.62,
        4670.22,
        4503.74,
        3421.11,
        4684.85,
        2786.87,
        0,
        0,
        0,
        0,
        4071.60
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
    nb_stations$nb_stations <- as.numeric(nb_stations$nb_stations)
    nb_stations$nb_stations125 <- as.numeric(nb_stations$nb_stations125)
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
            nomOpano = rep(nb_stations[i.poly, 'opano'], length(temp)),
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

  ## 4) Réordonner aléatoirement les statins et valider les profondeurs et distances entre stations
  ##
  diff.dist_min <- units::set_units(5, nautical_mile)
  diff.prof_min <- 100
  ## ATTENTION!!! valider la bathymetrie utilisée
  ##
  if (file.exists(fichier_raster)) {
    rast20 <- terra::rast(fichier_raster)
  } else {
    stop("Il n'y a pas de fichier de bathymétrie à l'emplacement fournis.")
  }
  stations_choisies$profondeur <- -terra::extract(
    x = rast20,
    sf::st_coordinates(stations_choisies)
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
    sf::st_coordinates(sf::st_transform(
      stations_choisies,
      sf::st_crs("epsg:4326")
    )),
    dist.temp.qcl
  )
  write.csv2(
    temp,
    file = file.path(dir_station, 'distanceStationsProposees.csv'),
    row.names = FALSE
  )
  # openxlsx::write.xlsx(
  #   temp,
  #   file.path(dir_station, 'distance_stations_proposees.xlsx')
  # )
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
  stations <- sf::st_transform(stations_choisies, sf::st_crs("epsg:4326")) #donc stations_choisies en qcl et stations en wgs84
  coord.temp <- cbind(
    as.data.frame(stations)[, c(
      'idGlobal',
      'strateOpano',
      'strateProf',
      'profondeur',
      'priorite'
    )],
    sf::st_coordinates(stations)
  )
  if (FALSE) {
    temp_init <- read.csv2(
      file = file.path(dir_station, 'stationsProposees.csv')
    )
    write.csv2(
      coord.temp,
      file = file.path(dir_station, 'stationsProposees_test.csv'),
      row.names = FALSE
    )
    coord.temp <- read.csv2(
      file = file.path(dir_station, 'stationsProposees_test.csv')
    )
    identical(temp_init, coord.temp)
    all.equal(temp_init, coord.temp)
  }
  write.csv2(
    coord.temp,
    file = file.path(dir_station, 'stationsProposees.csv'),
    row.names = FALSE
  )
  openxlsx::write.xlsx(
    coord.temp,
    file.path(dir_station, 'stations_proposees.xlsx')
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
      "%d°%06.3f'",
      floor(coord[, 'Y']),
      coord[, 'Y'] %% 1 * 60
    )
    coord['Longitude DMM'] <- sprintf(
      "-%d°%06.3f'",
      floor(-coord[, 'X']),
      -coord[, 'X'] %% 1 * 60
    )
    coord['Latitude DMS'] <- sprintf(
      "%d°%02d'%02d\"",
      floor(coord[, 'Y']),
      floor(coord[, 'Y'] %% 1 * 60),
      floor((coord[, 'Y'] %% 1 * 60) %% 1 * 60)
    )
    coord['Longitude DMS'] <- sprintf(
      "-%d°%02d'%02d\"",
      floor(-coord[, 'X']),
      floor(-coord[, 'X'] %% 1 * 60),
      floor((-coord[, 'X'] %% 1 * 60) %% 1 * 60)
    )
    coord['Latitude DD'] <- sprintf("%.5f°", coord[, 'Y'])
    coord['Longitude DD'] <- sprintf("-%.5f°", -coord[, 'X'])
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
    file = file.path(dir_station, 'stationsProposees_DMS.csv'),
    row.names = FALSE,
    fileEncoding = 'UTF-8',
    quote = FALSE
  )
  openxlsx::write.xlsx(
    temp,
    file.path(dir_station, 'stations_proposees_DMS.xlsx')
  )

  #####
  ##
  ## A partir d'ici, une inspection visuelle des stations proposées est nécessaire, pour ensuite suggérer l'utilisation de stations alternatives
  ##
  #####

  stations <- read.csv2(
    file = file.path(dir_station, 'stationsProposees_DMS.csv'),
    stringsAsFactors = FALSE
  )
  table(stations$priorite)
  switch(
    as.character(annee),
    '2024' = {
      stationsProb <- c('54')
      stationsRemplacement <- c('201')
    },
    '2023' = {
      stationsProb <- c('5', '73', '119', '236')
      stationsRemplacement <- c('159', '134', '293', '239')
    },
    '2022' = {
      stations[
        which(stations$idGlobal %in% c('87', '231', '36', '148')),
        'priorite'
      ] <- 'mauvaiseProf'
      stations[
        which(stations$idGlobal %in% c('271', '45', '208')),
        'priorite'
      ] <- 'base'
    },
    {
      stationsProb <- NA
      stationsRemplacement <- NA
    }
  )
  ##
  if (!is.na(stationsProb)) {
    stations[which(stations$idGlobal %in% stationsProb), ]
    stations[
      which(
        stations$strateOpano ==
          stations[which(stations$idGlobal %in% stationsProb), 'strateOpano'] &
          stations$strateProf ==
            stations[which(stations$idGlobal %in% stationsProb), 'strateProf'] &
          stations$priorite == 'alternative'
      ),
    ]
    stations[
      which(stations$idGlobal %in% stationsProb),
      'priorite'
    ] <- 'mauvaiseProf'
    stations[
      which(stations$idGlobal %in% stationsRemplacement),
      'priorite'
    ] <- 'base'
    table(stations$priorite)
  }
  write.csv2(
    stations,
    file = file.path(dir_station, 'stationsProposees_mod.csv'),
    row.names = FALSE
  )
  openxlsx::write.xlsx(
    stations,
    file.path(dir_station, 'stations_proposees_mod.xlsx')
  )

  if (TRUE) {
    # option4 ajouter 3 stations en 'cote' et 3 stations en 'prof' (préférée en 2025)
    stA <- stations[
      which(
        stations$priorite %in%
          c('alternative', 'autre') &
          stations$strateOpano == '4RA_' &
          stations$strateProf == 'cote'
      ),
    ]
    stB <- stations[
      which(
        stations$priorite %in%
          c('alternative', 'autre') &
          stations$strateOpano == '4RB_' &
          stations$strateProf == 'prof'
      ),
    ]
    ## polygon.temp <- st_transform(subset(prof.qcl, UnitArea%in%c('4RA','4RB')), crs=crs("epsg:4326"))
    ## plot(polygon.temp[,c('geometry','UnitArea')], col=2, axes=TRUE)
    ## plot(st_as_sf(stA[,c('X','Y','idGlobal')], coords=c('X','Y'), crs=crs("epsg:4326")), col=1, axes=TRUE, pch=16)
    stations[
      which(stations$idGlobal %in% stA$idGlobal[1:3]),
      'priorite'
    ] <- 'supplementaire4Ra' #cote
    stations[
      which(stations$idGlobal %in% stB$idGlobal[1:3]),
      'priorite'
    ] <- 'supplementaire4Ra' #prof
  }

  switch(
    as.character(annee),
    '2025' = {
      # ajouter 2 stations "exploratoire" fixées par GNSFPB et 3 stations "exploratoires" fixées par PEIFA
      stations[nrow(stations) + 1, ] <- list(
        idGlobal = NA,
        strateOpano = '4TL',
        strateProf = 'cote',
        profondeur = NA,
        priorite = 'exploratoireSud',
        X = -64 - 30.74 / 60,
        Y = 46 + 44.04 / 60
      )
      stations[
        head(
          which(
            stations$priorite %in%
              c('alternative') &
              stations$strateOpano == '4TG_' &
              stations$strateProf == 'cote'
          ),
          4
        ),
        'priorite'
      ] <- 'exploratoireSud'
    },
    '2024' = {
      # ajouter 2 stations "exploratoire" fixées par GNSFPB et 3 stations "exploratoires" fixées par PEIFA
      stations[nrow(stations) + 1:5, ] <- list(
        idGlobal = rep(NA, 5),
        strateOpano = c(rep('4TG', 2), '4TL', rep('4TG', 2)),
        strateProf = rep('cote', 5),
        profondeur = rep(NA, 5),
        priorite = rep('exploratoireSud', 5),
        X = c(
          -60 - 35.38 / 60, #121
          -61 - 14.78 / 60, #122
          -64 - 30.74 / 60, #123
          -63 - 24.80 / 60, #124
          -62 - 38.25 / 60
        ), #125
        Y = c(
          47 + 06.77 / 60, #121
          46 + 41.25 / 60, #122
          46 + 44.04 / 60, #123
          46 + 38.73 / 60, #124
          46 + 37.71 / 60
        ) #125
      )
    },
    '2023' = {
      # ajouter 3 stations "exploratoire" fixées par GNSFPB et 2 stations "exploratoires" fixées par PEIFA
      stations[nrow(stations) + 1:5, ] <- list(
        idGlobal = rep(NA, 5),
        X = c(
          -61 - 03.369 / 60, #121
          -61 - 12.674 / 60, #122
          -64 - 20 / 60 - 48.48 / 3600, #123
          -61 - 42.432 / 60, #124
          -62 - 55 / 60 - 50.82 / 3600
        ), #125
        Y = c(
          46 + 46.519 / 60, #121
          46 + 40.659 / 60, #122
          46 + 57 / 60 + .30 / 3600, #123
          46 + 57.579 / 60, #124
          45 + 51 / 60 + 6.84 / 3600
        ), #125
        strateProf = rep('cote', 5),
        strateOpano = c(rep('4TG', 2), '4TL', rep('4TG', 2)),
        profondeur = rep(NA, 5),
        priorite = rep('exploratoireSud', 5)
      )
    },
    '2022' = {
      # ajouter une station "exploratoire" PEIFA fixée et 4 aléatoires
      stations[nrow(stations) + 1, ] <- list(
        idGlobal = NA,
        X = -64.349033,
        Y = 46.866167,
        strateProf = 'cote',
        strateOpano = '4TL',
        profondeur = NA,
        priorite = 'exploratoireSud'
      )
      stations[
        head(
          which(
            stations$priorite %in%
              c('alternative') &
              stations$strateOpano == '4TG' &
              stations$strateProf == 'cote'
          ),
          4
        ),
        'priorite'
      ] <- 'exploratoireSud'
    },
    '2021' = {
      ## ajouter des stations "exploratoire" PEIFA
      tail(stations)
      stations[nrow(stations) + 1, ] <- list(
        Field1 = (max(stations$Field1) + 1),
        X = -63 - 55.37 / 60,
        Y = 47 + 11.01 / 60,
        strateProf = 'cote',
        UnitArea = '4Tl'
      )
      stations[nrow(stations) + 1, ] <- list(
        Field1 = (max(stations$Field1) + 1),
        X = -62 - 42.81 / 60,
        Y = 46 + 29.38 / 60,
        strateProf = 'cote',
        UnitArea = '4Tl'
      )
      ## ajouter station "exploratoire" GNSFPB
      tail(stations)
    }
  )

  ## mettre des id similaires aux précédentes années
  ## incluant 121à125 pour supplémentaires du sgsl, 126à131 pour 4Ra ffaw, 200:206 pour 3Pn ffaw)
  stations$id <- NA
  stations[stations$priorite == 'base', 'id'] <- 1:sum(
    stations$priorite == 'base'
  )
  stations[stations$priorite == 'exploratoireSud', 'id'] <- c(
    121,
    122,
    124,
    123,
    125
  ) #2023
  ## stations[stations$priorite=='exploratoireSud','id'] <- 121:125 #2022 et avant
  stations[stations$priorite == 'supplementaire4Ra', 'id'] <- 126 +
    1:sum(stations$priorite == 'supplementaire4Ra') -
    1
  ## enregistrer les stations finales
  write.csv2(
    stations,
    file = file.path(dir_station, 'stationsProposeesAvecID.csv'),
    row.names = FALSE
  )
  openxlsx::write.xlsx(
    stations,
    file.path(dir_station, 'stations_proposees_avecID.xlsx')
  )

  ## ajouter différents format de coordonnées
  stations.temp <- transcrireFormat(coord = stations)
  ##
  write.csv2(
    stations,
    file = file.path(dir_station, 'stationsProposeesAvecID_DMS.csv'),
    row.names = FALSE,
    fileEncoding = 'utf-8',
    quote = FALSE
  )
  openxlsx::write.xlsx(
    stations,
    file.path(dir_station, 'stations_proposees_avecID_DMS.xlsx')
  )

  stations.fin <- subset(
    stations,
    !is.na(id),
    select = c(
      'id',
      'idGlobal',
      'X',
      'Y',
      'strateOpano',
      'strateProf',
      'profondeur',
      'priorite'
    )
  )
  names(stations.fin) <- c(
    'id',
    'idGlobal',
    'X',
    'Y',
    'ssZoneOpano',
    'strateProfondeur',
    'profondeur',
    'choix'
  )
  write.csv2(
    stations.fin,
    file = file.path(dir_station, 'stationsFinales.csv'),
    row.names = FALSE
  )
  openxlsx::write.xlsx(
    stations.fin,
    file.path(dir_station, 'stations_finales.xlsx')
  )
  stations.fin.dms <- subset(
    stations.temp,
    !is.na(id),
    select = c(
      'id',
      'idGlobal',
      'strateOpano',
      'strateProf',
      'profondeur',
      'priorite',
      'Latitude DMM',
      'Longitude DMM',
      'Latitude DMS',
      'Longitude DMS',
      'Latitude DD',
      'Longitude DD'
    )
  )
  names(stations.fin.dms) <- c(
    'id',
    'idGlobal',
    'ssZoneOpano',
    'strateProfondeur',
    'profondeur',
    'choix',
    'Latitude DMM',
    'Longitude DMM',
    'Latitude DMS',
    'Longitude DMS',
    'Latitude DD',
    'Longitude DD'
  )
  write.csv2(
    stations.fin.dms,
    file = file.path(dir_station, 'stationsFinales_DMS.csv'),
    row.names = FALSE
  )
  openxlsx::write.xlsx(
    stations.fin.dms,
    file.path(dir_station, 'stations_finales_DMS.xlsx')
  )
  openxlsx::write.xlsx(
    subset(stations.fin.dms, id %in% 126:131),
    file.path(dir_station, 'stations_supplementaires_FFAW_DMS.xlsx')
  )

  stations
}
