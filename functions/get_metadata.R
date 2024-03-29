# Data metadata ---------------------------------------------------------------------------------------------------------------------------------
get_metadata <- function(census.level){

  # Initialize function and check args
  level.list <- grep(paste0(".[A-Z].",census.level), ls(envir = .GlobalEnv), value = TRUE)
  if (census.level == "block.groups" & level.list %>% length() > 0) {
     level.census.count <- c(534,3438,2147,4178,23212,3532,2585,574,11442,5333,875,
                            2630,9691,4814,2351,3285,3471,4985,1086,8205,4111,4506,
                            842,6155,572,1633,1449,1836,15464,9238,2965,2634,9740,
                            815,3059,654,4125,15811,1690,5332,522,4783,4489,410)
  } else if (census.level == "tracts" & level.list %>% length() > 0) {
    level.census.count <- c(167,1181,686,1526,8057,1249,833,218,4245,1969,351,
                            825,3123,1511,770,1115,1148,1478,358,2813,1338,1393,
                            271,2195,205,532,499,687,4919,2952,1046,834,3218,
                            244,1103,222,1497,5265,588,1907,184,1458,1409,132)
  } else if (census.level == "counties" & level.list %>% length() > 0) {
    level.census.count <- c(29,67,75,15,58,64,8,3,67,159,5,
                            99,102,92,105,120,64,14,16,83,87,115,
                            56,100,53,93,33,17,62,88,77,36,67,
                            5,46,66,95,254,29,134,14,39,72,23)
  } else {
    stop(paste0("Error: ",census.level," not in list, or no data for Census Level."))
  }

  # create columns
  level.abb <- c()
  level.count <- c()
  level.year.count <- c()
  level.row.count <- c()
  level.project.row <- c()
  level.coverage <- c()
  level.row.coverage <- c()
  level.nas.evict.rate <- c()
  level.nas.evict.count <- c()
  level.nas.evict.filings <- c()
  level.nas.evict.file.rate <- c()
  level.nas.poverty.rate <- c()
  level.nas.rent.burd <- c()
  level.nas.rent.occ.hh <- c()
  level.nas.pct.rent.occ <- c()
  level.nas.med.gross.rent <- c()
  level.nas.med.hh.income <- c()
  level.nas.med.prop.val <- c()
  level.imputed.count <- c()
  level.subbed.count <- c()
  level.lowflag.count <- c()

  for (n in level.list %>% seq_along()) {

    t.name <- level.list[n]
    abb <- stringr::str_sub(t.name,1,2)
    table <- get(t.name)

    # Get Current State metadata
    l.count <- table[, name] %>% unique() %>% length()
    y.count <- table[, year] %>% unique() %>% length()
    r.count <- nrow(table)
    row.project <- level.census.count[n]*y.count
    level.cover <- round(l.count/level.census.count[n], digits = 2)
    row.cover <- round(r.count/row.project, digits = 2)
    na.evict.rate <- table[, eviction.rate] %>% is.na() %>% sum()
    na.evict.count <- table[, evictions] %>% is.na() %>% sum()
    na.evict.evict.filings <- table[, eviction.filings] %>% is.na() %>% sum()
    na.evict.file.rate <- table[, eviction.filing.rate] %>% is.na() %>% sum()
    na.poverty.rate <- table[, poverty.rate] %>% is.na() %>% sum()
    na.rent.burd <- table[, rent.burden] %>% is.na() %>% sum()
    na.rent.occ.hh <- table[, renter.occupied.households] %>% is.na() %>% sum()
    na.pct.rent.occ <- table[, pct.renter.occupied] %>% is.na() %>% sum()
    na.med.gross.rent <- table[, median.gross.rent] %>% is.na() %>% sum()
    na.med.hh.income <- table[, median.household.income] %>% is.na() %>% sum()
    na.med.prop.val <- table[, median.property.value] %>% is.na() %>% sum()
    i.count <- table[, imputed] %>% sum()
    s.count <- table[, subbed] %>% sum()
    lf.count <- table[, low.flag] %>% sum()

    # Appending data
    level.abb <- append(level.abb, abb)
    level.count <- append(level.count, l.count)
    level.year.count <- append(level.year.count, y.count)
    level.row.count <- append(level.row.count, r.count)
    level.project.row <- append(level.project.row,row.project)
    level.coverage <- append(level.coverage, level.cover)
    level.row.coverage <- append(level.row.coverage, row.cover)
    level.nas.evict.rate <- append(level.nas.evict.rate, na.evict.rate)
    level.nas.evict.count <- append(level.nas.evict.count, na.evict.count)
    level.nas.evict.filings <- append(level.nas.evict.filings, na.evict.rate)
    level.nas.evict.file.rate <- append(level.nas.evict.file.rate, na.evict.file.rate)
    level.nas.poverty.rate <- append(level.nas.poverty.rate, na.poverty.rate)
    level.nas.rent.burd <- append(level.nas.rent.burd, na.rent.burd)
    level.nas.rent.occ.hh <- append(level.nas.rent.occ.hh, na.rent.occ.hh)
    level.nas.pct.rent.occ <- append(level.nas.pct.rent.occ, na.pct.rent.occ)
    level.nas.med.gross.rent <- append(level.nas.med.gross.rent, na.med.gross.rent)
    level.nas.med.hh.income <- append(level.nas.med.hh.income, na.med.hh.income)
    level.nas.med.prop.val <- append(level.nas.med.prop.val, na.med.prop.val)
    level.imputed.count <- append(level.imputed.count, i.count)
    level.subbed.count <- append(level.subbed.count, s.count)
    level.lowflag.count <- append(level.lowflag.count, lf.count)
  }

  level.dt <- data.table(state.abb = level.abb,
                         level = census.level,
                         level.count = level.count,
                         year.count = level.year.count,
                         row.count = level.row.count,
                         census.level.count = level.census.count,
                         row.projection = level.project.row,
                         pct.level.coverage = level.coverage,
                         pct.row.coverage = level.row.coverage,
                         NAs.evict.rate = level.nas.evict.rate,
                         NAs.evict.count = level.nas.evict.count,
                         NAs.evict.filiings = level.nas.evict.filings,
                         NAs.evict.filiing.rate = level.nas.evict.file.rate,
                         NAs.evict.poverty.rate = level.nas.poverty.rate,
                         NAs.evict.rent.burden = level.nas.rent.burd,
                         NAs.evict.rent.occ.hh = level.nas.rent.occ.hh,
                         NAs.evict.rent.pct.rent.occ = level.nas.pct.rent.occ,
                         NAs.evict.med.gross.rent = level.nas.med.gross.rent,
                         NAs.evict.med.hh.income = level.nas.med.hh.income,
                         NAs.evict.med.prop.val = level.nas.med.prop.val,
                         imputed.count = level.imputed.count,
                         subbed.count = level.subbed.count,
                         low.flag.count = level.lowflag.count)
  return(level.dt)
}