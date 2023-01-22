############################################
##### Data Completeness Exploration ########
############################################

### Completeness Metrics ###

# Generate table of metadata of each state according to block.groups ----------------------------------------------------------------------------

blockgroups.list <- grep(".[A-Z].block.groups", ls(), value = TRUE)
blockgroups.abb <- c()
blockgroups.block.count <- c()
blockgroups.year.count <- c()
blockgroups.row.count <- c()
blockgroups.census.count <- c(534,3438,2147,4178,23212,3532,2585,574,11442,5333,875,
                              2630,9691,4814,2351,3285,3471,4985,1086,8205,4111,4506,
                              842,6155,572,1633,1449,1836,15464,9238,2965,2634,9740,
                              815,3059,654,4125,15811,1690,5332,522,4783,4489,410)
blockgroups.project.row <- c()
blockgroups.level.coverage <- c()
blockgroups.row.coverage <- c()
blockgroups.nas.evict.rate <- c()
blockgroups.nas.evict.count <- c()
blockgroups.nas.evict.filings <- c()
blockgroups.nas.evict.file.rate <- c()
blockgroups.nas.poverty.rate <- c()
blockgroups.nas.rent.burd <- c()
blockgroups.nas.rent.occ.hh <- c()
blockgroups.nas.pct.rent.occ <- c()
blockgroups.nas.med.gross.rent <- c()
blockgroups.nas.med.hh.income <- c()
blockgroups.nas.med.prop.val <- c()
blockgroups.imputed.count <- c()
blockgroups.subbed.count <- c()
blockgroups.lowflag.count <- c()

for (n in seq_along(blockgroups.list)) {

   name <- blockgroups.list[n]
   abb <- stringr::str_sub(name,1,2)
   table <- get(name)

   # Get Current State metadata
   b.count <- table[, name] %>% unique() %>% length()
   y.count <- table[, year] %>% unique() %>% length()
   r.count <- nrow(table)
   row.project <- blockgroups.census.count[n]*y.count
   level.cover <- round(b.count/blockgroups.census.count[n], digits = 2)
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
   blockgroups.abb <- append(blockgroups.abb, abb)
   blockgroups.block.count <- append(blockgroups.block.count, b.count)
   blockgroups.year.count <- append(blockgroups.year.count, y.count)
   blockgroups.row.count <- append(blockgroups.row.count, r.count)
   blockgroups.project.row <- append(blockgroups.project.row,row.project)
   blockgroups.level.coverage <- append(blockgroups.level.coverage, level.cover)
   blockgroups.row.coverage <- append(blockgroups.row.coverage, row.cover)
   blockgroups.nas.evict.rate <- append(blockgroups.nas.evict.rate, na.evict.rate)
   blockgroups.nas.evict.count <- append(blockgroups.nas.evict.count, na.evict.count)
   blockgroups.nas.evict.filings <- append(blockgroups.nas.evict.filings, na.evict.rate)
   blockgroups.nas.evict.file.rate <- append(blockgroups.nas.evict.file.rate, na.evict.file.rate)
   blockgroups.nas.poverty.rate <- append(blockgroups.nas.poverty.rate, na.poverty.rate)
   blockgroups.nas.rent.burd <- append(blockgroups.nas.rent.burd, na.rent.burd)
   blockgroups.nas.rent.occ.hh <- append(blockgroups.nas.rent.occ.hh, na.rent.occ.hh)
   blockgroups.nas.pct.rent.occ <- append(blockgroups.nas.pct.rent.occ, na.pct.rent.occ)
   blockgroups.nas.med.gross.rent <- append(blockgroups.nas.med.gross.rent, na.med.gross.rent)
   blockgroups.nas.med.hh.income <- append(blockgroups.nas.med.hh.income, na.med.hh.income)
   blockgroups.nas.med.prop.val <- append(blockgroups.nas.med.prop.val, na.med.prop.val)
   blockgroups.imputed.count <- append(blockgroups.imputed.count, i.count)
   blockgroups.subbed.count <- append(blockgroups.subbed.count, s.count)
   blockgroups.lowflag.count <- append(blockgroups.lowflag.count, lf.count)

}

states.metadata.blockgroups <- data.table(state.abb = blockgroups.abb,
                                          level.count = blockgroups.block.count,
                                          year.count = blockgroups.year.count,
                                          row.count = blockgroups.row.count,
                                          census.level.count = blockgroups.census.count,
                                          row.projection = blockgroups.project.row,
                                          pct.level.coverage = blockgroups.level.coverage,
                                          pct.row.coverage = blockgroups.row.coverage,
                                          NAs.evict.rate = blockgroups.nas.evict.rate,
                                          NAs.evict.count = blockgroups.nas.evict.count,
                                          NAs.evict.filiings = blockgroups.nas.evict.filings,
                                          NAs.evict.filiing.rate = blockgroups.nas.evict.file.rate,
                                          NAs.evict.poverty.rate = blockgroups.nas.poverty.rate,
                                          NAs.evict.rent.burden = blockgroups.nas.rent.burd,
                                          NAs.evict.rent.occ.hh = blockgroups.nas.rent.occ.hh,
                                          NAs.evict.rent.pct.rent.occ = blockgroups.nas.pct.rent.occ,
                                          NAs.evict.med.gross.rent = blockgroups.nas.med.gross.rent,
                                          NAs.evict.med.hh.income = blockgroups.nas.med.hh.income,
                                          NAs.evict.med.prop.val = blockgroups.nas.med.prop.val,
                                          imputed.count = blockgroups.imputed.count,
                                          subbed.count = blockgroups.subbed.count,
                                          low.flag.count = blockgroups.lowflag.count)
states.metadata.blockgroups[, level:="block.group"]
setcolorder(states.metadata.blockgroups, c("state.abb",
                                           "level",
                                           "level.count",
                                           "year.count",
                                           "row.count",
                                           "census.level.count",
                                           "row.projection",
                                           "pct.level.coverage",
                                           "pct.row.coverage",
                                           "imputed.count",
                                           "subbed.count",
                                           "low.flag.count"))

# Generate table of metadata of each state according to tracts ----------------------------------------------------------------------------

tracts.list <- grep(".[A-Z].tracts", ls(), value = TRUE)
tracts.abb <- c()
tracts.tract.count <- c()
tracts.year.count <- c()
tracts.row.count <- c()
tracts.census.count <- c(167,1181,686,1526,8057,1249,833,218,4245,1969,351,
                         825,3123,1511,770,1115,1148,1478,358,2813,1338,1393,
                         271,2195,205,532,499,687,4919,2952,1046,834,3218,
                         244,1103,222,1497,5265,588,1907,184,1458,1409,132)
tracts.project.row <- c()
tracts.level.coverage <- c()
tracts.row.coverage <- c()
tracts.nas.evict.rate <- c()
tracts.nas.evict.count <- c()
tracts.nas.evict.filings <- c()
tracts.nas.evict.file.rate <- c()
tracts.nas.poverty.rate <- c()
tracts.nas.rent.burd <- c()
tracts.nas.rent.occ.hh <- c()
tracts.nas.pct.rent.occ <- c()
tracts.nas.med.gross.rent <- c()
tracts.nas.med.hh.income <- c()
tracts.nas.med.prop.val <- c()
tracts.imputed.count <- c()
tracts.subbed.count <- c()
tracts.lowflag.count <- c()

for (n in seq_along(tracts.list)) {

   name <- tracts.list[n]
   abb <- stringr::str_sub(name,1,2)
   table <- get(name)

   # Get Current State metadata
   t.count <- table[, name] %>% unique() %>% length()
   y.count <- table[, year] %>% unique() %>% length()
   r.count <- nrow(table)
   row.project <- tracts.census.count[n]*y.count
   level.cover <- round(t.count/tracts.census.count[n], digits = 2)
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
   i.count <- table[, sum(imputed)]
   s.count <- table[, sum(subbed)]
   lf.count <- table[, sum(low.flag)]

   # Appending data
   tracts.abb <- append(tracts.abb, abb)
   tracts.tract.count <- append(tracts.tract.count, t.count)
   tracts.year.count <- append(tracts.year.count, y.count)
   tracts.row.count <- append(tracts.row.count, r.count)
   tracts.project.row <- append(tracts.project.row,row.project)
   tracts.level.coverage <- append(tracts.level.coverage, level.cover)
   tracts.row.coverage <- append(tracts.row.coverage, row.cover)
   tracts.nas.evict.rate <- append(tracts.nas.evict.rate, na.evict.rate)
   tracts.nas.evict.count <- append(tracts.nas.evict.count, na.evict.count)
   tracts.nas.evict.filings <- append(tracts.nas.evict.filings, na.evict.rate)
   tracts.nas.evict.file.rate <- append(tracts.nas.evict.file.rate, na.evict.file.rate)
   tracts.nas.poverty.rate <- append(tracts.nas.poverty.rate, na.poverty.rate)
   tracts.nas.rent.burd <- append(tracts.nas.rent.burd, na.rent.burd)
   tracts.nas.rent.occ.hh <- append(tracts.nas.rent.occ.hh, na.rent.occ.hh)
   tracts.nas.pct.rent.occ <- append(tracts.nas.pct.rent.occ, na.pct.rent.occ)
   tracts.nas.med.gross.rent <- append(tracts.nas.med.gross.rent, na.med.gross.rent)
   tracts.nas.med.hh.income <- append(tracts.nas.med.hh.income, na.med.hh.income)
   tracts.nas.med.prop.val <- append(tracts.nas.med.prop.val, na.med.prop.val)
   tracts.imputed.count <- append(tracts.imputed.count, i.count)
   tracts.subbed.count <- append(tracts.subbed.count, s.count)
   tracts.lowflag.count <- append(tracts.lowflag.count, lf.count)

}

states.metadata.tracts <- data.table(state.abb = tracts.abb,
                                     level.count = tracts.tract.count,
                                     year.count = tracts.year.count,
                                     row.count = tracts.row.count,
                                     census.level.count = tracts.census.count,
                                     row.projection = tracts.project.row,
                                     pct.level.coverage = tracts.level.coverage,
                                     pct.row.coverage = tracts.row.coverage,
                                     NAs.evict.rate = tracts.nas.evict.rate,
                                     NAs.evict.count = tracts.nas.evict.count,
                                     NAs.evict.filiings = tracts.nas.evict.filings,
                                     NAs.evict.filiing.rate = tracts.nas.evict.file.rate,
                                     NAs.evict.poverty.rate = tracts.nas.poverty.rate,
                                     NAs.evict.rent.burden = tracts.nas.rent.burd,
                                     NAs.evict.rent.occ.hh = tracts.nas.rent.occ.hh,
                                     NAs.evict.rent.pct.rent.occ = tracts.nas.pct.rent.occ,
                                     NAs.evict.med.gross.rent = tracts.nas.med.gross.rent,
                                     NAs.evict.med.hh.income = tracts.nas.med.hh.income,
                                     NAs.evict.med.prop.val = tracts.nas.med.prop.val,
                                     imputed.count = tracts.imputed.count,
                                     subbed.count = tracts.subbed.count,
                                     low.flag.count = tracts.lowflag.count)
states.metadata.tracts[, level:="tract"]

# Generate table of metadata of each state according to county.level ----------------------------------------------------------------------------

countylevel.list <- grep(".[A-Z].counties", ls(), value = TRUE)
countylevel.abb <- c()
countylevel.county.count <- c()
countylevel.year.count <- c()
countylevel.row.count <- c()
countylevel.census.count <- c(29,67,75,15,58,64,8,3,67,159,5,
                                 99,102,92,105,120,64,14,16,83,87,115,
                                 56,100,53,93,33,17,62,88,77,36,67,
                                 5,46,66,95,254,29,134,14,39,72,23)
countylevel.project.row <- c()
countylevel.level.coverage <- c()
countylevel.row.coverage <- c()
countylevel.nas.evict.rate <- c()
countylevel.nas.evict.count <- c()
countylevel.nas.evict.filings <- c()
countylevel.nas.evict.file.rate <- c()
countylevel.nas.poverty.rate <- c()
countylevel.nas.rent.burd <- c()
countylevel.nas.rent.occ.hh <- c()
countylevel.nas.pct.rent.occ <- c()
countylevel.nas.med.gross.rent <- c()
countylevel.nas.med.hh.income <- c()
countylevel.nas.med.prop.val <- c()
countylevel.imputed.count <- c()
countylevel.subbed.count <- c()
countylevel.lowflag.count <- c()

for (n in seq_along(countylevel.list)) {

   name <- countylevel.list[n]
   abb <- stringr::str_sub(name,1,2)
   table <- get(name)

   # Get Current State metadata
   c.count <- table[, name] %>% unique() %>% length()
   y.count <- table[, year] %>% unique() %>% length()
   r.count <- nrow(table)
   row.project <- countylevel.census.count[n]*y.count
   level.cover <- round(c.count/countylevel.census.count[n], digits = 2)
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
   i.count <- table[, sum(imputed)]
   s.count <- table[, sum(subbed)]
   lf.count <- table[, sum(low.flag)]

   # Appending data
   countylevel.abb <- append(countylevel.abb, abb)
   countylevel.county.count <- append(countylevel.county.count, c.count)
   countylevel.year.count <- append(countylevel.year.count, y.count)
   countylevel.row.count <- append(countylevel.row.count, r.count)
   countylevel.project.row <- append(countylevel.project.row,row.project)
   countylevel.level.coverage <- append(countylevel.level.coverage, level.cover)
   countylevel.row.coverage <- append(countylevel.row.coverage, row.cover)
   countylevel.nas.evict.rate <- append(countylevel.nas.evict.rate, na.evict.rate)
   countylevel.nas.evict.count <- append(countylevel.nas.evict.count, na.evict.count)
   countylevel.nas.evict.filings <- append(countylevel.nas.evict.filings, na.evict.rate)
   countylevel.nas.evict.file.rate <- append(countylevel.nas.evict.file.rate, na.evict.file.rate)
   countylevel.nas.poverty.rate <- append(countylevel.nas.poverty.rate, na.poverty.rate)
   countylevel.nas.rent.burd <- append(countylevel.nas.rent.burd, na.rent.burd)
   countylevel.nas.rent.occ.hh <- append(countylevel.nas.rent.occ.hh, na.rent.occ.hh)
   countylevel.nas.pct.rent.occ <- append(countylevel.nas.pct.rent.occ, na.pct.rent.occ)
   countylevel.nas.med.gross.rent <- append(countylevel.nas.med.gross.rent, na.med.gross.rent)
   countylevel.nas.med.hh.income <- append(countylevel.nas.med.hh.income, na.med.hh.income)
   countylevel.nas.med.prop.val <- append(countylevel.nas.med.prop.val, na.med.prop.val)
   countylevel.imputed.count <- append(countylevel.imputed.count, i.count)
   countylevel.subbed.count <- append(countylevel.subbed.count, s.count)
   countylevel.lowflag.count <- append(countylevel.lowflag.count, lf.count)

}

states.metadata.countylevel <- data.table(state.abb = countylevel.abb,
                                     level.count = countylevel.county.count,
                                     year.count = countylevel.year.count,
                                     row.count = countylevel.row.count,
                                     census.level.count = countylevel.census.count,
                                     row.projection = countylevel.project.row,
                                     pct.level.coverage = countylevel.level.coverage,
                                     pct.row.coverage = countylevel.row.coverage,
                                     NAs.evict.rate = countylevel.nas.evict.rate,
                                     NAs.evict.count = countylevel.nas.evict.count,
                                     NAs.evict.filiings = countylevel.nas.evict.filings,
                                     NAs.evict.filiing.rate = countylevel.nas.evict.file.rate,
                                     NAs.evict.poverty.rate = countylevel.nas.poverty.rate,
                                     NAs.evict.rent.burden = countylevel.nas.rent.burd,
                                     NAs.evict.rent.occ.hh = countylevel.nas.rent.occ.hh,
                                     NAs.evict.rent.pct.rent.occ = countylevel.nas.pct.rent.occ,
                                     NAs.evict.med.gross.rent = countylevel.nas.med.gross.rent,
                                     NAs.evict.med.hh.income = countylevel.nas.med.hh.income,
                                     NAs.evict.med.prop.val = countylevel.nas.med.prop.val,
                                     imputed.count = countylevel.imputed.count,
                                     subbed.count = countylevel.subbed.count,
                                     low.flag.count = countylevel.lowflag.count)
states.metadata.countylevel[, level:="county"]


# metadata all levels ---------------------------------------------------------------------------------------------------------------------------

states.metadata.all <- rbind(states.metadata.blockgroups,
                             states.metadata.tracts,
                             states.metadata.countylevel)
setcolorder(states.metadata.all, c("state.abb",
                                   "level",
                                   "level.count",
                                   "year.count",
                                   "row.count",
                                   "census.level.count",
                                   "row.projection",
                                   "pct.level.coverage",
                                   "pct.row.coverage",
                                   "imputed.count",
                                   "subbed.count",
                                   "low.flag.count"))
states.metadata.all <- states.metadata.all[order(state.abb)]



eviction_metadata("block.groups")

# Write to Google -------------------------------------------------------------------------------------------------------------------------------
TDA.file <- as_sheets_id("https://docs.google.com/spreadsheets/d/1o1YUPIleg0LeakjJ9AePPJlx2c6nA_mEXkQxLv5nXsg/edit#gid=0")
googlesheets4::write_sheet(states.metadata.all,
                           ss = TDA.file,
                           sheet = "Metadata")

