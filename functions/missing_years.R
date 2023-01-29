# Finding candidate missing years for each county -----------------------------------------------------------------------------------------------
missing_years <- function(candidate){

   table <- paste0(candidate,".counties") %>% get()
   counties <- table[, name] %>% unique()
   years <- table[, year] %>% sort() %>% unique()

   all.combos <- expand.grid(name = counties, year = years)
   table.combos <- table[, .(name,year)]

   table.diff <- setdiff(all.combos, table.combos)
   return(table.diff)
}