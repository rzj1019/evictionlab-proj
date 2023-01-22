
#### Function to plot each counties eviction rate by state ####
evict.rate.state.county <- function(state) {

   if(!dir.exists(paste0(plots.dir, "/evict_rate"))) {
      dir.create(paste0(plots.dir, "/evict_rate"), recursive = TRUE)
      pdf.path <- paste0(plots.dir, "/evict_rate/", state, "_by_year.pdf")
   } else {
      pdf.path <- paste0(plots.dir, "/evict_rate/", state, "_by_year.pdf")
   }

   state.data <- all.counties[parent.location == state]
   state.years <- state.data[ , unique(year)] %>% sort()
   state.name <- state.data[ , parent.location] %>% unique()

   # open pdf
   pdf(file = pdf.path, width = 11, height = 8.5)
   message("Plotting data for ", state.name)

   # Run state through plot function
   for (y in seq_along(state.years)) {
      curr.year <- state.years[y]
      plot.data <- state.data[year == curr.year]
      temp.plot <- ggplot2::ggplot(plot.data,
                                   aes(x = name, y = eviction.rate)) +
         geom_hline(yintercept = plot.data[!is.na(eviction.rate) , mean(eviction.rate)]) +
         geom_bar(stat = "identity", width = 0.5) +
         labs(title = (paste0(state.name, " Eviction Rates by County")),
              subtitle = curr.year) +
         xlab("County") + ylab("Eviction Rate (%)") +
         theme(axis.text.x = element_text(angle = 90))

      #plot
      plot(temp.plot)
      message("Plotted: ", state.name, " - ", curr.year)
   }
   dev.off()
   message("-----------------------------------")
}


#### Function plot median property value over time with poverty rate labels ####
poverty.median.val <- function(state) {

   if(!dir.exists(paste0(plots.dir, "/med_prop_val"))) {
      dir.create(paste0(plots.dir, "/med_prop_val"), recursive = TRUE)
      pdf.path <- paste0(plots.dir, "/med_prop_val/", state, ".pdf")
   } else {
      pdf.path <- paste0(plots.dir, "/med_prop_val/", state, ".pdf")
   }

   state.data <- all.counties[parent.location == state]
   state.name <- state.data[ , parent.location] %>% unique()
   n.counties <- state.data[ , name] %>% unique()
   n.years <- state.data[ , unique(year)] %>% sort()

   # open pdf
   pdf(file = pdf.path, width = 11, height = 8.5)
   message("Plotting data for ", state.name)


   # Plot all counties: provery rate vs. med. prop. val.
   for (s in seq_along(n.counties)) {

      plot.data <- state.data %>% setorder(name)
      county <- n.counties[s]
      temp.plot <- ggplot2::ggplot(plot.data[name == county],
                                   aes(x = year,
                                       y = median.property.value)) +
         geom_point() +
         geom_line() +
         geom_text_repel(aes(label = poverty.rate),
                         box.padding = 0.5) +
         labs(title = paste0(state.name, ": ", county),
              subtitle = "Median Property Value by Year") +
         scale_x_continuous("Years", labels = n.years, breaks = n.years)


      #plot
      plot(temp.plot)
      message("Plotted: ", state.name, " - ", county)

   }

   dev.off()
   message("---------------------")
}


#### Function plot rent burden v poverty rate; order data by year ####
prb.func <- function(state) {

   if(!dir.exists(paste0(plots.dir, "/prb"))) {
      dir.create(paste0(plots.dir, "/prb"), recursive = TRUE)
      pdf.path <- paste0(plots.dir, "/prb/", state, ".pdf")
   } else {
      pdf.path <- paste0(plots.dir, "/prb/", state, ".pdf")
   }

   state.data <- all.counties[parent.location == state]
   state.name <- state.data[ , parent.location] %>% unique()
   n.counties <- state.data[ , name] %>% unique()

   # open pdf
   pdf(file = pdf.path, width = 11, height = 8.5)
   message("Plotting data for ", state.name)


   # Plot all counties: poverty rate vs. med. prop. val.
   for (s in seq_along(n.counties)) {

      plot.data <- state.data %>% setorder(name)
      county <- n.counties[s]

      temp.plot <- ggplot2::ggplot(plot.data[name == county],
                                   aes(x = rent.burden,
                                       y = poverty.rate)) +
         geom_point() +
         geom_line(alpha = 0.05) +
         labs(title = paste0(state.name, ": ", county),
              subtitle = "Rent Burden Vs Poverty Rate",
              caption = "Some points represent multiple years.")  +
         scale_x_continuous("Rent Burden (%)") +
         scale_y_continuous("Poverty Rate (%)")


      #plot
      plot(temp.plot)
      message("Plotted: ", state.name, " - ", county)

   }

   dev.off()
   message("---------------------")
}

#### Function Plot demographic dist. vs evictions ####
dem.evict.func <- function(demvar) {

   if(!dir.exists(paste0(plots.dir, "/dem_evict"))) {
      dir.create(paste0(plots.dir, "/dem_evict"), recursive = TRUE)
      pdf.path <- paste0(plots.dir, "/dem_evict/", demvar, ".pdf")
   } else {
      pdf.path <- paste0(plots.dir, "/dem_evict/", demvar, ".pdf")
   }

   dem.data <- all.counties[!is.na(demvar)]
   dem.name <- demvar

   # open pdf
   pdf(file = pdf.path, width = 11, height = 8.5)
   message("Plotting data for ", dem.name)

   # plot

   temp.plot <- ggplot2::ggplot(dem.data,
                                aes(x = get(demvar), y = evictions)) +
      geom_point(aes(color = parent.location)) +
      labs(title = paste0(state.name, ": ", county),
           subtitle = paste0(eval(demvar), " Vs Evictions"),
           caption = "Removed values which are NA") +
      xlim(demvar) +
      scale_y_continuous("Evictions")


   plot(temp.plot)
   dev.off()
}