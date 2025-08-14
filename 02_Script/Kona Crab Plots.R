
#Loading libraries
rm(list=ls())
library(dplyr)
library(this.path)
library(tidyr)
library(tidyverse)
library(lubridate)
library(forcats)

root_dir <- this.path::here(..=1) #Root directory is one folder above where this script is

FRS = read.csv(file.path (root_dir, "01_Data/WH2024.csv")) # WH2024 based on warehouse data in picfinfish MYSQL database using query species_fk = 701 or gear_fk = 40 (13916 records)
MHI = read.csv(file.path (root_dir, "01_Data/MHI_Yau.csv"))

## Area code 16123 (instead of 16123I, 16123F, and 16123C) is included for MHI area code.
#Area codes 16123I, 16123F, and 16123C are not contained in FRS (up to 2024)

State = read.csv(file.path (root_dir, "01_Data/State.csv"))
Dealer = read.csv(file.path (root_dir, "01_Data/Dealer_WH2024.csv"))

Dealer$year2 = as.factor(substr(Dealer$REPORT_DATE, 1, 4))
FRS$water = ifelse(FRS$AREA_FK %in% State$code, 1, 2) # 1 = state waters, 2 = federal waters
FRS$zone = ifelse(FRS$AREA_FK %in% State$code, 1, ifelse(FRS$AREA_FK == 331, 2, 3)) # 1 = state waters, 2 = Penguin banks, 3 = federal waters
FRS$year = FRS$REPORT_YEAR
FRS$catch = FRS$LBS_KEPT
FRS$year2 = as.factor(FRS$year) # having year as a factor allows us to use .drop in group_by to avoid losing years with no data
years = sort(unique(FRS$year), decreasing = F)
year_groups <- c("2000–2002", "2003–2005", "2006–2008", "2009–2011", "2012-2014")
year_groups2 <- c("1959-1961", "1962-1964", "1965-1967", "1968-1970",
                  "1971-1973", "1974-1976", "1977-1979", "1980-1982", "1983-1985", "1986-1988",
                  "1989-1991", "1992-1994", "1995-1997","1998-2000", "2001-2003", "2004-2006", "2007-2009", "2010-2012", "2013-2015",
                  "2016-2018", "2019-2021", "2022-2024")
FRS_catch = FRS[FRS$AREA_FK %in% MHI$code & FRS$REPORT_YEAR < 2025 & FRS$SPECIES_FK == 701,] # used to compute catch -- MHI and kona crab only and remove incomplete data from 2025
FRS_catchrate = FRS[FRS$AREA_FK%in%MHI$code & FRS$REPORT_YEAR < 2025 & FRS$GEAR_FK == 40,] # used to compute effort and catch rate -- MHI and dominant gear only and remove incomplete data from 2025
FRS_catchrate$kc_catch = ifelse(FRS_catchrate$SPECIES_FK == 701, FRS_catchrate$LBS_KEPT, 0)
pair_colors = c("gray40", "gray70")
state_colors = c("indianred3", "indianred4")
penguin_bank_colors = c("goldenrod1", "darkgoldenrod")
federal_colors = c("deepskyblue3", "deepskyblue4")
water_colors = c(federal_colors[1], state_colors[1])
zone_colors = c(federal_colors[1], penguin_bank_colors[1], state_colors[1])

#Plot function
plot_kc = function(file_name, plot_data, plot_years, ylab, plot_cols, filled, legend, legend_location = "topright", set_y_max = NA) {
  x_min = min(years[plot_years])
  x_max = max(years[plot_years])
  y_min = 0
  y_max = NA
  if(is.na(set_y_max)) {
    if(filled) {
      y_max = max(apply(plot_data[, plot_years], c(2), sum), na.rm = T) * 1.1
    } else {
      y_max = max(plot_data[, plot_years], na.rm = T) * 1.1
    }
  } else {
    y_max = set_y_max
  }
  xs = pretty(c(x_min, x_max), n = 4)
  xs = xs[xs >= x_min & xs <= x_max]
  ys = pretty(c(y_min, y_max), n = 4)
  ys = ys[ys >= y_min & ys <= y_max]
  #pdf(paste0(root_dir, "/03_Outputs/", file_name, ".pdf"), width = 6.5, height = 5, pointsize = 12)
  png(paste0(root_dir, "/03_Outputs/", file_name, ".png"), width = 6.5, height = 5, units = "in", res = 300, pointsize = 12)
  par(mar = c(4.5, 4.5, 1, 1))
  plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = ylab)
  axis(side = 1, at = xs)
  axis(side = 2, at = ys, las = 1)
  n_row = dim(plot_data)[1]
  for(i in 1:n_row) {
    if(filled) {
      polygon(x = c(min(years[plot_years]), years[plot_years], max(years[plot_years])), y = c(0, apply(plot_data[i:n_row, plot_years, drop = F], c(2), sum), 0), col = plot_cols[i], border = NA)
    } else {
      lines(x = years[plot_years], y = plot_data[i, plot_years], col = plot_cols[i], lwd = 3)
      n_col = dim(plot_data)[2]
      for(j in 1:n_col) {
        point_before = ifelse(j == 1, NA, plot_data[i, j - 1])
        point_after = ifelse(j == n_col, NA, plot_data[i, j + 1])
        
        if(is.na(point_before) & is.na(point_after)) {
          points(x = years[j], y = plot_data[i, j], col = plot_cols[i])
        }
      }
    }
    
  }
  box(which = "plot", lty = "solid")
  if(any(!is.na(legend))) {
    if(filled) {
      legend(x = legend_location, legend = legend, col = plot_cols, pch = 15, bty = "n")
    } else {
      legend(x = legend_location, legend = legend, col = plot_cols, lwd = 3, bty = "n")
    }
  }
  dev.off()
}

#Plot function for grouped years
plot_kc_grouped <- function(file_name, plot_data, year_groups, ylab, plot_cols, filled, legend, legend_location = "topright", set_y_max = NA) {
  
  x_vals <- 1:length(year_groups)  # Numeric x positions for year_groups
  y_min <- 0
  y_max <- if (is.na(set_y_max)) {
    if (filled) {
      max(apply(plot_data, 2, sum), na.rm = TRUE) * 1.1
    } else {
      max(plot_data, na.rm = TRUE) * 1.1
    }
  } else {
    set_y_max
  }
  
  #pdf(paste0(root_dir, "/03_Outputs/", file_name, ".pdf"), width = 6.5, height = 5, pointsize = 12)
  png(paste0(root_dir, "/03_Outputs/", file_name, ".png"), width = 6.5, height = 5, units = "in", res = 300, pointsize = 12)
  par(mar = c(4.5, 4.6, 1, 4),      # Increase bottom and left margins
      mgp = c(3.5, 1, 0),       # Push axis titles further from tick labels
      las = 1                  # Horizontal Y-axis tick labels (optional)
  )
  plot(NA, xlim = range(x_vals), ylim = c(y_min, y_max), axes = FALSE, xaxs = "i", yaxs = "i", xlab = "Year Group", ylab = ylab)
  axis(side = 1, at = x_vals, labels = FALSE)  # suppress default labels
  text(x = x_vals, y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
       labels = year_groups, srt = 90, adj = 1, xpd = TRUE, cex = 0.8)
  axis(side = 2, las = 1)
  
  n_row <- nrow(plot_data)
  
  for (i in 1:n_row) {
    if (filled) {
      polygon(x = c(min(x_vals), x_vals, max(x_vals)), 
              y = c(0, apply(plot_data[i:n_row, , drop = FALSE], 2, sum), 0), 
              col = plot_cols[i], border = NA)
    } else {
      lines(x = x_vals, y = plot_data[i, ], col = plot_cols[i], lwd = 3)
      
      n_col <- ncol(plot_data)
      for (j in 1:n_col) {
        point_before <- ifelse(j == 1, NA, plot_data[i, j - 1])
        point_after <- ifelse(j == n_col, NA, plot_data[i, j + 1])
        
        if (is.na(point_before) & is.na(point_after)) {
          points(x = x_vals[j], y = plot_data[i, j], col = plot_cols[i])
        }
      }
    }
  }
  
  box(which = "plot", lty = "solid")
  
  if (any(!is.na(legend))) {
    if (filled) {
      legend(x = legend_location, legend = legend, col = plot_cols, pch = 15, bty = "n")
    } else {
      legend(x = legend_location, legend = legend, col = plot_cols, lwd = 3, bty = "n")
    }
  }
  dev.off()
}


# Plot function for line plot
plot_kc_line_grouped <- function(file_name, plot_data, group_labels, ylab, plot_cols, legend = NA) {
  y_min <- 0
  y_max <- max(plot_data, na.rm = TRUE) * 1.1
  
  #pdf(paste0(root_dir, "/03_Outputs/", file_name, ".pdf"), width = 6.5, height = 5, pointsize = 12)
  png(paste0(root_dir, "/03_Outputs/", file_name, ".png"), width = 6.5, height = 5, units = "in", res = 300, pointsize = 12)
  par(mar = c(4.5, 4.5, 1, 2))
  plot(NA, xlim = c(1, length(group_labels)), ylim = c(y_min, y_max), axes = FALSE, xaxs = "i", yaxs = "i",
       xlab = "Year Group", ylab = ylab)
  axis(1, at = 1:length(group_labels), labels = group_labels)
  axis(2, las = 1)
  box(which = "plot", lty = "solid")
  
  lines(1:length(group_labels), plot_data[1, ], col = plot_cols, lwd = 3)
  #points(1:length(group_labels), plot_data[1, ], col = plot_cols, pch = 16)
  
  # Add "Penguin Banks" label
  legend("topright", legend = "Penguin Bank", col = plot_cols, lty = 1, lwd = 3, bty = "n")
  
  dev.off()
}


#Modified plot function to receive long data format from dplyr
plot_kc_dplyr <- function(file_name, plot_data_list, ylab, plot_cols, filled = FALSE, legend = NULL, legend_location = "topright", set_y_max = NA, year_range = NULL) {
  # Convert year2 to numeric inside each df and optionally filter by year_range
  plot_data_list <- lapply(plot_data_list, function(df) {
    df$year2 <- as.numeric(as.character(df$year2))
    if(!is.null(year_range)) {
      df <- df %>% filter(year2 >= year_range[1], year2 <= year_range[2])
    }
    df
  })
  
  # Combine all years from filtered data
  all_years <- sort(unique(unlist(lapply(plot_data_list, function(df) df$year2))))
  
  if(length(all_years) == 0) {
    stop("No data available in the specified year range.")
  }
  
  cpue_matrix <- sapply(plot_data_list, function(df) {
    cpue_vec <- rep(NA, length(all_years))
    match_years <- match(df$year2, all_years)
    cpue_vec[match_years] <- df$cpue
    return(cpue_vec)
  })
  cpue_matrix <- t(cpue_matrix)
  
  x_min <- if(!is.null(year_range)) year_range[1] else min(all_years)
  x_max <- if(!is.null(year_range)) year_range[2] else max(all_years)
  y_min <- 0
  y_max <- if (is.na(set_y_max)) {
    if (filled) {
      max(colSums(cpue_matrix, na.rm = TRUE)) * 1.1
    } else {
      max(cpue_matrix, na.rm = TRUE) * 1.1
    }
  } else {
    set_y_max
  }
  
  xs <- pretty(c(x_min, x_max), n = 4)
  xs <- xs[xs >= x_min & xs <= x_max]
  ys <- pretty(c(y_min, y_max), n = 4)
  ys <- ys[ys >= y_min & ys <= y_max]
  
  png(paste0(root_dir, "/03_Outputs/", file_name, ".png"), width = 6.5, height = 5, units = "in", res = 300, pointsize = 12)
  par(mar = c(4.5, 4.5, 1, 1))
  plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = FALSE, xaxs = "i", yaxs = "i", xlab = "Year", ylab = ylab)
  axis(side = 1, at = xs)
  axis(side = 2, at = ys, las = 1)
  
  n_row <- nrow(cpue_matrix)
  n_col <- ncol(cpue_matrix)
  
  for (i in 1:n_row) {
    if (filled) {
      polygon(x = c(x_min, all_years, x_max),
              y = c(0, colSums(cpue_matrix[i:n_row, , drop = FALSE], na.rm = TRUE), 0),
              col = plot_cols[i], border = NA)
    } else {
      lines(x = all_years, y = cpue_matrix[i, ], col = plot_cols[i], lwd = 3)
      for (j in 1:n_col) {
        point_before <- ifelse(j == 1, NA, cpue_matrix[i, j - 1])
        point_after  <- ifelse(j == n_col, NA, cpue_matrix[i, j + 1])
        if (is.na(point_before) & is.na(point_after) & !is.na(cpue_matrix[i, j])) {
          points(x = all_years[j], y = cpue_matrix[i, j], col = plot_cols[i])
        }
      }
    }
  }
  
  box(which = "plot", lty = "solid")
  if (!is.null(legend)) {
    if (filled) {
      legend(x = legend_location, legend = legend, col = plot_cols, pch = 15, bty = "n")
    } else {
      legend(x = legend_location, legend = legend, col = plot_cols, lwd = 3, bty = "n")
    }
  }
  dev.off()
}


#-------------------------------------------------------------------------#
# Fig.2: Catch vs Year, separately for MHI and entire archipelago
mhi = summarise(group_by(FRS[FRS$AREA_FK %in% MHI$code & FRS$SPECIES_FK == 701,], REPORT_YEAR), n = sum(LBS_KEPT))$n
archipelago = summarise(group_by(FRS[FRS$SPECIES_FK == 701,], REPORT_YEAR), n = sum(LBS_KEPT))$n
plot_years = years <= 2024
plot(x = years[plot_years], y = mhi[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], archipelago[plot_years], col = "orange")
plot_kc(file_name = "Fig.2", plot_data = matrix(c(archipelago - mhi, mhi) / 1000, nrow = 2, byrow = T), plot_years, ylab = "Catch (x 1000 lb)", plot_cols = pair_colors, filled = T, legend = c("NW Hawaiian Islands/Unknown", "Main Hawaiian Islands"))
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
# Fig.3: Catch vs year, stacked for state, and federal
#Create a zone for All federal together = 4
FRS_catch <- FRS_catch %>%
  mutate(zone_comb = case_when(zone == 3 | zone == 2 ~ 4,
                               TRUE ~ zone))
federal = summarise(group_by(FRS_catch[FRS_catch$zone_comb == 4,], year2, .drop = F), n = sum(catch))$n
state = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n
plot_years = years <= 2024
plot(x = years[plot_years], y = federal[plot_years] + state[plot_years], type = 'l', col = "darkblue")
lines(x = years[plot_years], y = state[plot_years], col = "indianred4")
#lines(x = years[plot_years], y = state[plot_years], col = "lightblue")
plot_kc(file_name = "Fig.3", plot_data = matrix(c(federal, state) / 1000, nrow = 2, byrow = T), plot_years, ylab = "Catch (x 1000 lb)", plot_cols = water_colors, filled = T, legend = c("Federal", "State"))
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
#Fig.4 but all federal waters combined
federal = summarise(group_by(FRS_catch[FRS_catch$zone_comb == 4,], year2, .drop = F), n = sum(catch))$n
state = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n
plot_years = years >= 2014 & years <= 2024
plot(x = years[plot_years], y = federal[plot_years] + state[plot_years], type = 'l', col = "darkblue")
lines(x = years[plot_years], y = state[plot_years], col = "orange")
plot_kc(file_name = "Fig.4", plot_data = matrix(c(federal, state), nrow = 2, byrow = T), plot_years, ylab = "Catch (lb)", plot_cols = water_colors, filled = T, legend = c("Federal", "State"))
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
# Fig.5 putting all federal catch together and combining years into 3 year groups:
#2000-2002; 2003-2005; 2006-2008; 2009-2011; 2012-2014
# 1. Create year groupings
group_breaks <- c(1999, 2002, 2005, 2008, 2011, 2014)
group_labels <- c("2000-2002", "2003-2005", "2006-2008", "2009-2011", "2012-2014")

# 2. Add year group column
FRS_catchrate$year_group <- cut(FRS_catchrate$year, breaks = group_breaks, labels = group_labels)

# 3. Aggregate total trips (distinct FISHER+DATE combos)
FRS_catchrate$trip_id <- paste(FRS_catchrate$FISHER_LIC_FK, FRS_catchrate$FISHED_DATE)
agg_trips <- aggregate(trip_id ~ year_group, data = FRS_catchrate[FRS_catchrate$zone == 2, ],
                       FUN = function(x) length(unique(x)))
names(agg_trips)[2] <- "all_trips"

# 4. Aggregate unique fishers per group
# Make sure year_group excludes NA and empty levels
FRS_filtered <- FRS_catchrate[FRS_catchrate$zone == 2 & !is.na(FRS_catchrate$year_group), ]
FRS_filtered$year_group <- droplevels(FRS_filtered$year_group)

agg_fishers <- aggregate(FISHER_LIC_FK ~ year_group, data = FRS_filtered,
                         FUN = function(x) length(unique(x)))
names(agg_fishers)[2] <- "licenses"

# 5. Merge both
agg <- merge(agg_trips, agg_fishers, by = "year_group")
group_labels <- as.character(agg$year_group)

# 6. Prepare data matrices
trips_data_grouped <- matrix(agg$all_trips, nrow = 1, byrow = TRUE)
licenses_data_grouped <- matrix(agg$licenses, nrow = 1, byrow = TRUE)

# 8. Plot individual PDFs
plot_kc_grouped("Fig.5.a", trips_data_grouped, group_labels, "Number of Trips", plot_cols = "darkgoldenrod", filled = TRUE, legend = NA)
plot_kc_grouped("Fig.5.b", licenses_data_grouped, group_labels, "Number of Licenses", plot_cols = "black", filled = FALSE, legend = NA)

# 9. Combined dual-axis plot (Grouped_E.pdf)
y_min <- 0
y_max_trips <- max(trips_data_grouped, na.rm = TRUE) * 1.1
y_max_licenses <- max(licenses_data_grouped, na.rm = TRUE) * 1.1
ys_trips <- pretty(c(y_min, y_max_trips), n = 4)
ys_licenses <- pretty(c(y_min, y_max_licenses), n = 4)

#pdf(paste0(root_dir, "/03_Outputs/Grouped_E.pdf"), width = 6.5, height = 5, pointsize = 12)
png(paste0(root_dir, "/03_Outputs/Fig.5.png"), width = 6.5, height = 5, units = "in", res = 300, pointsize = 12)
par(mar = c(4.5, 4.5, 1, 5))
plot(NA, xlim = c(1, length(group_labels)), ylim = c(0, 1.12), axes = FALSE, xaxs = "i", yaxs = "i", xlab = "Year Group", ylab = "")
axis(1, at = 1:length(group_labels), labels = group_labels)
axis(2, at = ys_trips / y_max_trips, labels = ys_trips, las = 1)
axis(4, at = ys_licenses / y_max_licenses, labels = ys_licenses, las = 1)
mtext(side = 2, "Number of Trips", line = 3)
mtext(side = 4, "Number of Licenses", line = 3)

# Plot trips (as area)
polygon(x = c(1, 1:length(group_labels), length(group_labels)),
        y = c(0, trips_data_grouped[1, ], 0) / y_max_trips,
        col = "darkgoldenrod", border = NA)

# Plot licenses
lines(1:length(group_labels), licenses_data_grouped[1, ] / y_max_licenses, col = "black", lty = "dashed", lwd = 2)

box(which = "plot", lty = "solid")
legend("topleft", legend = c("Total Trips to Penguin Bank", "Licenses"),
       col = c("darkgoldenrod", "black"), lty = c(NA, "dashed"), lwd = c(NA, 2), pch = c(15, NA), bty = "n")
dev.off()
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
#Fig.6 for grouped years
# Filter zone 2 data
FRS_zone2 <- FRS_catchrate[FRS_catchrate$zone == 2 & !is.na(FRS_catchrate$year_group), ]
FRS_zone2$trip_id <- paste(FRS_zone2$FISHER_LIC_FK, FRS_zone2$FISHED_DATE)

# Calculate total catch and number of trips per group
agg <- FRS_zone2 %>%
  group_by(year_group) %>%
  summarize(
    total_catch = sum(kc_catch, na.rm = TRUE),
    total_trips = n_distinct(trip_id),
    avg_catch_rate = total_catch / total_trips,
    .groups = "drop"
  )

group_labels <- as.character(agg$year_group)
avg_catch_rate_data <- matrix(agg$avg_catch_rate, nrow = 1, byrow = TRUE)

plot_kc_line_grouped("Fig.6", avg_catch_rate_data, group_labels,
                     ylab = "Average Catch Rate (lb/trip)", plot_cols = "darkgoldenrod")
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
# Fig.7: # Trips (federal and state) and licenses (federal and state) vs year
trips_federal = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 3,], year2, .drop = F), n = n_distinct(paste(FISHER_LIC_FK, FISHED_DATE)))$n # number of trips to federal waters by year
trips_state = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 1,], year2, .drop = F), n = n_distinct(paste(FISHER_LIC_FK, FISHED_DATE)))$n # number of trips to state waters by year
licenses_federal = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 3,], year2, .drop = F), n = n_distinct(FISHER_LIC_FK))$n # number of fishers making trips to federal waters by year
licenses_state = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 1,], year2, .drop = F), n = n_distinct(FISHER_LIC_FK))$n # number of fishers making trips to state waters by year
plot_years = years >= 2000 & years <= 2024
plot(x = years[plot_years], y = trips_state[plot_years], ylim = c(0, max(trips_state)), type = 'l', col = "orange")
lines(x = years[plot_years], y = trips_federal[plot_years], col = "blue")
plot(x = years[plot_years], y = licenses_state[plot_years], ylim = c(0, max(licenses_state)), type = 'l', col = "orange")
lines(x = years[plot_years], y = licenses_federal[plot_years], col = "blue")
plot_kc(file_name = "Fig.7.a", plot_data = matrix(c(trips_federal, trips_state), nrow = 2, byrow = T), plot_years, ylab = "Number of Trips", plot_cols = water_colors, filled = F, legend = c("Federal", "State"))
plot_kc(file_name = "Fig.7.b", plot_data = matrix(c(licenses_federal, licenses_state), nrow = 2, byrow = T), plot_years, ylab = "Number of Licenses", plot_cols = water_colors, filled = F, legend = c("Federal", "State"))
trips_data = matrix(c(trips_federal, trips_state), nrow = 2, byrow = T)
licenses_data = matrix(c(licenses_federal, licenses_state), nrow = 2, byrow = T)


x_min = min(years[plot_years])
x_max = max(years[plot_years])
y_min = 0
y_max = 1.1
y_max_trips = max(trips_data[, plot_years], na.rm = T) * 1.1
y_max_licenses = max(licenses_data[, plot_years], na.rm = T) * 1.1
xs = pretty(c(x_min, x_max), n = 4)
xs = xs[xs >= x_min & xs <= x_max]
ys_trips = pretty(c(y_min, y_max_trips), n = 4)
ys_trips = ys_trips[ys_trips >= y_min & ys_trips <= y_max_trips]
ys_licenses = pretty(c(y_min, y_max_licenses), n = 4)
ys_licenses = ys_licenses[ys_licenses >= y_min & ys_licenses <= y_max_licenses]
#pdf(paste0(root_dir, "/03_Outputs/Fig.7.pdf"), width = 6.5, height = 5, pointsize = 12)
png(paste0(root_dir, "/03_Outputs/Fig.7.png"), width = 6.5, height = 5, units = "in", res = 300, pointsize = 12)

par(mar = c(4.5, 4.5, 1, 5))
plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = "")
axis(side = 1, at = xs)
axis(side = 2, at = ys_trips / y_max_trips, labels = ys_trips, las = 1)
axis(side = 4, at = ys_licenses / y_max_licenses, labels = ys_licenses, las = 1)
mtext(side = 2, "Number of Trips", line = 3)
mtext(side = 4, "Number of Licenses", line = 3)
lines(x = years[plot_years], y = trips_data[1, plot_years] / y_max_trips, col = water_colors[1], lwd = 2)
lines(x = years[plot_years], y = trips_data[2, plot_years] / y_max_trips, col = water_colors[2], lwd = 2)
lines(x = years[plot_years], y = licenses_data[1, plot_years] / y_max_licenses, col = water_colors[1], lty = "dashed", lwd = 2)
lines(x = years[plot_years], y = licenses_data[2, plot_years] / y_max_licenses, col = water_colors[2], lty = "dashed", lwd = 2)
box(which = "plot", lty = "solid")
legend(x = "topright", legend = c("Federal Trips", "Federal Licenses", "State Trips", "State Licenses"), col = c(rep(water_colors[1], 2), rep(water_colors[2], 2)), lty = c("solid", "dashed", "solid", "dashed"), lwd = 2, bty = "n")
dev.off()
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
# Fig.8: Catch rate vs year, separately for federal and state
federal = summarise(group_by(summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 3,], FISHER_LIC_FK, FISHED_DATE), n = sum(kc_catch), year2 = first(year2)), year2, .drop = F), n = mean(n))$n # average catch rate across trips to federal waters by year; weighted by trip, not fisher
state = summarise(group_by(summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 1,], FISHER_LIC_FK, FISHED_DATE), n = sum(kc_catch), year2 = first(year2)), year2, .drop = F), n = mean(n))$n # average catch rate across trips to state waters by year; weighted by trip, not fisher
plot_years = years >= 2000 & years <= 2024
plot(x = years[plot_years], y = federal[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], y = state[plot_years], col = "orange")
plot_kc(file_name = "Fig.8", plot_data = matrix(c(federal, state), nrow = 2, byrow = T), plot_years, ylab = "Catch Rate (lb / trip)", plot_cols = water_colors, filled = F, legend = c("Federal", "State"))
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
# Figure 9: # Trips (federal and state) and licenses (federal and state) vs year

# Calculating catch rate by hoopnet instead of by trip
FRS_catchrate_gear <- FRS_catchrate %>%
  filter(!(NUM_GEAR == 0)) %>% # Only reporting gear number after 2002
  group_by(FISHED_DATE, FISHER_LIC_FK) %>%
  arrange(desc(SPECIES_FK == 701)) %>%  # puts 701 first if it exists
  slice(1) %>%                           # keep only the top-priority row
  ungroup() %>%
  mutate(kc_catch_gear = kc_catch / NUM_GEAR)


gear_federal = summarise(group_by(FRS_catchrate_gear[FRS_catchrate_gear$zone == 3,], year2, .drop = F), n = sum(NUM_GEAR))$n # number of trips to federal waters by year
gear_state = summarise(group_by(FRS_catchrate_gear[FRS_catchrate_gear$zone == 1,], year2, .drop = F), n = sum(NUM_GEAR))$n # number of trips to state waters by year
licenses_federal = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 3,], year2, .drop = F), n = n_distinct(FISHER_LIC_FK))$n # number of fishers making trips to federal waters by year
licenses_state = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 1,], year2, .drop = F), n = n_distinct(FISHER_LIC_FK))$n # number of fishers making trips to state waters by year

plot_years = years >= 2000 & years <= 2024

# Plot individual lines (optional preview plots)
plot(x = years[plot_years], y = gear_state[plot_years], ylim = c(0, max(gear_state)), type = 'l', col = "orange")
lines(x = years[plot_years], y = gear_federal[plot_years], col = "blue")
plot(x = years[plot_years], y = licenses_state[plot_years], ylim = c(0, max(licenses_state)), type = 'l', col = "orange")
lines(x = years[plot_years], y = licenses_federal[plot_years], col = "blue")

# Create gear and license data matrices
gear_data = matrix(c(gear_federal, gear_state)/1000, nrow = 2, byrow = T)
licenses_data = matrix(c(licenses_federal, licenses_state), nrow = 2, byrow = T)

# Mask gear data before 2002
gear_data_masked <- gear_data
gear_data_masked[, years < 2002] <- NA

# Axes and scaling setup
x_min = min(years[plot_years])
x_max = max(years[plot_years])
y_min = 0
y_max = 1.1
y_max_gear = max(gear_data[, plot_years], na.rm = T) * 1.1
y_max_licenses = max(licenses_data[, plot_years], na.rm = T) * 1.1

xs = pretty(c(x_min, x_max), n = 4)
xs = xs[xs >= x_min & xs <= x_max]

ys_gear = pretty(c(y_min, y_max_gear), n = 4)
ys_gear_norm = ys_gear / y_max_gear

ys_licenses = pretty(c(y_min, y_max_licenses), n = 4)
ys_licenses = ys_licenses[ys_licenses >= y_min & ys_licenses <= y_max_licenses]

# Output to PNG
png(paste0(root_dir, "/03_Outputs/Fig.9.png"), width = 6.5, height = 5, units = "in", res = 300, pointsize = 12)

# Start plotting
par(mar = c(4.5, 4.5, 1, 5))
plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = "")

# Axes
axis(side = 1, at = xs)
axis(side = 2, at = ys_gear / y_max_gear, labels = ys_gear, las = 1)
axis(side = 4, at = ys_licenses / y_max_licenses, labels = ys_licenses, las = 1)
mtext(side = 2, "Number of Hoop Nets (x 1000)", line = 3)
mtext(side = 4, "Number of Licenses", line = 3)

# Plot masked gear data (starts from 2002)
lines(x = years[plot_years], y = gear_data_masked[1, plot_years] / y_max_gear, col = water_colors[1], lwd = 2)
lines(x = years[plot_years], y = gear_data_masked[2, plot_years] / y_max_gear, col = water_colors[2], lwd = 2)

# Plot full license data
lines(x = years[plot_years], y = licenses_data[1, plot_years] / y_max_licenses, col = water_colors[1], lty = "dashed", lwd = 2)
lines(x = years[plot_years], y = licenses_data[2, plot_years] / y_max_licenses, col = water_colors[2], lty = "dashed", lwd = 2)

# Finalize
box(which = "plot", lty = "solid")
legend(x = "topright",
       legend = c("Federal Hoopnets", "Federal Licenses", "State Hoopnets", "State Licenses"),
       col = c(rep(water_colors[1], 2), rep(water_colors[2], 2)),
       lty = c("solid", "dashed", "solid", "dashed"),
       lwd = 2, bty = "n")

dev.off()
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
# Fig.10: Catch rate per gear vs year, separately for federal and state using ratio of means
# 1. Federal waters (zone 3): total CPUE per year = sum(catch) / sum(gear)
federal_df <- FRS_catchrate_gear %>%
  filter(zone == 3) %>%
  mutate(year2 = as.numeric(as.character(year2))) %>%
  group_by(year2) %>%
  summarise(
    cpue = sum(kc_catch, na.rm = TRUE) / sum(NUM_GEAR, na.rm = TRUE),
    .groups = "drop"
  )

# 2. State waters (zone 1): total CPUE per year = sum(catch) / sum(gear)
state_df <- FRS_catchrate_gear %>%
  filter(zone == 1) %>%
  mutate(year2 = as.numeric(as.character(year2))) %>%
  group_by(year2) %>%
  summarise(
    cpue = sum(kc_catch, na.rm = TRUE) / sum(NUM_GEAR, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Plot using plot_kc_dplyr
plot_kc_dplyr(
  file_name = "Fig.10",  # total CPUE, not average of trip CPUE
  plot_data_list = list(federal_df, state_df),
  ylab = "Catch Rate (lb / hoopnet)",
  plot_cols = water_colors,  # e.g., c("blue", "orange")
  filled = FALSE,
  legend = c("Federal", "State"),
  year_range = c(2000, 2024),
  set_y_max = 2
)
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
# Fig.11: Catch vs year, separately for MHI FRS kept and sold
catch = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(catch))$n
sold_frs = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(LBS_SOLD))$n
sold_dealer = summarise(group_by(Dealer, year2, .drop = F), n = sum(LBS_SOLD))$n
sold = sold_frs
sold[years >= 2000 & years <= 2024] = sold_dealer
plot_years = years <= 2024
plot(x = years[plot_years], y = catch[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], y = sold[plot_years], col = "orange")
plot_kc(file_name = "Fig.11", plot_data = matrix(c(catch - sold, sold) / 1000, nrow = 2, byrow = T), plot_years, ylab = "Catch (x 1000 lb)", plot_cols = pair_colors, filled = T, legend = c("Non-Sold", "Sold"))
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
# Figure 12: Penguin bank catches by year group due to confidentiality
group_breaks <- c(1958, 1961, 1964, 1967, 1970, 1973, 1976, 1979, 1982, 1985, 1988, 1991, 1994,
                  1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
group_labels <- year_groups2

# 2. Add year group column
FRS_catchrate$year_group2 <- cut(FRS_catchrate$year, breaks = group_breaks, labels = group_labels)

FRS_zone2 <- FRS_catchrate[FRS_catchrate$zone == 2 & !is.na(FRS_catchrate$year_group2), ]
FRS_zone2$trip_id <- paste(FRS_zone2$FISHER_LIC_FK, FRS_zone2$FISHED_DATE)

# Ensure year_group2 is a factor with all desired levels
FRS_zone2 <- FRS_zone2 %>%
  mutate(year_group2 = factor(year_group2, levels = year_groups2))

# Aggregate and include missing groups with 0 catch
agg <- FRS_zone2 %>%
  group_by(year_group2) %>%
  summarize(
    total_catch = sum(kc_catch, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(year_group2 = year_groups2, fill = list(total_catch = 0))

group_labels <- as.character(agg$year_group2)
sum_catch_data <- matrix(agg$total_catch, nrow = 1, byrow = TRUE)



FRS_zone3 <- FRS_catchrate %>%
  filter(zone == 3, !is.na(year_group2)) %>%
  mutate(
    trip_id = paste(FISHER_LIC_FK, FISHED_DATE),
    year_group2 = factor(year_group2, levels = year_groups2)
  )

# 5. Aggregate for Zone 3
agg_zone3 <- FRS_zone3 %>%
  group_by(year_group2) %>%
  summarize(total_catch = sum(kc_catch, na.rm = TRUE), .groups = "drop") %>%
  complete(year_group2 = year_groups2, fill = list(total_catch = 0))

# 6. Combine into matrix (Zone 2 on row 1, Zone 3 on row 2)
sum_catch_data2 <- rbind(agg$total_catch, agg_zone3$total_catch)

# 7. Call the plotting function
plot_kc_grouped(
  file_name = "Fig.12",
  plot_data = sum_catch_data2/1000,
  year_groups = year_groups2,
  ylab = "Catch (x 1000 lb)",
  plot_cols = c("darkgoldenrod", "deepskyblue3"),
  filled = TRUE,
  legend = c("Penguin Bank", "Other Federal Waters")
)
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
#Figure 13 - using stacked bar plot and excluding data we cannot show due to confidentiality when licenses N < 3
kauai_niihau = summarise(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(520, 521, 525, 526),], year2, .drop = F), n = sum(catch))$n
federal = summarise(group_by(FRS_catch[FRS_catch$zone == 3,], year2, .drop = F), n = sum(catch))$n
plot_years = years >= 2014 & years <= 2024
zero_years <- c(2014, 2016, 2019, 2020, 2022, 2024)
kauai_niihau = summarise(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(520, 521, 525, 526),], year2, .drop = F), n = sum(catch))$n
kauai_niihau2 <- kauai_niihau

# Loop through zero_years and set kauai_niihau to zero for those years
for (yr in zero_years) {
  idx <- which(years == yr)
  if(length(idx) == 1) {
    kauai_niihau2[idx] <- 0
  }
}

bar_data <- rbind(kauai_niihau2[plot_years], federal[plot_years]-kauai_niihau2[plot_years])

# Define years to mark with asterisk
asterisk_years <- c(2014, 2016, 2019, 2020, 2022, 2024)

# Calculate max Y value and add 400
y_max <- max(colSums(bar_data), na.rm = TRUE) + 300

png(paste0(root_dir, "/03_Outputs/Fig.13.png"), width = 6.5, height = 5, units = "in", res = 300, pointsize = 12)
par(mar = c(4.5, 4.5, 1, 1))

# Create the barplot with custom ylim
bar_positions <- barplot(bar_data,
                         names.arg = rep("", ncol(bar_data)),  # Must match number of bars
                         col = c("deepskyblue4", "deepskyblue3"),
                         xlab = "Year",
                         ylab = "Catch (lb)",
                         ylim = c(0, y_max),
                         axes = FALSE,
                         legend.text = c("Kauai/Niihau", "All Federal Waters"),
                         args.legend = list(x = "topright", bty = "n"))

# Add custom y-axis
axis(side = 2, at = c(500, 1000), las = 1)

# Add custom x-axis: ticks only where labels exist
label_indices <- seq(1, ncol(bar_data), by = 2)  # Change to desired spacing
axis(side = 1,
     at = bar_positions[label_indices],
     labels = years[plot_years][label_indices],
     tick = TRUE)

# Add asterisks to specific years
target_indices <- which(years[plot_years] %in% asterisk_years)
text(x = bar_positions[target_indices],
     y = colSums(bar_data)[target_indices] + 50,  # Add a little vertical offset
     labels = "*",
     cex = 1.2)

# Box around plot
box(which = "plot", lty = "solid")

# Close PNG device
dev.off()
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
# Figure A1: % of sold catch vs year for FRS
catch = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(catch))$n
sold_frs = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(LBS_SOLD))$n
sold_dealer = summarise(group_by(Dealer, year2, .drop = F), n = sum(LBS_SOLD))$n
sold = sold_frs
sold[years >= 2000 & years <= 2024] = sold_dealer
plot_years = years <= 2024
plot(x = years[plot_years], y = sold[plot_years] / catch[plot_years], type = 'l', col = "black")
plot_kc(file_name = "Fig.A1", plot_data = matrix(sold / catch, nrow = 1, byrow = T), plot_years, ylab = "Percent of Catch Sold", plot_cols = "gray40", filled = F, legend = NA, set_y_max = 1)
dev.off()
#-------------------------------------------------------------------------#


#-------------------------------------------------------------------------#
#Figure A5: Large scale seasonal variability of catch
# Ensure FISHED_DATE is Date type
FRS$FISHED_DATE <- as.Date(FRS$FISHED_DATE)

# Extract year and month
FRS <- FRS %>%
  mutate(
    Year = year(FISHED_DATE),
    Month = month(FISHED_DATE, label = TRUE, abbr = TRUE),  # abbreviated month labels
    Month_num = month(FISHED_DATE)
  )

# Calculate proportional catch by Month and Year
monthly_catch <- FRS %>%
  group_by(Year, Month, Month_num) %>%
  summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop",
            licences = n_distinct(FISHER_LIC_FK)) %>%
  group_by(Year) %>%
  mutate(proportion = catch / sum(catch)) %>%
  ungroup()


# Create a consistent unordered factor for Month
# Create a common month level set
month_levels <- month.abb  # "Jan", "Feb", ..., "Dec"

# Create full grid with correct Month factor (unordered)
full_grid <- expand_grid(
  Year = 1948:2024,
  Month_num = 1:12,
) %>%
  mutate(
    Month = factor(month.abb[Month_num], levels = month_levels, ordered = FALSE),
    Year_group_start = floor((Year - min(Year)) / 16) * 16 + min(Year),
    Year_group_end = Year_group_start + 15,
    Year_group = paste0(Year_group_start, "–", Year_group_end)
  ) %>%
  mutate(Year_group = case_when(Year_group == "2012–2027" ~ "2012–2024",
                                TRUE ~ Year_group))

# Ensure monthly_catch uses the same factor type
monthly_catch <- monthly_catch %>%
  mutate(
    Month = as.character(Month),  # strip ordering
    Month = factor(Month, levels = month_levels, ordered = FALSE)
  )

# Now safe to join
monthly_catch_full <- full_grid %>%
  left_join(monthly_catch, by = c("Year", "Month", "Month_num")) %>%
  replace_na(list(proportion = 0))


monthly_catch_full <- monthly_catch_full %>%
  mutate(
    Year = factor(Year, levels = 1948:2024),
    Month = factor(Month, levels = rev(month.abb))  # Reverse month order for y-axis
  )


# Several years put together due to confidentiality
# Aggregate by Year_group and Month
monthly_grouped <- monthly_catch_full %>%
  group_by(Year_group, Month) %>%
  summarise(proportion = mean(proportion, na.rm = TRUE), .groups = "drop")

# Plot using Year_group
ggplot(monthly_grouped, aes(x = Year_group, y = fct_rev(Month), fill = proportion)) +
  geom_tile(color = "grey80") +
  scale_fill_viridis_c(
    name = "Proportional Catch",
    trans = "sqrt",
    limits = c(0, 1)
  ) +
  facet_wrap(~ Year_group, scales = "free_x", nrow = 1) +
  theme_bw() +
  labs(
    title = "Average Proportional Catch by Year Group - All Waters",
    x = "Year Group",
    y = "Month"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )

ggsave(paste0(root_dir, "/03_Outputs/Fig.A5.png"), width = 6, height = 4, units = "in", dpi = 300)
#-------------------------------------------------------------------------#

































































































































library(dplyr)
setwd("C:/Users/Hongguang.Ma/Documents/Kona crab/FRS")
#setwd("/Users/Toby/Documents/Work/20250129 Kona Crab/")
FRS = read.csv("WH2024.csv") # based on warehouse data in picfinfish MYSQL database using query species_fk = 701 or gear_fk = 40 (13839 records)
MHI = read.csv("MHI_Yau.csv")
# area code 16123 didn't include 16123I/F/C
# except for 99999, largest area code is 875 in FRS
State = read.csv("State.csv")
Dealer = read.csv("Dealer_WH2024.csv")
Dealer$year2 = as.factor(substr(Dealer$REPORT_DATE, 1, 4))

FRS$water = ifelse(FRS$AREA_FK %in% State$code, 1, 2) # 1 = state waters, 2 = federal waters
FRS$zone = ifelse(FRS$AREA_FK %in% State$code, 1, ifelse(FRS$AREA_FK == 331, 2, 3)) # 1 = state waters, 2 = Penguin bank, 3 = federal waters
FRS$year = FRS$REPORT_YEAR
FRS$catch = FRS$LBS_KEPT
FRS$year2 = as.factor(FRS$year) # having year as a factor allows us to use .drop in group_by to avoid losing years with no data
years = sort(unique(FRS$year), decreasing = F)

FRS_catch = FRS[FRS$AREA_FK %in% MHI$code & FRS$REPORT_YEAR < 2025 & FRS$SPECIES_FK == 701,] # used to compute catch -- MHI and kona crab only and remove incomplete data from 2024

FRS_catchrate = FRS[FRS$AREA_FK%in%MHI$code & FRS$REPORT_YEAR < 2025 & FRS$GEAR_FK == 40,] # used to compute effort and catch rate -- MHI and dominant gear only and remove incomplete data from 2024
FRS_catchrate$kc_catch = ifelse(FRS_catchrate$SPECIES_FK == 701, FRS_catchrate$LBS_KEPT, 0)

pair_colors = c("gray40", "gray70")
state_colors = c("indianred3", "indianred4")
penguin_bank_colors = c("goldenrod1", "darkgoldenrod")
federal_colors = c("deepskyblue3", "deepskyblue4")

water_colors = c(federal_colors[1], state_colors[1])
zone_colors = c(federal_colors[1], penguin_bank_colors[1], state_colors[1])

plot_kc = function(file_name, plot_data, plot_years, ylab, plot_cols, filled, legend, legend_location = "topright", set_y_max = NA) {
  x_min = min(years[plot_years])
  x_max = max(years[plot_years])
  y_min = 0
  y_max = NA
  if(is.na(set_y_max)) {
    if(filled) {
      y_max = max(apply(plot_data[, plot_years], c(2), sum), na.rm = T) * 1.1
    } else {
      y_max = max(plot_data[, plot_years], na.rm = T) * 1.1
    }
  } else {
    y_max = set_y_max
  }
  
  xs = pretty(c(x_min, x_max), n = 4)
  xs = xs[xs >= x_min & xs <= x_max]
  ys = pretty(c(y_min, y_max), n = 4)
  ys = ys[ys >= y_min & ys <= y_max]
  
  pdf(paste0(file_name, ".pdf"), width = 6.5, height = 5, pointsize = 12)
  
  par(mar = c(4.5, 4.5, 1, 1))
  
  plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = ylab)
  
  axis(side = 1, at = xs)
  axis(side = 2, at = ys, las = 1)
  
  n_row = dim(plot_data)[1]
  for(i in 1:n_row) {
    if(filled) {
      polygon(x = c(min(years[plot_years]), years[plot_years], max(years[plot_years])), y = c(0, apply(plot_data[i:n_row, plot_years, drop = F], c(2), sum), 0), col = plot_cols[i], border = NA)
    } else {
      lines(x = years[plot_years], y = plot_data[i, plot_years], col = plot_cols[i], lwd = 3)
      
      n_col = dim(plot_data)[2]
      for(j in 1:n_col) {
        point_before = ifelse(j == 1, NA, plot_data[i, j - 1])
        point_after = ifelse(j == n_col, NA, plot_data[i, j + 1])
        
        if(is.na(point_before) & is.na(point_after)) {
          points(x = years[j], y = plot_data[i, j], col = plot_cols[i])
        }
      }
    }
  }
  
  box(which = "plot", lty = "solid")
  
  if(any(!is.na(legend))) {
    if(filled) {
      legend(x = legend_location, legend = legend, col = plot_cols, pch = 15, bty = "n")
    } else {
      legend(x = legend_location, legend = legend, col = plot_cols, lwd = 3, bty = "n")
    }
  }
  
  dev.off()
}

# Figure A: Catch vs Year, separately for MHI and entire archipelago
mhi = summarise(group_by(FRS[FRS$AREA_FK %in% MHI$code & FRS$SPECIES_FK == 701,], REPORT_YEAR), n = sum(LBS_KEPT))$n
archipelago = summarise(group_by(FRS[FRS$SPECIES_FK == 701,], REPORT_YEAR), n = sum(LBS_KEPT))$n

plot_years = years <= 2024
plot(x = years[plot_years], y = mhi[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], archipelago[plot_years], col = "orange")

plot_kc(file_name = "A", plot_data = matrix(c(archipelago - mhi, mhi) / 1000, nrow = 2, byrow = T), plot_years, ylab = "Catch (x 1000 lb)", plot_cols = pair_colors, filled = T, legend = c("NW Hawaiian Islands/Unknown", "Main Hawaiian Islands"))

# Figure B: Catch vs year, stacked for state, PB, and federal
federal = summarise(group_by(FRS_catch[FRS_catch$zone == 3,], year2, .drop = F), n = sum(catch))$n
pb = summarise(group_by(FRS_catch[FRS_catch$zone == 2,], year2, .drop = F), n = sum(catch))$n
state = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n

plot_years = years <= 2024
plot(x = years[plot_years], y = federal[plot_years] + pb[plot_years] + state[plot_years], type = 'l', col = "darkblue")
lines(x = years[plot_years], y = state[plot_years] + pb[plot_years], col = "orange")
lines(x = years[plot_years], y = state[plot_years], col = "lightblue")

plot_kc(file_name = "B", plot_data = matrix(c(federal, pb, state) / 1000, nrow = 3, byrow = T), plot_years, ylab = "Catch (x 1000 lb)", plot_cols = zone_colors, filled = T, legend = c("Federal", "Penguin Bank", "State"))

# Figure C: Catch vs year, separately for MHI FRS kept and sold
catch = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(catch))$n
sold_frs = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(LBS_SOLD))$n
sold_dealer = summarise(group_by(Dealer, year2, .drop = F), n = sum(LBS_SOLD))$n
sold = sold_frs
sold[years >= 2000 & years <= 2024] = sold_dealer

plot_years = years <= 2024
plot(x = years[plot_years], y = catch[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], y = sold[plot_years], col = "orange")

plot_kc(file_name = "C", plot_data = matrix(c(catch - sold, sold) / 1000, nrow = 2, byrow = T), plot_years, ylab = "Catch (x 1000 lb)", plot_cols = pair_colors, filled = T, legend = c("Non-Sold", "Sold"))

# Figure D: % of sold catch vs year for FRS
catch = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(catch))$n
sold_frs = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(LBS_SOLD))$n
sold_dealer = summarise(group_by(Dealer, year2, .drop = F), n = sum(LBS_SOLD))$n
sold = sold_frs
sold[years >= 2000 & years <= 2024] = sold_dealer

plot_years = years <= 2024
plot(x = years[plot_years], y = sold[plot_years] / catch[plot_years], type = 'l', col = "black")

plot_kc(file_name = "D", plot_data = matrix(sold / catch, nrow = 1, byrow = T), plot_years, ylab = "Percent of Catch Sold", plot_cols = "gray40", filled = F, legend = NA, set_y_max = 1)

# Figure E: # Trips (dominant fisher and all PB) and # licenses vs year
trips_by_fisher = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 2,], FISHER_LIC_FK), n = n_distinct(FISHED_DATE)) # number of trips to Penguin Bank by each fisher
dominant_fisher = trips_by_fisher[order(trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Penguin Bank

trips_all = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 2,], year2, .drop = F), n = n_distinct(paste(FISHER_LIC_FK, FISHED_DATE)))$n # number of trips to Penguin Bank by year
trips_dominant_fisher = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 2 & FRS_catchrate$FISHER_LIC_FK == dominant_fisher,], year2, .drop = F), n = n_distinct(FISHED_DATE))$n # number of dominant fisher trips to Penguin Bank by year
licenses = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 2,], year2, .drop = F), n = n_distinct(FISHER_LIC_FK))$n # number of fishers making trips to Penguin Bank by year

plot_years = years >= 2000 & years <= 2013
plot(x = years[plot_years], y = trips_all[plot_years], type = 'l', col = "orange")
lines(x = years[plot_years], y = trips_dominant_fisher[plot_years], col = "blue")

plot(x = years[plot_years], y = licenses[plot_years], type = 'l', lty = "dashed")

plot_kc(file_name = "E1", plot_data = matrix(c(trips_all - trips_dominant_fisher, trips_dominant_fisher), nrow = 2, byrow = T), plot_years, ylab = "Number of Trips", plot_cols = penguin_bank_colors, filled = T, legend = c("Other Fishers", "Highliner"))

plot_kc(file_name = "E2", plot_data = matrix(licenses, nrow = 1, byrow = T), plot_years, ylab = "Number of Licenses", plot_cols = "black", filled = F, legend = NA)

trips_data = matrix(c(trips_all - trips_dominant_fisher, trips_dominant_fisher), nrow = 2, byrow = T)
licenses_data = matrix(licenses, nrow = 1, byrow = T)

x_min = min(years[plot_years])
x_max = max(years[plot_years])
y_min = 0
y_max = 1.12

y_max_trips = max(trips_data[, plot_years], na.rm = T) * 1.1
y_max_licenses = max(licenses_data[, plot_years], na.rm = T) * 1.1

xs = pretty(c(x_min, x_max), n = 4)
xs = xs[xs >= x_min & xs <= x_max]
ys_trips = pretty(c(y_min, y_max_trips), n = 4)
ys_trips = ys_trips[ys_trips >= y_min & ys_trips <= y_max_trips]
ys_licenses = pretty(c(y_min, y_max_licenses), n = 4)
ys_licenses = ys_licenses[ys_licenses >= y_min & ys_licenses <= y_max_licenses]

pdf("E.pdf", width = 6.5, height = 5, pointsize = 12)

par(mar = c(4.5, 4.5, 1, 5))

plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = "")

axis(side = 1, at = xs)
axis(side = 2, at = ys_trips / y_max_trips, labels = ys_trips, las = 1)
axis(side = 4, at = ys_licenses / y_max_licenses, labels = ys_licenses, las = 1)

mtext(side = 2, "Number of Trips", line = 3)
mtext(side = 4, "Number of Licenses", line = 3)

n_row = dim(trips_data)[1]
for(i in 1:n_row) {
  polygon(x = c(min(years[plot_years]), years[plot_years], max(years[plot_years])), y = c(0, apply(trips_data[i:n_row, plot_years, drop = F], c(2), sum), 0) / y_max_trips, col = penguin_bank_colors[i], border = NA)
}
lines(x = years[plot_years], y = licenses_data[1, plot_years] / y_max_licenses, col = "black", lty = "dashed", lwd = 2)

box(which = "plot", lty = "solid")

legend(x = "topleft", legend = c("Other Penguin Bank Trips", "Penguin Bank Highliner Trips", "Licenses"), col = c(penguin_bank_colors, "black"), lty = c(NA, NA, "dashed"), lwd = c(NA, NA, 2), pch = c(15, 15, NA), bty = "n")

dev.off()

# Figure F: Catch rate vs year, separately for dominant fisher and all fishers
trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$zone == 2,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Penguin Bank by each fisher
dominant_fisher = trips_by_fisher[order(trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Penguin Bank

catch_rate_dominant = summarise(group_by(summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 2 & FRS_catchrate$FISHER_LIC_FK == dominant_fisher,], FISHED_DATE), n = sum(kc_catch), year2 = first(year2)), year2, .drop = F), n = mean(n))$n # average catch rate for the dominant fisher across trips by year
catch_rate_all = summarise(group_by(summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 2,], FISHER_LIC_FK, FISHED_DATE), n = sum(kc_catch), year2 = first(year2)), year2, .drop = F), n = mean(n))$n # average catch rate across trips by year; weighted by trip, not fisher
catch_rate_non_dominant = summarise(group_by(summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 2 & FRS_catchrate$FISHER_LIC_FK != dominant_fisher,], FISHER_LIC_FK, FISHED_DATE), n = sum(kc_catch), year2 = first(year2)), year2, .drop = F), n = mean(n))$n # average catch rate across trips by year; weighted by trip, not fisher

plot_years = years >= 2000 & years <= 2013
plot(x = years[plot_years], y = catch_rate_dominant[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], y = catch_rate_all[plot_years], col = "orange")

plot_kc(file_name = "F", plot_data = matrix(c(catch_rate_non_dominant, catch_rate_dominant), nrow = 2, byrow = T), plot_years, ylab = "Catch Rate (lb / trip)", plot_cols = penguin_bank_colors, filled = F, legend = c("Other Penguin Bank Fishers", "Penguin Bank Highliner"))

# Figure G: Catch vs year, stacked for state, PB, and federal (same as Figure B, but only since 2000)
federal = summarise(group_by(FRS_catch[FRS_catch$zone == 3,], year2, .drop = F), n = sum(catch))$n
pb = summarise(group_by(FRS_catch[FRS_catch$zone == 2,], year2, .drop = F), n = sum(catch))$n
state = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n

plot_years = years >= 2000 & years <= 2024
plot(x = years[plot_years], y = federal[plot_years] + pb[plot_years] + state[plot_years], type = 'l', col = "darkblue")
lines(x = years[plot_years], y = state[plot_years] + pb[plot_years], col = "orange")
lines(x = years[plot_years], y = state[plot_years], col = "lightblue")

plot_kc(file_name = "G", plot_data = matrix(c(federal, pb, state) / 1000, nrow = 3, byrow = T), plot_years, ylab = "Catch (x 1000 lb)", plot_cols = zone_colors, filled = T, legend = c("Federal", "Penguin Bank", "State"))

# Figure H: Catch vs year, from PB separately for dominant fisher and all fishers
trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$zone == 2,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Penguin Bank by each fisher
dominant_fisher = trips_by_fisher[order(trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Penguin Bank

pb_all = summarise(group_by(FRS_catch[FRS_catch$zone == 2,], year2, .drop = F), n = sum(catch))$n
pb_dominant_fisher = summarise(group_by(FRS_catch[FRS_catch$zone == 2 & FRS_catch$FISHER_LIC_FK == dominant_fisher,], year2, .drop = F), n = sum(catch))$n

plot_years = years >= 2000 & years <= 2024
plot(x = years[plot_years], y = pb_all[plot_years], type = 'l', col = "yellow")
lines(x = years[plot_years], y = pb_dominant_fisher[plot_years], type = 'l', col = "orange")

plot_kc(file_name = "H", plot_data = matrix(c(pb_all - pb_dominant_fisher, pb_dominant_fisher), nrow = 2, byrow = T), plot_years, ylab = "Catch (lb)", plot_cols = penguin_bank_colors, filled = T, legend = c("Other Penguin Bank Fishers", "Penguin Bank Highliner"))

# Figure I: Catch vs year, stacked for state, PB, and federal (same as Figures B/G, but only since 2014)
federal = summarise(group_by(FRS_catch[FRS_catch$zone == 3,], year2, .drop = F), n = sum(catch))$n
pb = summarise(group_by(FRS_catch[FRS_catch$zone == 2,], year2, .drop = F), n = sum(catch))$n
state = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n

plot_years = years >= 2014 & years <= 2024
plot(x = years[plot_years], y = federal[plot_years] + pb[plot_years] + state[plot_years], type = 'l', col = "darkblue")
lines(x = years[plot_years], y = state[plot_years] + pb[plot_years], col = "orange")
lines(x = years[plot_years], y = state[plot_years], col = "lightblue")

plot_kc(file_name = "I", plot_data = matrix(c(federal, pb, state), nrow = 3, byrow = T), plot_years, ylab = "Catch (lb)", plot_cols = zone_colors, filled = T, legend = c("Federal", "Penguin Bank", "State"))

# Figure J: Catch rate vs year, separately for federal and state
federal = summarise(group_by(summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 3,], FISHER_LIC_FK, FISHED_DATE), n = sum(kc_catch), year2 = first(year2)), year2, .drop = F), n = mean(n))$n # average catch rate across trips to federal waters by year; weighted by trip, not fisher
state = summarise(group_by(summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 1,], FISHER_LIC_FK, FISHED_DATE), n = sum(kc_catch), year2 = first(year2)), year2, .drop = F), n = mean(n))$n # average catch rate across trips to state waters by year; weighted by trip, not fisher

plot_years = years >= 2000 & years <= 2024
plot(x = years[plot_years], y = federal[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], y = state[plot_years], col = "orange")

plot_kc(file_name = "J", plot_data = matrix(c(federal, state), nrow = 2, byrow = T), plot_years, ylab = "Catch Rate (lb / trip)", plot_cols = water_colors, filled = F, legend = c("Federal", "State"))

# Figure K: # Trips (federal and state) and licenses (federal and state) vs year
trips_federal = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 3,], year2, .drop = F), n = n_distinct(paste(FISHER_LIC_FK, FISHED_DATE)))$n # number of trips to federal waters by year
trips_state = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 1,], year2, .drop = F), n = n_distinct(paste(FISHER_LIC_FK, FISHED_DATE)))$n # number of trips to state waters by year
licenses_federal = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 3,], year2, .drop = F), n = n_distinct(FISHER_LIC_FK))$n # number of fishers making trips to federal waters by year
licenses_state = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 1,], year2, .drop = F), n = n_distinct(FISHER_LIC_FK))$n # number of fishers making trips to state waters by year

plot_years = years >= 2000 & years <= 2024
plot(x = years[plot_years], y = trips_state[plot_years], ylim = c(0, max(trips_state)), type = 'l', col = "orange")
lines(x = years[plot_years], y = trips_federal[plot_years], col = "blue")

plot(x = years[plot_years], y = licenses_state[plot_years], ylim = c(0, max(licenses_state)), type = 'l', col = "orange")
lines(x = years[plot_years], y = licenses_federal[plot_years], col = "blue")

plot_kc(file_name = "K1", plot_data = matrix(c(trips_federal, trips_state), nrow = 2, byrow = T), plot_years, ylab = "Number of Trips", plot_cols = water_colors, filled = F, legend = c("Federal", "State"))

plot_kc(file_name = "K2", plot_data = matrix(c(licenses_federal, licenses_state), nrow = 2, byrow = T), plot_years, ylab = "Number of Licenses", plot_cols = water_colors, filled = F, legend = c("Federal", "State"))

trips_data = matrix(c(trips_federal, trips_state), nrow = 2, byrow = T)
licenses_data = matrix(c(licenses_federal, licenses_state), nrow = 2, byrow = T)

x_min = min(years[plot_years])
x_max = max(years[plot_years])
y_min = 0
y_max = 1.1

y_max_trips = max(trips_data[, plot_years], na.rm = T) * 1.1
y_max_licenses = max(licenses_data[, plot_years], na.rm = T) * 1.1

xs = pretty(c(x_min, x_max), n = 4)
xs = xs[xs >= x_min & xs <= x_max]
ys_trips = pretty(c(y_min, y_max_trips), n = 4)
ys_trips = ys_trips[ys_trips >= y_min & ys_trips <= y_max_trips]
ys_licenses = pretty(c(y_min, y_max_licenses), n = 4)
ys_licenses = ys_licenses[ys_licenses >= y_min & ys_licenses <= y_max_licenses]

pdf("K.pdf", width = 6.5, height = 5, pointsize = 12)

par(mar = c(4.5, 4.5, 1, 5))

plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = "")

axis(side = 1, at = xs)
axis(side = 2, at = ys_trips / y_max_trips, labels = ys_trips, las = 1)
axis(side = 4, at = ys_licenses / y_max_licenses, labels = ys_licenses, las = 1)

mtext(side = 2, "Number of Trips", line = 3)
mtext(side = 4, "Number of Licenses", line = 3)

lines(x = years[plot_years], y = trips_data[1, plot_years] / y_max_trips, col = water_colors[1], lwd = 2)
lines(x = years[plot_years], y = trips_data[2, plot_years] / y_max_trips, col = water_colors[2], lwd = 2)
lines(x = years[plot_years], y = licenses_data[1, plot_years] / y_max_licenses, col = water_colors[1], lty = "dashed", lwd = 2)
lines(x = years[plot_years], y = licenses_data[2, plot_years] / y_max_licenses, col = water_colors[2], lty = "dashed", lwd = 2)

box(which = "plot", lty = "solid")

legend(x = "topright", legend = c("Federal Trips", "Federal Licenses", "State Trips", "State Licenses"), col = c(rep(water_colors[1], 2), rep(water_colors[2], 2)), lty = c("solid", "dashed", "solid", "dashed"), lwd = 2, bty = "n")

dev.off()

# Figure L: Catch vs year, stacked for Haleiwa/Kahuku and state
haleiwa_kahuku = summarise(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(404, 405) & FRS_catch$year >= 2000,], year2, .drop = F), n = sum(catch))$n
state = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n

plot_years = years >= 2000 & years <= 2024
plot(x = years[plot_years], y = state[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], y = haleiwa_kahuku[plot_years], col = "orange")

plot_kc(file_name = "L", plot_data = matrix(c(state - haleiwa_kahuku, haleiwa_kahuku), nrow = 2, byrow = T), plot_years, ylab = "Catch (lb)", plot_cols = state_colors, filled = T, legend = c("Other State Waters", "Haleiwa/Kahuku"))

# Figure M: Catch vs year, stacked for dominant Haleiwa/Kahuku fisher and state
trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(404, 405) & FRS_catch$year >= 2000,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Haleiwa/Kahuku by each fisher
dominant_fisher = trips_by_fisher[order(trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Haleiwa/Kahuku

dominant_fisher = summarise(group_by(FRS_catch[FRS_catch$zone == 1 & FRS_catch$FISHER_LIC_FK == dominant_fisher,], year2, .drop = F), n = sum(catch))$n
state = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n

plot_years = years >= 2000 & years <= 2024
plot(x = years[plot_years], y = state[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], y = dominant_fisher[plot_years], col = "orange")

plot_kc(file_name = "M", plot_data = matrix(c(state - dominant_fisher, dominant_fisher), nrow = 2, byrow = T), plot_years, ylab = "Catch (lb)", plot_cols = state_colors, filled = T, legend = c("Other State Waters Fishers", "Haleiwa/Kahuku Highliner"))

# Figure N: Catch vs year, stacked for dominant Kawaihae/Mahukona and state
kawaihae_mahukona = summarise(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(102, 103),], year2, .drop = F), n = sum(catch))$n
state = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n

plot_years = years >= 2000 & years <= 2024
plot(x = years[plot_years], y = state[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], y = kawaihae_mahukona[plot_years], col = "orange")

plot_kc(file_name = "N", plot_data = matrix(c(state - kawaihae_mahukona, kawaihae_mahukona), nrow = 2, byrow = T), plot_years, ylab = "Catch (lb)", plot_cols = state_colors, filled = T, legend = c("Other State Waters", "Kawaihae/Mahukona"))

# Figure O: Catch vs year, stacked for dominant fishers (Haleiwa/Kahuku, Kawaihae/Mahukona) (3115, 1888) and state
haleiwa_kahuku_trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(404, 405) & FRS_catch$year >= 2000,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Haleiwa/Kahuku by each fisher
haleiwa_kahuku_dominant_fisher = haleiwa_kahuku_trips_by_fisher[order(haleiwa_kahuku_trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Haleiwa/Kahuku
kawaihae_mahukona_trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(102, 103) & FRS_catch$year >= 2000,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Kawaihae/Mahukona by each fisher
kawaihae_mahukona_dominant_fisher = kawaihae_mahukona_trips_by_fisher[order(kawaihae_mahukona_trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Kawaihae/Mahukona

dominant_fishers = summarise(group_by(FRS_catch[FRS_catch$zone == 1 & FRS_catch$FISHER_LIC_FK %in% c(haleiwa_kahuku_dominant_fisher, kawaihae_mahukona_dominant_fisher),], year2, .drop = F), n = sum(catch))$n
state = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n

plot_years = years >= 2000 & years <= 2024
plot(x = years[plot_years], y = state[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], y = dominant_fishers[plot_years], col = "orange")

plot_kc(file_name = "O", plot_data = matrix(c(state - dominant_fishers, dominant_fishers), nrow = 2, byrow = T), plot_years, ylab = "Catch (lb)", plot_cols = state_colors, filled = T, legend = c("Other State Waters Fishers", "2 State Waters Highliners"))

# Figure P: Catch vs year, stacked for Kauai/Niihau and federal
kauai_niihau = summarise(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(520, 521, 525, 526),], year2, .drop = F), n = sum(catch))$n
federal = summarise(group_by(FRS_catch[FRS_catch$zone == 3,], year2, .drop = F), n = sum(catch))$n

plot_years = years >= 2014 & years <= 2024
plot(x = years[plot_years], y = federal[plot_years], type = 'l', col = "blue")
lines(x = years[plot_years], y = kauai_niihau[plot_years], col = "orange")

plot_kc(file_name = "P", plot_data = matrix(c(federal - kauai_niihau, kauai_niihau), nrow = 2, byrow = T), plot_years, ylab = "Catch (lb)", plot_cols = federal_colors, filled = T, legend = c("Other Federal Waters", "Kauai/Niihau"))

# Memo Plot 2: Catch over time, split between state/PB/federal and comparing to ACL (B/G/I) and previous assessment (last year of data = 2016)

federal = summarise(group_by(FRS_catch[FRS_catch$zone == 3,], year2, .drop = F), n = sum(catch))$n
state = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n
pb = summarise(group_by(FRS_catch[FRS_catch$zone == 2,], year2, .drop = F), n = sum(catch))$n

plot_years = years <= 2024
plot_data = matrix(c(federal, state, pb) / 1000, nrow = 3, byrow = T)
plot_cols = zone_colors[c(1, 3, 2)]

x_min = min(years[plot_years])
x_max = max(years[plot_years])
y_min = 0
y_max = NA
y_max = 80

xs = pretty(c(x_min, x_max), n = 4)
xs = xs[xs >= x_min & xs <= x_max]
ys = c(0, 20, 40, 60, 80)

pdf("memo plot 2.pdf", width = 6.5, height = 5, pointsize = 12)

par(mar = c(4.5, 4.5, 1, 1))

plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = "Reported Commercial Catch (x 1000 lb)")

axis(side = 1, at = xs)
axis(side = 2, at = ys, las = 1)

rect(2016.5, y_min, x_max, y_max, col = "gray80", border = NA)
text("New \nYears", x = x_max, y = y_max, adj = c(1.06, 1.15))

n_row = dim(plot_data)[1]
for(i in 1:n_row) {
  polygon(x = c(min(years[plot_years]), years[plot_years], max(years[plot_years])), y = c(0, apply(plot_data[i:n_row, plot_years, drop = F], c(2), sum), 0), col = plot_cols[i], border = NA)
}

lines(x = c(x_min, x_max), y = rep(30.802, 2), lty = "dashed", lwd = 2)
#text("Current Annual Catch Limit", x = 2016.5, y = 32.5, adj = c(1.01, -0.3))
text("Current Annual\nCatch Limit   ", x = 2016, y = 30.4, adj = c(1.01, -0.3))

box(which = "plot", lty = "solid")

legend(x = "topleft", legend = c("Other Federal Waters", "State Waters", "Penguin Bank"), col = plot_cols, pch = 15, bty = "n")

dev.off()

# Memo Plot 3: Proportion of catch from state/PB/federal over time + contribution of highliners

split_state_highliners = F # whether to plot the two state highliners separately or together

state_catch = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n
pb_catch = summarise(group_by(FRS_catch[FRS_catch$zone == 2,], year2, .drop = F), n = sum(catch))$n
federal_catch = summarise(group_by(FRS_catch[FRS_catch$zone == 3,], year2, .drop = F), n = sum(catch))$n
total_catch = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(catch))$n

pb_trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$zone == 2,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Penguin Bank by each fisher
#pb_dominant_fisher = pb_trips_by_fisher[order(pb_trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Penguin Bank
pb_dominant_fisher = FRS_catchrate[FRS_catchrate$FISHER_LNAME=="NAKASHIMA"& FRS_catchrate$FISHER_FNAME=="HIROSHI",]$FISHER_LIC_FK
haleiwa_kahuku_trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(404, 405) & FRS_catch$year >= 2000,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Haleiwa/Kahuku by each fisher
haleiwa_kahuku_dominant_fisher = haleiwa_kahuku_trips_by_fisher[order(haleiwa_kahuku_trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Haleiwa/Kahuku
kawaihae_mahukona_trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(102, 103) & FRS_catch$year >= 2000,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Kawaihae/Mahukona by each fisher
kawaihae_mahukona_dominant_fisher = kawaihae_mahukona_trips_by_fisher[order(kawaihae_mahukona_trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Kawaihae/Mahukona

pb_dominant_fisher = summarise(group_by(FRS_catch[FRS_catch$zone == 2 & FRS_catch$FISHER_LIC_FK %in% pb_dominant_fisher,], year2, .drop = F), n = sum(catch))$n
haleiwa_kahuku_dominant_fisher_catch = summarise(group_by(FRS_catch[FRS_catch$zone == 1 & FRS_catch$FISHER_LIC_FK == haleiwa_kahuku_dominant_fisher,], year2, .drop = F), n = sum(catch))$n
kawaihae_mahukona_dominant_fisher_catch = summarise(group_by(FRS_catch[FRS_catch$zone == 1 & FRS_catch$FISHER_LIC_FK == kawaihae_mahukona_dominant_fisher,], year2, .drop = F), n = sum(catch))$n

plot_years = years >= 1986 & years <= 2024
plot_data = NA
if(split_state_highliners) {
  plot_data = matrix(c(federal_catch, state_catch - haleiwa_kahuku_dominant_fisher_catch - kawaihae_mahukona_dominant_fisher_catch, haleiwa_kahuku_dominant_fisher_catch, kawaihae_mahukona_dominant_fisher_catch, pb_catch - pb_dominant_fisher, pb_dominant_fisher) / total_catch, nrow = 6, byrow = T)
} else {
  plot_data = matrix(c(federal_catch, state_catch - haleiwa_kahuku_dominant_fisher_catch - kawaihae_mahukona_dominant_fisher_catch, haleiwa_kahuku_dominant_fisher_catch + kawaihae_mahukona_dominant_fisher_catch, pb_catch - pb_dominant_fisher, pb_dominant_fisher) / total_catch, nrow = 5, byrow = T)
}
plot_cols = NA
if(split_state_highliners) {
  plot_cols = c(federal_colors[1], "indianred1", state_colors, penguin_bank_colors)
} else {
  plot_cols = c(federal_colors[1], state_colors, penguin_bank_colors)
}

x_min = min(years[plot_years])
x_max = max(years[plot_years])
y_min = 0
y_max = 1

xs = pretty(c(x_min, x_max), n = 4)
xs = xs[xs >= x_min & xs <= x_max]
ys = c(0, 0.25, 0.5, 0.75, 1)

pdf("memo plot 3.pdf", width = 6.5, height = 5, pointsize = 12)

par(mar = c(4.5, 4.5, 1, 1))

plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = "Proportion of Catch")

axis(side = 1, at = xs)
axis(side = 2, at = ys, las = 1)

n_row = dim(plot_data)[1]
for(i in 1:n_row) {
  polygon(x = c(min(years[plot_years]), years[plot_years], max(years[plot_years])), y = c(0, apply(plot_data[i:n_row, plot_years, drop = F], c(2), sum), 0), col = plot_cols[i], border = NA)
}

text(x = 2003, y = 0.15, "Penguin Bank Highliner", col = "white")
if(split_state_highliners) {
  text(x = 2018.5, y = 0.31, "State Highliner #1", col = "white")
  text(x = 2016.3, y = 0.055, "State\n                Highliner #2", col = "white")
} else {
  text(x = 2018.5, y = 0.21, "State Highliners", col = "white")
}

box(which = "plot", lty = "solid")

legend(x = "topleft", legend = c("     Other Federal Waters", "     State Waters", "     Penguin Bank"), col = "white", bg = rgb(red = 1, green = 1, blue = 1, alpha = 0.8), pch = 15)
rect(x_min + (x_max - x_min) * 0.02, y_min + (y_max - y_min) * 0.935, x_min + (x_max - x_min) * 0.08, y_min + (y_max - y_min) * 0.965, col = federal_colors[1], border = NA)

if(split_state_highliners) {
  rect(x_min + (x_max - x_min) * 0.02, y_min + (y_max - y_min) * 0.8825, x_min + (x_max - x_min) * 0.04, y_min + (y_max - y_min) * 0.9125, col = "indianred1", border = NA)
  rect(x_min + (x_max - x_min) * 0.04, y_min + (y_max - y_min) * 0.8825, x_min + (x_max - x_min) * 0.06, y_min + (y_max - y_min) * 0.9125, col = state_colors[1], border = NA)
  rect(x_min + (x_max - x_min) * 0.06, y_min + (y_max - y_min) * 0.8825, x_min + (x_max - x_min) * 0.08, y_min + (y_max - y_min) * 0.9125, col = state_colors[2], border = NA)
} else {
  rect(x_min + (x_max - x_min) * 0.02, y_min + (y_max - y_min) * 0.8825, x_min + (x_max - x_min) * 0.05, y_min + (y_max - y_min) * 0.9125, col = state_colors[1], border = NA)
  rect(x_min + (x_max - x_min) * 0.05, y_min + (y_max - y_min) * 0.8825, x_min + (x_max - x_min) * 0.08, y_min + (y_max - y_min) * 0.9125, col = state_colors[2], border = NA)
}

rect(x_min + (x_max - x_min) * 0.02, y_min + (y_max - y_min) * 0.83, x_min + (x_max - x_min) * 0.05, y_min + (y_max - y_min) * 0.86, col = penguin_bank_colors[1], border = NA)
rect(x_min + (x_max - x_min) * 0.05, y_min + (y_max - y_min) * 0.83, x_min + (x_max - x_min) * 0.08, y_min + (y_max - y_min) * 0.86, col = penguin_bank_colors[2], border = NA)

dev.off()

# Memo Plot 4: Trips and licenses by state and federal (K, but check if PB is included); catch rate (J) too?
# Two panel with K and J

pdf("memo plot 4.pdf", width = 6.5, height = 4, pointsize = 12)

layout(matrix(c(1, 2), ncol = 2), widths = c(7, 5))

# Left plot: trips and licenses

trips_federal = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 3,], year2, .drop = F), n = n_distinct(paste(FISHER_LIC_FK, FISHED_DATE)))$n # number of trips to federal waters by year
trips_state = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 1,], year2, .drop = F), n = n_distinct(paste(FISHER_LIC_FK, FISHED_DATE)))$n # number of trips to state waters by year
licenses_federal = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 3,], year2, .drop = F), n = n_distinct(FISHER_LIC_FK))$n # number of fishers making trips to federal waters by year
licenses_state = summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 1,], year2, .drop = F), n = n_distinct(FISHER_LIC_FK))$n # number of fishers making trips to state waters by year

plot_years = years >= 2000 & years <= 2023
trips_data = matrix(c(trips_federal, trips_state), nrow = 2, byrow = T)
licenses_data = matrix(c(licenses_federal, licenses_state), nrow = 2, byrow = T)

par(mar = c(4.5, 4.5, 1, 5.5))

x_min = min(years[plot_years])
x_max = max(years[plot_years])
y_min = 0
y_max = 1.1

y_max_trips = max(trips_data[, plot_years], na.rm = T) * 1.1
y_max_licenses = max(licenses_data[, plot_years], na.rm = T) * 1.1

xs = pretty(c(x_min, x_max), n = 4)
xs = xs[xs >= x_min & xs <= x_max]
ys_trips = pretty(c(y_min, y_max_trips), n = 4)
ys_trips = ys_trips[ys_trips >= y_min & ys_trips <= y_max_trips]
ys_licenses = pretty(c(y_min, y_max_licenses), n = 4)
ys_licenses = ys_licenses[ys_licenses >= y_min & ys_licenses <= y_max_licenses]

plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = "")

text("(a)", x = x_min - (x_max - x_min) * 0.39, y = y_max, xpd = T)

axis(side = 1, at = xs)
axis(side = 2, at = ys_trips / y_max_trips, labels = ys_trips, las = 1)
axis(side = 4, at = ys_licenses / y_max_licenses, labels = ys_licenses, las = 1)

mtext(side = 2, "Reported Effort (Fishing Days)", line = 3)
text("Number of Licenses", x = x_max + (x_max - x_min) * 0.35, y = y_min + (y_max - y_min) * 0.5, srt = 270, xpd = T)

lines(x = years[plot_years], y = trips_data[1, plot_years] / y_max_trips, col = water_colors[1], lwd = 2)
lines(x = years[plot_years], y = trips_data[2, plot_years] / y_max_trips, col = water_colors[2], lwd = 2)
lines(x = years[plot_years], y = licenses_data[1, plot_years] / y_max_licenses, col = water_colors[1], lty = "dashed", lwd = 2)
lines(x = years[plot_years], y = licenses_data[2, plot_years] / y_max_licenses, col = water_colors[2], lty = "dashed", lwd = 2)

box(which = "plot", lty = "solid")

legend(x = "topright", legend = c("Federal Effort", "Federal Licenses", "State Effort", "State Licenses"), col = c(rep(water_colors[1], 2), rep(water_colors[2], 2)), lty = c("solid", "dashed", "solid", "dashed"), lwd = 2, bty = "n", cex = 0.75)

# Right plot: catch rate

catch_rate_federal = summarise(group_by(summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 3,], FISHER_LIC_FK, FISHED_DATE), n = sum(kc_catch), year2 = first(year2)), year2, .drop = F), n = mean(n))$n # average catch rate across trips to federal waters by year; weighted by trip, not fisher
catch_rate_state = summarise(group_by(summarise(group_by(FRS_catchrate[FRS_catchrate$zone == 1,], FISHER_LIC_FK, FISHED_DATE), n = sum(kc_catch), year2 = first(year2)), year2, .drop = F), n = mean(n))$n # average catch rate across trips to state waters by year; weighted by trip, not fisher

plot_years = years >= 2000 & years <= 2023
plot_data = matrix(c(catch_rate_federal, catch_rate_state), nrow = 2, byrow = T)
plot_cols = water_colors

par(mar = c(4.5, 3.85, 1, 0.75))

x_min = min(years[plot_years])
x_max = max(years[plot_years])
y_min = 0
y_max = max(plot_data[, plot_years], na.rm = T) * 1.1

xs = pretty(c(x_min, x_max), n = 4)
xs = xs[xs >= x_min & xs <= x_max]
ys = pretty(c(y_min, y_max), n = 4)
ys = ys[ys >= y_min & ys <= y_max]

plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = "")

text("(b)", x = x_min - (x_max - x_min) * 0.38, y = y_max, xpd = T)

axis(side = 1, at = xs)
axis(side = 2, at = ys, las = 1)

mtext(side = 2, "Catch Rate (lb / fishing day)", line = 3)

n_row = dim(plot_data)[1]
for(i in 1:n_row) {
  lines(x = years[plot_years], y = plot_data[i, plot_years], col = plot_cols[i], lwd = 2)
  
  n_col = dim(plot_data)[2]
  for(j in 1:n_col) {
    point_before = ifelse(j == 1, NA, plot_data[i, j - 1])
    point_after = ifelse(j == n_col, NA, plot_data[i, j + 1])
    
    if(is.na(point_before) & is.na(point_after)) {
      points(x = years[j], y = plot_data[i, j], col = plot_cols[i])
    }
  }
}

box(which = "plot", lty = "solid")

legend(x = "topleft", legend = c("Federal", "State"), col = plot_cols, lwd = 2, bty = "n", cex = 0.75)

dev.off()

#*** Extra Plots

# Memo Penguin Bank Plot

pb_catch = summarise(group_by(FRS_catch[FRS_catch$zone == 2,], year2, .drop = F), n = sum(catch))$n
total_catch = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(catch))$n

trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$zone == 2,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Penguin Bank by each fisher
dominant_fisher = trips_by_fisher[order(trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Penguin Bank

pb_all = summarise(group_by(FRS_catch[FRS_catch$zone == 2,], year2, .drop = F), n = sum(catch))$n
pb_dominant_fisher = summarise(group_by(FRS_catch[FRS_catch$zone == 2 & FRS_catch$FISHER_LIC_FK == dominant_fisher,], year2, .drop = F), n = sum(catch))$n

plot_years = years >= 1990 & years <= 2023
plot_data = matrix(c(pb_all - pb_dominant_fisher, pb_dominant_fisher) / total_catch, nrow = 2, byrow = T)
plot_cols = penguin_bank_colors

x_min = min(years[plot_years])
x_max = max(years[plot_years])
y_min = 0
y_max = 1

xs = pretty(c(x_min, x_max), n = 4)
xs = xs[xs >= x_min & xs <= x_max]
ys = pretty(c(y_min, y_max), n = 4)
ys = ys[ys >= y_min & ys <= y_max]

pdf("memo plot penguin bank.pdf", width = 6.5, height = 5, pointsize = 12)

par(mar = c(4.5, 4.5, 1, 1))

plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = "Proportion of Total Catch")

axis(side = 1, at = xs)
axis(side = 2, at = ys, las = 1)

n_row = dim(plot_data)[1]
for(i in 1:n_row) {
  polygon(x = c(min(years[plot_years]), years[plot_years], max(years[plot_years])), y = c(0, apply(plot_data[i:n_row, plot_years, drop = F], c(2), sum), 0), col = plot_cols[i], border = NA)
}

box(which = "plot", lty = "solid")

legend(x = "topright", legend = c("Other Penguin Bank Fishing", "Penguin Bank Highliner"), col = plot_cols, pch = 15, bty = "n")

dev.off()

# Memo State Plot

haleiwa_kahuku_trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(404, 405) & FRS_catch$year >= 2000,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Haleiwa/Kahuku by each fisher
haleiwa_kahuku_dominant_fisher = haleiwa_kahuku_trips_by_fisher[order(haleiwa_kahuku_trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Haleiwa/Kahuku
kawaihae_mahukona_trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(102, 103) & FRS_catch$year >= 2000,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Kawaihae/Mahukona by each fisher
kawaihae_mahukona_dominant_fisher = kawaihae_mahukona_trips_by_fisher[order(kawaihae_mahukona_trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Kawaihae/Mahukona

haleiwa_kahuku_dominant_fisher_catch = summarise(group_by(FRS_catch[FRS_catch$zone == 1 & FRS_catch$FISHER_LIC_FK == haleiwa_kahuku_dominant_fisher,], year2, .drop = F), n = sum(catch))$n
kawaihae_mahukona_dominant_fisher_catch = summarise(group_by(FRS_catch[FRS_catch$zone == 1 & FRS_catch$FISHER_LIC_FK == kawaihae_mahukona_dominant_fisher,], year2, .drop = F), n = sum(catch))$n
state_catch = summarise(group_by(FRS_catch[FRS_catch$zone == 1,], year2, .drop = F), n = sum(catch))$n
total_catch = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(catch))$n

plot_years = years >= 2010 & years <= 2023
plot_data = matrix(c(state_catch - haleiwa_kahuku_dominant_fisher_catch - kawaihae_mahukona_dominant_fisher_catch, haleiwa_kahuku_dominant_fisher_catch, kawaihae_mahukona_dominant_fisher_catch) / total_catch, nrow = 3, byrow = T)
plot_cols = c("indianred1", state_colors)

x_min = min(years[plot_years])
x_max = max(years[plot_years])
y_min = 0
y_max = 1

xs = pretty(c(x_min, x_max), n = 4)
xs = xs[xs >= x_min & xs <= x_max]
ys = pretty(c(y_min, y_max), n = 4)
ys = ys[ys >= y_min & ys <= y_max]

pdf("memo plot state.pdf", width = 6.5, height = 5, pointsize = 12)

par(mar = c(4.5, 4.5, 1, 1))

plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = "Proportion of Total Catch")

axis(side = 1, at = xs)
axis(side = 2, at = ys, las = 1)

n_row = dim(plot_data)[1]
for(i in 1:n_row) {
  polygon(x = c(min(years[plot_years]), years[plot_years], max(years[plot_years])), y = c(0, apply(plot_data[i:n_row, plot_years, drop = F], c(2), sum), 0), col = plot_cols[i], border = NA)
}

box(which = "plot", lty = "solid")

legend(x = "topleft", legend = c("Other State Waters Fishing", "Haleiwa/Kahuku Highliner", "Kawaihae/Mahukona Highliner"), col = plot_cols, pch = 15, bty = "n")

dev.off()

# Memo Federal Plot

haleiwa_kahuku_trips_by_fisher = summarise(group_by(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(520, 521, 525, 526) & FRS_catch$year >= 2010,], FISHER_LIC_FK, FISHED_DATE), FISHER_LIC_FK), n = n()) # number of trips to Haleiwa/Kahuku by each fisher
haleiwa_kahuku_dominant_fisher = haleiwa_kahuku_trips_by_fisher[order(haleiwa_kahuku_trips_by_fisher$n, decreasing = T),]$FISHER_LIC_FK[1] # which fisher made the most trips to Haleiwa/Kahuku

f = summarise(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(520, 521, 525, 526) & FRS_catch$FISHER_LIC_FK %in% c(2103, 12213),], year2, .drop = F), n = sum(catch))$n
kauai_niihau_catch = summarise(group_by(FRS_catch[FRS_catch$AREA_FK %in% c(520, 521, 525, 526),], year2, .drop = F), n = sum(catch))$n
federal_catch = summarise(group_by(FRS_catch[FRS_catch$zone == 3,], year2, .drop = F), n = sum(catch))$n
total_catch = summarise(group_by(FRS_catch, year2, .drop = F), n = sum(catch))$n

plot_years = years >= 2010 & years <= 2023
plot_data = matrix(c(federal_catch - kauai_niihau_catch, kauai_niihau_catch) / total_catch, nrow = 2, byrow = T)
plot_cols = federal_colors

x_min = min(years[plot_years])
x_max = max(years[plot_years])
y_min = 0
y_max = 1

xs = pretty(c(x_min, x_max), n = 4)
xs = xs[xs >= x_min & xs <= x_max]
ys = pretty(c(y_min, y_max), n = 4)
ys = ys[ys >= y_min & ys <= y_max]

pdf("memo plot federal.pdf", width = 6.5, height = 5, pointsize = 12)

par(mar = c(4.5, 4.5, 1, 1))

plot(NA, xlim = c(x_min, x_max), ylim = c(y_min, y_max), axes = F, xaxs = "i", yaxs = "i", xlab = "Year", ylab = "Proportion of Total Catch")

axis(side = 1, at = xs)
axis(side = 2, at = ys, las = 1)

n_row = dim(plot_data)[1]
for(i in 1:n_row) {
  polygon(x = c(min(years[plot_years]), years[plot_years], max(years[plot_years])), y = c(0, apply(plot_data[i:n_row, plot_years, drop = F], c(2), sum), 0), col = plot_cols[i], border = NA)
}

box(which = "plot", lty = "solid")

legend(x = "topright", legend = c("Other Federal Waters Fishing", "Kauai/Niihau Federal Waters"), col = plot_cols, pch = 15, bty = "n")

dev.off()
