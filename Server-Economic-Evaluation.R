library(sf)
library(leaflet)
library(ggplot2)
library(data.table)


data_tza <- read.csv("Economic-Evaluation-dashboard/tza_sample_data.csv")
shape_file_tza <- st_read("Economic-Evaluation-dashboard/shapefiles/TZA_shapefile_correctNamesDHIS2_Dist.shp")


setDT(data_tza)

# List out the 11 interventions 
int_names <- c("LSM", "IPTSc", "IRS", "Vaccine", "ICCM", "CM", 
               "STD_Nets", "PBO_Nets", "IG2_Nets", "PMC", "SMC")

# This randomly decide if a district is allowed to use a specific intervention.

set.seed(42)
for (int in int_names) {
  col_name <- paste0("eligible_int_", int)
  data_tza[, (col_name) := sample(c(TRUE, FALSE), 1), by = admin_2]
}


wtp_val         <- 200   
budget_inc_pct  <- 10    # Allowed budget increase for the "Varying Budget" plan
ref_plan_name   <- "bau" 


u_costs <- list(
  LSM=2.0, IPTSc=1.0, IRS=5.0, Vaccine=12.0, ICCM=2.0, 
  CM=3.5, STD_Nets=2.5, PBO_Nets=3.5, IG2_Nets=4.5, PMC=1.0, SMC=1.2
)

# How much each each scenario cost

dt_costed <- copy(data_tza)
dt_costed[, total_cost := 0]

for (int in names(u_costs)) {
  # Get the column names for status, eligibility, and coverage
  act_col  <- paste0("active_int_", int)
  elig_col <- paste0("eligible_int_", int)
  cov_col  <- paste0("coverage_int_", int)
  cost_col <- paste0("cost_", int)
  
  
  #  Population * Is it Active? * Coverage * Price * Is it allowed?
  dt_costed[, (cost_col) := nHost * get(act_col) * get(cov_col) * u_costs[[int]] * as.numeric(get(elig_col))]
  
  # Add this specific cost to the total for the row
  dt_costed[, total_cost := total_cost + get(cost_col)]
}

#  Summed up costs and cases across all years to get a single total per scenario.
# We focus on the "0-100" age group to represent the whole population.
ds_agg <- dt_costed[age_group == "0-100", .(
  cost = sum(total_cost),
  cases = sum(cum_nUncomp + cum_nSevere)
), by = .(admin_2, scenario_name, plan, EIR_CI)]

#  select out the BAU results for every district to act as our reference
ref_dt <- ds_agg[tolower(plan) == tolower(ref_plan_name)]
ref_dt <- ref_dt[, .SD[1], by = .(admin_2, EIR_CI)][, .(admin_2, EIR_CI, ref_cost = cost, ref_cases = cases)]

# S Matching every scenario with its BAU 
ds_cea <- merge(ds_agg, ref_dt, by = c("admin_2", "EIR_CI"), all.x = TRUE)

# This calculates the incremental cost

ds_cea[, cost_diff := cost - ref_cost]
ds_cea[, cases_averted := ref_cases - cases]

# ICER and NMB
ds_cea[, ICER := cost_diff / cases_averted]
ds_cea[, NMB := (cases_averted * wtp_val) - cost_diff]

# Is the health benefit worth the cost? (Cost-Effective)
ds_cea[, is.CE := NMB > 0]


# We perform our selection using the lower estimate of transmission (lci)
ds_opt <- ds_cea[EIR_CI == "EIR_lci"]

# Calculate the national budget limits
budget_current  <- ds_opt[tolower(plan) == tolower(ref_plan_name), sum(cost)]
budget_envelope <- budget_current * (1 + (budget_inc_pct/100))


# For each district, pick the choice that gives the most NMB and is cost-effective.
plan_ce <- ds_opt[is.CE == TRUE][order(admin_2, -NMB), .SD[1], by = admin_2]


# For each district, pick the choice that stops the most cases.
plan_vary <- ds_opt[order(admin_2, -cases_averted), .SD[1], by = admin_2]


# Here we translate the TRUE/FALSE columns back into readable names (e.g., "IRS, CM")
int_cols <- names(data_tza)[grep("active_int_", names(data_tza))]
lookup <- unique(data_tza[age_group == "0-100", .SD, .SDcols = c("admin_2", "scenario_name", int_cols)])

final_view <- merge(plan_ce[, .(admin_2, scenario_name, NMB, ICER)], lookup, by = c("admin_2", "scenario_name"))

final_view[, active_interventions := apply(.SD, 1, function(row) {
  active <- gsub("active_int_", "", int_cols[which(as.logical(row))])
  if(length(active) == 0) "None" else paste(active, collapse = ", ")
}), .SDcols = int_cols]


print(final_view[, .(admin_2, active_interventions, NMB, ICER)])

# some view visualizations

# We compare the winning plan to the BAU plan to see if we added or removed interventions.
footprint_bau <- merge(ref_dt[, .(admin_2, scenario_name = "bau")], lookup, by = c("admin_2", "scenario_name"))
footprint_ce  <- merge(plan_ce[, .(admin_2, scenario_name)], lookup, by = c("admin_2", "scenario_name"))

footprint_bau[, n_active_ref := rowSums(.SD), .SDcols = int_cols]
footprint_ce[,  n_active_win := rowSums(.SD), .SDcols = int_cols]

map_data_ce <- merge(footprint_bau[, .(admin_2, n_active_ref)], footprint_ce[, .(admin_2, n_active_win)], by = "admin_2")
map_data_ce[, change := ifelse(n_active_win > n_active_ref, "Added", 
                               ifelse(n_active_win < n_active_ref, "Removed", "No Change"))]

# Join results to the shapefile
shape_file_tza <- st_transform(shape_file_tza, 4326) # Match Leaflet's coordinate system
map_sf_ce <- merge(shape_file_tza, map_data_ce, by = "admin_2")

# Create the interactive map
pal <- colorFactor(palette = c("#2ca25f", "#feb24c", "#de2d26"), levels = c("Added", "No Change", "Removed"))

map1 <- leaflet(map_sf_ce) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal(change), weight = 1, color = "white", fillOpacity = 0.7,
              label = ~paste0(admin_2, ": ", change)) %>%
  addLegend(pal = pal, values = ~change, title = "Intensity Change")

map1 


#map2




# Show all 11 interventions in a grid to see where each interventions is being used.
footprint_vary <- merge(plan_vary[, .(admin_2, scenario_name)], lookup, by = c("admin_2", "scenario_name"))
vary_long <- melt(footprint_vary, id.vars = "admin_2", measure.vars = int_cols)
vary_long[, intervention := gsub("active_int_", "", variable)]

map_sf_vary <- merge(shape_file_tza, vary_long, by = "admin_2")

map3 <- ggplot(map_sf_vary) +
  geom_sf(aes(fill = value), color = NA) +
  facet_wrap(~intervention, ncol = 4) +
  scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#f0f0f0"), name = "Deployed") +
  theme_void() +
  labs(title = " Where is each intervention used?",
       subtitle = "Blue districts represent active deployment for that intervention")

print(map3) 

























































