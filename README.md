# Economic-Evaluation-dashboard
Development of economic evaluation dashboard that takes into account how far an intervention extends people lives with regards to its cost and available budget.
# This code I worked out the logic behind the economic Evaluation, thus :

## Overview

This script performs a district-level cost-effectiveness analysis of 11 malaria interventions in Tanzania. It compares intervention scenarios to a Business-As-Usual (BAU) baseline and identifies optimal strategies under budget constraints.

The process:

Loads simulation and spatial data.

Assigns intervention eligibility by district.

Calculates total intervention costs per scenario.

Aggregates costs and malaria cases.

Computes:

Incremental Cost

Cases Averted

ICER

Net Monetary Benefit (NMB)

Identifies:

Most cost-effective plan (highest NMB)

Plan that averts the most cases

Visualizes results using:

Interactive Leaflet maps

Faceted ggplot intervention maps

Output

District-level optimal intervention portfolios

Cost-effectiveness metrics (NMB, ICER)

Maps showing where interventions are added, removed, or deployed