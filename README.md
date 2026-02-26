
## Malaria Economic Evaluation & Optimization Dashboard 
Project Overview

This dashboard is a decision-support tool designed to evaluate the cost-effectiveness of malaria intervention strategies at the sub-national level in Tanzania. It bridges the gap between epidemiological simulation data and health-economic policy by identifying optimal resource allocation strategies under fixed and varying budget constraints.

1. Data Structure

The analysis is performed on a panel dataset containing:

Geographic Level: Admin-2 (Districts).

Temporal Scope: Multi-year simulations (e.g., 2026–2030).

Interventions: 11 specific tools including Case Management (CM), iCCM, Larval Source Management (LSM), various Bednets (PBO, IG2, STD), SMC, PMC, IPTSc, IRS, and Vaccines.

Uncertainty: Confidence intervals for transmission intensity (EIR_CI) and stochastic variation (seeds).

2. Mathematical Framework
A. Cost Calculation (Bottom-Up)

The cost of an intervention in a specific district is calculated using the formula:
Cost = Population (nHost) × Coverage (%) × Unit Cost (USD) × Eligibility (0/1)

Eligibility: A policy filter determining if a district is permitted to use a specific tool.

Ignore Active Logic: Following the policy-first approach, costs are driven by eligibility rather than raw simulation status.

B. Health Benefit (Incremental Impact)

Health impact is measured as Cases Averted using an "Odometer" logic:

Period Burden: Cumulative Cases (End Year) - Cumulative Cases (Year Start - 1).

Benefit Calculation: Reference Burden (BAU) - Scenario Burden (NSP).

C. Economic Metrics

ICER (Incremental Cost-Effectiveness Ratio): The price tag per health gain.
ICER = (Scenario Cost - Reference Cost) / Cases Averted.

NMB (Net Monetary Benefit): The total value added to society.
NMB = (Cases Averted × Willingness to Pay) - Incremental Cost.

is.CE (Cost-Effective): A binary flag where TRUE if NMB > 0 (or ICER < WTP).

3. Optimization Logic (The "Shared Wallet")

The dashboard uses Linear Programming (lpSolve) to identify the best national strategy. Unlike simple ranking, this approach looks at all 184 districts simultaneously to find the Global Maximum.

Objective: Maximize National Health (Cases Averted) or Value (NMB).

Budget Constraint: The total sum of all district scenario costs must be 
≤
≤
 the Budget Envelope.

Exclusive Choice: Every district is attributed exactly one scenario (ensuring no district is left empty and no district is over-allocated).

4. Visualizations & Interpretations
Map 1: Most Cost-Effective Plan Changes

Visualizes the geographic shift from the baseline to the strategy maximizing Net Monetary Benefit.

Green: Districts where an upgrade (NSP) is recommended because it provides the highest value for money.

Purple/Gray: Districts kept on the reference plan (BAU) due to budget limits or lower efficiency.

Map 2: Optimal Assessment Changes

Identifies where to reallocate resources to maximize Health Outcomes (Cases Averted) within the strictly fixed current budget.

This map reveals how to get the most "health" out of the existing wallet without spending an extra dollar.

Map 3: Grouped Strategy Footprint

Reveals the national footprint of intervention groups optimized for a Flexible Budget Envelope.

Grouping Logic: To simplify the strategy, similar tools are grouped (e.g., CM & iCCM, PMC & SMC).

Mixture Detection: The legend distinguishes between districts receiving one tool in a group versus those receiving a "Both" mixture, using distinct color shades (e.g., Light Blue for CM Only, Dark Blue for Both).

