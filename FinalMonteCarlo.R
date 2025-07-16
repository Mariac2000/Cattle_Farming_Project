library(dplyr)
library(decisionSupport)
library(ggplot2)

input_estimates <- read.csv(file='Reduction_of_GHG.csv', header = TRUE, dec = ",", sep = ";")


model_function <- function(){
  
  var_CV <- 50
  n_years <- 15
  discount_rate <- 9.75
  Biogas_per_Cow_m3_year <- 102.2
  methane_value_conversion <- 0.1 # USD per m³ methane
  
  
  Number_of_Cattle <- vv(Number_of_Cattle, var_CV, n_years)
  Land_size <- vv(Land_size, var_CV, n_years)
  Infrastructure_establishment_cost <- vv(Infrastructure_establishment_cost, var_CV, n_years)
  Establishment_labour_cost <- vv(Establishment_labour_cost, var_CV, n_years)
  Labour_cost <- vv(Labour_cost, var_CV, n_years)
  Management_cost <- vv(Management_cost, var_CV, n_years)
  cattle_profit <- vv(cattle_profit, var_CV, n_years)
  Biogas_Profit <- vv(Biogas_Profit, var_CV, n_years)
  Fertilizer_value_from_slurry <- vv(Fertilizer_value_from_slurry, var_CV, n_years)
  Methane_captured_from_biogas_generation <- vv(Methane_captured_from_biogas_generation, var_CV, n_years)
  Land_applied_Manure <- vv(Land_applied_Manure, var_CV, n_years)
  Carbon_Sequerstered_from_Manure <- vv(Carbon_Sequerstered_from_Manure, var_CV, n_years)
  Carbon_Sequestered_from_Slurry <- vv(Carbon_Sequestered_from_Slurry, var_CV, n_years)
  
  # Calculations
  Cattle_per_ha <- Number_of_Cattle / Land_size
  Initial_costs <- Infrastructure_establishment_cost[1] + Establishment_labour_cost[1]
  Annual_costs <- Labour_cost + Management_cost
  Annual_costs[1] <- Annual_costs[1] + Initial_costs
  
  # Profit calculations
  cattle_profit_annual <- Number_of_Cattle * (cattle_profit*0.25) #Cattle is sold evvery 4 years
  biogas_profit_annual <- Biogas_Profit
  slurry_value_annual <- Fertilizer_value_from_slurry
  methane_profit_value <- Methane_captured_from_biogas_generation * methane_value_conversion
  
  #Collecting manure
  benefits_collect <- cattle_profit_annual + biogas_profit_annual + slurry_value_annual + methane_profit_value
  profit_collect <- benefits_collect - Annual_costs
  
  # Not collecting manure
  benefits_no_collect <- cattle_profit_annual
  profit_no_collect <- benefits_no_collect - Annual_costs
  
  # GHG calculations
  carbon_sequestering_overall <- Carbon_Sequerstered_from_Manure + Methane_captured_from_biogas_generation + Carbon_Sequestered_from_Slurry
  benefits_collect_GHG <- Methane_captured_from_biogas_generation * methane_value_conversion + carbon_sequestering_overall
  benefits_no_collect_GHG <- Land_applied_Manure
  
  # NPV calculations
  NPV_no_poo <- discount(profit_no_collect, discount_rate, calculate_NPV = TRUE)
  NPV_collect_poo <- discount(profit_collect, discount_rate, calculate_NPV = TRUE)
  NPV_decision_profit <- NPV_collect_poo - NPV_no_poo
  
  NPV_no_GHG <- discount(benefits_no_collect_GHG, discount_rate, calculate_NPV = TRUE)
  NPV_collect_GHG <- discount(benefits_collect_GHG, discount_rate, calculate_NPV = TRUE)
  NPV_decision_GHG <- NPV_collect_GHG - NPV_no_GHG
  
  return(list(
    NPV_no_poo = NPV_no_poo,
    NPV_collect_poo = NPV_collect_poo,
    NPV_decision_profit = NPV_decision_profit,
    NPV_no_GHG = NPV_no_GHG,
    NPV_collect_GHG = NPV_collect_GHG,
    NPV_decision_GHG = NPV_decision_GHG
  ))
}

example_mc_simulation <- mcSimulation(
  estimate = as.estimate(input_estimates),
  model_function = model_function,
  numberOfModelRuns = 10000,
  functionSyntax = 'plainNames'
)


# Plot profit results
plot_distributions(
  mcSimulation_object = example_mc_simulation,
  vars = c('NPV_decision_profit'),
  old_name = c('NPV_decision_profit'),
  new_name = c('NPV decision Profit'),
  x_axis_name = "Profits [USD]",
  method = 'smooth_simple_overlay',
  new_names = 'NPV Difference (Collecting vs Not Collecting Manure)',
  colors = 'hotpink'
)

# Plot GHG results
plot_distributions(
  mcSimulation_object = example_mc_simulation,
  vars = c('NPV_decision_GHG'),
  old_name =c('NPV_decision_GHG'),
  new_name = c('NPV decision GHG'),
  x_axis_name = 'CO2 saved [tons]',
  method = 'smooth_simple_overlay',
  new_names = 'GHG Benefit (Collecting vs Not Collecting Manure)',
  colors = 'darkred'
)

#Plot Profit and GHG results together
plot_distributions(
  mcSimulation_object = example_mc_simulation,
  vars = c('NPV_decision_profit', 'NPV_decision_GHG'),
  old_names = c('NPV_decision_profit', 'NPV_decision_GHG'),
  new_names = c('NPV decision profit', 'NPV decision GHG'),
  method = 'smooth_simple_overlay',
  x_axis_name = 'Outcome NPF: Profit and GHG',
  colors = c('darkgreen', 'darkblue')
)

#Plotting EVPI GHG
mcSimulation_table_EVPI <- data.frame(example_mc_simulation$x, 
                                      example_mc_simulation$y)

evpi <- multi_EVPI(mc = mcSimulation_table_EVPI, 
                        first_out_var = 'NPV_decision_GHG')

plot_evpi(evpi, decision_vars = 'NPV_decision_GHG')

#Plotting EVPI Profit
mcSimulation_table_EVPI2 <- data.frame(example_mc_simulation$x, 
                                      example_mc_simulation$y)

evpi2 <- multi_EVPI(mc = mcSimulation_table_EVPI2, 
                   first_out_var = 'NPV_decision_profit')

plot_evpi(evpi2, decision_vars = 'NPV_decision_profit')


