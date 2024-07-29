## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE, message = FALSE
)


## ----setup--------------------------------------------------------------------

library(devtools)
install_github("vjoshy/ATQ_Surveillance_Package")

library(ATQ)

## ----populationSimulation-----------------------------------------------------

set.seed(123)

# Simulate 16 catchments of 80x80 squares and the number of schools they contain
catchment <- catchment_sim(16, 80, dist_func = stats::rgamma, shape = 4.313, rate = 3.027)

# Simulate population size of elementary schools 
elementary<- elementary_pop(catchment, dist_func = stats::rgamma, shape = 5.274, rate = 0.014)

# Simulate households with children
house_children <- subpop_children(elementary, n = 5,
                                  prop_parent_couple = 0.7668901,
                                  prop_children_couple = c(0.3634045, 0.4329440, 0.2036515),
                                  prop_children_lone = c(0.5857832, 0.3071523, 0.1070645),
                                  prop_elem_age = 0.4976825)

# Simulate households without children using pre-specified proportions
house_noChild <- subpop_noChildren(house_children, elementary,
                                   prop_house_size = c(0.23246269, 0.34281716, 0.16091418, 0.16427239, 0.09953358),
                                   prop_house_Children = 0.4277052)

# Combine household simulations and generate individual-level data
households <- simulate_households(house_children, house_noChild)


## ----epidemicSimulation-------------------------------------------------------

# isolate individuals data
individuals <- households$individual_sim

# simulate epidemics for 10 years, each with a period of 300 days and 32 individuals infected initially
# infection period of 4 days 
epidemic <- ssir(nrow(individuals), T = 300, alpha = 0.298, avg_start = 45, 
                 min_start = 20, inf_period = 4, inf_init = 32, report = 0.02, lag = 7, rep = 10)

# Summarize and plot the epidemic simulation results
summary(epidemic)

plot(epidemic)


## ----absenteeism--------------------------------------------------------------

absent_data <- compile_epi(epidemic, individuals)

dplyr::glimpse(absent_data)


## ----metrics, fig.height = 4, fig.width = 7-----------------------------------
# Evaluate alarm metrics for epidemic detection
# lag of 15
alarms <- eval_metrics(absent_data, maxlag = 15, thres = seq(0.1,0.6,by = 0.05))

summary(alarms$results)

# Plot various alarm metrics values
plot(alarms$metrics, "FAR")    # False Alert Rate
plot(alarms$metrics, "ADD")    # Accumulated Days Delayed
plot(alarms$metrics, "FATQ")   # First Alert Time Quality
plot(alarms$metrics, "AATQ")   # Average ATQ
plot(alarms$metrics, "WFATQ")  # Weighted FATQ
plot(alarms$metrics, "WAATQ")  # Weighted Average ATQ

# visualization of epidemics with alarms raised.
alarm_plots <- plot(alarms$plot_data)
for(i in seq_along(alarm_plots)) { 
  print(alarm_plots[[i]]) 
}


