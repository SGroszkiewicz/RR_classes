
# Initial cleaning using 'styler' package ------------------------------------------------------

# install.packages("styler")
# library(styler)
# style_file("Exercise_4_solution.R")

# done successfully!


# The file itself ------------------------------------------------------------------------------

# Sets the path to the directory of this file (of Exercise_4_solution.R)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
library(DescTools)
library(dplyr)
library(Hmisc)
library(readxl)
library(stringr)
library(xlsx)

#   Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using: https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.

task_data <- read.csv("Data\\onet_tasks.csv")
# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)

# Introduce number_of_categories variable that corresponds to the number of "ISCO%" sheets in our .xlsx file
sheet_names <- loadWorkbook("Data\\Eurostat_employment_isco.xlsx") %>%
  getSheets() %>%
  names()

number_of_categories <- sheet_names %like% "ISCO%" %>% sum()

# isco% <- read_excel("Data\\Eurostat_employment_isco.xlsx", sheet="ISCO%") in a 'for' loop
for (i in 1:number_of_categories) {
  assign(paste0("isco", i), read_excel("Data\\Eurostat_employment_isco.xlsx", sheet = paste0("ISCO", i)))
}

# We will focus on three countries, but perhaps we could clean this code to allow it
# to easily run for all the countries in the sample?

countries <- c("Belgium", "Spain", "Poland")

# This will calculate worker totals in each of the chosen countries.
for (i in 1:length(countries)) {
  assign(paste0("total_", countries[i]), 0)

  for (j in 1:number_of_categories) {
    assign(
      paste0("total_", countries[i]),
      get(paste0("total_", countries[i])) + get(paste0("isco", j))[[countries[i]]]
    )
  }
}

# Let's merge all these datasets. We'll need a column that stores the occupation categories:
for (i in 1:number_of_categories) {
  ISCO <- rep(i, nrow(get(paste0("isco", i))))
  assign(
    paste0("isco", i),
    cbind(get(paste0("isco", i)), ISCO)
  )
}

# and this gives us one large file with employment in all occupations.
all_data <- lapply(ls(pattern = "isco"), get) %>% data.table::rbindlist()

# We have 9 occupations and the same time range for each, so we an add the totals by
# adding a vector that is 9 times the previously calculated totals
for (i in 1:length(countries)) {
  all_data[[paste0("total_", countries[i])]] <- rep(
    get(paste0("total_", countries[i])),
    number_of_categories
  )
}

# And this will give us shares of each occupation among all workers in a period-country
for (i in 1:length(countries)) {
  all_data[[paste0("share_", countries[i])]] <-
    all_data[[countries[i]]] / all_data[[paste0("total_", countries[i])]]
}

# Now let's look at the task data. We want the first digit of the ISCO variable only

task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level
# (more on what these tasks are below)

aggdata <- aggregate(task_data,
  by = list(task_data$isco08_1dig),
  FUN = mean, na.rm = TRUE
)
aggdata$isco08 <- NULL

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

# These are the ones we're interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others

# Let's combine the data.

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us which is:

task_items <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

std_task_by_country <- function(combined, country, task_item) {
  temp_mean <- wtd.mean(combined[[task_item]], combined[[paste0("share_", country)]])
  temp_sd <- wtd.var(combined[[task_item]], combined[[paste0("share_", country)]]) %>% sqrt()
  combined[[paste0("std_", country, "_", task_item)]] <- (combined[[task_item]] - temp_mean) / temp_sd

  return(combined)
}

for (i in 1:length(countries)) {
  for (j in 1:length(task_items)) {
    combined <- std_task_by_country(combined, countries[i], task_items[j])
  }
}

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce

# Therefore we create a similar function as before, but some changes were required
std_NRCA_by_country <- function(combined, country) {
  temp_mean <- wtd.mean(combined[[paste0(country, "_NRCA")]], combined[[paste0("share_", country)]])
  temp_sd <- wtd.var(combined[[paste0(country, "_NRCA")]], combined[[paste0("share_", country)]]) %>% sqrt()
  combined[[paste0("std_", country, "_NRCA")]] <- (combined[[paste0(country, "_NRCA")]] - temp_mean) / temp_sd

  return(combined)
}

# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:
for (i in 1:length(countries)) {
  combined[[paste0(countries[i], "_NRCA")]] <- rowSums(combined %>%
    select(paste0("std_", countries[i], "_", task_items)))

  # And we standardise NRCA in a similar way.
  combined <- std_NRCA_by_country(combined, countries[i])
}

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.
for (i in 1:length(countries)) {
  combined[[paste0("multip_", countries[i], "_NRCA")]] <-
    combined[[paste0("std_", countries[i], "_NRCA")]] * combined[[paste0("share_", countries[i])]]
}

# Step 2: sum it up (it basically becomes another weighted mean)
for (i in 1:length(countries)) {
  assign(
    paste0("agg_", countries[i]),
    aggregate(combined[[paste0("multip_", countries[i], "_NRCA")]],
      by = list(combined$TIME),
      FUN = sum, na.rm = TRUE
    )
  )
}

# We can plot it now!
plot_by_country <- function(country) {
  plot(get(paste0("agg_", country))$x, xaxt = "n", ylab = paste0("agg_", country))
  axis(1, at = seq(1, 40, 3), labels = agg_Poland$Group.1[seq(1, 40, 3)])
}

plot_by_country("Poland")

plot_by_country("Spain")

plot_by_country("Belgium")

# If this code gets automated and cleaned properly,
#  you should be able to easily add other countries as well as other tasks.
# E.g.:

# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment
