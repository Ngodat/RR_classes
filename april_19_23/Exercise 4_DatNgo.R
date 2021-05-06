
# Sets the path to the parent directory of RR classes
setwd("//media//datngo//Driver1//Uni of Warsaw//2.5 Reproducible Research//w07//RR_classes//april_19_23")

#   Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using: https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.

task_data <- read.csv("Data//onet_tasks.csv")
EuroStatIsco_dir <- "Data//Eurostat_employment_isco.xlsx" 
no_isco <- 9
country_list <- c("Belgium","Spain","Poland")
task_list <- c("t_4A2a4","t_4A2a4","t_4A4a1")
# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)
library(readxl)                     

for (i in 1:no_isco){
  assign(paste0("isco",i),read_excel(EuroStatIsco_dir, sheet = paste0("ISCO",i)))
}

# We will focus on three countries, but perhaps we could clean this code to allow it
# to easily run for all the countries in the sample?

# This will calculate worker totals in each of the chosen countries.

for (country in country_list){
  assign(paste0("total_",country),rep(0, nrow(isco1)))
  for (i in 1:no_isco){
    assign(paste0("total_",country),(get(paste0("total_",country)) + get(paste0("isco",i))[,country]))
  }
}

# Let's merge all these datasets. We'll need a column that stores the occupation categories:
for (i in 1:no_isco){
  assign(paste0("isco",i), cbind(get(paste0("isco",i)), ISCO = rep(i, nrow(isco1))))
}

# and this gives us one large file with employment in all occupations.
all_data <- isco1
for (i in 2:no_isco){
  all_data = rbind(all_data, get(paste0("isco",i)))
}

# We have 9 occupations and the same time range for each, so we an add the totals by
# adding a vector that is 9 times the previously calculated totals
for (country in country_list){
  all_data <- cbind(all_data, rep(get(paste0("total_",country))[,country],no_isco))
  colnames(all_data)[length(all_data)] <- paste0("total_",country)
}

# And this will give us shares of each occupation among all workers in a period-country
for (country in country_list){
  all_data[,paste0("share_",country)] <- all_data[,country]/all_data[,paste0("total_",country)]
}

# Now let's look at the task data. We want the first digit of the ISCO variable only
library(stringr)

task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)

aggdata <-aggregate(task_data, by=list(task_data$isco08_1dig),
                    FUN=mean, na.rm=TRUE)
aggdata$isco08 <- NULL

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

#These are the ones we're interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others

#Let's combine the data.
library(dplyr)

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

#install.packages("Hmisc")
library(Hmisc)

# define the standardize function
standardise <- function(task_code, country){
  temp_mean <- wtd.mean(combined[,task_code], combined[,paste0("share_",country)])
  temp_sd <- wtd.var(combined[,task_code], combined[,paste0("share_",country)]) %>% sqrt()
  combined[, paste0("std_",country,"_",task_code)] <- (combined[,task_code] - temp_mean)/temp_sd

} 

for (task_code in task_list){
  for (country in country_list){
    standardise(task_code, country)
  }
}

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:
# And we standardise NRCA in a similar way.

for (country in country_list){
  combined[,paste0(country, "_NRCA")] <- sum(combined[, paste0("std_",country,"_", task_list)])
  standardise("NRCA", country)
}

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.
for (country in country_list){
  combined[,paste0("multip_",country, "_NRCA")] <- combined[, paste0("std_",country,"_NRCA")]*combined[,paste0("share_",country)]
  standardise("NRCA", country)
}

# Step 2: sum it up (it basically becomes another weighted mean)
for (country in country_list){
  assign(paste0("agg_", country),aggregate(combined[,paste0("multip_",country, "_NRCA")]
                                           , by=list(combined$TIME),
                                           FUN=sum, na.rm=TRUE))
}

# We can plot it now!
plot(agg_Poland$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Poland$Group.1[seq(1, 40, 3)])

plot(agg_Spain$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Poland$Group.1[seq(1, 40, 3)])

plot(agg_Belgium$x, xaxt="n")
axis(1, at=seq(1, 40, 3), labels=agg_Poland$Group.1[seq(1, 40, 3)])


# If this code gets automated and cleaned properly,
#  you should be able to easily add other countries as well as other tasks.
# E.g.:

# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment

