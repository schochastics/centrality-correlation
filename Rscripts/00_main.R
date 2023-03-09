# helper script ----
source("Rscripts/functions.R")

# create Figure 1 and Figure 2 ----
source("Rscripts/01_misc_figures.R")

# threshold rewire experiment ----
source("Rscripts/02a_threshold_rewire_sim.R")
source("Rscripts/02b_threshold_rewire_ana.R")

# cluster graph experiments ----
source("Rscripts/03a_cluster_sim.R")
source("Rscripts/03b_cluster_ana.R")

# high discordance graphs ----
source("Rscripts/04a_uncor_sim.R")
source("Rscripts/04b_uncor_ana.R")

# real networks ----
source("Rscripts/05a_real_graph_sim.R")
source("Rscripts/05b_real_graph_ana.R")
