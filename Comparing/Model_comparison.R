#Packages - the locations to the package directories will need to be rewritten
library("Cairo", lib.loc="~/pathway/to/package/directory")
library("coda", lib.loc="~/pathway/to/package/directory")
library("phytools", lib.loc="~/pathway/to/package/directory")
library("foreign", lib.loc="~/pathway/to/package/directory")
library("MASS", lib.loc="~/pathway/to/package/directory")

#List of directories containing the simulated outbreaks
Outbreaks <- c("Outbreak_directory_1", "Outbreak_directory_2", "Outbreak_directory_3")


#List of the proportion of isolates from each simulation sample that are from the animal population
Animal_sample_proportion <- c(0.04, 0.97, 0.96)

Human_sample_proportion <- 1 - Animal_sample_proportion


#list of source names. Must be identical to those called in BEAST
my.sources <- c("Animal", "Human") 

source_number <- length(my.sources)

Outbreak_number <- length(Outbreaks)

Outbreak_matrix <-matrix(c(1:((Outbreak_number+1) * 40)), nrow=40, ncol=(Outbreak_number+1))

Outbreak_matrix[1,1] <- "Structured_coalescent_Animal_length_HPD_lower"
Outbreak_matrix[2,1] <- "Structured_coalescent_Animal_length_HPD_higher"
Outbreak_matrix[3,1] <- "Structured_coalescent_Animal_length_median"
Outbreak_matrix[4,1] <- "Structured_coalescent_Animal_length_mean"

Outbreak_matrix[5,1] <- "Structured_coalescent_Human_length_HPD_lower"
Outbreak_matrix[6,1] <- "Structured_coalescent_Human_length_HPD_higher"
Outbreak_matrix[7,1] <- "Structured_coalescent_Human_length_median"
Outbreak_matrix[8,1] <- "Structured_coalescent_Human_length_mean"

Outbreak_matrix[9,1] <- "Structured_coalescent_Animal_Human_rate_HPD_lower"
Outbreak_matrix[10,1] <- "Structured_coalescent_Animal_Human_rate_HPD_higher"
Outbreak_matrix[11,1] <- "Structured_coalescent_Animal_Human_rate_median"
Outbreak_matrix[12,1] <- "Structured_coalescent_Animal_Human_rate_mean"

Outbreak_matrix[13,1] <- "Structured_coalescent_Human_Animal_rate_HPD_lower"
Outbreak_matrix[14,1] <- "Structured_coalescent_Human_Animal_rate_HPD_higher"
Outbreak_matrix[15,1] <- "Structured_coalescent_Human_Animal_rate_median"
Outbreak_matrix[16,1] <- "Structured_coalescent_Human_Animal_rate_mean"


Outbreak_matrix[17,1] <- "Discrete_phylogeographic_Animal_Markov_reward_HPD_lower"
Outbreak_matrix[18,1] <- "Discrete_phylogeographic_Animal_Markov_reward_HPD_higher"
Outbreak_matrix[19,1] <- "Discrete_phylogeographic_Animal_Markov_reward_median"
Outbreak_matrix[20,1] <- "Discrete_phylogeographic_Animal_Markov_reward_mean"

Outbreak_matrix[21,1] <- "Discrete_phylogeographic_Human_Markov_reward_HPD_lower"
Outbreak_matrix[22,1] <- "Discrete_phylogeographic_Human_Markov_reward_HPD_higher"
Outbreak_matrix[23,1] <- "Discrete_phylogeographic_Human_Markov_reward_median"
Outbreak_matrix[24,1] <- "Discrete_phylogeographic_Human_Markov_reward_mean"

Outbreak_matrix[25,1] <- "Discrete_phylogeographic_Animal_Human_Markov_jump_HPD_lower"
Outbreak_matrix[26,1] <- "Discrete_phylogeographic_Animal_Human_Markov_jump_HPD_higher"
Outbreak_matrix[27,1] <- "Discrete_phylogeographic_Animal_Human_Markov_jump_median"
Outbreak_matrix[28,1] <- "Discrete_phylogeographic_Animal_Human_Markov_jump_mean"

Outbreak_matrix[29,1] <- "Discrete_phylogeographic_Human_Animal_Markov_jump_HPD_lower"
Outbreak_matrix[30,1] <- "Discrete_phylogeographic_Human_Animal_Markov_jump_HPD_higher"
Outbreak_matrix[31,1] <- "Discrete_phylogeographic_Human_Animal_Markov_jump_median"
Outbreak_matrix[32,1] <- "Discrete_phylogeographic_Human_Animal_Markov_jump_mean"

Outbreak_matrix[33,1] <- "Structured_coalescent_Animal_Human_transitions_HPD_lower"
Outbreak_matrix[34,1] <- "Structured_coalescent_Animal_Human_transitions_HPD_higher"
Outbreak_matrix[35,1] <- "Structured_coalescent_Animal_Human_transitions_median"
Outbreak_matrix[36,1] <- "Structured_coalescent_Animal_Human_transitions_mean"

Outbreak_matrix[37,1] <- "Structured_coalescent_Human_Animal_transitions_HPD_lower"
Outbreak_matrix[38,1] <- "Structured_coalescent_Human_Animal_transitions_HPD_higher"
Outbreak_matrix[39,1] <- "Structured_coalescent_Human_Animal_transitions_median"
Outbreak_matrix[40,1] <- "Structured_coalescent_Human_Animal_transitions_mean"


ESS_matrix <-matrix(c(1:((Outbreak_number+1) * 10)), nrow=10, ncol=(Outbreak_number+1))

ESS_matrix[1,1] <- "Structured_coalescent_Animal_length_ESS"
ESS_matrix[2,1] <- "Structured_coalescent_Human_length_ESS"
ESS_matrix[3,1] <- "Structured_coalescent_Animal_Human_rate_ESS"
ESS_matrix[4,1] <- "Structured_coalescent_Human_animal_rate_ESS"

ESS_matrix[5,1] <- "Discrete_phylogeographic_Animal_length_ESS"
ESS_matrix[6,1] <- "Discrete_phylogeographic_Human_length_ESS"
ESS_matrix[7,1] <- "Discrete_phylogeographic_Animal_Human_rate_ESS"
ESS_matrix[8,1] <- "Discrete_phylogeographic_Human_animal_rate_ESS"

ESS_matrix[9,1] <- "Structured_coalescent_Animal_Human_transitions_ESS"
ESS_matrix[10,1] <- "Structured_coalescent_Human_animal_transitions_ESS"


Outbreak_matrix_true <-matrix(c(1:((Outbreak_number+1) * 6)), nrow=6, ncol=(Outbreak_number+1))

Outbreak_matrix_true[1,1] <- "Animal_total_edge"
Outbreak_matrix_true[2,1] <- "Human_total_edge"
Outbreak_matrix_true[3,1] <- "Animal_Human_transmissions"
Outbreak_matrix_true[4,1] <- "Human_Animal_transmissions"
Outbreak_matrix_true[5,1] <- "Animal_Human_rate"
Outbreak_matrix_true[6,1] <- "Human_Animal_rate"


Outbreak_matrix_raw <-matrix(c(1:((Outbreak_number+1) * 40)), nrow=40, ncol=(Outbreak_number+1))

Outbreak_matrix_raw[1,1] <- "Structured_coalescent_Animal_length_HPD_lower"
Outbreak_matrix_raw[2,1] <- "Structured_coalescent_Animal_length_HPD_higher"
Outbreak_matrix_raw[3,1] <- "Structured_coalescent_Animal_length_median"
Outbreak_matrix_raw[4,1] <- "Structured_coalescent_Animal_length_mean"

Outbreak_matrix_raw[5,1] <- "Structured_coalescent_Human_length_HPD_lower"
Outbreak_matrix_raw[6,1] <- "Structured_coalescent_Human_length_HPD_higher"
Outbreak_matrix_raw[7,1] <- "Structured_coalescent_Human_length_median"
Outbreak_matrix_raw[8,1] <- "Structured_coalescent_Human_length_mean"

Outbreak_matrix_raw[9,1] <- "Structured_coalescent_Animal_Human_rate_HPD_lower"
Outbreak_matrix_raw[10,1] <- "Structured_coalescent_Animal_Human_rate_HPD_higher"
Outbreak_matrix_raw[11,1] <- "Structured_coalescent_Animal_Human_rate_median"
Outbreak_matrix_raw[12,1] <- "Structured_coalescent_Animal_Human_rate_mean"

Outbreak_matrix_raw[13,1] <- "Structured_coalescent_Human_Animal_rate_HPD_lower"
Outbreak_matrix_raw[14,1] <- "Structured_coalescent_Human_Animal_rate_HPD_higher"
Outbreak_matrix_raw[15,1] <- "Structured_coalescent_Human_Animal_rate_median"
Outbreak_matrix_raw[16,1] <- "Structured_coalescent_Human_Animal_rate_mean"


Outbreak_matrix_raw[17,1] <- "Discrete_phylogeographic_Animal_Markov_reward_HPD_lower"
Outbreak_matrix_raw[18,1] <- "Discrete_phylogeographic_Animal_Markov_reward_HPD_higher"
Outbreak_matrix_raw[19,1] <- "Discrete_phylogeographic_Animal_Markov_reward_median"
Outbreak_matrix_raw[20,1] <- "Discrete_phylogeographic_Animal_Markov_reward_mean"

Outbreak_matrix_raw[21,1] <- "Discrete_phylogeographic_Human_Markov_reward_HPD_lower"
Outbreak_matrix_raw[22,1] <- "Discrete_phylogeographic_Human_Markov_reward_HPD_higher"
Outbreak_matrix_raw[23,1] <- "Discrete_phylogeographic_Human_Markov_reward_median"
Outbreak_matrix_raw[24,1] <- "Discrete_phylogeographic_Human_Markov_reward_mean"

Outbreak_matrix_raw[25,1] <- "Discrete_phylogeographic_Animal_Human_Markov_jump_HPD_lower"
Outbreak_matrix_raw[26,1] <- "Discrete_phylogeographic_Animal_Human_Markov_jump_HPD_higher"
Outbreak_matrix_raw[27,1] <- "Discrete_phylogeographic_Animal_Human_Markov_jump_median"
Outbreak_matrix_raw[28,1] <- "Discrete_phylogeographic_Animal_Human_Markov_jump_mean"

Outbreak_matrix_raw[29,1] <- "Discrete_phylogeographic_Human_Animal_Markov_jump_HPD_lower"
Outbreak_matrix_raw[30,1] <- "Discrete_phylogeographic_Human_Animal_Markov_jump_HPD_higher"
Outbreak_matrix_raw[31,1] <- "Discrete_phylogeographic_Human_Animal_Markov_jump_median"
Outbreak_matrix_raw[32,1] <- "Discrete_phylogeographic_Human_Animal_Markov_jump_mean"

Outbreak_matrix_raw[33,1] <- "Structured_coalescent_Animal_Human_transitions_HPD_lower"
Outbreak_matrix_raw[34,1] <- "Structured_coalescent_Animal_Human_transitions_HPD_higher"
Outbreak_matrix_raw[35,1] <- "Structured_coalescent_Animal_Human_transitions_median"
Outbreak_matrix_raw[36,1] <- "Structured_coalescent_Animal_Human_transitions_mean"

Outbreak_matrix_raw[37,1] <- "Structured_coalescent_Human_Animal_transitions_HPD_lower"
Outbreak_matrix_raw[38,1] <- "Structured_coalescent_Human_Animal_transitions_HPD_higher"
Outbreak_matrix_raw[39,1] <- "Structured_coalescent_Human_Animal_transitions_median"
Outbreak_matrix_raw[40,1] <- "Structured_coalescent_Human_Animal_transitions_mean"


ESS_matrix_raw <-matrix(c(1:((Outbreak_number+1) * 10)), nrow=10, ncol=(Outbreak_number+1))

ESS_matrix_raw[1,1] <- "Structured_coalescent_Animal_length_ESS"
ESS_matrix_raw[2,1] <- "Structured_coalescent_Human_length_ESS"
ESS_matrix_raw[3,1] <- "Structured_coalescent_Animal_Human_rate_ESS"
ESS_matrix_raw[4,1] <- "Structured_coalescent_Human_animal_rate_ESS"

ESS_matrix_raw[5,1] <- "Discrete_phylogeographic_Animal_length_ESS"
ESS_matrix_raw[6,1] <- "Discrete_phylogeographic_Human_length_ESS"
ESS_matrix_raw[7,1] <- "Discrete_phylogeographic_Animal_Human_rate_ESS"
ESS_matrix_raw[8,1] <- "Discrete_phylogeographic_Human_animal_rate_ESS"

ESS_matrix_raw[9,1] <- "Structured_coalescent_Animal_Human_transitions_ESS"
ESS_matrix_raw[10,1] <- "Structured_coalescent_Human_animal_transitions_ESS"


Outbreak_matrix_true_raw <-matrix(c(1:((Outbreak_number+1) * 6)), nrow=6, ncol=(Outbreak_number+1))

Outbreak_matrix_true_raw[1,1] <- "Animal_total_edge"
Outbreak_matrix_true_raw[2,1] <- "Human_total_edge"
Outbreak_matrix_true_raw[3,1] <- "Animal_Human_transmissions"
Outbreak_matrix_true_raw[4,1] <- "Human_Animal_transmissions"
Outbreak_matrix_true_raw[5,1] <- "Animal_Human_rate"
Outbreak_matrix_true_raw[6,1] <- "Human_Animal_rate"


for(x in 1:Outbreak_number){
  
  #Pathway to sampled tree file
  tree_2 <- paste(Outbreaks[x], "/Output.nexus_altered_extended_root_altered", sep = "")
  
  my.tree_2 <- read.newick(tree_2)
  
  
  #Pathway to structured coalescent log file
  sc_log <- paste(Outbreaks[x], "Structured_coalescent/Outbreak_SC.log", sep = "")
  
  sc <- read.table(sc_log, header=TRUE)
  
  sc_j <- nrow(sc)
  sc_i <- round(((nrow(sc))/10), digits = 0)
  
  
  #Pathway to discrete phylogegraphic log file
  dp_log <- paste(Outbreaks[x], "/Discrete_phylogeographic/Outbreak_isolates_condensed_time.log.txt", sep = "")
  
  dp <- read.table(dp_log, header=TRUE)
  
  dp_j <- nrow(dp)
  dp_i <- round(((nrow(dp))/10), digits = 0)
  
  
  #Pathways to structured coalescent animal-to-human and human-to-animal rates within log file
  sc_AH_rate <- paste("sc$migModel.t.Outbreak_isolates_condensed_time.rateMatrix_Animal_Human[sc_i:sc_j]", sep = "")
  sc_HA_rate <- paste("sc$migModel.t.Outbreak_isolates_condensed_time.rateMatrix_Human_Animal[sc_i:sc_j]", sep = "")
  
  sc_Animal_Human_rate <- eval(parse(text=sc_AH_rate))
  sc_Human_Animal_rate <- eval(parse(text=sc_HA_rate))
  
  
  #Pathways to structured coalescent animal-to-human and human-to-animal counts in log file
  sc_AH_transmission <- paste("sc$Tree.t.Outbreak_isolates_condensed_time.count_Animal_to_Human[sc_i:sc_j]", sep = "")
  sc_HA_transmission <- paste("sc$Tree.t.Outbreak_isolates_condensed_time.count_Human_to_Animal[sc_i:sc_j]", sep = "")
  
  sc_Animal_Human_transmissions <- eval(parse(text=sc_AH_transmission))
  sc_Human_Animal_transmissions <- eval(parse(text=sc_HA_transmission))
  
  
  #Pathways to structured coalescent animal and human population sizes in log file
  sc_A_pop <- paste("sc$migModel.t.Outbreak_isolates_condensed_time.popSize_Animal[sc_i:sc_j]", sep = "")
  sc_H_pop <- paste("sc$migModel.t.Outbreak_isolates_condensed_time.popSize_Human[sc_i:sc_j]", sep = "")
  
  sc_Animal_pop <- eval(parse(text=sc_A_pop))
  sc_Human_pop <- eval(parse(text=sc_H_pop))
  
  
  #Pathways to structured coalescent animal and human lengths in log file
  sc_A_length <- paste("sc$Tree.t.Outbreak_isolates_condensed_time.length_Animal[sc_i:sc_j]", sep = "")
  sc_H_length <- paste("sc$Tree.t.Outbreak_isolates_condensed_time.length_Human[sc_i:sc_j]", sep = "")
  
  sc_Animal_length <- eval(parse(text=sc_A_length))
  sc_Human_length <- eval(parse(text=sc_H_length))
  
  
  #Pathways to discrete phylogoegraphic markov jump and reward counts in log file
  dp_Animal_Human <- dp$c_Animal_to_Human.1.[dp_i:dp_j]
  dp_Human_Animal <- dp$c_Human_to_Animal.1.[dp_i:dp_j]
  
  dp_Animal <- dp$c_reward_Animal.1.[dp_i:dp_j]
  dp_Human <- dp$c_reward_Human.1.[dp_i:dp_j]
  
  
  #Section of script that reads in sampled nexus file and calculate the amount of time spent in each host population and the number of transmissions between the host populations
  m_2 <- 1
  
  source.table_2 <-c(my.sources, c(1:source_number))
  
  g_2 <- c((1:m_2)+1000000)
  
  my.matrix <-matrix(c(1:((source_number*source_number)*m_2)), nrow=(source_number*source_number), ncol=m_2)
  
  p_2 <- length(my.tree_2$tip.label) #number of isolates
  n_2 <- (nrow(my.tree_2$edge)+1)
  my.list_2 <- as.character(c(1:n_2)+1000000)
  my.transitions_2 <- c(my.tree_2$tip.label, my.tree_2$node.label)
  my.first_2 <- as.character(my.tree_2$edge[,1]+1000000)
  my.second_2 <- as.character(my.tree_2$edge[,2]+1000000)
  for (i_2 in 1:n_2) {my.first_2 <- gsub(my.list_2[i_2], my.transitions_2[i_2], my.first_2)}
  for (i_2 in 1:n_2) {my.second_2 <- gsub(my.list_2[i_2], my.transitions_2[i_2], my.second_2)}
  my.edge.length_2 <- my.tree_2$edge.length
  
  source.length_2 <- matrix(c(rep(0, source_number)),nrow = source_number, ncol = 1)
  
  for (i_2 in 1:(n_2-1)) {
    s_2 <- match(my.second_2[i_2], my.sources)
    source.length_2[s_2,1] <- sum(source.length_2[s_2,1], my.edge.length_2[i_2])
  }  
  
  for (i_2 in 1:n_2) {my.first_2 <- gsub(my.list_2[i_2], my.transitions_2[i_2], my.first_2)}
  for (i_2 in 1:n_2) {my.second_2 <- gsub(my.list_2[i_2], my.transitions_2[i_2], my.second_2)}
  my.first_2 <- c(my.first_2, my.sources) 
  my.second_2 <- c(my.second_2, my.sources)
  q_2 <- as.matrix(table(data.frame(my.first_2, my.second_2)))
  
  for (t_2 in 1:source_number){
    q_2[t_2,t_2] <- q_2[t_2,t_2] - 1
  }
  
  my.node_2 <- matrix(c(1:(source_number*source_number)), nrow = source_number, ncol = source_number)
  
  for(r_2 in 1:source_number){
    for(u_2 in 1:source_number){
      my.matrix[((r_2-1)*source_number+u_2),1] <- ((q_2[r_2,u_2])/source.length_2[r_2])
    }
  }
  
  
  True_A <- (source.length_2[1]/(source.length_2[1]+source.length_2[2]))
  True_H <- (source.length_2[2]/(source.length_2[1]+source.length_2[2]))
  
  True_AH <- (q_2[1,2]/(q_2[1,2]+q_2[2,1]))
  True_HA <- (q_2[2,1]/(q_2[1,2]+q_2[2,1]))
  
  True_AH_rate <- ((q_2[1,2])/(source.length_2[1]))/(((q_2[1,2])/(source.length_2[1])) + ((q_2[2,1])/(source.length_2[2])))
  True_HA_rate <- ((q_2[2,1])/(source.length_2[2]))/(((q_2[1,2])/(source.length_2[1])) + ((q_2[2,1])/(source.length_2[2])))
  
  
  True_A_raw <- (source.length_2[1])
  True_H_raw <- (source.length_2[2])
  
  True_AH_raw <- (q_2[1,2])
  True_HA_raw <- (q_2[2,1])
  
  True_AH_rate_raw <- ((q_2[1,2])/(source.length_2[1]))
  True_HA_rate_raw <- ((q_2[2,1])/(source.length_2[2]))
  
  
  #Structured coalescent animal length statistics
  Outbreak_matrix[1,(x+1)] <-  HPDinterval(mcmc((sc_Animal_length)/(sc_Animal_length + sc_Human_length)), prob = 0.95)[1]
  Outbreak_matrix[2,(x+1)] <-  HPDinterval(mcmc((sc_Animal_length)/(sc_Animal_length + sc_Human_length)), prob = 0.95)[2]
  Outbreak_matrix[3,(x+1)] <- median((sc_Animal_length)/(sc_Animal_length + sc_Human_length))
  Outbreak_matrix[4,(x+1)] <- mean((sc_Animal_length)/(sc_Animal_length + sc_Human_length))
  
  ESS_matrix[1,(x+1)] <- effectiveSize(mcmc((sc_Animal_length)/(sc_Animal_length + sc_Human_length)))
  
  #Structured coalescent human length statistics
  Outbreak_matrix[5,(x+1)] <-  HPDinterval(mcmc((sc_Human_length)/(sc_Animal_length + sc_Human_length)), prob = 0.95)[1]
  Outbreak_matrix[6,(x+1)] <-  HPDinterval(mcmc((sc_Human_length)/(sc_Animal_length + sc_Human_length)), prob = 0.95)[2]
  Outbreak_matrix[7,(x+1)] <- median((sc_Human_length)/(sc_Animal_length + sc_Human_length))
  Outbreak_matrix[8,(x+1)] <- mean((sc_Human_length)/(sc_Animal_length + sc_Human_length))
  
  ESS_matrix[2,(x+1)] <- effectiveSize(mcmc((sc_Human_length)/(sc_Animal_length + sc_Human_length)))
  
  #Structured coalescent animal to Human rate statistics
  Outbreak_matrix[9,(x+1)] <- HPDinterval(mcmc(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop))/(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)) + ((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop)))), prob = 0.95)[1]
  Outbreak_matrix[10,(x+1)] <- HPDinterval(mcmc(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop))/(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)) + ((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop)))), prob = 0.95)[2]
  Outbreak_matrix[11,(x+1)] <- median(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop))/(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)) + ((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop))))
  Outbreak_matrix[12,(x+1)] <- mean(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop))/(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)) + ((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop))))
  
  ESS_matrix[3,(x+1)] <- effectiveSize(mcmc(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop))/(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)) + ((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop)))))
  
  #Structured coalescent human to animal rate statistics
  Outbreak_matrix[13,(x+1)] <- HPDinterval(mcmc(((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop))/(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)) + ((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop)))), prob = 0.95)[1]
  Outbreak_matrix[14,(x+1)] <- HPDinterval(mcmc(((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop))/(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)) + ((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop)))), prob = 0.95)[2]
  Outbreak_matrix[15,(x+1)] <- median(((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop))/(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)) + ((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop))))
  Outbreak_matrix[16,(x+1)] <- mean(((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop))/(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)) + ((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop))))
  
  ESS_matrix[4,(x+1)] <- effectiveSize(mcmc(((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop))/(((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)) + ((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop)))))
  
  
  #Discrete phylogeographic animal count statistics
  Outbreak_matrix[17,(x+1)] <- HPDinterval(mcmc((dp_Animal)/(dp_Animal + dp_Human)), prob = 0.95)[1]
  Outbreak_matrix[18,(x+1)] <- HPDinterval(mcmc((dp_Animal)/(dp_Animal + dp_Human)), prob = 0.95)[2]
  Outbreak_matrix[19,(x+1)] <- median((dp_Animal)/(dp_Animal + dp_Human))
  Outbreak_matrix[20,(x+1)] <- mean((dp_Animal)/(dp_Animal + dp_Human))
  
  ESS_matrix[5,(x+1)] <- effectiveSize(mcmc((dp_Animal)/(dp_Animal + dp_Human)))
  
  #Discrete phylogeographic human count statistics
  Outbreak_matrix[21,(x+1)] <- HPDinterval(mcmc((dp_Human)/(dp_Animal + dp_Human)), prob = 0.95)[1]
  Outbreak_matrix[22,(x+1)] <- HPDinterval(mcmc((dp_Human)/(dp_Animal + dp_Human)), prob = 0.95)[2]
  Outbreak_matrix[23,(x+1)] <- median((dp_Human)/(dp_Animal + dp_Human))
  Outbreak_matrix[24,(x+1)] <- mean((dp_Human)/(dp_Animal + dp_Human))
  
  ESS_matrix[6,(x+1)] <- effectiveSize(mcmc((dp_Human)/(dp_Animal + dp_Human)))
  
  #Discrete phylogeographic animal to human count statistics
  Outbreak_matrix[25,(x+1)] <- HPDinterval(mcmc((dp_Animal_Human)/(dp_Animal_Human + dp_Human_Animal)), prob = 0.95)[1]
  Outbreak_matrix[26,(x+1)] <- HPDinterval(mcmc((dp_Animal_Human)/(dp_Animal_Human + dp_Human_Animal)), prob = 0.95)[2]
  Outbreak_matrix[27,(x+1)] <- median((dp_Animal_Human)/(dp_Animal_Human + dp_Human_Animal))
  Outbreak_matrix[28,(x+1)] <- mean((dp_Animal_Human)/(dp_Animal_Human + dp_Human_Animal))
  
  ESS_matrix[7,(x+1)] <- effectiveSize(mcmc((dp_Animal_Human)/(dp_Animal_Human + dp_Human_Animal)))
  
  #Discrete phylogeographic human to animal count statistics
  Outbreak_matrix[29,(x+1)] <- HPDinterval(mcmc((dp_Human_Animal)/(dp_Animal_Human + dp_Human_Animal)), prob = 0.95)[1]
  Outbreak_matrix[30,(x+1)] <- HPDinterval(mcmc((dp_Human_Animal)/(dp_Animal_Human + dp_Human_Animal)), prob = 0.95)[2]
  Outbreak_matrix[31,(x+1)] <- median((dp_Human_Animal)/(dp_Animal_Human + dp_Human_Animal))
  Outbreak_matrix[32,(x+1)] <- mean((dp_Human_Animal)/(dp_Animal_Human + dp_Human_Animal))
  
  ESS_matrix[8,(x+1)] <- effectiveSize(mcmc((dp_Human_Animal)/(dp_Animal_Human + dp_Human_Animal)))
  
  
  #Structured coalescent animal to Human transmissions statistics
  Outbreak_matrix[33,(x+1)] <- HPDinterval(mcmc((sc_Animal_Human_transmissions)/(sc_Animal_Human_transmissions+sc_Human_Animal_transmissions)), prob = 0.95)[1]
  Outbreak_matrix[34,(x+1)] <- HPDinterval(mcmc((sc_Animal_Human_transmissions)/(sc_Animal_Human_transmissions+sc_Human_Animal_transmissions)), prob = 0.95)[2]
  Outbreak_matrix[35,(x+1)] <- median((sc_Animal_Human_transmissions)/(sc_Animal_Human_transmissions+sc_Human_Animal_transmissions))
  Outbreak_matrix[36,(x+1)] <- mean((sc_Animal_Human_transmissions)/(sc_Animal_Human_transmissions+sc_Human_Animal_transmissions))
  
  ESS_matrix[9,(x+1)] <- effectiveSize(mcmc((sc_Animal_Human_transmissions)/(sc_Animal_Human_transmissions+sc_Human_Animal_transmissions)))
  
  #Structured coalescent animal to Human transmissions statistics
  Outbreak_matrix[37,(x+1)] <- HPDinterval(mcmc((sc_Human_Animal_transmissions)/(sc_Animal_Human_transmissions+sc_Human_Animal_transmissions)), prob = 0.95)[1]
  Outbreak_matrix[38,(x+1)] <- HPDinterval(mcmc((sc_Human_Animal_transmissions)/(sc_Animal_Human_transmissions+sc_Human_Animal_transmissions)), prob = 0.95)[2]
  Outbreak_matrix[39,(x+1)] <- median((sc_Human_Animal_transmissions)/(sc_Animal_Human_transmissions+sc_Human_Animal_transmissions))
  Outbreak_matrix[40,(x+1)] <- mean((sc_Human_Animal_transmissions)/(sc_Animal_Human_transmissions+sc_Human_Animal_transmissions))
  
  ESS_matrix[10,(x+1)] <- effectiveSize(mcmc((sc_Human_Animal_transmissions)/(sc_Animal_Human_transmissions+sc_Human_Animal_transmissions)))
  
  #True paramaters
  Outbreak_matrix_true[1,(x+1)] <- True_A
  Outbreak_matrix_true[2,(x+1)] <- True_H
  Outbreak_matrix_true[3,(x+1)] <- True_AH
  Outbreak_matrix_true[4,(x+1)] <- True_HA
  Outbreak_matrix_true[5,(x+1)] <- True_AH_rate
  Outbreak_matrix_true[6,(x+1)] <- True_HA_rate
  
  
  #Structured coalescent animal length raw statistics
  Outbreak_matrix_raw[1,(x+1)] <-  HPDinterval(mcmc(sc_Animal_length), prob = 0.95)[1]
  Outbreak_matrix_raw[2,(x+1)] <-  HPDinterval(mcmc(sc_Animal_length), prob = 0.95)[2]
  Outbreak_matrix_raw[3,(x+1)] <- median(sc_Animal_length)
  Outbreak_matrix_raw[4,(x+1)] <- mean(sc_Animal_length)
  
  ESS_matrix_raw[1,(x+1)] <- effectiveSize(mcmc(sc_Animal_length))
  
  #Structured coalescent human length length raw statistics
  Outbreak_matrix_raw[5,(x+1)] <-  HPDinterval(mcmc(sc_Human_length), prob = 0.95)[1]
  Outbreak_matrix_raw[6,(x+1)] <-  HPDinterval(mcmc(sc_Human_length), prob = 0.95)[2]
  Outbreak_matrix_raw[7,(x+1)] <- median(sc_Human_length)
  Outbreak_matrix_raw[8,(x+1)] <- mean(sc_Human_length)
  
  ESS_matrix_raw[2,(x+1)] <- effectiveSize(mcmc(sc_Human_length))
  
  #Structured coalescent animal to Human rate length raw statistics
  Outbreak_matrix_raw[9,(x+1)] <- HPDinterval(mcmc((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)), prob = 0.95)[1]
  Outbreak_matrix_raw[10,(x+1)] <- HPDinterval(mcmc((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)), prob = 0.95)[2]
  Outbreak_matrix_raw[11,(x+1)] <- median((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop))
  Outbreak_matrix_raw[12,(x+1)] <- mean((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop))
  
  ESS_matrix_raw[3,(x+1)] <- effectiveSize(mcmc((sc_Human_Animal_rate)*(sc_Animal_pop)/(sc_Human_pop)))
  
  #Structured coalescent human to animal rate length raw statistics
  Outbreak_matrix_raw[13,(x+1)] <- HPDinterval(mcmc((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop)), prob = 0.95)[1]
  Outbreak_matrix_raw[14,(x+1)] <- HPDinterval(mcmc((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop)), prob = 0.95)[2]
  Outbreak_matrix_raw[15,(x+1)] <- median((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop))
  Outbreak_matrix_raw[16,(x+1)] <- mean((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop))
  
  ESS_matrix_raw[4,(x+1)] <- effectiveSize(mcmc((sc_Animal_Human_rate)*(sc_Human_pop)/(sc_Animal_pop)))
  
  
  #Discrete phylogeographic animal count length raw statistics
  Outbreak_matrix_raw[17,(x+1)] <- HPDinterval(mcmc(dp_Animal), prob = 0.95)[1]
  Outbreak_matrix_raw[18,(x+1)] <- HPDinterval(mcmc(dp_Animal), prob = 0.95)[2]
  Outbreak_matrix_raw[19,(x+1)] <- median(dp_Animal)
  Outbreak_matrix_raw[20,(x+1)] <- mean(dp_Animal)
  
  ESS_matrix_raw[5,(x+1)] <- effectiveSize(mcmc(dp_Animal))
  
  #Discrete phylogeographic human count length raw statistics
  Outbreak_matrix_raw[21,(x+1)] <- HPDinterval(mcmc(dp_Human), prob = 0.95)[1]
  Outbreak_matrix_raw[22,(x+1)] <- HPDinterval(mcmc(dp_Human), prob = 0.95)[2]
  Outbreak_matrix_raw[23,(x+1)] <- median(dp_Human)
  Outbreak_matrix_raw[24,(x+1)] <- mean(dp_Human)
  
  ESS_matrix_raw[6,(x+1)] <- effectiveSize(mcmc(dp_Human))  
  
  #Discrete phylogeographic animal to human count length raw statistics
  Outbreak_matrix_raw[25,(x+1)] <- HPDinterval(mcmc(dp_Animal_Human), prob = 0.95)[1]
  Outbreak_matrix_raw[26,(x+1)] <- HPDinterval(mcmc(dp_Animal_Human), prob = 0.95)[2]
  Outbreak_matrix_raw[27,(x+1)] <- median(dp_Animal_Human)
  Outbreak_matrix_raw[28,(x+1)] <- mean(dp_Animal_Human)
  
  ESS_matrix_raw[7,(x+1)] <- effectiveSize(mcmc(dp_Animal_Human))  
  
  #Discrete phylogeographic human to animal count length raw statistics
  Outbreak_matrix_raw[29,(x+1)] <- HPDinterval(mcmc(dp_Human_Animal), prob = 0.95)[1]
  Outbreak_matrix_raw[30,(x+1)] <- HPDinterval(mcmc(dp_Human_Animal), prob = 0.95)[2]
  Outbreak_matrix_raw[31,(x+1)] <- median(dp_Human_Animal)
  Outbreak_matrix_raw[32,(x+1)] <- mean(dp_Human_Animal)
  
  ESS_matrix_raw[8,(x+1)] <- effectiveSize(mcmc(dp_Human_Animal))  
  
  
  #Structured coalescent animal to Human transmissions length raw statistics
  Outbreak_matrix_raw[33,(x+1)] <- HPDinterval(mcmc(sc_Animal_Human_transmissions), prob = 0.95)[1]
  Outbreak_matrix_raw[34,(x+1)] <- HPDinterval(mcmc(sc_Animal_Human_transmissions), prob = 0.95)[2]
  Outbreak_matrix_raw[35,(x+1)] <- median(sc_Animal_Human_transmissions)
  Outbreak_matrix_raw[36,(x+1)] <- mean(sc_Animal_Human_transmissions)
  
  ESS_matrix_raw[9,(x+1)] <- effectiveSize(mcmc(sc_Animal_Human_transmissions)) 
  
  #Structured coalescent animal to Human transmissions length raw statistics
  Outbreak_matrix_raw[37,(x+1)] <- HPDinterval(mcmc(sc_Human_Animal_transmissions), prob = 0.95)[1]
  Outbreak_matrix_raw[38,(x+1)] <- HPDinterval(mcmc(sc_Human_Animal_transmissions), prob = 0.95)[2]
  Outbreak_matrix_raw[39,(x+1)] <- median(sc_Human_Animal_transmissions)
  Outbreak_matrix_raw[40,(x+1)] <- mean(sc_Human_Animal_transmissions)
  
  ESS_matrix_raw[10,(x+1)] <- effectiveSize(mcmc(sc_Human_Animal_transmissions)) 
  
  
  #True paramaters raw
  Outbreak_matrix_true_raw[1,(x+1)] <- True_A_raw
  Outbreak_matrix_true_raw[2,(x+1)] <- True_H_raw
  Outbreak_matrix_true_raw[3,(x+1)] <- True_AH_raw
  Outbreak_matrix_true_raw[4,(x+1)] <- True_HA_raw
  Outbreak_matrix_true_raw[5,(x+1)] <- True_AH_rate_raw
  Outbreak_matrix_true_raw[6,(x+1)] <- True_HA_rate_raw
  
}


#Structured coalescent Animal length plot
my_sc_plot_A_SC <- function(data){

  mean <- (sapply(Outbreak_matrix[4,2:(Outbreak_number+1)], as.numeric))
  HPD_lower <-(sapply(Outbreak_matrix[1,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[2,2:(Outbreak_number+1)], as.numeric))
  position <- (sapply(Outbreak_matrix_true[1,2:(Outbreak_number+1)], as.numeric))
  colour <- c(rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="True value", ylab="Estimate")
  title("A", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[4,2:(Outbreak_number+1)], as.numeric))
position <- (sapply(Outbreak_matrix_true[1,2:(Outbreak_number+1)], as.numeric))

SC_A_cor <- cor(mean,position)


#Discrete phylogeographic Animal Markov reward plot
my_sc_plot_A_DP <- function(data){
  
  mean <- (sapply(Outbreak_matrix[20, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[17,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[18,2:(Outbreak_number+1)], as.numeric))
  position <- (sapply(Outbreak_matrix_true[1,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="True value", ylab="Estimate")
  title("C", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[20, 2:(Outbreak_number+1)], as.numeric))
position <- (sapply(Outbreak_matrix_true[1,2:(Outbreak_number+1)], as.numeric)) 
DP_A_cor <- cor(mean,position)


#Structured coalescent Human length plot
my_sc_plot_H_SC <- function(data){
    
  mean <- (sapply(Outbreak_matrix[8,2:(Outbreak_number+1)], as.numeric))
  HPD_lower <-(sapply(Outbreak_matrix[5,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[6,2:(Outbreak_number+1)], as.numeric))
  position <- (sapply(Outbreak_matrix_true[2,2:(Outbreak_number+1)], as.numeric))
  colour <- c(rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="True value", ylab="Estimate")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=1, lty=2)
  }
}  

mean <- (sapply(Outbreak_matrix[8,2:(Outbreak_number+1)], as.numeric))
position <- (sapply(Outbreak_matrix_true[2,2:(Outbreak_number+1)], as.numeric))
SC_H_cor <- cor(mean,position)
  

#Discrete phylogeographic Human Markov reward plot
my_sc_plot_H_DP <- function(data){

  mean <- (sapply(Outbreak_matrix[24, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[21,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[22,2:(Outbreak_number+1)], as.numeric))
  position <- (sapply(Outbreak_matrix_true[2,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="True value", ylab="Estimate")
  title("D", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[24, 2:(Outbreak_number+1)], as.numeric))
position <- (sapply(Outbreak_matrix_true[2,2:(Outbreak_number+1)], as.numeric))
DP_H_cor <- cor(mean,position)



#Discrete phylogeographic Animal Human Markov jump plot
my_sc_plot_AH_DP <- function(data){
  
  mean <- (sapply(Outbreak_matrix[28, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[25,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[26,2:(Outbreak_number+1)], as.numeric))
  position <- (sapply(Outbreak_matrix_true[3,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="True value", ylab="Estimate")
  title("C", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[28, 2:(Outbreak_number+1)], as.numeric))
position <- (sapply(Outbreak_matrix_true[3,2:(Outbreak_number+1)], as.numeric))
DP_AH_cor <- cor(mean,position)


#Discrete phylogeographic Human Animal Markov jump plot
my_sc_plot_HA_DP <- function(data){
  
  mean <- (sapply(Outbreak_matrix[32, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[29,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[30,2:(Outbreak_number+1)], as.numeric))
  position <- (sapply(Outbreak_matrix_true[4,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="True value", ylab="Estimate")
  title("D", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[32, 2:(Outbreak_number+1)], as.numeric))
position <- (sapply(Outbreak_matrix_true[4,2:(Outbreak_number+1)], as.numeric))
DP_HA_cor <- cor(mean,position)
  

#Structured coalescent Animal Human transmissions plot
my_sc_plot_AH_SC <- function(data){
  
  mean <- (sapply(Outbreak_matrix[36, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[33,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[34,2:(Outbreak_number+1)], as.numeric))
  position <- (sapply(Outbreak_matrix_true[3,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="True value", ylab="Estimate")
  title("A", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=1, lty=2)
  }
}
mean <- (sapply(Outbreak_matrix[36, 2:(Outbreak_number+1)], as.numeric))
position <- (sapply(Outbreak_matrix_true[3,2:(Outbreak_number+1)], as.numeric))
SC_AH_cor <- cor(mean,position)
  

#Structured coalescent Human Animal transmissions plot
my_sc_plot_HA_SC <- function(data){
  
  mean <- (sapply(Outbreak_matrix[40, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[37,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[38,2:(Outbreak_number+1)], as.numeric))
  position <- (sapply(Outbreak_matrix_true[4,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="True value", ylab="Estimate")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[40, 2:(Outbreak_number+1)], as.numeric))
position <- (sapply(Outbreak_matrix_true[4,2:(Outbreak_number+1)], as.numeric))
SC_HA_cor <- cor(mean,position)
  

Cairo(file="Multiple_simulations_population_proportions.png", type="png", width=6000, height=6000, pointsize=12, dpi=600)
par(mfrow=c(2,2))
my_sc_plot_A_SC(data)
my_sc_plot_H_SC(data)
my_sc_plot_A_DP(data)
my_sc_plot_H_DP(data)
dev.off()


Cairo(file="Multiple_simulations_transmission_proportions.png", type="png", width=6000, height=6000, pointsize=12, dpi=600)
par(mfrow=c(2,2))
my_sc_plot_AH_SC(data)
my_sc_plot_HA_SC(data)
my_sc_plot_AH_DP(data)
my_sc_plot_HA_DP(data)
dev.off()


#Structured coalescent Animal length plot sample proportion
my_sc_plot_A_SC_sample_proportion <- function(data){
  
  mean <- (sapply(Outbreak_matrix[4,2:(Outbreak_number+1)], as.numeric))
  HPD_lower <-(sapply(Outbreak_matrix[1,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[2,2:(Outbreak_number+1)], as.numeric))
  position <- Animal_sample_proportion
  colour <- c(rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Animal sample proportion", ylab="Estimate")
  title("A", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[4,2:(Outbreak_number+1)], as.numeric))
position <- Animal_sample_proportion
SC_A_sample_cor <- cor(mean,position)

#Discrete phylogeographic Animal Markov reward plot sample proportion
my_sc_plot_A_DP_sample_proportion <- function(data){
  
  mean <- (sapply(Outbreak_matrix[20, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[17,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[18,2:(Outbreak_number+1)], as.numeric))
  position <- Animal_sample_proportion
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Animal sample proportion", ylab="Estimate")
  title("C", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[20, 2:(Outbreak_number+1)], as.numeric))
position <- Animal_sample_proportion
DP_A_sample_cor <- cor(mean,position)

#Structured coalescent Human length plot sample proportion
my_sc_plot_H_SC_sample_proportion <- function(data){
  
  mean <- (sapply(Outbreak_matrix[8,2:(Outbreak_number+1)], as.numeric))
  HPD_lower <-(sapply(Outbreak_matrix[5,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[6,2:(Outbreak_number+1)], as.numeric))
  position <- Human_sample_proportion
  colour <- c(rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Human sample proportion", ylab="Estimate")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=1, lty=2)
  }
}  

mean <- (sapply(Outbreak_matrix[8,2:(Outbreak_number+1)], as.numeric))
position <- Human_sample_proportion
SC_H_sample_cor <- cor(mean,position)

#Discrete phylogeographic Human Markov reward plot sample proportion
my_sc_plot_H_DP_sample_proportion <- function(data){
  
  mean <- (sapply(Outbreak_matrix[24, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[21,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[22,2:(Outbreak_number+1)], as.numeric))
  position <- Human_sample_proportion
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Human sample proportion", ylab="Estimate")
  title("D", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[24, 2:(Outbreak_number+1)], as.numeric))
position <- Human_sample_proportion
DP_H_sample_cor <- cor(mean,position)

#Structured coalescent Animal Human transmissions plot sample_proportion
my_sc_plot_AH_SC_sample_proportion <- function(data){
  
  mean <- (sapply(Outbreak_matrix[36, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[33,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[34,2:(Outbreak_number+1)], as.numeric))
  position <- Animal_sample_proportion
  colour <- (rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Animal sample proportion", ylab="Estimate")
  title("A", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[36, 2:(Outbreak_number+1)], as.numeric))
position <- Animal_sample_proportion
SC_AH_sample_cor <- cor(mean,position)

#Discrete phylogeographic Animal Human Markov jump plot sample_proportion
my_sc_plot_AH_DP_sample_proportion <- function(data){
  
  mean <- (sapply(Outbreak_matrix[28, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[25,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[26,2:(Outbreak_number+1)], as.numeric))
  position <- Animal_sample_proportion
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Animal sample proportion", ylab="Estimate")
  title("C", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[28, 2:(Outbreak_number+1)], as.numeric))
position <- Animal_sample_proportion
DP_AH_sample_cor <- cor(mean,position)

#Structured coalescent Human Animal transmissions plot sample_proportion
my_sc_plot_HA_SC_sample_proportion <- function(data){
  
  mean <- (sapply(Outbreak_matrix[40, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[37,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[38,2:(Outbreak_number+1)], as.numeric))
  position <- Human_sample_proportion
  colour <- (rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Human sample proportion", ylab="Estimate")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[40, 2:(Outbreak_number+1)], as.numeric))
position <- Human_sample_proportion
SC_HA_sample_cor <- cor(mean,position)

#Discrete phylogeographic Human Animal Markov jump plot sample proportion
my_sc_plot_HA_DP_sample_proportion <- function(data){
  
  mean <- (sapply(Outbreak_matrix[32, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[29,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[30,2:(Outbreak_number+1)], as.numeric))
  position <- Human_sample_proportion
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Human sample proportion", ylab="Estimate")
  title("D", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=1, lty=2)
  }
}

mean <- (sapply(Outbreak_matrix[32, 2:(Outbreak_number+1)], as.numeric))
position <- Human_sample_proportion
DP_HA_sample_cor <- cor(mean,position)

Cairo(file="Multiple_simulations_population_proportions_sample_proportion.png", type="png", width=6000, height=6000, pointsize=12, dpi=600)
par(mfrow=c(2,2))
my_sc_plot_A_SC_sample_proportion(data)
my_sc_plot_H_SC_sample_proportion(data)
my_sc_plot_A_DP_sample_proportion(data)
my_sc_plot_H_DP_sample_proportion(data)
dev.off()


Cairo(file="Multiple_simulations_transmission_proportions_sample_proportion.png", type="png", width=6000, height=6000, pointsize=12, dpi=600)
par(mfrow=c(2,2))
my_sc_plot_AH_SC_sample_proportion(data)
my_sc_plot_HA_SC_sample_proportion(data)
my_sc_plot_AH_DP_sample_proportion(data)
my_sc_plot_HA_DP_sample_proportion(data)
dev.off()


#Structured coalescent Animal length plot difference
my_sc_plot_A_SC_difference <- function(data){
  
  position <- (sapply(Outbreak_matrix_true[1,2:(Outbreak_number+1)], as.numeric))
  mean <- (sapply(Outbreak_matrix[4,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <-(sapply(Outbreak_matrix[1,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[2,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- c(rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="True value", ylab="Difference between predicted and true values")
  title("A", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=0, lty=2)
  }
}

#Discrete phylogeographic Animal Markov reward plot difference
my_sc_plot_A_DP_difference <- function(data){
  
  position <- (sapply(Outbreak_matrix_true[1,2:(Outbreak_number+1)], as.numeric))  
  mean <- (sapply(Outbreak_matrix[20, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[17,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[18,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="True value", ylab="Difference between predicted and true values")
  title("C", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=0, lty=2)
  }
}

#Structured coalescent Human length plot difference
my_sc_plot_H_SC_difference <- function(data){
  
  position <- (sapply(Outbreak_matrix_true[2,2:(Outbreak_number+1)], as.numeric))
  mean <- (sapply(Outbreak_matrix[8,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <-(sapply(Outbreak_matrix[5,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[6,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- c(rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="True value", ylab="Difference between predicted and true values")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=0, lty=2)
  }
}  

#Discrete phylogeographic Human Markov reward plot difference
my_sc_plot_H_DP_difference <- function(data){
  
  position <- (sapply(Outbreak_matrix_true[2,2:(Outbreak_number+1)], as.numeric))
  mean <- (sapply(Outbreak_matrix[24, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[21,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[22,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="True value", ylab="Difference between predicted and true values")
  title("D", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=0, lty=2)
  }
}

#Structured coalescent Animal Human transmissions plot difference
my_sc_plot_AH_SC_difference <- function(data){
  
  position <- (sapply(Outbreak_matrix_true[3,2:(Outbreak_number+1)], as.numeric))
  mean <- (sapply(Outbreak_matrix[36, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[33,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[34,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="True value", ylab="Difference between predicted and true values")
  title("A", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=0, lty=2)
  }
}

#Discrete phylogeographic Animal Human Markov jump plot difference
my_sc_plot_AH_DP_difference <- function(data){
  
  position <- (sapply(Outbreak_matrix_true[3,2:(Outbreak_number+1)], as.numeric))
  mean <- (sapply(Outbreak_matrix[28, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[25,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[26,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="True value", ylab="Difference between predicted and true values")
  title("C", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=0, lty=2)
  }
}

#Structured coalescent Human Animal transmissions plot difference
my_sc_plot_HA_SC_difference <- function(data){
  
  position <- (sapply(Outbreak_matrix_true[4,2:(Outbreak_number+1)], as.numeric))
  mean <- (sapply(Outbreak_matrix[40, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[37,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[38,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="True value", ylab="Difference between predicted and true values")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=0, lty=2)
  }
}

#Discrete phylogeographic Human Animal Markov jump plot difference
my_sc_plot_HA_DP_difference <- function(data){
  
  position <- (sapply(Outbreak_matrix_true[4,2:(Outbreak_number+1)], as.numeric))
  mean <- (sapply(Outbreak_matrix[32, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[29,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[30,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="True value", ylab="Difference between predicted and true values")
  title("D", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=0, lty=2)
  }
}


Cairo(file="Multiple_simulations_population_proportions_difference.png", type="png", width=6000, height=6000, pointsize=12, dpi=600)
par(mfrow=c(2,2))
my_sc_plot_A_SC_difference(data)
my_sc_plot_H_SC_difference(data)
my_sc_plot_A_DP_difference(data)
my_sc_plot_H_DP_difference(data)
dev.off()


Cairo(file="Multiple_simulations_transmission_proportions_difference.png", type="png", width=6000, height=6000, pointsize=12, dpi=600)
par(mfrow=c(2,2))
my_sc_plot_AH_SC_difference(data)
my_sc_plot_HA_SC_difference(data)
my_sc_plot_AH_DP_difference(data)
my_sc_plot_HA_DP_difference(data)
dev.off()



#Structured coalescent Animal length plot sample proportion difference
my_sc_plot_A_SC_sample_proportion_difference <- function(data){
  
  position <- Animal_sample_proportion
  mean <- (sapply(Outbreak_matrix[4,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <-(sapply(Outbreak_matrix[1,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[2,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- c(rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="Animal sample proportion", ylab="Difference between predicted values \nand animal sample proportion")
  title("A", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=0, lty=2)
  }
}

#Discrete phylogeographic Animal Markov reward plot sample proportion difference
my_sc_plot_A_DP_sample_proportion_difference <- function(data){
  
  position <- Animal_sample_proportion
  mean <- (sapply(Outbreak_matrix[20, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[17,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[18,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="Animal sample proportion", ylab="Difference between predicted values \nand animal sample proportion")
  title("C", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=0, lty=2)
  }
}

#Structured coalescent Human length plot sample proportion difference
my_sc_plot_H_SC_sample_proportion_difference <- function(data){
  
  position <- Human_sample_proportion
  mean <- (sapply(Outbreak_matrix[8,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <-(sapply(Outbreak_matrix[5,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[6,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- c(rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="Human sample proportion", ylab="Difference between predicted values \nand human sample proportion")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=0, lty=2)
  }
}  

#Discrete phylogeographic Human Markov reward plot sample proportion difference
my_sc_plot_H_DP_sample_proportion_difference <- function(data){
  
  position <- Human_sample_proportion
  mean <- (sapply(Outbreak_matrix[24, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[21,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[22,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="Human sample proportion", ylab="Difference between predicted values \nand human sample proportion")
  title("D", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=0, lty=2)
  }
}

#Structured coalescent Animal Human transmissions plot sample_proportion difference
my_sc_plot_AH_SC_sample_proportion_difference <- function(data){
  
  position <- Animal_sample_proportion
  mean <- (sapply(Outbreak_matrix[36, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[33,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[34,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="Animal sample proportion", ylab="Difference between predicted values \nand animal sample proportion")
  title("A", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=0, lty=2)
  }
}

#Discrete phylogeographic Animal Human Markov jump plot sample_proportion difference
my_sc_plot_AH_DP_sample_proportion_difference <- function(data){
  
  position <- Animal_sample_proportion
  mean <- (sapply(Outbreak_matrix[28, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[25,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[26,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="Animal sample proportion", ylab="Difference between predicted values \nand animal sample proportion")
  title("C", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=0, lty=2)
  }
}

#Structured coalescent Human Animal transmissions plot sample_proportion difference
my_sc_plot_HA_SC_sample_proportion_difference <- function(data){
  
  position <- Human_sample_proportion
  mean <- (sapply(Outbreak_matrix[40, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[37,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[38,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#377eb8", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="Human sample proportion", ylab="Difference between predicted values \nand human sample proportion")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#377eb8")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    abline(a=0, b=0, lty=2)
  }
}

#Discrete phylogeographic Human Animal Markov jump plot sample proportion difference
my_sc_plot_HA_DP_sample_proportion_difference <- function(data){
  
  position <- Human_sample_proportion
  mean <- (sapply(Outbreak_matrix[32, 2:(Outbreak_number+1)], as.numeric)) - position
  HPD_lower <- (sapply(Outbreak_matrix[29,2:(Outbreak_number+1)], as.numeric)) - position
  HPD_upper <- (sapply(Outbreak_matrix[30,2:(Outbreak_number+1)], as.numeric)) - position
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(0, 1), c(-1, 1), type="n", xlab="Human sample proportion", ylab="Difference between predicted values \nand human sample proportion")
  title("D", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], mean[i], col="#e41a1c")
    lines(c(position[i],position[i]), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    abline(a=0, b=0, lty=2)
  }
}

Cairo(file="Multiple_simulations_population_proportions_sample_proportion_difference.png", type="png", width=6000, height=6000, pointsize=12, dpi=600)
par(mar=c(4.1, 6.1, 4.1, 1.1))
par(mfrow=c(2,2))
my_sc_plot_A_SC_sample_proportion_difference(data)
my_sc_plot_H_SC_sample_proportion_difference(data)
my_sc_plot_A_DP_sample_proportion_difference(data)
my_sc_plot_H_DP_sample_proportion_difference(data)
dev.off()


Cairo(file="Multiple_simulations_transmission_proportions_sample_proportion_difference.png", type="png", width=6000, height=6000, pointsize=12, dpi=600)
par(mar=c(4.1, 6.1, 4.1, 1.1))
par(mfrow=c(2,2))
my_sc_plot_AH_SC_sample_proportion_difference(data)
my_sc_plot_HA_SC_sample_proportion_difference(data)
my_sc_plot_AH_DP_sample_proportion_difference(data)
my_sc_plot_HA_DP_sample_proportion_difference(data)
dev.off()


#parameter vs sample propotion
#Animal true vs proportion difference
my_sc_plot_A_true_sample_proportion <- function(data){
  
  position <- Animal_sample_proportion
  true_value <- (sapply(Outbreak_matrix_true[1,2:(Outbreak_number+1)], as.numeric))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Animal sample proportion", ylab="True value")
  title("A", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], true_value[i])
  }
}

position <- Animal_sample_proportion
true_value <- (sapply(Outbreak_matrix_true[1,2:(Outbreak_number+1)], as.numeric))
A_sample_cor <- cor(position,true_value)

#Human true vs proportion difference
my_sc_plot_H_true_sample_proportion <- function(data){
  
  position <- Human_sample_proportion
  true_value <- (sapply(Outbreak_matrix_true[2,2:(Outbreak_number+1)], as.numeric))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Human sample proportion", ylab="True value")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], true_value[i])
  }
}

position <- Human_sample_proportion
true_value <- (sapply(Outbreak_matrix_true[2,2:(Outbreak_number+1)], as.numeric))
H_sample_cor <- cor(position,true_value)

#Animal Human true vs proportion difference
my_sc_plot_AH_true_sample_proportion <- function(data){
  
  position <- Animal_sample_proportion
  true_value <- (sapply(Outbreak_matrix_true[3,2:(Outbreak_number+1)], as.numeric))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Animal sample proportion", ylab="True value")
  title("C", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], true_value[i])
  }
}

position <- Animal_sample_proportion
true_value <- (sapply(Outbreak_matrix_true[3,2:(Outbreak_number+1)], as.numeric))
AH_sample_cor <- cor(position,true_value)

#Human Animal true vs proportion difference
my_sc_plot_HA_true_sample_proportion <- function(data){
  
  position <- Human_sample_proportion
  true_value <- (sapply(Outbreak_matrix_true[4,2:(Outbreak_number+1)], as.numeric))
  
  plot(c(0, 1), c(0, 1), type="n", xlab="Human sample proportion", ylab="True value")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(position[i], true_value[i])
  }
}

position <- Human_sample_proportion
true_value <- (sapply(Outbreak_matrix_true[4,2:(Outbreak_number+1)], as.numeric))
HA_sample_cor <- cor(position,true_value)

Cairo(file="Multiple_simulations_true_sample_proportion.png", type="png", width=6000, height=6000, pointsize=12, dpi=600)
par(mar=c(4.1, 6.1, 4.1, 1.1))
par(mfrow=c(2,2))
my_sc_plot_A_true_sample_proportion(data)
my_sc_plot_H_true_sample_proportion(data)
my_sc_plot_AH_true_sample_proportion(data)
my_sc_plot_HA_true_sample_proportion(data)
dev.off()


#Statistics
#Structured coalescent Animal length 
true_value <- (sapply(Outbreak_matrix_true[1,2:(Outbreak_number+1)], as.numeric))
mean_value <- (sapply(Outbreak_matrix[4,2:(Outbreak_number+1)], as.numeric))
HPD_lower <-(sapply(Outbreak_matrix[1,2:(Outbreak_number+1)], as.numeric))
HPD_upper <- (sapply(Outbreak_matrix[2,2:(Outbreak_number+1)], as.numeric)) 


mean_difference <- abs(true_value - mean_value)
HPD_range <- abs(HPD_upper - HPD_lower)
HPD_contained <- matrix(c(1:Outbreak_number), nrow = 1, ncol = Outbreak_number)
for(cont in 1:Outbreak_number){
  if(true_value[cont] < HPD_upper[cont] & true_value[cont] > HPD_lower[cont]) {
    HPD_contained[1, cont] <- 1
  } else {
    HPD_contained[1, cont] <- 0
  }
}
square_error <- ((true_value - mean_value)^2)

SC_A_MSE <- mean(square_error)
SC_A_mean_diff <- mean(mean_difference)
SC_A_HPD_range <- mean(HPD_range)
SC_A_HPD_contained <- mean(HPD_contained)

#Discrete_phylogeographic Animal length 
true_value <- (sapply(Outbreak_matrix_true[1,2:(Outbreak_number+1)], as.numeric))
mean_value <- (sapply(Outbreak_matrix[20,2:(Outbreak_number+1)], as.numeric))
HPD_lower <-(sapply(Outbreak_matrix[17,2:(Outbreak_number+1)], as.numeric))
HPD_upper <- (sapply(Outbreak_matrix[18,2:(Outbreak_number+1)], as.numeric)) 

mean_difference <- abs(true_value - mean_value)
HPD_range <- abs(HPD_upper - HPD_lower)
HPD_contained <- matrix(c(1:Outbreak_number), nrow = 1, ncol = Outbreak_number)
for(cont in 1:Outbreak_number){
  if(true_value[cont] < HPD_upper[cont] & true_value[cont] > HPD_lower[cont]) {
    HPD_contained[1, cont] <- 1
  } else {
    HPD_contained[1, cont] <- 0
  }
}
square_error <- ((true_value - mean_value)^2)

DP_A_MSE <- mean(square_error)
DP_A_mean_diff <- mean(mean_difference)
DP_A_HPD_range <- mean(HPD_range)
DP_A_HPD_contained <- mean(HPD_contained)

#Structured coalescent Human length 
true_value <- (sapply(Outbreak_matrix_true[2,2:(Outbreak_number+1)], as.numeric))
mean_value <- (sapply(Outbreak_matrix[8,2:(Outbreak_number+1)], as.numeric))
HPD_lower <-(sapply(Outbreak_matrix[5,2:(Outbreak_number+1)], as.numeric))
HPD_upper <- (sapply(Outbreak_matrix[6,2:(Outbreak_number+1)], as.numeric)) 

mean_difference <- abs(true_value - mean_value)
HPD_range <- abs(HPD_upper - HPD_lower)
HPD_contained <- matrix(c(1:Outbreak_number), nrow = 1, ncol = Outbreak_number)
for(cont in 1:Outbreak_number){
  if(true_value[cont] < HPD_upper[cont] & true_value[cont] > HPD_lower[cont]) {
    HPD_contained[1, cont] <- 1
  } else {
    HPD_contained[1, cont] <- 0
  }
}
square_error <- ((true_value - mean_value)^2)

SC_H_MSE <- mean(square_error)
SC_H_mean_diff <- mean(mean_difference)
SC_H_HPD_range <- mean(HPD_range)
SC_H_HPD_contained <- mean(HPD_contained)

#Discrete_phylogeographic Human length 
true_value <- (sapply(Outbreak_matrix_true[2,2:(Outbreak_number+1)], as.numeric))
mean_value <- (sapply(Outbreak_matrix[24,2:(Outbreak_number+1)], as.numeric))
HPD_lower <-(sapply(Outbreak_matrix[21,2:(Outbreak_number+1)], as.numeric))
HPD_upper <- (sapply(Outbreak_matrix[22,2:(Outbreak_number+1)], as.numeric)) 

mean_difference <- abs(true_value - mean_value)
HPD_range <- abs(HPD_upper - HPD_lower)
HPD_contained <- matrix(c(1:Outbreak_number), nrow = 1, ncol = Outbreak_number)
for(cont in 1:Outbreak_number){
  if(true_value[cont] < HPD_upper[cont] & true_value[cont] > HPD_lower[cont]) {
    HPD_contained[1, cont] <- 1
  } else {
    HPD_contained[1, cont] <- 0
  }
}
square_error <- ((true_value - mean_value)^2)

DP_H_MSE <- mean(square_error)
DP_H_mean_diff <- mean(mean_difference)
DP_H_HPD_range <- mean(HPD_range)
DP_H_HPD_contained <- mean(HPD_contained)


#Structured coalescent Animal Human transmission 
true_value <- (sapply(Outbreak_matrix_true[3,2:(Outbreak_number+1)], as.numeric))
mean_value <- (sapply(Outbreak_matrix[36,2:(Outbreak_number+1)], as.numeric))
HPD_lower <-(sapply(Outbreak_matrix[33,2:(Outbreak_number+1)], as.numeric))
HPD_upper <- (sapply(Outbreak_matrix[34,2:(Outbreak_number+1)], as.numeric)) 

mean_difference <- abs(true_value - mean_value)
HPD_range <- abs(HPD_upper - HPD_lower)
HPD_contained <- matrix(c(1:Outbreak_number), nrow = 1, ncol = Outbreak_number)
for(cont in 1:Outbreak_number){
  if(true_value[cont] < HPD_upper[cont] & true_value[cont] > HPD_lower[cont]) {
    HPD_contained[1, cont] <- 1
  } else {
    HPD_contained[1, cont] <- 0
  }
}
square_error <- ((true_value - mean_value)^2)

SC_AH_MSE <- mean(square_error)
SC_AH_mean_diff <- mean(mean_difference)
SC_AH_HPD_range <- mean(HPD_range)
SC_AH_HPD_contained <- mean(HPD_contained)

#Discrete_phylogeographic Animal Human transmission
true_value <- (sapply(Outbreak_matrix_true[3,2:(Outbreak_number+1)], as.numeric))
mean_value <- (sapply(Outbreak_matrix[28,2:(Outbreak_number+1)], as.numeric))
HPD_lower <-(sapply(Outbreak_matrix[25,2:(Outbreak_number+1)], as.numeric))
HPD_upper <- (sapply(Outbreak_matrix[26,2:(Outbreak_number+1)], as.numeric)) 

mean_difference <- abs(true_value - mean_value)
HPD_range <- abs(HPD_upper - HPD_lower)
HPD_contained <- matrix(c(1:Outbreak_number), nrow = 1, ncol = Outbreak_number)
for(cont in 1:Outbreak_number){
  if(true_value[cont] < HPD_upper[cont] & true_value[cont] > HPD_lower[cont]) {
    HPD_contained[1, cont] <- 1
  } else {
    HPD_contained[1, cont] <- 0
  }
}
square_error <- ((true_value - mean_value)^2)

DP_AH_MSE <- mean(square_error)
DP_AH_mean_diff <- mean(mean_difference)
DP_AH_HPD_range <- mean(HPD_range)
DP_AH_HPD_contained <- mean(HPD_contained)

#Structured coalescent Human Animal transmission 
true_value <- (sapply(Outbreak_matrix_true[4,2:(Outbreak_number+1)], as.numeric))
mean_value <- (sapply(Outbreak_matrix[40,2:(Outbreak_number+1)], as.numeric))
HPD_lower <-(sapply(Outbreak_matrix[37,2:(Outbreak_number+1)], as.numeric))
HPD_upper <- (sapply(Outbreak_matrix[38,2:(Outbreak_number+1)], as.numeric)) 

mean_difference <- abs(true_value - mean_value)
HPD_range <- abs(HPD_upper - HPD_lower)
HPD_contained <- matrix(c(1:Outbreak_number), nrow = 1, ncol = Outbreak_number)
for(cont in 1:Outbreak_number){
  if(true_value[cont] < HPD_upper[cont] & true_value[cont] > HPD_lower[cont]) {
    HPD_contained[1, cont] <- 1
  } else {
    HPD_contained[1, cont] <- 0
  }
}
square_error <- ((true_value - mean_value)^2)

SC_HA_MSE <- mean(square_error)
SC_HA_mean_diff <- mean(mean_difference)
SC_HA_HPD_range <- mean(HPD_range)
SC_HA_HPD_contained <- mean(HPD_contained)

#Discrete_phylogeographic Human Animal transmission
true_value <- (sapply(Outbreak_matrix_true[4,2:(Outbreak_number+1)], as.numeric))
mean_value <- (sapply(Outbreak_matrix[32,2:(Outbreak_number+1)], as.numeric))
HPD_lower <-(sapply(Outbreak_matrix[29,2:(Outbreak_number+1)], as.numeric))
HPD_upper <- (sapply(Outbreak_matrix[30,2:(Outbreak_number+1)], as.numeric)) 

mean_difference <- abs(true_value - mean_value)
HPD_range <- abs(HPD_upper - HPD_lower)
HPD_contained <- matrix(c(1:Outbreak_number), nrow = 1, ncol = Outbreak_number)
for(cont in 1:Outbreak_number){
  if(true_value[cont] < HPD_upper[cont] & true_value[cont] > HPD_lower[cont]) {
    HPD_contained[1, cont] <- 1
  } else {
    HPD_contained[1, cont] <- 0
  }
}
square_error <- ((true_value - mean_value)^2)

DP_HA_MSE <- mean(square_error)
DP_HA_mean_diff <- mean(mean_difference)
DP_HA_HPD_range <- mean(HPD_range)
DP_HA_HPD_contained <- mean(HPD_contained)


#Summary statistics
Summary_matrix <- matrix(c(1:45), nrow = 5, ncol = 9)

Summary_matrix[1,1] <-SC_A_HPD_contained
Summary_matrix[2,1] <-SC_A_mean_diff
Summary_matrix[3,1] <-SC_A_HPD_range
Summary_matrix[4,1] <-SC_A_MSE

Summary_matrix[1,2] <-DP_A_HPD_contained
Summary_matrix[2,2] <-DP_A_mean_diff
Summary_matrix[3,2] <-DP_A_HPD_range
Summary_matrix[4,2] <-DP_A_MSE

Summary_matrix[1,3] <-SC_H_HPD_contained
Summary_matrix[2,3] <-SC_H_mean_diff
Summary_matrix[3,3] <-SC_H_HPD_range
Summary_matrix[4,3] <-SC_H_MSE

Summary_matrix[1,4] <-DP_H_HPD_contained
Summary_matrix[2,4] <-DP_H_mean_diff
Summary_matrix[3,4] <-DP_H_HPD_range
Summary_matrix[4,4] <-DP_H_MSE

Summary_matrix[1,5] <-SC_AH_HPD_contained
Summary_matrix[2,5] <-SC_AH_mean_diff
Summary_matrix[3,5] <-SC_AH_HPD_range
Summary_matrix[4,5] <-SC_AH_MSE

Summary_matrix[1,6] <-DP_AH_HPD_contained
Summary_matrix[2,6] <-DP_AH_mean_diff
Summary_matrix[3,6] <-DP_AH_HPD_range
Summary_matrix[4,6] <-DP_AH_MSE

Summary_matrix[1,7] <-SC_HA_HPD_contained
Summary_matrix[2,7] <-SC_HA_mean_diff
Summary_matrix[3,7] <-SC_HA_HPD_range
Summary_matrix[4,7] <-SC_HA_MSE

Summary_matrix[1,8] <-DP_HA_HPD_contained
Summary_matrix[2,8] <-DP_HA_mean_diff
Summary_matrix[3,8] <-DP_HA_HPD_range
Summary_matrix[4,8] <-DP_HA_MSE

Summary_matrix[1,9] <-"Proportion of parameter values within the model's 95% HPD"
Summary_matrix[2,9] <-"Mean absolute difference between a model's estimates and the known parameter"
Summary_matrix[3,9] <-"Mean 95% HPD interval size"
Summary_matrix[4,9] <-"Mean square error between a model's estimates and the known parameter"
Summary_matrix[5,9] <-""

Summary_matrix[1,9] <-"Structured coalescent animal population estimate"
Summary_matrix[2,9] <-"Discrete trait analysis animal population estimate"
Summary_matrix[3,9] <-"Structured coalescent human population estimate"
Summary_matrix[4,9] <-"Discrete trait analysis human population estimate"

Summary_matrix[5,9] <-"Structured coalescent animal-to-human transmission estimate"
Summary_matrix[6,9] <-"Discrete trait analysis animal-to-human transmission estimate"
Summary_matrix[7,9] <-"Structured coalescent human-to-animal transmission estimate"
Summary_matrix[8,9] <-"Discrete trait analysis human-to-animal transmission estimate"

write.table(as.table(Summary_matrix), file="Simulations_summary.txt")


#Correlation matrixes

Correlation_matrix <- matrix(c(1:16), nrow = 8, ncol = 2)

Correlation_matrix[1,1] <- SC_A_cor
Correlation_matrix[1,2] <- DP_A_cor
Correlation_matrix[1,3] <- SC_H_cor
Correlation_matrix[1,4] <- DP_H_cor

Correlation_matrix[1,5] <- SC_AH_cor
Correlation_matrix[1,6] <- DP_AH_cor
Correlation_matrix[1,7] <- SC_HA_cor
Correlation_matrix[1,8] <- DP_HA_cor

Correlation_matrix[2,1] <- "Correlation coefficient between structured coalescent estimates and known animal population parameters"
Correlation_matrix[2,2] <- "Correlation coefficient between discrete trait analysis estimates and known animal population parameters"
Correlation_matrix[2,3] <- "Correlation coefficient between structured coalescent estimates and known human population parameters"
Correlation_matrix[2,4] <- "Correlation coefficient between discrete trait analysis estimates and known human population parameters"

Correlation_matrix[2,5] <- "Correlation coefficient between structured coalescent estimates and known human-to-animal transmission parameters"
Correlation_matrix[2,6] <- "Correlation coefficient between discrete trait analysis estimates and known human-to-animal transmission parameters"
Correlation_matrix[2,7] <- "Correlation coefficient between structured coalescent estimates and known human-to-animal transmission parameters"
Correlation_matrix[2,8] <- "Correlation coefficient between discrete trait analysis estimates and known human-to-animal transmission parameters"


Correlation_matrix_2 <- matrix(c(1:16), nrow = 8, ncol = 2)

Correlation_matrix_2[1,1] <- SC_A_sample_cor
Correlation_matrix_2[1,2] <- DP_A_sample_cor
Correlation_matrix_2[1,3] <- SC_H_sample_cor
Correlation_matrix_2[1,4] <- DP_H_sample_cor

Correlation_matrix_2[1,5] <- SC_AH_sample_cor
Correlation_matrix_2[1,6] <- DP_AH_sample_cor
Correlation_matrix_2[1,7] <- SC_HA_sample_cor
Correlation_matrix_2[1,8] <- DP_HA_sample_cor

Correlation_matrix_2[2,1] <- "Correlation coefficient between structured coalescent animal population estimates and the proportion of samples from the animal population"
Correlation_matrix_2[2,2] <- "Correlation coefficient between discrete trait analysis animal population estimates and the proportion of samples from the animal population"
Correlation_matrix_2[2,3] <- "Correlation coefficient between structured coalescent human population estimates and the proportion of samples from the human population"
Correlation_matrix_2[2,4] <- "Correlation coefficient between discrete trait analysis human population estimates and the proportion of samples from the human population"

Correlation_matrix_2[2,5] <- "Correlation coefficient between structured coalescent animal-to-human transmission estimates and the proportion of samples from the animal population"
Correlation_matrix_2[2,6] <- "Correlation coefficient between discrete trait analysis animal-to-human transmission estimates and the proportion of samples from the animal population"
Correlation_matrix_2[2,7] <- "Correlation coefficient between structured coalescent human-to-animal transmission estimates and the proportion of samples from the human population"
Correlation_matrix_2[2,8] <- "Correlation coefficient between discrete trait analysis human-to-animal transmission and the proportion of samples from the human population"



Correlation_matrix_3 <- matrix(c(1:8), nrow = 4, ncol = 2)

Correlation_matrix_3[1,1] <- A_sample_cor
Correlation_matrix_3[1,2] <- H_sample_cor
Correlation_matrix_3[1,3] <- AH_sample_cor
Correlation_matrix_3[1,4] <- HA_sample_cor

Correlation_matrix_3[2,1] <- "Correlation coefficient between the known animal population parameters and the proportion of animal samples"
Correlation_matrix_3[2,2] <- "Correlation coefficient between the known human population parameters and the proportion of human samples"
Correlation_matrix_3[2,3] <- "Correlation coefficient between the known animal-to-human transmission parameters and the proportion of animal samples"
Correlation_matrix_3[2,4] <- "Correlation coefficient between the known human-to-animal transmission parameters and the proportion of human samples"


write.table(as.table(Correlation_matrix), file="Model_parameter_correlations.txt")
write.table(as.table(Correlation_matrix_2), file="Model_sample_correlations.txt")
write.table(as.table(Correlation_matrix_3), file="Paramer_sample_correlations.txt")


#Structured coalescent Animal length plot sample proportion difference
my_sc_plot_A_SC_population_size <- function(data){
  
  position <- Population
  mean <- (sapply(Outbreak_matrix[4,2:(Outbreak_number+1)], as.numeric))
  HPD_lower <-(sapply(Outbreak_matrix[1,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[2,2:(Outbreak_number+1)], as.numeric))
  colour <- c(rep("#377eb8", Outbreak_number))
  
  plot(c(log10(min(Population)), log10(max(Population))), c(0, 1), type="n", xlab="Number of animals and humans infected (log scale)", ylab="Estimate")
  title("A", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(log10(position[i]), mean[i], col="#377eb8")
    lines(c(log10(position[i]),log10(position[i])), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    
  }
}

#Discrete phylogeographic Animal Markov reward plot sample proportion difference
my_sc_plot_A_DP_population_size <- function(data){
  
  position <- Population
  mean <- (sapply(Outbreak_matrix[20, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[17,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[18,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(log10(min(Population)), log10(max(Population))), c(0, 1), type="n", xlab="Number of animals and humans infected (log scale)", ylab="Estimate")
  title("C", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(log10(position[i]), mean[i], col="#e41a1c")
    lines(c(log10(position[i]),log10(position[i])), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    
  }
}

#Structured coalescent Human length plot sample proportion difference
my_sc_plot_H_SC_population_size <- function(data){
  
  position <- Population
  mean <- (sapply(Outbreak_matrix[8,2:(Outbreak_number+1)], as.numeric))
  HPD_lower <-(sapply(Outbreak_matrix[5,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[6,2:(Outbreak_number+1)], as.numeric))
  colour <- c(rep("#377eb8", Outbreak_number))
  
  plot(c(log10(min(Population)), log10(max(Population))), c(0, 1), type="n", xlab="Number of animals and humans infected (log scale)", ylab="Estimate")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(log10(position[i]), mean[i], col="#377eb8")
    lines(c(log10(position[i]),log10(position[i])), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    
  }
}  

#Discrete phylogeographic Human Markov reward plot sample proportion difference
my_sc_plot_H_DP_population_size <- function(data){
  
  position <- Population
  mean <- (sapply(Outbreak_matrix[24, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[21,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[22,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(log10(min(Population)), log10(max(Population))), c(0, 1), type="n", xlab="Number of animals and humans infected (log scale)", ylab="Estimate")
  title("D", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(log10(position[i]), mean[i], col="#e41a1c")
    lines(c(log10(position[i]),log10(position[i])), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    
  }
}

#Structured coalescent Animal Human transmissions plot sample_proportion difference
my_sc_plot_AH_SC_population_size <- function(data){
  
  position <- Population
  mean <- (sapply(Outbreak_matrix[36, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[33,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[34,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#377eb8", Outbreak_number))
  
  plot(c(log10(min(Population)), log10(max(Population))), c(0, 1), type="n", xlab="Number of animals and humans infected (log scale)", ylab="Estimate")
  title("A", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(log10(position[i]), mean[i], col="#377eb8")
    lines(c(log10(position[i]),log10(position[i])), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    
  }
}

#Discrete phylogeographic Animal Human Markov jump plot sample_proportion difference
my_sc_plot_AH_DP_population_size <- function(data){
  
  position <- Population
  mean <- (sapply(Outbreak_matrix[28, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[25,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[26,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(log10(min(Population)), log10(max(Population))), c(0, 1), type="n", xlab="Number of animals and humans infected (log scale)", ylab="Estimate")
  title("C", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(log10(position[i]), mean[i], col="#e41a1c")
    lines(c(log10(position[i]),log10(position[i])), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    
  }
}

#Structured coalescent Human Animal transmissions plot sample_proportion difference
my_sc_plot_HA_SC_population_size <- function(data){
  
  position <- Population
  mean <- (sapply(Outbreak_matrix[40, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[37,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[38,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#377eb8", Outbreak_number))
  
  plot(c(log10(min(Population)), log10(max(Population))), c(0, 1), type="n", xlab="Number of animals and humans infected (log scale)", ylab="Estimate")
  title("B", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(log10(position[i]), mean[i], col="#377eb8")
    lines(c(log10(position[i]),log10(position[i])), c(HPD_lower[i], HPD_upper[i]), col="#377eb8")
    
  }
}

#Discrete phylogeographic Human Animal Markov jump plot sample proportion difference
my_sc_plot_HA_DP_population_size <- function(data){
  
  position <- Population
  mean <- (sapply(Outbreak_matrix[32, 2:(Outbreak_number+1)], as.numeric))
  HPD_lower <- (sapply(Outbreak_matrix[29,2:(Outbreak_number+1)], as.numeric))
  HPD_upper <- (sapply(Outbreak_matrix[30,2:(Outbreak_number+1)], as.numeric))
  colour <- (rep("#e41a1c", Outbreak_number))
  
  plot(c(log10(min(Population)), log10(max(Population))), c(0, 1), type="n", xlab="Number of animals and humans infected (log scale)", ylab="Estimate")
  title("D", adj = 0, cex.main = 1.5)
  for (i in 1:Outbreak_number) {
    points(log10(position[i]), mean[i], col="#e41a1c")
    lines(c(log10(position[i]),log10(position[i])), c(HPD_lower[i], HPD_upper[i]), col="#e41a1c")
    
  }
}

Cairo(file="Multiple_simulations_population_proportions_population_size.png", type="png", width=6000, height=6000, pointsize=12, dpi=600)
par(mar=c(4.1, 6.1, 4.1, 1.1))
par(mfrow=c(2,2))
my_sc_plot_A_SC_population_size(data)
my_sc_plot_H_SC_population_size(data)
my_sc_plot_A_DP_population_size(data)
my_sc_plot_H_DP_population_size(data)
dev.off()


Cairo(file="Multiple_simulations_transmission_proportions_population_size.png", type="png", width=6000, height=6000, pointsize=12, dpi=600)
par(mar=c(4.1, 6.1, 4.1, 1.1))
par(mfrow=c(2,2))
my_sc_plot_AH_SC_population_size(data)
my_sc_plot_HA_SC_population_size(data)
my_sc_plot_AH_DP_population_size(data)
my_sc_plot_HA_DP_population_size(data)
dev.off()

