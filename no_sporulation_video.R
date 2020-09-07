# Population and cost videos
.libPaths('~/R/x86_64-pc-linux-gnu-library/3.6')
library(parallel)
library(ggplot2)
library(tidyverse)
library(stringr)
library(withr)
library(data.table)

# Choose a run
my_files <- list.files('/hosts/linuxhome/mutant26/tmp/bramve/evolving_mutation_mutants_seeded_fifthmutants_produce_r6/grid/', full.names = TRUE, recursive = FALSE)

############################## Population data ##################################################################
# Get the files with the population data
type_files <- grep('type', my_files, value = TRUE)
type_files <- grep('txt', type_files, value = TRUE)
type_files_interest <- list()
for(files in type_files){
  Time = as.numeric(gsub('^.*_type|\\_|.txt', '', files))
  if((Time-2500) %% 3000000 == 0 | (Time-5000) %% 3000000 == 0 | (Time-7500) %% 3000000 == 0){
    type_files_interest <- c(type_files_interest, files)
    
  }
}

# start_cycle <- grep("000000", type_files, value = TRUE)
# quarter_cycle <- grep("002500", type_files, value = TRUE)
# half_cycle <- grep("005000", type_files, value = TRUE)
# three_quarter_cycle <- grep("007500", type_files, value = TRUE)
# Define function which authomatically makes and saves the plot for the population data
mytypeplotfunction <- function(file){
  
  # read data
  temp <- fread(file)
  colnames(temp) <- c('type', 'x1', 'y1')
  
  timepoint <- as.numeric(gsub('.*type|\\.txt$', '', head(file)))
  timepoint <- with_options(
    c(scipen = 999),
    str_pad(timepoint, 7, pad = '0')
  )
  
  temp_plot <- temp %>%
    ggplot(aes(x = x1, y = y1, fill = as.character(type))) +
    geom_raster() +
    scale_fill_manual(values = c('black', 'white', 'red', ' blue')) +
    theme_void() +
    theme(legend.position = 'none') +
    ggtitle(timepoint)
  
  temptypefilename <- paste0(dirname(type_files[1]), '/Pictures/Type/type_', timepoint, '.png')
  ggsave(file = temptypefilename,
         width =7.4,
         height = 8,
         units = 'cm'
  )
  cat(paste(' just saved plot for', which(file == type_files), '/' , length(type_files), '\r'))
  
}

# Apply the function to the files with populationdata, running on multiple cores.
mclapply(type_files_interest, 
         mytypeplotfunction, 
         mc.cores = detectCores() - 2)
# mclapply(quarter_cycle,
#          mytypeplotfunction, 
#          mc.cores = detectCores() - 2)
# mclapply(half_cycle,
#          mytypeplotfunction, 
#          mc.cores = detectCores() - 2)
# mclapply(three_quarter_cycle,
#          mytypeplotfunction, 
#          mc.cores = detectCores() - 2)
############################ Cost data #############################################################

# Get files with cost data
cost_files <- grep('costs', my_files, value = TRUE)
cost_files <- grep('txt', cost_files, value = TRUE)
cost_files_interest <- list()
for(files in cost_files){
  Time = as.numeric(gsub('^.*_costs|\\_|.txt', '', files))
  if((Time-2500) %% 3000000 == 0 | (Time-5000) %% 3000000 == 0 | (Time-7500) %% 3000000 == 0){
    cost_files_interest <- c(cost_files_interest, files)
    
  }
}
# start_cycle <- grep("000000", cost_files, value = TRUE)
# quarter_cycle <- grep("002500", cost_files, value = TRUE)
# half_cycle <- grep("005000", cost_files, value = TRUE)
# three_quarter_cycle <- grep("007500", cost_files, value = TRUE)
# Function which reads cost data and automatically saves it to (dirrun)/grid

mycostplotfunction <- function(file){
  
  temp <- fread(file)
  colnames(temp) <- c('cost', 'x1', 'y1')
  
  timepoint <- as.numeric(gsub('.*costs|\\.txt$', '', head(file)))
  timepoint <- with_options(
    c(scipen = 999),
    str_pad(timepoint, 7, pad = '0')
  )
  
  temp_plot <- temp %>%
    ggplot(aes(x = x1, y = y1, fill = cost)) +
    geom_raster() +
    theme_void() +
    scale_fill_viridis_c(limits = c(0,0.8), option = "A") +
    guides(fill = guide_colourbar()) + 
    ggtitle(timepoint)
  
  tempcostfilename <- paste0(dirname(cost_files[1]), '/Pictures/Cost/cost_', timepoint, '.png')
  ggsave(file = tempcostfilename,
         width =9,
         height = 8,
         units = 'cm'
  )
  cat(paste(' just saved plot for', which(file == cost_files), '/' , length(cost_files), '\r'))
  
}

# Apply the function to the files with costdata, running on multiple cores.

mclapply(cost_files_interest, 
         mycostplotfunction, 
         mc.cores = detectCores() - 2)
# mclapply(quarter_cycle,
#          mycostplotfunction, 
#          mc.cores = detectCores() - 2)
# mclapply(half_cycle,
#          mycostplotfunction, 
#          mc.cores = detectCores() - 2)
# mclapply(three_quarter_cycle,
#          mycostplotfunction, 
#          mc.cores = detectCores() - 2)

############################ Production data #############################################################

# Get files with production data
production_files <- grep('production', my_files, value = TRUE)
production_files <- grep('txt', production_files, value = TRUE)
production_files_interest <- list()
for(files in production_files){
  Time = as.numeric(gsub('^.*_production|\\_|.txt', '', files))
  if((Time-2500) %% 3000000 == 0 | (Time-5000) %% 3000000 == 0 | (Time-7500) %% 3000000 == 0){
    production_files_interest <- c(production_files_interest, files)
    
  }
}
# start_cycle <- grep("000000", production_files, value = TRUE)
# quarter_cycle <- grep("002500", production_files, value = TRUE)
# half_cycle <- grep("005000", production_files, value = TRUE)
# three_quarter_cycle <- grep("007500", production_files, value = TRUE)
# Function which reads cost data and automatically saves it to (dirrun)/grid

myproductionplotfunction <- function(file){
  
  temp <- fread(file)
  colnames(temp) <- c('production', 'x1', 'y1')
  
  timepoint <- as.numeric(gsub('.*production|\\.txt$', '', head(file)))
  timepoint <- with_options(
    c(scipen = 999),
    str_pad(timepoint, 7, pad = '0')
  )
  
  temp_plot <- temp %>%
    ggplot(aes(x = x1, y = y1, fill = production)) +
    geom_raster() +
    theme_void() +
    scale_fill_viridis_c(limits = c(0,3), option = "A") +
    guides(fill = guide_colourbar()) + 
    ggtitle(timepoint)
  
  tempproductionfilename <- paste0(dirname(production_files[1]), '/Pictures/Production/production_', timepoint, '.png')
  ggsave(file = tempproductionfilename,
         width =9,
         height = 8,
         units = 'cm'
  )
  cat(paste(' just saved plot for', which(file == production_files), '/' , length(production_files), '\r'))
  
}

# Apply the function to the files with costdata, running on multiple cores.

mclapply(production_files_interest, 
         myproductionplotfunction, 
         mc.cores = detectCores() - 2)
# mclapply(quarter_cycle,
#          myproductionplotfunction, 
#          mc.cores = detectCores() - 2)
# mclapply(half_cycle,
#          myproductionplotfunction, 
#          mc.cores = detectCores() - 2)
# mclapply(three_quarter_cycle,
#          myproductionplotfunction, 
#          mc.cores = detectCores() - 2)

############################ Mutation data #############################################################

# Get files with mutation data
mut_files <- grep('grid_mut', my_files, value = TRUE)
mut_files <- grep('txt', mut_files, value = TRUE)
mut_files_interest <- list()
for(files in mut_files){
  Time = as.numeric(gsub('^.*_mut|\\_|.txt', '', files))
  if((Time-2500) %% 3000000 == 0 | (Time-5000) %% 3000000 == 0 | (Time-7500) %% 3000000 == 0){
    mut_files_interest <- c(mut_files_interest, files)
    
  }
}
# start_cycle <- grep("000000", mut_files, value = TRUE)
# quarter_cycle <- grep("002500", mut_files, value = TRUE)
# half_cycle <- grep("005000", mut_files, value = TRUE)
# three_quarter_cycle <- grep("007500", mut_files, value = TRUE)
# Function which reads cost data and automatically saves it to (dirrun)/grid

mymutplotfunction <- function(file){
  
  temp <- fread(file)
  colnames(temp) <- c('mutrate', 'x1', 'y1')
  
  timepoint <- as.numeric(gsub('.*mut|\\.txt$', '', head(file)))
  timepoint <- with_options(
    c(scipen = 999),
    str_pad(timepoint, 7, pad = '0')
  )
  
  temp_plot <- temp %>%
    ggplot(aes(x = x1, y = y1, fill = mutrate)) +
    geom_raster() +
    theme_void() +
    scale_fill_viridis_c(limits = c(0,0.12), option = "A") +
    guides(fill = guide_colourbar()) + 
    ggtitle(timepoint)
  
  tempmutfilename <- paste0(dirname(mut_files[1]), '/Pictures/Mut/mut_', timepoint, '.png')
  ggsave(file = tempmutfilename,
         width =9,
         height = 8,
         units = 'cm'
  )
  cat(paste(' just saved plot for', which(file == mut_files), '/' , length(mut_files), '\r'))
  
}

# Apply the function to the files with mutdata, running on multiple cores.

mclapply(mut_files_interest, 
         mymutplotfunction, 
         mc.cores = detectCores() - 2)
# mclapply(quarter_cycle,
#          mymutplotfunction, 
#          mc.cores = detectCores() - 2)
# mclapply(half_cycle,
#          mymutplotfunction, 
#          mc.cores = detectCores() - 2)
# mclapply(three_quarter_cycle,
#          mymutplotfunction, 
#          mc.cores = detectCores() - 2)