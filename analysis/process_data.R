library("R.matlab")

dataset_aVBDM <- R.matlab::readMat("data/datastruct_aVBDM.mat")
names(dataset_aVBDM)
length(dataset_aVBDM)

dataset_aVBDM <- dataset_aVBDM[[1]]
names(dataset_aVBDM)
length(dataset_aVBDM)
# the dataset holds data for 39 participants and has the following structure:
#  1: trialnum: trial number (100 total trials, but some trials removed if no fixations occurred within 40ms at beginning or end of the trial per Krajbich et al., (2010))
#  2: fixdur: fixation duration [s] per each item fixation
#  3: fixitem: fixated item (item 1 or item 2)
#  4: itemval: value associated with each item (column number corresponds to item number)
#  5: choice: chosen item at end of trial
#  6: rt: response time [s]
#  7: tItem: total fixation time [s] spent on either item

N_part <- 39
n_obj  <- 7

data_list <- NULL
for(j in 1:N_part) {
	for(i in 1:n_obj) {
		data_list[[j]] <- list(participant = j,
							   trialnum = dataset_aVBDM[[(j-1)*n_obj + 1]],
							   fixdur   = dataset_aVBDM[[(j-1)*n_obj + 2]],
							   fixitem  = dataset_aVBDM[[(j-1)*n_obj + 3]],
							   itemval  = dataset_aVBDM[[(j-1)*n_obj + 4]],
							   choice   = dataset_aVBDM[[(j-1)*n_obj + 5]],
							   rt       = dataset_aVBDM[[(j-1)*n_obj + 6]],
							   tItem    = dataset_aVBDM[[(j-1)*n_obj + 7]])
	}
}

library(tidyverse)
map_int(data_list, function(x) length(x[["trialnum"]]))
table(map_int(data_list, function(x) length(x[["trialnum"]])))

map_dbl(data_list, function(x) max(x[["trialnum"]]))
table(map_dbl(data_list, function(x) max(x[["trialnum"]])))
map_dbl(data_list, function(x) max(x[["trialnum"]]) - length(x[["trialnum"]]))
# this means that trialnum is an index going from 1 to the total number of valid trials
# it does not indicate the number of the trial in the sequence from 1 to 100

f_create_fix_data <- function(list_item) {
	aux_df <- map2(list_item[["fixdur"]], list_item[["fixitem"]], 
				   function(x, y) tibble(fix_number = c(1:length(x[[1]][1, ])),
				   					  fix_duration = x[[1]][1, ], 
				   					  fix_item = y[[1]][1, ]))
	names(aux_df) <- paste0("trial_", 1:length(aux_df))
	aux_df <- map2(aux_df, names(aux_df), function(x, y) x %>% mutate(trialnum = as.numeric(str_remove(y, "trial_"))))
	aux_df <- map_dfr(aux_df, rbind)
	aux_df <- aux_df %>%
		mutate(participant = list_item[["participant"]]) %>%
		relocate(participant, trialnum)
	return(aux_df)	
}

f_create_choice_data <- function(list_item) {
	df_item_info <- tibble(participant = list_item[["participant"]],
						   trialnum    = list_item[["trialnum"]][, 1],
						   value_item1 = list_item[["itemval"]][, 1],
						   value_item2 = list_item[["itemval"]][, 2],
						   chosen_item = list_item[["choice"]][, 1],
						   rt = list_item[["rt"]][, 1],
						   totaltime_item1 = list_item[["tItem"]][, 1],
						   totaltime_item2 = list_item[["tItem"]][, 2])
	return(df_item_info)
}

fixation_data <- map(data_list, f_create_fix_data)
choice_data <- map(data_list, f_create_choice_data)

fixation_data <- map_dfr(fixation_data, rbind)
choice_data <- map_dfr(choice_data, rbind)

save(N_part, fixation_data, choice_data, file = "analysis/processed_data.RData")
