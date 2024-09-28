##################################################
# LOAD PACKAGES
##################################################


library(tidyverse)

library(curl)

library(purrr)

library(questionr)

library(FactoMineR) 

library(RColorBrewer) 

library(ggrepel) 

library(explor) 

library(flextable)

library(khroma)

##################################################
# PERFORM AND VISUALIZE A MCA
##################################################


# PERFORMING THE MCA ----


# First visualization of the results ----

# STATISTICAL DESCRIPTION OF THE AXES ----

variances <- as.data.frame(res_acm$eig) %>%
  rownames_to_column() %>% # retrieve the row names (dim 1, dim 2, etc.) in a separate column
  slice(1:10) %>% # keep only the information from the first 10 axes
  mutate(Axes = str_replace_all(rowname, "dim", "Axe")) %>% # create a new variable from rowname that takes the values "Axis 1, Axis 2, etc." instead of "dim 1, dim 2, etc."
  select(-rowname) %>% # remove this column that is no longer needed
  rename(`Valeurs propres` = eigenvalue) %>%
  rename(`% de variance` = `percentage of variance`) %>% # rename other columns
  rename(`% cumulé de variance` = `cumulative percentage of variance`) %>% 
  mutate(Axes = fct_relevel(Axes, paste("Axe", 1:10))) %>% # to ensure that the order from 1 to 10 is properly maintained in the graphs
  select(Axes, `Valeurs propres`, `% de variance`, `% cumulé de variance`)

variances %>% 
  flextable() %>% 
  colformat_double(decimal.mark = ",", digits = 2) %>% 
  autofit() %>% 
  set_caption("Valeurs propres et variances le long des axes")

# Contribution Threshold ----

seuil <- 100 / nrow(res_acm$var$coord)

# This threshold is obviously arbitrary; we will not discuss this point here. The threshold chosen here is the average contribution (1 / number of active modalities). The principle is simple: a modality is said to be "contributive" if it "carries" more weight on an axis than its average weight in the entire original cloud.


# Frequencies of active and supplementary modalities ----

# First step: retrieve the frequencies of each modality. In doing so (using the map function), we also obtain the list of all modalities in the dataset, as well as the variables to which they belong. This will be very useful later on for drawing different symbols by variable...

frequences <- d_acm %>% 
  pivot_longer(everything(),
               names_to = "variables", 
               values_to = "modalites") %>%  # count all occurrences of the variable/modality pairs
  count(variables, modalites) %>% # count the number of unique "variable/modality" pairs (thus the number of individuals per modality in the dataset)
  group_by(variables) %>% 
  mutate(pourcentage = round(100 * n / nrow(d_acm), 1)) %>% # calculate percentages for each variable group
  ungroup() %>% 
  select(variables, modalites, n, pourcentage)  # select the variables in a more readable order

frequences %>% 
  flextable() %>% 
  colformat_double(decimal.mark = ",", digits = 1) %>% 
  autofit() %>% 
  set_caption("Tableau de fréquence de l'ensemble des variables du jeu de données")

# Active Modalities ----

# Second step: retrieve the statistical indicators of the active modalities.

# Coordinates (active modalities) ----

coordonnees <- as_tibble(res_acm$var$coord,
                         rownames = "modalites") %>%  # retrieve the coordinates of the active modalities
  mutate_if(is.numeric, round, digits = 2) %>%  # round the numerical variables to 2 decimal places (this is quite sufficient)
  rename_all(tolower) %>% # all in lowercase
  rename_all(~ str_replace(., " ", "")) %>% # rename the variables by removing the spaces: for example, 'dim 1' becomes 'dim1'
  rename_if(is.numeric, ~ str_c(., "coord", sep = "_")) # add the suffix _coord to each variable name (except for the modality variable). This results in, for example, 'dim1_coord'

# Contributions (active modalities) ----
contributions <- as_tibble(res_acm$var$contrib,
                           rownames = "modalites") %>%  # retrieve the coordinates of the active modalities
  mutate_if(is.numeric, round, digits = 2) %>%  # round the numerical variables to 2 decimal places (this is quite sufficient)
  rename_all(tolower) %>% # all in lowercase
  rename_all(~ str_replace(., " ", "")) %>% # rename the variables by removing the spaces: for example, 'dim 1' becomes 'dim1'
  rename_if(is.numeric, ~ str_c(., "contrib", sep = "_")) # the same, except that here we get 'dim1_contrib'

# Cosinus carrés (modalités actives) ----
cos2 <- as_tibble(res_acm$var$cos2,
                  rownames = "modalites") %>%  # retrieve the coordinates of the active modalities
  mutate_if(is.numeric, round, digits = 2) %>%  # round the numerical variables to 2 decimal places (this is quite sufficient)
  rename_all(tolower) %>% # tout en minuscules
  rename_all(~ str_replace(., " ", "")) %>% # rename the variables by removing the spaces: for example, 'dim 1' becomes 'dim1'
  rename_if(is.numeric, ~ str_c(., "cos2", sep = "_")) # the same, except that here we get 'dim1_cos2'


# vtest (active modalities) ----
vtest <- as_tibble(res_acm$var$v.test,
                   rownames = "modalites") %>%  # retrieve the coordinates of the active modalities
  mutate_if(is.numeric, round, digits = 2) %>%  # round the numerical variables to 2 decimal places (this is quite sufficient)
  rename_all(tolower) %>% # all in lowercase
  rename_all(~ str_replace(., " ", "")) %>% # rename the variables by removing the spaces: for example, 'dim 1' becomes 'dim1'
  rename_if(is.numeric, ~ str_c(., "vtest", sep = "_")) # the same, except that here we get 'dim1_vtest'

# Assembly of statistical results (active modalities) ----

resultats_actives <- frequences %>% 
  right_join(coordonnees) %>% 
  right_join(contributions) %>% 
  right_join(cos2) %>% 
  right_join(vtest) %>% # merge the datasets; the merging key (implicit) is the variable 'modalites', which is common to all.
  mutate(type = "Variable active") %>% # adding a column containing the string "Active variable" (to be able to distinguish later with the supplementary variables)
  select(type, variables, modalites, n, pourcentage,
         contains("dim1"), contains("dim2"),
         contains("dim3"), contains("dim4"))  # keep and reorganize the relevant variables axis by axis



resultats_actives %>% 
  flextable() %>% 
  colformat_double(decimal.mark = ",", digits = 1) %>% 
  autofit() %>% 
  set_caption("Résultats statistiques variables actives")


# Supplementary Modalities ----

# Coordinates (supplementary modalities) ----

coordonnees_sup <- as_tibble(res_acm$quali.sup$coord,
                             rownames = "modalites") %>%
  mutate_if(is.numeric, round, digits = 2) %>%  
  rename_all(tolower) %>%
  rename_all(~ str_replace(., " ", "")) %>%
  rename_if(is.numeric, ~ str_c(., "coord", sep = "_"))

# Squared Cosines (active modalities) ----
cos2_sup <- as_tibble(res_acm$quali.sup$cos2,
                      rownames = "modalites") %>%  
  mutate_if(is.numeric, round, digits = 2) %>% 
  rename_all(tolower) %>% 
  rename_all(~ str_replace(., " ", "")) %>%
  rename_if(is.numeric, ~ str_c(., "cos2", sep = "_")) 

# vtest (active modalities) ----
vtest_sup <- as_tibble(res_acm$quali.sup$v.test,
                       rownames = "modalites") %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  rename_all(tolower) %>%
  rename_all(~ str_replace(., " ", "")) %>%
  rename_if(is.numeric, ~ str_c(., "vtest", sep = "_")) 

# Assembly of Statistical Results (Active Modalities) ----

resultats_sup <- frequences %>% 
  right_join(coordonnees_sup) %>% 
  right_join(cos2_sup) %>% 
  right_join(vtest_sup) %>% 
  mutate(type = "Variable supplémentaire") %>%
  select(type, variables, modalites, n, pourcentage,
         contains("dim1"), contains("dim2"),
         contains("dim3"), contains("dim4")) 

resultats_sup %>% 
  flextable() %>% 
  colformat_double(decimal.mark = ",", digits = 1) %>% 
  autofit() %>% 
  set_caption("Résultats statistiques variables supplémentaires")

# Assembly of the complete results table (active and supplementary variables) ----

# We concatenate the rows of the tables containing all the statistical results of the active and supplementary variables.

resultats_complet <- bind_rows(resultats_actives, resultats_sup) 

