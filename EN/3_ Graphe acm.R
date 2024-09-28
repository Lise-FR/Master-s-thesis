## MCA: displaying axes 1 and 2

library(ggplot2)
library(ggrepel)
library(sysfonts)
library(showtext)

# Contribution threshold ----

seuil <- 100 / nrow(res_acm$var$coord)

# Add the Times New Roman font
font_add("Times New Roman", regular = "times.ttf")
showtext_auto()

# Graph axes 1 & 2
resultats_actives %>%
  filter(dim1_contrib > seuil |
           dim2_contrib > seuil) %>%
  ggplot(aes(x = dim2_coord, y = dim1_coord, label = modalites, color = factor(ifelse(dim1_contrib > dim2_contrib, "dim1", "dim2")))) +
  geom_point() +
  coord_fixed(ratio = 1) + # to ensure that the scales of the axes are identical
  geom_text_repel(size=5, family = "Times New Roman", segment.alpha = 0.5) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype = "longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "longdash") +
  xlab(paste0("Axe 2 (", round(variances[2, 3], 1), " %)")) +
  ylab(paste0("Axe 1 (", round(variances[1, 3], 1), " %)")) +
  scale_color_manual(values = c("dim1" = "#AA3377", "dim2" = "#4477AA")) + # Define colors for dim1 and dim2
  labs(
    title = "Figure 2 : Projection des genres et goûts de lecture",
    caption = "Source : Pratiques culturelles des Français 2018 du ministère de la Culture.\nChamp : Lecteur·ices d'au moins 15 ans.\nNote : Ne sont affichées que les modalités contribuant plus que la moyenne à la construction des axes.",
    subtitle = "sur les deux premiers axes de l'ACM",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = 'none', # Remove the color legend
    legend.text = element_text(family = "Times New Roman", size = 16),
    legend.title = element_text(family = "Times New Roman", size = 16),
    title = element_text(family = "Times New Roman", size = 16),
    axis.title = element_text(family = "Times New Roman", size = 15),
    axis.text = element_text(family = "Times New Roman", size = 15),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, family = "Times New Roman", size = 15)
  )


## MCA with only supplementary variables
# Add Times New Roman font
font_add("Times New Roman", regular = "times.ttf")
showtext_auto()

# the supplementary modalities whose coordinates
# deviate from +/- 0.3 from the centroid (this is perfectly arbitrary...)

resultats_complet %>% 
  filter(dim2_contrib > seuil|is.na(dim2_contrib) & dim1_coord > 0.29 |
           is.na(dim2_contrib) & dim1_coord < -0.31 |
           is.na(dim2_contrib) & dim2_coord > 0.29 |
           is.na(dim2_contrib) & dim2_coord < -0.31) %>% 
  ggplot(aes(x = dim2_coord, y = dim1_coord, 
             label = modalites)) + 
  geom_point() +
  coord_fixed() + # to ensure that the scales of the axes are identical
  geom_text_repel(size = 4.5, 
                  segment.alpha = 0.5,
                  max.overlaps = Inf, 
                  box.padding = 0.5,  # More space around the text boxes
                  point.padding = 0.5,  # More space around the points
                  family = "Times New Roman") +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype = "longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "longdash") +
  xlab(paste0("Axe 2 (", round(variances[2, 3], 1), " %)")) +
  ylab(paste0("Axe 1 (", round(variances[1, 3], 1), " %)")) +
  scale_color_manual(values = "black") +
  labs(
    title = "Figure 2 : Projection des variables supplémentaires",
    caption = "Source : Pratiques culturelles des Français 2018 du ministère de la Culture.\nChamp : Lecteur·ices d'au moins 15 ans.\nNote : Ne sont affichées que les modalités supplémentaires dont les coordonnées s'écartent de 0,3 du barycentre.",
    subtitle = "sur les deux premiers axes de l'ACM",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    legend.text = element_text(family = "Times New Roman", size = 16),
    legend.title = element_text(family = "Times New Roman", size = 16),
    title = element_text(family = "Times New Roman", size = 16),
    axis.title = element_text(family = "Times New Roman", size = 15),
    axis.text = element_text(family = "Times New Roman", size = 15),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, family = "Times New Roman", size = 15)
  )



# Graph axes 3 & 2
resultats_actives %>%
  filter(dim3_contrib > seuil |
           dim2_contrib > seuil) %>%
  ggplot(aes(x = dim2_coord, y = dim3_coord, label = modalites, color = factor(ifelse(dim3_contrib > dim2_contrib, "dim1", "dim2")))) +
  geom_point() +
  coord_fixed(ratio = 1) +
  geom_text_repel(size=5, family = "Times New Roman", segment.alpha = 0.5) +
  geom_hline(yintercept = 0, colour = "darkgrey", linetype = "longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "longdash") +
  xlab(paste0("Axe 2 (", round(variances[2, 3], 1), " %)")) +
  ylab(paste0("Axe 3 (", round(variances[3, 3], 1), " %)")) +
  scale_color_manual(values = c("dim1" = "#AA3377", "dim2" = "#4477AA")) +
  labs(
    title = "Figure : Projection des genres et goûts de lecture",
    caption = "Source : Pratiques culturelles des Français 2018 du ministère de la Culture.\nChamp : Résidents sur le territoire français âgés d'au moins 15 ans. \nNote : Sont affichées les modalités contribuant davantage que la moyenne à la formation du plan factoriel.",
    subtitle = "sur les deux premiers axes de l'ACM",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    legend.text = element_text(family = "Times New Roman", size = 16),
    legend.title = element_text(family = "Times New Roman", size = 16),
    title = element_text(family = "Times New Roman", size = 16),
    axis.title = element_text(family = "Times New Roman", size = 12),
    axis.text = element_text(family = "Times New Roman", size = 12),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, family = "Times New Roman", size = 12)
  )




## MCA with supplementary variables

resultats_complet %>% 
  filter(dim2_contrib > seuil|
           dim1_contrib > seuil|
           is.na(dim2_contrib) & dim1_coord > 0.29 |
           is.na(dim2_contrib) & dim1_coord < -0.31|
           is.na(dim2_contrib) & dim2_coord > 0.29 |
           is.na(dim2_contrib) & dim2_coord < -0.31) %>% 
  ggplot(aes(x = dim2_coord, y = dim1_coord, 
             label = modalites,
             colour = type # we differentiate the active and supplementary variables using different colors
  )) + 
  
  geom_point() +
  coord_fixed() +
  geom_text_repel(size = 3, segment.alpha = 0.5,max.overlaps =600) +
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +
  
  xlab(paste0("Axe 2 (", round(variances[2, 3], 1), " %)")) +
  ylab(paste0("Axe 1 (", round(variances[1, 3], 1), " %)")) +
  
  scale_color_manual(values = c("black", "#44AA99")) +
  
  
  guides(shape = guide_legend(title="Nom des variables (actives et supplémentaires)", # title of the variable names legend
                              title.position = "top"), 
         colour = guide_legend(title = "Type de variable", # title of the legend distinguishing active and supplementary variables
                               title.position = "top",
                               nrow = 2),
         size = FALSE) + # no legend for the points sizes
  
  theme_minimal() +
  theme(legend.position="bottom")

