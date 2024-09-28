
## MCA for my master's thesis on the comics readers.
## I can't send the database due to the statistic secret.


## Recoding to remove NA in the 'lecture' variable. 
## I clean the variables I want to put in my MCA, which concern reading practices. 

PC18 <- mutate(PC18,
               lecture = if_else(freq_livre == "Jamais" | lecteurs_freq == "Pas de livres",
                                 "Pas lecteur",
                                 "Lecteur"))
table(PC18$lecture)

## F6 : You, personally, which category(ies) of books do you read?

PC18 <- mutate(PC18,
               lecteurs_BD = case_when(F5=="4"~"Ne lit pas",
                                       F609 == "1" ~ "Lect.BD",
                                       F609=="0" ~ "Pas.BD",
                                       .default = NA_character_))%>%
  filter(!is.na(lecteurs_BD))


PC18 <- mutate(PC18,
               lecteurs_comic = case_when(F5=="4"~"Ne lit pas",
                                          F610=="1" ~ "Lect.comic",
                                          F610=="0" ~ "Pas.comic",
                                          .default = NA_character_))%>%
  filter(!is.na(lecteurs_comic))

PC18 <- mutate(PC18,
               lecteurs_manga = case_when(F5=="4"~"Ne lit pas",
                                          F611=="1" ~ "Lect.manga",
                                          F611=="0" ~ "Pas.manga",
                                          .default = NA_character_))%>%
  filter(!is.na(lecteurs_manga))

PC18 <- mutate(PC18,
               lecteurs_classique= case_when(F5=="4"~"Ne lit pas",
                                             F601 == "1" ~ "Lect.class",
                                             F601=="0" ~ "Pas.class",
                                             .default = NA_character_))%>%
  filter(!is.na(lecteurs_classique))

PC18 <- mutate(PC18,
               lecteurs_sf= case_when(F5=="4"~"Ne lit pas",
                                      F603 == "1" ~ "Lect.sf",
                                      F603=="0" ~ "Pas.sf",
                                      .default = NA_character_))%>%
  filter(!is.na(lecteurs_sf))

PC18 <- mutate(PC18,
               lecteurs_histoire= case_when(F5=="4"~"Ne lit pas",
                                            F604 == "1" ~ "Lect.hist",
                                            F604=="0" ~ "Pas.hist",
                                            .default = NA_character_))%>%
  filter(!is.na(lecteurs_histoire))

PC18 <- mutate(PC18,
               lecteurs_sentiments= case_when(F5=="4"~"Ne lit pas",
                                              F605 == "1" ~ "Lect.sent",
                                              F605=="0" ~ "Pas.sent",
                                              .default = NA_character_))%>%
  filter(!is.na(lecteurs_sentiments))

PC18 <- mutate(PC18,
               lecteurs_prix= case_when(F5=="4"~"Ne lit pas",
                                        F606 == "1" ~ "Lect.prix",
                                        F606=="0" ~ "Pas.prix",
                                        .default = NA_character_))%>%
  filter(!is.na(lecteurs_prix))

PC18 <- mutate(PC18,
               lecteurs_bio= case_when(F5=="4"~"Ne lit pas",
                                       F607 == "1" ~ "Lect.bio",
                                       F607=="0" ~ "Pas.bio",
                                       .default = NA_character_))%>%
  filter(!is.na(lecteurs_bio))

PC18 <- mutate(PC18,
               lecteurs_contempo= case_when(F5=="4"~"Ne lit pas",
                                            F608 == "1" ~ "Lect.contempo",
                                            F608=="0" ~ "Pas.contempo",
                                            .default = NA_character_))%>%
  filter(!is.na(lecteurs_contempo))

PC18 <- mutate(PC18,
               lecteurs_science= case_when(F5=="4"~"Ne lit pas",
                                           F612 == "1" ~ "Lect.essai",
                                           F612=="0" ~ "Pas.essai",
                                           .default = NA_character_))%>%
  filter(!is.na(lecteurs_science))

PC18 <- mutate(PC18,
               lecteurs_actu= case_when(F5=="4"~"Ne lit pas",
                                        F613 == "1" ~ "Lect.actu",
                                        F613=="0" ~ "Pas.actu",
                                        .default = NA_character_))%>%
  filter(!is.na(lecteurs_actu))

PC18 <- mutate(PC18,
               lecteurs_dvtperso= case_when(F5=="4"~"Ne lit pas",
                                            F614 == "1" ~ "Lect.dvtperso",
                                            F614=="0" ~ "Pas.dvtperso",
                                            .default = NA_character_))%>%
  filter(!is.na(lecteurs_dvtperso))

PC18 <- mutate(PC18,
               lecteurs_viepratique= case_when(F5=="4"~"Ne lit pas",
                                               F615 == "1" ~ "Lect.viepratique",
                                               F615=="0" ~ "Pas.viepratique",
                                               .default = NA_character_))%>%
  filter(!is.na(lecteurs_viepratique))

PC18 <- mutate(PC18,
               lecteurs_art= case_when(F5=="4"~"Ne lit pas",
                                       F616 == "1" ~ "Lect.art",
                                       F616=="0" ~ "Pas.art",
                                       .default = NA_character_))%>%
  filter(!is.na(lecteurs_art))


PC18 <- mutate(PC18,
               lecteurs_autre= case_when(F5=="4"~"Ne lit pas",
                                         F617 == "1" ~ "Lect.autre",
                                         F617=="0" ~ "Pas.autre",
                                         .default = NA_character_))%>%
  filter(!is.na(lecteurs_autre))


PC18 <- mutate(PC18,
               lecteurs_aucun= case_when(F5=="4"~"Ne lit pas",
                                         F618 == "1" ~ "Lect.aucun",
                                         F618=="0" ~ "Pas.aucun",
                                         .default = NA_character_))%>%
  filter(!is.na(lecteurs_aucun))


## Recoding taste with also dislike and indifference

PC18 <- mutate(PC18,
               gout_BD = case_when(F5=="4"~"Ne lit pas",
                                   F709 == "1" ~ "G.BD",
                                   F809=="1" ~ "Dg.BD",
                                   F709 == "0" & F809=="0" ~"Indif.BD",
                                   .default = NA_character_)) 


PC18 <- mutate(PC18,
               gout_comic = case_when(F5=="4"~"Ne lit pas",
                                      F710=="1" ~ "G.comic",
                                      F810=="1" ~ "Dg.comic",
                                      F710 == "0" & F810=="0" ~"Indif.comic",
                                      .default = NA_character_))


PC18 <- mutate(PC18,
               gout_comic = case_when(F5=="4"~"Ne lit pas",
                                      F710=="1" ~ "G.comic",
                                      F810=="1" ~ "Dg.comic",
                                      F710 == "0" & F810=="0" ~"Indif.comic",
                                      .default = NA_character_))

PC18 <- mutate(PC18,
               gout_manga = case_when(F5=="4" ~"Ne lit pas",
                                      F711=="1" ~ "G.manga",
                                      F811=="1" ~ "Dg.manga",
                                      F711 == "0" & F811=="0" ~"Indif.manga",
                                      .default = NA_character_))

PC18 <- mutate(PC18,
               gout_classique= case_when(F5=="4"~"Ne lit pas",
                                         F701 == "1" ~ "G.classique",
                                         F801=="1" ~ "Dg.classique",
                                         F701 == "0" & F801=="0" ~"Indif.classique",
                                         .default = NA_character_))

PC18 <- mutate(PC18,
               gout_sf= case_when(F5=="4"~"Ne lit pas",
                                  F703 == "1" ~ "G.sf",
                                  F803=="1" ~ "Dg.sf",
                                  F703 == "0" & F803=="0" ~"Indif.sf",
                                  .default = NA_character_))

PC18 <- mutate(PC18,
               gout_histoire= case_when(F5=="4"~"Ne lit pas",
                                        F704 == "1" ~ "G.hist",
                                        F804=="1" ~ "Dg.hist",
                                        F704 == "0" & F804=="0" ~"Indif.hist",
                                        .default = NA_character_))
PC18 <- mutate(PC18,
               gout_sentiments= case_when(F5=="4"~"Ne lit pas",
                                          F705 == "1" ~ "G.sent",
                                          F805=="1" ~ "Dg.sent",
                                          F705 == "0" & F805=="0" ~"Indif.sent",
                                          .default = NA_character_))

PC18 <- mutate(PC18,
               gout_prix= case_when(F5=="4"~"Ne lit pas",
                                    F706 == "1" ~ "G.prix",
                                    F806=="1" ~ "Dg.prix",
                                    F706 == "0" & F806=="0" ~"Indif.prix",
                                    .default = NA_character_))

PC18 <- mutate(PC18,
               gout_bio= case_when(F5=="4"~"Ne lit pas",
                                   F707 == "1" ~ "G.bio",
                                   F807=="1" ~ "Dg.bio",
                                   F707 == "0" & F807=="0" ~"Indif.bio",
                                   .default = NA_character_))

PC18 <- mutate(PC18,
               gout_contempo= case_when(F5=="4"~"Ne lit pas",
                                        F708 == "1" ~ "G.contempo",
                                        F808=="1" ~ "Dg.contempo",
                                        F708 == "0" & F808=="0" ~"Indif.contempo",
                                        .default = NA_character_))


PC18 <- mutate(PC18,
               gout_science= case_when(F5=="4"~"Ne lit pas",
                                       F712 == "1" ~ "G.essai",
                                       F812=="1" ~ "Dg.essai",
                                       F712 == "0" & F812=="0" ~"Indif.essai",
                                       .default = NA_character_))

PC18 <- mutate(PC18,
               gout_actu= case_when(F5=="4"~"Ne lit pas",
                                    F713 == "1" ~ "G.actu",
                                    F813=="1" ~ "Dg.actu",
                                    F713 == "0" & F813=="0" ~"Indif.actu",
                                    .default = NA_character_))

PC18 <- mutate(PC18,
               gout_dvtperso= case_when(F5=="4"~"Ne lit pas",
                                        F714 == "1" ~ "G.dvtperso",
                                        F814=="1" ~ "Dg.dvtperso",
                                        F714 == "0" & F814=="0" ~"Indif.dvtperso",
                                        .default = NA_character_))


PC18 <- mutate(PC18,
               gout_viepratique= case_when(F5=="4"~"Ne lit pas",
                                           F715 == "1" ~ "G.viepratique",
                                           F815=="1" ~ "Dg.viepratique",
                                           F715 == "0" & F815=="0" ~"Indif.viepratique",
                                           .default = NA_character_))
freq(lecteur$gout_art)

PC18 <- mutate(PC18,
               gout_art= case_when(F5=="4"~"Ne lit pas",
                                   F716 == "1" ~ "G.art",
                                   F816=="1" ~ "Dg.art",
                                   F716 == "0" & F816=="0" ~"Indif.art",
                                   .default = NA_character_))


PC18 <- mutate(PC18,
               gout_autre= case_when(F5=="4"~"Ne lit pas",
                                     F717 == "1" ~ "G.autre",
                                     F817=="1" ~ "Dg.autre",
                                     F717 == "0" & F817=="0" ~"Indif.autre",
                                     .default = NA_character_))


PC18 <- mutate(PC18,
               gout_aucun= case_when(F5=="4"~"Ne lit pas",
                                     F718 == "1" ~ "G.aucun",
                                     F818=="1" ~ "Dg.aucun",
                                     F718 == "0" & F818=="0" ~"Indif.aucun",
                                     .default = NA_character_)) 


## Creation of a dataset with only readers

lecteur <- filter(PC18, lecture == "Lecteur")
lecture_BD <- filter(lecteur, lecteurs_BD == "Lect.BD")
lecture_dessin <- filter(lecteur, lecteurs_dessins == "Lecteur·ices de BD, comics ou mangas")
## I considered non-readers to be those who declared themselves as people who do not read books
## or those who declared they had not read any books in the last twelve months.

## Analysis 1: taste and reading

d_acm <- lecteur |> 
  select(sexe_r, age_classe, diplome_acm, PCS, SITUA_r, revenu,
         lecteurs_freq,freq_livre,
         livre_papier, livre_liseuse, livre_tablette, livre_format_papier,
         nb_livre_an, nb_BD_an,   
         lect_enfant_BD_r, lect_enfant_r, 
         nbr_genre_lecture_r, nb_lect_dessin,
         inclus_albums, 
         lect_wk, lect_conge,
         freq_biblio, Bibli.seul, Bibli.couple, Bibli.enfant, 
         Bibli.petitsenfants, Bibli.proches, Bibli.amis,Bibli.groupe, Bibli.pasrègle,
         manquer_lecture, gout_aucun,
         gout_BD, gout_comic, gout_manga, gout_art, gout_viepratique,gout_dvtperso, gout_actu, gout_bio, 
         gout_classique,gout_sf,gout_histoire,gout_sentiments,gout_prix, gout_bio, gout_contempo, 
         lecteurs_actu, lecteurs_manga, lecteurs_BD,lecteurs_bio, lecteurs_comic, lecteurs_classique, 
         lecteurs_sf, lecteurs_histoire, lecteurs_sentiments, lecteurs_prix, lecteurs_contempo, 
         lecteurs_science, lecteurs_dvtperso, lecteurs_viepratique, lecteurs_art, lecteurs_autre
  ) %>% 
  mutate_all(as.factor)

res.impute<-missMDA::imputeMCA(d_acm, ncp = 5,quali.sup = 1:32)

res_acm <- res.impute$completeObs |>
  FactoMineR::MCA(ncp = 5, graph = FALSE,  quali.sup = 1:32)

# res_acm %>%  explor::explor()

# num_apprentissage, num_creation, num_diffusion, num_aucun

# 
# # Performing Hierarchical Clustering (HC)
# # Calculating distances between individuals
# dist_acm <- dist(res_acm$ind$coord)
# 
# # Performing HC on the factorial coordinates
# hc <- fastcluster::hclust(dist_acm, method = "ward.D2")
# 
# hc |> 
#   factoextra::fviz_dend(show_labels = FALSE) +
#   ggplot2::ggtitle("Dendrogramme (distance de Gower)")
# 
# Visualizing the dendrogram
# fviz_dend(hc, k = 4, # Choose the desired number of clusters
#           cex = 0.5, # Label size
#           k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
#           color_labels_by_k = TRUE, # Color labels by clusters
#           rect = TRUE, rect_border = "jco", rect_fill = TRUE # Add rectangles around clusters
# )
# 

