#####Exemple de script à nettoyer#####

rm(list = ls())
gc()

#Gestion de l'environnement-----

library(yaml)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyverse)
library(MASS)
library(forcats)

api_token <- yaml::read_yaml("secrets.yaml")$key

#Appel des fonctions-----

source("functions.R", encoding = "UTF-8")

#Import des données-----

df <- readr::read_csv2(
  file="individu_reg.csv",
  col_names = T,
  col_select = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
                 "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp", "trans", "ur")
)

#Retraitement des données------

##Recodage des valeurs manquantes------

df <- recode_na(df,na38,"ZZ")
df <- recode_na(df,trans,"Z") 
df <- recode_na(df,tp,"Z")
df <- recode_na(df,surf,"Z")

df[endsWith(df$naf08, "ZZ"), "naf08"] <- NA

##Variables catégorielles------

df <- df%>%
  dplyr::mutate(dplyr::across(.cols=c(trans,ur,sexe),.fns=factor))

df$sexe <- fct_recode(df$sexe, 
                      "Homme" = "1", 
                      "Femme" = "2")

#Convertir aged en numeric

df$age <- as.numeric(df$aged)

#Statistiques descriptives------

# COMPTE PROFESSIONS =================

print("Nombre de professions :")
print(summarise(df, length(unique(unlist(cs3[!is.na(cs3)])))))
print("Nombre de professions :")
print(summarise(df, length(unique(unlist(cs2[!is.na(cs2)])))))
print("Nombre de professions :")
print(summarise(df, length(unique(unlist(cs1[!is.na(cs1)])))))

# STATISTIQUES AGE ======================

summarise(group_by(df, age), n())

df %>%
  dplyr::select(age) %>%
  ggplot(.) + geom_histogram(aes(x = 5 * floor(age / 5)),
                             stat = "count")

ggplot(df[(df$age) > 50,],
       aes(x = age,
           y = ..density..,
           fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
       ),
       alpha = 0.2) + geom_histogram()

# part d'homme dans chaque cohorte ===================

ggplot(part_par_statut(df,"age","sexe")%>%
         dplyr::filter(sexe == "Homme")) +
  geom_bar(aes(x = age,
               y = y), stat = "identity") +
  geom_point(aes(x = age,
                 y = y), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


# stats surf par statut ==================

ggplot(part_par_statut(df,"couple","surf")) +
  geom_bar(aes(x = surf, y = y, color = factor(couple)),
           stat = "identity", position = "dodge")

# stats trans par statut ===================

ggplot(part_par_statut(df,"couple","trans")) + 
  geom_bar(aes(x = trans, y = y, color = factor(couple)),
           stat = "identity", position = "dodge")

# STATS AGREGEES =================

stats_agregees(df %>%
                 filter(sexe == "Homme") %>%
                 pull(age), na.rm = TRUE)
stats_agregees(df %>%
                 filter(sexe == "Femme") %>%
                 pull(age), na.rm = TRUE)
stats_agregees(df %>%
                 filter(sexe == "Homme" & couple == "2") %>%
                 pull(age), na.rm = TRUE)
stats_agregees(df %>%
                 filter(sexe == "Femme" & couple == "2") %>%
                 pull(age), na.rm = TRUE)


# MODELISATION ----------------------------


df3 <- df %>%
  dplyr::select(surf, cs1, ur, couple, age) %>%
  dplyr::filter(surf != "Z")

polr(factor(surf) ~ cs1 + factor(ur),
     df3 %>%
       filter(
         couple == 2 &
           age > 40 &
           age < 60)
)
