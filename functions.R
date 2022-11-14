#Définition des fonctions------

decennie_a_partir_annee <- function(ANNEE) 
{
  return(ANNEE - ANNEE%%10)
}

stats_agregees <- function(a, stat = "moyenne",
                           ...) 
{
  match.arg(stat,
            c("moyenne",
              "variance",
              "ecart-type",
              "sd",
              "ecart type")
  )
  
  switch(stat,
         moyenne = mean(a, ...),
         variance = var(a, ...),
         sd(a, ...)
  )
  
}

stats_agregees(rnorm(10))
stats_agregees(rnorm(10), "cart type")
stats_agregees(rnorm(10), "ecart type")
stats_agregees(rnorm(10), "variance")

recode_na <- function(data,var,toreplace)
{
  data <- data%>%
    dplyr::mutate({{var}}:=na_if({{var}},toreplace))
  return(data)
}

part_par_statut <- function(data,var_groupe,var_part)
{
  output <- data %>%
    dplyr::group_by(!!sym(var_groupe),!!sym(var_part)) %>%
    dplyr::summarise(x = n()) %>%
    dplyr::group_by(!!sym(var_groupe)) %>%
    dplyr::mutate(y = 100 * x / sum(x))
  return(output)
}