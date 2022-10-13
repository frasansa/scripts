# Funciones base de datos------------------------------------------------------
inner_join_NA <- function(x, y, ...) {
  inner_join(x = x, y = y, by = ...) %>% 
    mutate(across(everything(), ~replace_na(.x, "")))
}

anti_join_NA <- function(x, y, ...) {
  anti_join(x = x, y = y, by = ...) %>% 
    mutate(across(everything(), ~replace_na(.x, "")))
}

semi_join_NA <- function(x, y, ...) {
  semi_join(x = x, y = y, by = ...) %>% 
    mutate(across(everything(), ~replace_na(.x, "")))
}

left_join_NA <- function(x, y, ...) {
  left_join(x = x, y = y, by = ...) %>% 
    mutate(across(everything(), ~replace_na(.x, "")))
}

right_join_NA <- function(x, y, ...) {
  right_join(x = x, y = y, by = ...) %>% 
    mutate(across(everything(), ~replace_na(.x, "")))
}

full_join_NA <- function(x, y, ...) {
  full_join(x = x, y = y, by = ...) %>% 
    mutate(across(everything(), ~replace_na(.x, "")))
}

fill_NA <- function(x, fill = "") {
  x %>%
    mutate_all(~replace_na(., fill))
}

# Función para obtener el rango intercuartil-----------------------------------
set_IQR <- function (x, round = 2) {
  IQR <- round(quantile (x, probs=c(0.5, 0.25, 0.75), na.rm=TRUE),
               digits=round)
  IQR <- paste0( IQR[ 1 ] , " (" , IQR[ 2 ] , "-" , IQR[ 3 ], ")" ) 
  return(IQR)
}

# función para hacer NAs-------------------------------------------------------
replace_with_na_all_2 <- function(df, formule) {
  df[rlang::as_function(formule)(df)] <- NA
  df
}

# Definir vacio----------------------------------------------------------------
vacio <- c("", NA, NULL, "[Vacio]", Inf)
valores_na <- c("", " ", NULL, "[Vacio]", Inf)

# función que calcula la moda--------------------------------------------------
get_mode <- function(x, na.rm = FALSE) {
    if(na.rm){
      x = x[!is.na(x)]
    }
    
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }

nearest <- function(x, vector = duracion_ok) {
if(is.na(x)) return(NA)
vector[which.min(abs(x-vector))] 
}

# función para colorear un rmarkdown independientemente de si es pdf o html----
colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, 
            x)
  } else x
}

# función para hacer ci de una normal------------------------------------------
hacer_ci <- function(x, round = 2){
 if (length(x) > 1){
 paste0(round(ci(x)[1], 2),
 "(", max(0, round(ci(x)[2], round)), "-", min(1, round(ci(x)[3], round)), ")")
} else{ paste0(round(x, round), "(", round(x, round), "-", round(x, round), ")")}
  }

#IC a mano---------------------------------------------------------------------
ic_nri <- function(casos_mal_rec, casos_bien_rec, n, alpha = 0.05) {
  
  # proporción
  p_hat <- (casos_bien_rec + casos_mal_rec)/n
  p <- (casos_bien_rec - casos_mal_rec)/n
  
  # z-score
  z = qnorm(1-alpha/2)
  
  # Compute the CI
  ci_p <- p + c(-1,1) * (z * sqrt(p_hat * (1-p_hat)/n) + 0.5/n)
  
  ci_p
}

#IC a mano---------------------------------------------------------------------
ic_nri2 <- function(casos_mal, casos_bien, n, alpha = 0.05) {
  
  # z-score
  z <- qnorm(1-alpha/2)
  
  # p
  p_bien <- casos_bien/n
  p_mal <- casos_mal/n
  p <- (casos_bien - casos_mal)/n
  
  # s
  a <- (casos_bien + casos_mal)/(n^2)
  b1 <- (casos_bien - casos_mal)^2
  b2 <- b1/(n^3)
  s <- sqrt(a-b2)
  
  # Compute the CI
  ci_p <- p + c(-1, 1) * (z * s)
  ci_p
}

ic_nri3 <- function(casos_mal, casos_bien, n, alpha = 0.05) {
  
  # z-score
  z <- qnorm(1-alpha/2)
  
  # p
  p_bien <- casos_bien/n
  p_mal <- casos_mal/n
  p <- (casos_bien - casos_mal)/n
  
  # s
  a <- (casos_bien + casos_mal)/(n^2)
  b1 <- (casos_bien - casos_mal)^2
  b2 <- b1/(n^3)
  s <- sqrt(a-b2)
  
  # Compute the CI
  ci_p <- format(round(100 * (p + c(-1, 1) * (z * s)), 2), nsmall = 2)
  ci_p2 <- glue("{format(round(100 * p, 2), nsmall = 2)}% [{ci_p[1]}%, {ci_p[2]}%]")
  ci_p2
}

# Función cargar datos y pasar fechas IDATE a Date
fread2 <- function(x, ...){
  fread(x, ...) %>% 
    mutate_if(is.Date, ymd)
}

# crear función que separe los millares
pretty2 <- purrr::partial(prettyNum, big.mark = "&hairsp;")
nrow2 <- compose(nrow, pretty2, .dir = "forward")
