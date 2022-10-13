# Definir path-----------------------------------------------------------------
path_opioides <- file.path( "y:", "OPIOIDES_SD2054")
path_origenes <- file.path( path_opioides, "1-ORIGENES", "TXT")
path_intermedias <- file.path( path_opioides, "3-INTERMEDIAS")
path_intermedias_raiz <- file.path( path_opioides, "3-INTERMEDIAS")

path_intermedias_pres <- file.path( path_intermedias, "PRESCRIPCIONES")
path_intermedias_pres_A <- file.path( path_intermedias_pres, "AINES")
path_intermedias_pres_B <- file.path( path_intermedias_pres, "ANALGESICOS")
path_intermedias_pres_C <- file.path( path_intermedias_pres, "ANSIOLITICOS")
path_intermedias_pres_D <- file.path( path_intermedias_pres, "HIPNOTICOS")
path_intermedias_pres_E <- file.path( path_intermedias_pres, "BENZODIACEPINAS")
path_intermedias_pres_F <- file.path( path_intermedias_pres, "ACIDO_VALPROICO")
path_intermedias_pres_G <- file.path( path_intermedias_pres, "GABAPENTINOIDES")
path_intermedias_pres_H <- file.path( path_intermedias_pres, "ANTIPSICOTICOS")
path_intermedias_pres_I <- file.path( path_intermedias_pres, "OPIOIDES")


path_intermedias_disp <- file.path( path_intermedias, "DISPENSACIONES")
path_intermedias_disp_A <- file.path( path_intermedias_disp, "AINES")
path_intermedias_disp_B <- file.path( path_intermedias_disp, "ANALGESICOS")
path_intermedias_disp_C <- file.path( path_intermedias_disp, "ANSIOLITICOS")
path_intermedias_disp_D <- file.path( path_intermedias_disp, "HIPNOTICOS")
path_intermedias_disp_E <- file.path( path_intermedias_disp, "BENZODIACEPINAS")
path_intermedias_disp_F <- file.path( path_intermedias_disp, "ACIDO_VALPROICO")
path_intermedias_disp_G <- file.path( path_intermedias_disp, "GABAPENTINOIDES")
path_intermedias_disp_H <- file.path( path_intermedias_disp, "ANTIPSICOTICOS")
path_intermedias_disp_I <- file.path( path_intermedias_disp, "OPIOIDES")

path_intermedias_tx <- file.path( path_intermedias, "TRATAMIENTOS")

path_finales <- file.path( path_opioides, "4-FINALES", "ORIGINALES", "GAIA")
path_finales_A <- file.path( path_finales, "AINES")
path_finales_B <- file.path( path_finales, "ANALGESICOS")
path_finales_C <- file.path( path_finales, "ANSIOLITICOS")
path_finales_D <- file.path( path_finales, "HIPNOTICOS")
path_finales_E <- file.path( path_finales, "BENZODIACEPINAS")
path_finales_F <- file.path( path_finales, "ACIDO_VALPROICO")
path_finales_G <- file.path( path_finales, "GABAPENTINOIDES")
path_finales_H <- file.path( path_finales, "ANTIPSICOTICOS")
path_finales_I <- file.path( path_finales, "OPIOIDES")


# CONSIGN
path_origenes_consign <- file.path( "Z:", "CONSIGN_SD2270", 
                                    "1-ORIGENES", "TXT")
path_intermedias_consign <- file.path( "Z:", "CONSIGN_SD2270", 
                                       "3-INTERMEDIAS", "CSV")

