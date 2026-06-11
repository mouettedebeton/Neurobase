# ============================================================
# Dashboard - Base multicentrique Neurolésés (ART)
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(scales)
library(tidyr)
library(ggsci)
library(forcats)
library(ggupset)
library(survival)

# ---- Chargement & préparation des données ------------------
df_raw <- read_rds("data.rds") |>
  select(age, diagnostic.factor, rfstdtc, centre_inclusion.factor, sex.factor, age, imc, date_admission, date_symptomes, sortie_date,
         date_deces, sortie_date, gcs_total_admission, gcs_total_prerea_admission, deces_rea, deces_total, pronostic_dichotomie,
         hta.factor, diabete.factor, tabac.factor, rankin_score, ventilation_rea.factor, ventilation_rea_duree, infection_rea.factor,
         lata.factor, edme.factor, gos_sortie, gose_sortie, delai_admission, hsa_wfns, eval_distance_score_mrs, gcs_total_detail_admission,
         hsa_fisher_modifiee, aic_nihss, tcdb_scan_classe, gcs_total_pire_24_heures, htic.factor, htic_pic_max, capteur_pic.factor, neurochirurgie.factor,
         sedation_rea.factor, sedation_rea_duree, ventilation_rea.factor, ventilation_rea_duree, ata.factor, deces_rea.factor, trachetomie.factor,
         infection_rea.factor, hsa_anevrysme_traitement_type.factor, hsa_vasospasme.factor, hsa_vasospasme_traitement,
         hsa_anevrysme_localisation, hsa_anevrysme_complication,
         eval_distance, starts_with(c("htic_traitement___", "sedation_rea_type___", "neurochirurgie_type___", "hsa_vasospasme_dg_type____", "hsa_vasospasme_traitement_type___", "hsa_anevrysme_compli_type___")),
         aic_thrombolyse, aic_thrombectomie,
         neurochirurgie_date, neurochirurgie_nombre,
         infection_rea_precoce, infection_rea_precoce_inh, infection_rea_tardive,
         evaluation_distance_date, pmo,
         hip_taille_ml, hip_taille_30ml, sortie_rea.factor)

# Conversion des dates clés
df_raw$date_admission  <- as.Date(df_raw$date_admission)
df_raw$date_symptomes  <- as.Date(df_raw$date_symptomes)
df_raw$sortie_date     <- as.Date(df_raw$sortie_date)
df_raw$date_deces      <- as.Date(df_raw$date_deces)
df_raw$neurochirurgie_date <- as.Date(df_raw$neurochirurgie_date)
df_raw$evaluation_distance_date <- as.Date(df_raw$evaluation_distance_date)

# Variables clés nettoyées
df <- df_raw |>
  mutate(
    diagnostic    = case_match(diagnostic.factor,
                               "Traumatisme cranien" ~ "TC",
                               "Hemorragie meningee" ~ "HSA",
                               "Accident vasculaire cerebral ischemique" ~ "AVCi",
                               "Accident vasculaire cerebral hemorragique" ~ "AVCh",
                               "Infection neuro-meningee" ~ "INM",
                               "Tumeur" ~ "Tumeur"),
    centre        = as.character(centre_inclusion.factor),
    sexe          = as.character(sex.factor),
    age           = as.numeric(age),
    imc           = as.numeric(imc),
    gcs_adm       = as.numeric(gcs_total_admission),
    gcs_preadm    = as.numeric(gcs_total_prerea_admission),
    deces_rea     = as.numeric(deces_rea),
    deces_total   = as.numeric(deces_total),
    pronostic     = as.character(pronostic_dichotomie),
    hta           = as.character(hta.factor),
    diabete       = case_match(diabete.factor,
                               "Non" ~ "Non",
                               "Non disponible" ~ "Non disponible",
                               "Type 1" ~ "Type 1",
                               "Type 2 insulino-requerant" ~ "Type 2",
                               "Type 2 non insulino-requerant" ~ "Type 2"),
    tabac         = as.character(tabac.factor),
    rankin_pre    = as.numeric(rankin_score),
    ventilation   = as.character(ventilation_rea.factor),
    duree_ventil  = as.numeric(ventilation_rea_duree),
    infection_rea = as.character(infection_rea.factor),
    lata          = as.character(lata.factor),
    edme          = as.character(edme.factor),
    gos_sortie    = as.numeric(gos_sortie),
    gose_sortie   = as.numeric(gose_sortie),
    mrs_distance  = as.character(eval_distance_score_mrs),
    delai_adm_h   = as.numeric(delai_admission),
    # Durée de séjour
    duree_sejour  = as.numeric(sortie_date - date_admission),
    # GCS pré-hospit (combiné)
    gcs_initial   = coalesce(as.numeric(gcs_total_prerea_admission),
                             as.numeric(gcs_total_detail_admission)),
    # Scores spécifiques
    hsa_wfns      = as.numeric(hsa_wfns),
    hsa_fisher    = as.numeric(hsa_fisher_modifiee),
    # Localisation de l'anévrysme regroupée (terminaison carotidienne classée antérieure)
    hsa_loc       = case_when(
      as.character(hsa_anevrysme_localisation) %in% c("1","2","3","4","8") ~ "Antérieure",
      as.character(hsa_anevrysme_localisation) %in% c("5","6","7")         ~ "Postérieure",
      TRUE ~ NA_character_),
    nihss         = as.numeric(aic_nihss),
    gcs_pire24    = as.numeric(gcs_total_pire_24_heures),
    # AVCh : volume de l'hématome et seuil 30 mL
    hip_vol       = as.numeric(hip_taille_ml),
    hip_30        = case_when(
      as.character(hip_taille_30ml) %in% c("Oui","1") ~ "\u2265 30 mL",
      as.character(hip_taille_30ml) %in% c("Non","0") ~ "< 30 mL",
      TRUE ~ NA_character_),
    # --- Réanimation ---
    htic_yn       = as.character(htic.factor),
    pic_max       = as.numeric(htic_pic_max),
    capteur_pic   = as.character(capteur_pic.factor),
    neurochir     = as.character(neurochirurgie.factor),
    sedation      = as.character(sedation_rea.factor),
    duree_sedation= as.numeric(sedation_rea_duree),
    ventilation   = as.character(ventilation_rea.factor),
    duree_ventil  = as.numeric(ventilation_rea_duree),
    lata          = as.character(lata.factor),
    ata           = as.character(ata.factor),
    edme          = as.character(edme.factor),
    deces_rea_f   = as.character(deces_rea.factor),
    tracheotomie  = as.character(trachetomie.factor),
    infection_rea = as.character(infection_rea.factor),
    # HTIC traitements
    htic_ttt_sedation   = as.numeric(htic_traitement___1),
    htic_ttt_curare     = as.numeric(htic_traitement___2),
    htic_ttt_normotherm = as.numeric(htic_traitement___3),
    htic_ttt_hypotherm  = as.numeric(htic_traitement___4),
    htic_ttt_dve        = as.numeric(htic_traitement___5),
    htic_ttt_craniect   = as.numeric(htic_traitement___6),
    htic_ttt_burst      = as.numeric(htic_traitement___7),
    htic_ttt_osmo       = as.numeric(htic_traitement___8),
    htic_ttt_hypocapnie = as.numeric(htic_traitement___9),
    htic_ttt_lombaire   = as.numeric(htic_traitement___11),
    # Sédation types
    sed_midazolam = as.numeric(sedation_rea_type___1),
    sed_ketamine  = as.numeric(sedation_rea_type___2),
    sed_propofol  = as.numeric(sedation_rea_type___3),
    sed_dexmede   = as.numeric(sedation_rea_type___4),
    sed_clonidine = as.numeric(sedation_rea_type___5),
    # Neurochir types
    nc_evacuation    = as.numeric(neurochirurgie_type___1),
    nc_dve           = as.numeric(neurochirurgie_type___2),
    nc_clipping      = as.numeric(neurochirurgie_type___3),
    nc_craniect_hemi = as.numeric(neurochirurgie_type___4),
    nc_autre         = as.numeric(neurochirurgie_type___5),
    nc_craniect_fp   = as.numeric(neurochirurgie_type___6),
    nc_dvp           = as.numeric(neurochirurgie_type___7),
    # Neurochirurgie : délai admission -> 1re intervention, et nombre de gestes
    delai_neurochir  = as.numeric(neurochirurgie_date - date_admission),
    nb_neurochir     = as.numeric(neurochirurgie_nombre)
  )

# Plages de dates pour slider
date_min <- min(df$date_admission, na.rm = TRUE)
date_max <- max(df$date_admission, na.rm = TRUE)

# Plages de GCS pour le slider
gcs_pire24_min <- min(df$gcs_pire24, na.rm = TRUE)
gcs_pire24_max <- max(df$gcs_pire24, na.rm = TRUE)

# Plages d'âge pour le slider
age_min <- min(df$age, na.rm = TRUE)
age_max <- max(df$age, na.rm = TRUE)

# Listes pour filtres
diagnostics_dispo <- sort(unique(na.omit(df$diagnostic)))
centres_dispo     <- sort(unique(na.omit(df$centre)))
sexes_dispo       <- sort(unique(na.omit(df$sexe)))

# ---- Comparaison inter-centres : indicateurs disponibles ----
# Liste déroulante : libellé affiché -> clé interne
comp_indicateurs <- c(
  "Effectif (N)"                       = "n",
  "Âge médian (ans)"                   = "age",
  "Mortalité en réanimation (%)"       = "deces_rea",
  "Mortalité totale (%)"               = "deces_total",
  "HTIC (%)"                           = "htic",
  "Neurochirurgie (%)"                 = "neurochir",
  "Sédation (%)"                       = "sedation",
  "Décision LATA (%)"                  = "lata",
  "Durée de séjour médiane (j)"        = "duree_sejour",
  "Durée de ventilation médiane (j)"   = "duree_ventil",
  "PIC maximale médiane (mmHg)"        = "pic_max"
)

# Calcule la valeur d'un indicateur pour un sous-groupe de données
compute_comp <- function(d, key) {
  if (nrow(d) == 0) return(NA_real_)
  switch(key,
         n            = nrow(d),
         age          = median(d$age, na.rm = TRUE),
         deces_rea    = 100 * mean(d$deces_rea_f == "Oui", na.rm = TRUE),
         deces_total  = 100 * mean(d$deces_total == 1, na.rm = TRUE),
         htic         = 100 * mean(d$htic_yn == "Oui", na.rm = TRUE),
         neurochir    = 100 * mean(d$neurochir == "Oui", na.rm = TRUE),
         sedation     = 100 * mean(d$sedation == "Oui", na.rm = TRUE),
         lata         = 100 * mean(d$lata == "Oui", na.rm = TRUE),
         duree_sejour = median(d$duree_sejour, na.rm = TRUE),
         duree_ventil = median(d$duree_ventil, na.rm = TRUE),
         pic_max      = median(d$pic_max, na.rm = TRUE),
         NA_real_)
}

# Indique si l'indicateur est un pourcentage (pour le formatage de l'axe)
comp_is_pct <- function(key) key %in% c("deces_rea","deces_total","htic",
                                        "neurochir","sedation","lata")

# ---- Palette couleurs par pathologie -----------------------
pal_patho <- c(
  "TC"  = "#E63946",
  "HSA" = "#F4A261",
  "AVCi"= "#2A9D8F",
  "AVCh"= "#E9C46A",
  "INM" = "#264653",
  "Tumeur"= "#9B5DE5"
)

pal_centre <- c(
  "Bicêtre"               = "#457B9D",
  "Pitié-Salpêtrière"     = "#1D3557",
  "Lariboisière"          = "#A8DADC",
  "Beaujon"               = "#F1FAEE",
  "Henri Mondor"          = "#E63946"
)

# Palette des centres (couleurs par défaut de ggplot, identiques au graphe des
# inclusions cumulatives) : appliquée partout où un centre est codé en couleur
pal_centre_auto <- setNames(
  scales::hue_pal()(length(centres_dispo)),
  centres_dispo
)

# Palette ordinale du score de Rankin (mRS 0 = meilleur -> 6 = décès)
pal_mrs <- c("0"="#1A9850","1"="#66BD63","2"="#A6D96A","3"="#FEE08B",
             "4"="#FDAE61","5"="#F46D43","6"="#A50026")

# Palette localisation de l'anévrysme (HSA)
pal_loc <- c("Antérieure" = "#457B9D", "Postérieure" = "#E63946")

# ---- Thème ggplot ------------------------------------------
theme_dashboard <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = "#f8f9fa", color = NA),
      panel.background = element_rect(fill = "#f8f9fa", color = NA),
      panel.grid.major = element_line(color = "#dee2e6"),
      panel.grid.minor = element_blank(),
      axis.title       = element_text(color = "#495057", size = 11),
      axis.text        = element_text(color = "#6c757d"),
      legend.position  = "bottom",
      plot.title       = element_text(face = "bold", color = "#212529", size = 12),
      strip.text       = element_text(face = "bold", color = "#495057")
    )
}

# ---- Fonctions utilitaires ---------------------------------
value_box_custom <- function(title, value, icon = NULL, color = "#457B9D") {
  div(
    style = glue::glue("background:{color};color:white;border-radius:10px;padding:18px 20px;
                        box-shadow:0 2px 8px rgba(0,0,0,.12);height:100%;"),
    div(style = "font-size:0.85rem;opacity:0.85;margin-bottom:4px;", title),
    div(style = "font-size:1.7rem;font-weight:700;line-height:1;", value)
  )
}

n_fmt <- function(x) format(x, big.mark = " ", nsmall = 0)
pct   <- function(x, n) sprintf("%.0f%%", 100 * x / n)

prop_bar_plot <- function(data, var, y_label, by_col, pal_vec = pal_patho) {
  d <- data |>
    filter(!is.na(.data[[var]]), !is.na(.data[[by_col]])) |>
    count(.data[[by_col]], val = .data[[var]]) |>
    group_by(.data[[by_col]]) |>
    mutate(pct = n / sum(n)) |>
    filter(val == "Oui")
  if (nrow(d) == 0) return(plotly_empty())
  p <- ggplot(d, aes(x = reorder(.data[[by_col]], pct), y = pct,
                     fill = .data[[by_col]],
                     text = paste0(.data[[by_col]], "<br>",
                                   round(100*pct,1), "% (N=", n, ")"))) +
    geom_col(width = 0.6) +
    scale_fill_manual(values = pal_vec, drop = FALSE) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,1)) +
    labs(x = NULL, y = y_label) + coord_flip() +
    theme_dashboard() + theme(legend.position = "none")
  ggplotly(p, tooltip = "text") |>
    layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
}

# ---- Variables pour données manquantes ----------------------
vars_missing <- list(
  "Démographie"  = c("age","sexe","imc","rankin_pre"),
  "Admission"    = c("gcs_initial","delai_adm_h","pas_admission",
                     "fc_admission"),
  "Réanimation"  = c("htic_yn","pic_max","capteur_pic","neurochir",
                     "sedation","ventilation",
                     "lata","ata","edme","deces_rea_f"),
  "Devenir"      = c("deces_total","duree_sejour","mrs_distance")
)

labels_missing <- c(
  age="Âge", sexe="Sexe", imc="IMC", rankin_pre="mRS pré-morbide",
  gcs_initial="GCS initial", delai_adm_h="Délai symptômes→admission (h)",
  pas_admission="PAS admission", pam_admission="PAM admission",
  fc_admission="FC admission", temp_admission="Température admission",
  hsa_wfns="Score WFNS", hsa_fisher="Fisher modifié",
  nihss="NIHSS", tcdb_scan_classe="Marshall (TDB)", mecanisme_trauma="Mécanisme TBI",
  htic_yn="HTIC", pic_max="PIC maximale (mmHg)", capteur_pic="Capteur PIC",
  neurochir="Neurochirurgie", sedation="Sédation",
  duree_sedation="Durée sédation (j)", ventilation="Ventilation mécanique",
  duree_ventil="Durée ventilation (j)", lata="LATA", ata="ATA",
  edme="EME", deces_rea_f="Décès en réanimation",
  pronostic="Pronostic (dichotomie)", deces_total="Décès total",
  duree_sejour="Durée de séjour (j)", mrs_distance="mRS à distance"
)

# ---- UI ----------------------------------------------------
ui <- page_navbar(
  title = tags$span(
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/3209/3209018.png",
             height = "28px", style = "margin-right:10px;vertical-align:middle;"),
    "Dashboard NB",
  ),
  theme = bs_theme(
    bootswatch  = "flatly",
    primary     = "#457B9D",
    base_font   = font_google("Inter"),
    heading_font= font_google("Inter")
  ),
  bg = "#1D3557",
  inverse = TRUE,
  
  # ─── SIDEBAR FILTRES GLOBAUX ─────────────────────────────
  sidebar = sidebar(
    width = 270,
    bg = "#f1f3f5",
    h5("Filtres globaux", style = "color:#1D3557;font-weight:700;"),
    hr(),
    checkboxGroupInput(
      "filtre_diag", "Pathologie(s)",
      choices  = diagnostics_dispo,
      selected = diagnostics_dispo
    ),
    hr(),
    checkboxGroupInput(
      "filtre_centre", "Centre(s) d'inclusion",
      choices  = centres_dispo,
      selected = centres_dispo
    ),
    hr(),
    checkboxGroupInput(
      "filtre_sexe", "Sexe",
      choices  = sexes_dispo,
      selected = sexes_dispo,
      inline   = TRUE
    ),
    hr(),
    sliderInput(
      "filtre_pire_gcs", "Pire Glasgow à H24",
      min = gcs_pire24_min,
      max = gcs_pire24_max,
      value = c(gcs_pire24_min, gcs_pire24_max),
      step = 1
    ),
    sliderInput(
      "filtre_age", "Âge à l'inclusion",
      min = age_min,
      max = 105,
      value = c(age_min, 105)
    ),
    dateRangeInput(
      "filtre_dates", "Période d'admission",
      start = date_min,
      end   = date_max,
      min   = date_min,
      max   = date_max,
      language = "fr",
      separator = "→",
      format = "dd-mm-yyyy"
    ),
    sliderInput("filtre_dates_2",
                "",
                min = date_min,
                max = date_max,
                value=c(date_min, date_max)),
    hr(),
    actionButton("reset_filters", "↺ Réinitialiser", class = "btn-outline-secondary btn-sm w-100")
  ),
  
  # ─── ONGLET 1 : VUE GLOBALE ──────────────────────────────
  nav_panel(
    title = "📊 Vue globale",
    padding = 20,
    div(style = "min-height:90px; margin-bottom:5px;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          uiOutput("vb_n"),
          uiOutput("vb_sexe_h"),
          uiOutput("vb_deces"),
          uiOutput("vb_age")
        )),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Répartition par pathologie"),
        plotlyOutput("plot_patho_pie", height = "320px")
      ),
      card(
        card_header("Répartition par centre d'inclusion"),
        plotlyOutput("plot_centre_bar", height = "320px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Timeline des inclusions (par date d'admission)"),
        plotlyOutput("plot_timeline", height = "280px")
      ),
      card(
        card_header("Inclusions cumulatives (par centre)"),
        plotlyOutput("plot_inclusion_cum", height = "280px")
      )
    )
  ),
  # ─── ONGLET 2 : DÉMOGRAPHIE ──────────────────────────────
  nav_panel(
    title = "👤 Démographie",
    padding = 20,
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Distribution de l'âge par pathologie"),
        plotlyOutput("plot_age_box", height = "350px")
      ),
      card(
        card_header("Sexe ratio par pathologie"),
        plotlyOutput("plot_sexe", height = "350px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Antécédents — Tabac"),
        plotlyOutput("plot_tabac", height = "300px")
      ),
      card(
        card_header("Score de Rankin (mRS) pré-morbide — par pathologie"),
        plotlyOutput("plot_rankin_pre", height = "300px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Antécédents — HTA"),
        plotlyOutput("plot_hta", height = "300px")
      ),
      card(
        card_header("Antécédents — Diabète"),
        plotlyOutput("plot_diabete", height = "300px")
      )
    )
  ),
  
  # ── 3. RÉANIMATION ─────────────────────────────────────
  nav_panel("🏥 Réanimation", padding=20,
            div(style = "min-height:90px; margin-bottom:5px;",
                layout_columns(col_widths=breakpoints(sm=12, md=6, xl=3),
                               uiOutput("vb_rea_htic"), uiOutput("vb_rea_neurochir"),
                               uiOutput("vb_rea_vm"),   uiOutput("vb_rea_deces")
                )),
            navset_tab(
              
              nav_panel("HTIC", br(),
                        layout_columns(col_widths=breakpoints(sm=12, xl=6),
                                       card(card_header("Capteur PIC & HTIC — par pathologie"),
                                            plotlyOutput("plot_rea_capteur_htic", height="320px")),
                                       card(card_header("PIC maximale (mmHg) — par pathologie"),
                                            plotlyOutput("plot_rea_pic_max", height="320px"))
                        ),
                        layout_columns(col_widths=12,
                                       card(card_header("Combinaisons de traitements de l'HTIC — par centre"),
                                            plotOutput("plot_rea_htic_ttt", height="430px"))
                        )
              ),
              
              nav_panel("Neurochirurgie", br(),
                        layout_columns(col_widths=breakpoints(sm=12, xl=6),
                                       card(card_header("Recours neurochirurgie — par pathologie"),
                                            plotlyOutput("plot_rea_neurochir_yn", height="300px")),
                                       card(card_header("Types de gestes"),
                                            plotlyOutput("plot_rea_neurochir_types", height="300px"))
                        ),
                        layout_columns(col_widths=breakpoints(sm=12, xl=6),
                                       card(card_header("Délai admission → 1re neurochirurgie (jours)"),
                                            plotlyOutput("plot_rea_neurochir_delai", height="300px")),
                                       card(card_header("Nombre de neurochirurgies — par pathologie"),
                                            plotlyOutput("plot_rea_neurochir_nb", height="300px"))
                        )
              ),
              
              nav_panel("Sédation & VM", br(),
                        layout_columns(col_widths=breakpoints(sm=12, xl=6),
                                       card(card_header("Molécules de sédation"),
                                            plotlyOutput("plot_rea_sed_types", height="300px")),
                                       card(card_header("Durée de sédation (jours)"),
                                            plotlyOutput("plot_rea_sed_duree", height="300px"))
                        ),
                        layout_columns(col_widths=breakpoints(sm=12, xl=6),
                                       card(card_header("Durée de ventilation mécanique (jours)"),
                                            plotlyOutput("plot_rea_vm_duree", height="300px")),
                                       card(card_header("Infections en réanimation — par pathologie"),
                                            plotlyOutput("plot_rea_infection", height="300px"))
                        )
              ),
              
              nav_panel("LAT · EME · Décès", br(),
                        layout_columns(col_widths=breakpoints(sm=12, md=6, xl=3),
                                       uiOutput("vb_rea_lata"), uiOutput("vb_rea_ata"),
                                       uiOutput("vb_rea_edme"), uiOutput("vb_rea_deces2")
                        ), br(),
                        layout_columns(col_widths=12,
                                       card(card_header("LATA & ATA — par pathologie"),
                                            plotlyOutput("plot_rea_lat_combined", height="360px"))
                        ),
                        layout_columns(col_widths=12,
                                       card(card_header("Décès, mort encéphalique & PMO — par pathologie"),
                                            plotlyOutput("plot_rea_deces_eme", height="320px"))
                        )
              )
            )
  ),
  
  # ─── ONGLET 4 : PAR PATHOLOGIE ───────────────────────────
  nav_panel(
    title = "🧠 Par pathologie",
    padding = 20,
    navset_tab(
      
      # ── TC ──────────────────────────────────────────────
      nav_panel("TC", br(),
                # Value boxes TC
                div(style = "min-height:80px; margin-bottom:8px;",
                    layout_columns(col_widths = c(3,3,3,3),
                                   uiOutput("vb_tc_n"), uiOutput("vb_tc_htic"),
                                   uiOutput("vb_tc_neurochir"), uiOutput("vb_tc_deces")
                    )
                ),
                # Rang 1 : Gravité à l'admission
                layout_columns(col_widths = c(6, 6),
                               card(card_header("Classification de Marshall / TCDB"),
                                    plotlyOutput("plot_tbi_tcdb", height = "300px")),
                               card(card_header("Évolution du GCS : admission → pire à H24"),
                                    plotlyOutput("plot_tbi_gcs", height = "300px"))
                ),
                # Rang 2 : Réanimation spécifique TC
                layout_columns(col_widths = c(6, 6),
                               card(card_header("HTIC et mesure de la PIC"),
                                    plotlyOutput("plot_tbi_htic_pic", height = "300px")),
                               card(card_header("Combinaisons de traitements de l'HTIC (TC)"),
                                    plotOutput("plot_tbi_htic_upset", height = "300px"))
                ),
                # Rang 3 : Devenir fonctionnel (mRS à distance)
                layout_columns(col_widths = 12,
                               card(card_header("Devenir : score de Rankin (mRS) à distance"),
                                    plotlyOutput("plot_tbi_mrs", height = "300px"))
                )
      ),
      
      # ── HSA ─────────────────────────────────────────────
      nav_panel("HSA", br(),
                div(style = "min-height:80px; margin-bottom:8px;",
                    layout_columns(col_widths = c(3,3,3,3),
                                   uiOutput("vb_hsa_n"), uiOutput("vb_hsa_vasospasme"),
                                   uiOutput("vb_hsa_clip_coil"), uiOutput("vb_hsa_deces")
                    )
                ),
                # Scores de gravité (stratifiés par localisation de l'anévrysme)
                layout_columns(col_widths = c(6, 6),
                               card(card_header("Score WFNS à l'admission (par localisation)"),
                                    plotlyOutput("plot_hsa_wfns", height = "280px")),
                               card(card_header("Score de Fisher modifié (par localisation)"),
                                    plotlyOutput("plot_hsa_fisher", height = "280px"))
                ),
                # Prise en charge
                layout_columns(col_widths = c(6, 6),
                               card(card_header("Type de traitement de l'anévrysme"),
                                    plotlyOutput("plot_hsa_ttt_anev2", height = "280px")),
                               card(card_header("Vasospasme : prévalence et type de diagnostic"),
                                    plotlyOutput("plot_hsa_vasospasme_dg", height = "280px"))
                ),
                # Complications du traitement endovasculaire + délai
                layout_columns(col_widths = c(6, 6),
                               card(card_header("Complications du traitement endovasculaire — par centre"),
                                    plotlyOutput("plot_hsa_compli", height = "280px")),
                               card(card_header("Délai symptômes → admission (heures)"),
                                    plotlyOutput("plot_hsa_delai", height = "280px"))
                ),
                # Devenir
                layout_columns(col_widths = c(12),
                               card(card_header("mRS à distance (ordinal) selon le grade WFNS"),
                                    plotlyOutput("plot_hsa_pronostic_wfns", height = "300px"))
                )
      ),
      
      # ── AVCi ────────────────────────────────────────────
      nav_panel("AVCi", br(),
                div(style = "min-height:80px; margin-bottom:8px;",
                    layout_columns(col_widths = c(3,3,3,3),
                                   uiOutput("vb_avci_n"), uiOutput("vb_avci_tpa"),
                                   uiOutput("vb_avci_thrombecto"), uiOutput("vb_avci_deces")
                    )
                ),
                layout_columns(col_widths = c(6, 6),
                               card(card_header("NIHSS à l'admission — distribution"),
                                    plotlyOutput("plot_avc_nihss", height = "280px")),
                               card(card_header("NIHSS — classes de gravité"),
                                    plotlyOutput("plot_avci_nihss_classe", height = "280px"))
                ),
                layout_columns(col_widths = c(6, 6),
                               card(card_header("Thrombolyse et thrombectomie"),
                                    plotlyOutput("plot_avc_ttt", height = "280px")),
                               card(card_header("Délai symptômes → admission (heures)"),
                                    plotlyOutput("plot_avci_delai", height = "280px"))
                ),
                layout_columns(col_widths = c(12),
                               card(card_header("mRS à distance (ordinal) selon la reperfusion"),
                                    plotlyOutput("plot_avci_pronostic", height = "320px"))
                )
      ),
      
      # ── AVCh ────────────────────────────────────────────
      nav_panel("AVCh", br(),
                div(style = "min-height:80px; margin-bottom:8px;",
                    layout_columns(col_widths = c(3,3,3,3),
                                   uiOutput("vb_avch_n"), uiOutput("vb_avch_htic"),
                                   uiOutput("vb_avch_neurochir"), uiOutput("vb_avch_deces")
                    )
                ),
                layout_columns(col_widths = c(6, 6),
                               card(card_header("GCS à l'admission"),
                                    plotlyOutput("plot_avch_gcs", height = "280px")),
                               card(card_header("Volume de l'hématome (mL) selon le seuil de 30 mL"),
                                    plotlyOutput("plot_avch_volume", height = "280px"))
                ),
                layout_columns(col_widths = c(6, 6),
                               card(card_header("Capteur de PIC & HTIC (effectifs)"),
                                    plotlyOutput("plot_avch_htic", height = "280px")),
                               card(card_header("Neurochirurgie — types de gestes"),
                                    plotlyOutput("plot_avch_neurochir", height = "280px"))
                ),
                layout_columns(col_widths = c(6, 6),
                               card(card_header("Délai symptômes → admission (heures)"),
                                    plotlyOutput("plot_avch_delai", height = "280px")),
                               card(card_header("mRS à distance (ordinal)"),
                                    plotlyOutput("plot_avch_pronostic", height = "280px"))
                )
      )
    )
  ),
  
  # ─── ONGLET 5 : DEVENIR ──────────────────────────────────
  nav_panel(
    title = "📈 Devenir & Mortalité",
    padding = 20,
    div(style = "min-height:90px; margin-bottom:5px;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          uiOutput("vb_suivi"),
          uiOutput("vb_deces2"),
          uiOutput("vb_edme"),
          uiOutput("vb_lata")
        )),
    navset_tab(
      
      nav_panel("Mortalité", br(),
                layout_columns(col_widths = c(6, 6),
                               card(card_header("Survie après l'admission (Kaplan-Meier) — par pathologie"),
                                    plotlyOutput("plot_km_patho", height = "520px")),
                               card(card_header("Survie après l'admission (Kaplan-Meier) — par centre"),
                                    plotlyOutput("plot_km_centre", height = "520px"))
                )
      ),
      
      nav_panel("Devenir", br(),
                layout_columns(col_widths = c(6, 6),
                               card(card_header("mRS dichotomisé (< 3 ou ≥ 3) par pathologie"),
                                    plotlyOutput("plot_pronostic", height = "360px")),
                               card(card_header("mRS détaillé par pathologie"),
                                    plotlyOutput("plot_mrs", height = "360px"))
                )
      ),
      
      nav_panel("Sortie de réa", br(),
                layout_columns(col_widths = 12,
                               card(card_header("Durée de séjour en réanimation (jours)"),
                                    plotlyOutput("plot_duree_sejour", height = "320px"))
                ),
                layout_columns(col_widths = 12,
                               card(card_header("Parcours des patients : pire GCS H24 → diagnostic → sortie de réa → mRS"),
                                    plotlyOutput("plot_sankey", height = "540px"))
                )
      )
    )
  ),
  # ── 6. DONNÉES MANQUANTES ──────────────────────────────
  nav_panel("🔍 Données manquantes", padding = 20,
            radioButtons("missing_stratif","Stratification :",
                         choices=c("Par centre"="centre",
                                   "Par pathologie"="diagnostic",
                                   "Globale"="global"),
                         selected="centre", inline=TRUE),
            navset_tab(
              nav_panel("Heatmap", br(),
                        layout_columns(col_widths=c(12),
                                       card(
                                         card_header("Heatmap — Taux de données manquantes (%)"),
                                         plotlyOutput("plot_missing_heatmap", height="90vh")
                                       ))),
              nav_panel("Table", br(),
                        layout_columns(col_widths=c(12),
                                       card(
                                         card_header("Table détaillée — Données manquantes par variable et groupe"),
                                         DTOutput("table_missing", height="90%")
                                       )))
            )),
  
  # ─── ONGLET : COMPARAISON INTER-CENTRES ──────────────────
  nav_panel(
    title = "⚖️ Comparaison centres",
    padding = 20,
    layout_columns(
      col_widths = c(5, 7),
      selectInput(
        "comp_var", "Indicateur à comparer entre centres",
        choices = comp_indicateurs
      ),
      radioButtons(
        "comp_strat", "Affichage",
        choices = c("Tous centres confondus" = "global",
                    "Stratifié par pathologie" = "patho"),
        selected = "global", inline = TRUE
      )
    ),
    card(
      card_header(textOutput("comp_titre", inline = TRUE)),
      plotlyOutput("plot_comp_centre", height = "460px")
    ),
    card(
      card_header("Tableau comparatif"),
      DTOutput("table_comp_centre")
    ),
    card(
      card_header("Funnel plot — comparaison ajustée sur l'effectif (indicateurs en %)"),
      plotlyOutput("plot_funnel_centre", height = "420px")
    )
  )
)

# ---- SERVER ------------------------------------------------
server <- function(input, output, session) {
  
  # Reset filtres
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "filtre_diag",   selected = diagnostics_dispo)
    updateCheckboxGroupInput(session, "filtre_centre", selected = centres_dispo)
    updateCheckboxGroupInput(session, "filtre_sexe",   selected = sexes_dispo)
    updateSliderInput(session, "filtre_pire_gcs", value = c(gcs_pire24_min, gcs_pire24_max))
    updateSliderInput(session, "filtre_age", value = c(age_min, age_max))
    updateDateRangeInput(session, "filtre_dates",
                         start = date_min, end = date_max)
    updateSliderInput(session, "filtre_dates_2",
                      value = c(date_min,date_max))
  })
  
  # Dataset filtré réactif
  # debounce(700) : on attend 0,7 s après le dernier changement de filtre avant
  # de recalculer, ce qui évite les mises à jour multiples lors du déplacement
  # d'un slider (ex. 15 -> 14 -> 13).
  df_f <- reactive({
    req(input$filtre_diag, input$filtre_centre, input$filtre_sexe, input$filtre_dates)
    df |>
      filter(
        diagnostic %in% input$filtre_diag,
        centre     %in% input$filtre_centre,
        sexe       %in% input$filtre_sexe,
        !is.na(date_admission),
        date_admission >= input$filtre_dates[1],
        date_admission <= input$filtre_dates[2],
        date_admission >= input$filtre_dates_2[1],
        date_admission <= input$filtre_dates_2[2],
        gcs_total_pire_24_heures >= input$filtre_pire_gcs[1],
        gcs_total_pire_24_heures <= input$filtre_pire_gcs[2],
        age >= input$filtre_age[1],
        age <= input$filtre_age[2]
      )
  }) |> debounce(700)
  
  # ── Value Boxes ─────────────────────────────────────────
  output$vb_n <- renderUI({
    n <- nrow(df_f())
    value_box_custom("Patients inclus", n_fmt(n), color = "#1D3557")
  })
  output$vb_sexe_h <- renderUI({
    d <- df_f(); tot <- sum(!is.na(d$sexe))
    n <- sum(d$sexe == "M", na.rm = TRUE)
    val <- if (tot > 0) sprintf("%s (%s)", n_fmt(n), pct(n, tot)) else "—"
    value_box_custom("Patients de sexe masculin", val, color = "#457B9D")
  })
  output$vb_deces <- renderUI({
    d <- df_f()
    n <- sum(d$deces_total == 1, na.rm = TRUE)
    tot <- nrow(d)
    val <- if (tot > 0) sprintf("%s (%s)", n_fmt(n), pct(n, tot)) else "—"
    value_box_custom("Décès total", val, color = "#E63946")
  })
  output$vb_age <- renderUI({
    med <- median(df_f()$age, na.rm = TRUE)
    val <- if (!is.na(med)) sprintf("%.0f ans", med) else "—"
    value_box_custom("Âge médian", val, color = "#2A9D8F")
  })
  output$vb_deces2 <- renderUI({
    d <- df_f()
    n <- sum(d$deces_rea == 1, na.rm = TRUE)
    tot <- nrow(d)
    val <- if (tot > 0) sprintf("%s (%s)", n_fmt(n), pct(n, tot)) else "—"
    value_box_custom("Décès en réanimation", val, color = "#E63946")
  })
  output$vb_suivi <- renderUI({
    d <- df_f()
    n <- sum(d$eval_distance == 1, na.rm = TRUE)
    tot <- nrow(d |> filter(!is.na(eval_distance)))
    val <- if (tot > 0) sprintf("%s (%s)", n_fmt(n), pct(n, tot)) else "—"
    value_box_custom("Suivi à distance", val, color = "#6c757d")
  })
  output$vb_edme <- renderUI({
    n <- sum(df_f()$edme == "Oui", na.rm = TRUE)
    tot <- nrow(df_f())
    val <- if (tot > 0) sprintf("%s (%s)", n_fmt(n), pct(n, tot)) else "—"
    value_box_custom("État de mort encéphalique", val, color = "#6c757d")
  })
  output$vb_lata <- renderUI({
    n <- sum(df_f()$lata == "Oui", na.rm = TRUE)
    tot <- nrow(df_f())
    val <- if (tot > 0) sprintf("%s (%s)", n_fmt(n), pct(n, tot)) else "—"
    value_box_custom("Décision LATA", val, color = "#F4A261")
  })
  
  # Value Boxes réanimation
  output$vb_rea_htic     <- renderUI({
    n=sum(df_f()$htic_yn=="Oui",na.rm=TRUE); tot=nrow(df_f())
    value_box_custom("HTIC", sprintf("%s (%s)",n_fmt(n),pct(n,tot)), color = "#E63946")
  })
  output$vb_rea_neurochir<- renderUI({
    n=sum(df_f()$neurochir=="Oui",na.rm=TRUE); tot=nrow(df_f())
    value_box_custom("Neurochirurgie", sprintf("%s (%s)",n_fmt(n),pct(n,tot)), color = "#9B5DE5")
  })
  output$vb_rea_vm       <- renderUI({
    med=median(df_f()$duree_ventil,na.rm=TRUE)
    value_box_custom("Durée VM médiane", if(!is.na(med)) sprintf("%.0f j",med) else "—", color = "#457B9D")
  })
  output$vb_rea_deces    <- renderUI({
    n=sum(df_f()$deces_rea_f=="Oui",na.rm=TRUE); tot=nrow(df_f())
    value_box_custom("Décès en réa", sprintf("%s (%s)",n_fmt(n),pct(n,tot)), color = "#1D3557")
  })
  output$vb_rea_lata     <- renderUI({
    n=sum(df_f()$lata=="Oui",na.rm=TRUE); tot=nrow(df_f())
    value_box_custom("LATA", sprintf("%s (%s)",n_fmt(n),pct(n,tot)), color = "#F4A261")
  })
  output$vb_rea_ata      <- renderUI({
    n=sum(df_f()$ata=="Oui",na.rm=TRUE); tot=nrow(df_f())
    value_box_custom("ATA", sprintf("%s (%s)",n_fmt(n),pct(n,tot)), color = "#E9C46A")
  })
  output$vb_rea_edme     <- renderUI({
    n=sum(df_f()$edme=="Oui",na.rm=TRUE); tot=nrow(df_f())
    value_box_custom("EME", sprintf("%s (%s)",n_fmt(n),pct(n,tot)), color = "#6c757d")
  })
  output$vb_rea_deces2   <- renderUI({
    n=sum(df_f()$deces_rea_f=="Oui",na.rm=TRUE); tot=nrow(df_f())
    value_box_custom("Décès en réa", sprintf("%s (%s)",n_fmt(n),pct(n,tot)), color = "#E63946")
  })
  
  # ── Vue globale ─────────────────────────────────────────
  output$plot_patho_pie <- renderPlotly({
    d <- df_f() |> filter(!is.na(diagnostic)) |> count(diagnostic) |>
      mutate(pct = n / sum(n))
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = reorder(diagnostic, n), y = n, fill = diagnostic,
                       text = paste0(diagnostic, "<br>N = ", n,
                                     " (", round(100*pct, 1), "%)"))) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Nombre de patients") +
      theme_dashboard() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_centre_bar <- renderPlotly({
    d <- df_f() |> count(centre, diagnostic) |>
      filter(!is.na(centre), !is.na(diagnostic)) |>
      group_by(centre) |>
      mutate(pct = n / sum(n)) |>
      ungroup()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = centre, y = pct, fill = diagnostic,
                       text = paste0(centre, "<br>", diagnostic, "<br>",
                                     round(100*pct, 1), "% (N=", n, ")"))) +
      geom_col(position = "fill", width = 0.6) +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      scale_y_continuous(labels = percent_format()) +
      labs(x = NULL, y = "Proportion", fill = NULL) +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_timeline <- renderPlotly({
    d <- df_f() |>
      filter(!is.na(date_admission)) |>
      mutate(mois = floor_date(date_admission, "month")) |>
      count(mois, diagnostic)
    p <- ggplot(d, aes(x = mois, y = n, fill = diagnostic,
                       text = paste0(format(mois, "%b %Y"), "<br>", diagnostic, "<br>N = ", n))) +
      geom_col(position = "stack") +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      labs(x = NULL, y = "Inclusions / mois", fill = NULL) +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_inclusion_cum <- renderPlotly({
    d <- df_f() |>
      select(rfstdtc, diagnostic.factor, centre_inclusion.factor) |>
      filter(!is.na(centre_inclusion.factor)) |>
      group_by(rfstdtc, diagnostic.factor, centre_inclusion.factor) |>
      count() |>
      ungroup() |>
      mutate(nb_inclusions_cumulatives = cumsum(n)) |>
      group_by(centre_inclusion.factor) |>
      mutate(nb_inclusions_cumulatives_par_centre = cumsum(n)) |>
      ungroup() |>
      group_by(diagnostic.factor) |>
      mutate(nb_inclusions_cumulatives_par_diagnostic = cumsum(n))
    
    p <- ggplot(d, aes(x = rfstdtc, y = nb_inclusions_cumulatives_par_centre, col = centre_inclusion.factor)) +
      geom_line() +
      scale_color_manual(values = pal_centre_auto, drop = FALSE) +
      labs(x = "Date d'inclusion", y = "Nombre d'inclusions", col = "Centre d'inclusion")
    
    ggplotly(p) |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  # ── Démographie ─────────────────────────────────────────
  output$plot_age_box <- renderPlotly({
    d <- df_f() |> filter(!is.na(age), !is.na(diagnostic))
    p <- ggplot(d, aes(x = reorder(diagnostic, age, median, na.rm = TRUE),
                       y = age, fill = diagnostic,
                       text = paste0(diagnostic, "<br>Âge = ", round(age, 1), " ans"))) +
      geom_boxplot(alpha = 0.8, outlier.shape = 21) +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      labs(x = NULL, y = "Âge (ans)", fill = NULL) +
      scale_y_continuous(limits = c(0,100)) +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_sexe <- renderPlotly({
    d <- df_f() |>
      filter(!is.na(sexe), !is.na(diagnostic)) |>
      count(diagnostic, sexe) |>
      group_by(diagnostic) |>
      mutate(pct = n / sum(n))
    p <- ggplot(d, aes(x = reorder(diagnostic, -pct, max),
                       y = pct, fill = sexe,
                       text = paste0(sexe, "<br>", round(100*pct,1), "%  (N=", n, ")"))) +
      geom_col(position = "fill", width = 0.65) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_manual(values = c("F" = "#E63946", "M" = "#457B9D")) +
      labs(x = NULL, y = "Proportion", fill = "Sexe") +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  antecedent_plot <- function(var, titre) {
    renderPlotly({
      d <- df_f() |>
        filter(!is.na(.data[[var]]), !is.na(diagnostic)) |>
        count(diagnostic, val = .data[[var]]) |>
        group_by(diagnostic) |>
        mutate(pct = n / sum(n))
      p <- ggplot(d, aes(x = diagnostic, y = pct, fill = val,
                         text = paste0(val, "<br>", round(100*pct,1), "% (N=", n, ")"))) +
        geom_col(position = "fill", width = 0.65) +
        scale_y_continuous(labels = percent_format()) +
        labs(x = NULL, y = "Proportion", fill = titre, title = titre) +
        theme_dashboard() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9))
      ggplotly(p, tooltip = "text") |>
        layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
    })
  }
  output$plot_hta     <- antecedent_plot("hta", "HTA")
  output$plot_diabete <- antecedent_plot("diabete", "Diabète")
  output$plot_tabac   <- antecedent_plot("tabac", "Tabac")
  
  # ── Devenir ─────────────────────────────────────────────
  output$plot_pronostic <- renderPlotly({
    d <- df_f() |>
      filter(!is.na(pronostic), !is.na(diagnostic)) |>
      mutate(pronostic = factor(pronostic, levels = c("Bon pronostic", "Mauvais pronostic", "Décès"))) |>
      filter(!is.na(pronostic)) |>
      count(diagnostic, pronostic) |>
      group_by(diagnostic) |>
      mutate(pct = n / sum(n)) |>
      ungroup()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = diagnostic, y = pct, fill = pronostic,
                       text = paste0(diagnostic, "<br>", pronostic, "<br>",
                                     round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = position_fill(reverse = TRUE), width = 0.7) +
      scale_fill_manual(values = c("Bon pronostic" = "#1A9850",
                                   "Mauvais pronostic" = "#FDAE61",
                                   "Décès" = "#A50026"), name = NULL) +
      scale_y_continuous(labels = percent_format()) +
      coord_flip() +
      labs(x = NULL, y = "Proportion") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_mrs <- renderPlotly({
    d <- df_f() |>
      filter(!is.na(mrs_distance), !is.na(diagnostic)) |>
      mutate(mrs = factor(mrs_distance, levels = as.character(0:6))) |>
      filter(!is.na(mrs)) |>
      count(diagnostic, mrs) |>
      group_by(diagnostic) |>
      mutate(pct = n / sum(n)) |>
      ungroup()
    if (nrow(d) == 0) return(plotly_empty())
    # Shift plot : barre horizontale empilée, échelle ordinale complète mRS 0 -> 6
    p <- ggplot(d, aes(x = diagnostic, y = pct, fill = mrs,
                       text = paste0(diagnostic, "<br>mRS ", mrs,
                                     "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = position_fill(reverse = TRUE), width = 0.7) +
      scale_fill_manual(values = pal_mrs, drop = FALSE, name = "mRS") +
      scale_y_continuous(labels = percent_format()) +
      coord_flip() +
      labs(x = NULL, y = "Proportion") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_duree_sejour <- renderPlotly({
    d <- df_f() |> filter(!is.na(duree_sejour), !is.na(diagnostic), duree_sejour >= 0)
    p <- ggplot(d, aes(x = reorder(diagnostic, duree_sejour, median, na.rm = TRUE),
                       y = duree_sejour, fill = diagnostic,
                       text = paste0(diagnostic, "<br>Durée = ", duree_sejour, " j"))) +
      geom_boxplot(alpha = 0.8, outliers = FALSE) +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      scale_y_continuous(limits = c(0,100)) +
      labs(x = NULL, y = "Durée de séjour (jours)", fill = NULL) +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")
    
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  # ═══════ PAR PATHOLOGIE ═══════════════════════════════
  
  # ── TC ──────────────────────────────────────────────────
  df_tbi <- reactive({ df_f() |> filter(diagnostic == "TC") })
  
  output$vb_tc_n        <- renderUI({
    n <- nrow(df_tbi())
    value_box_custom("Patients TC", n_fmt(n), color = "#E63946")
  })
  output$vb_tc_htic     <- renderUI({
    n <- sum(df_tbi()$htic_yn == "Oui", na.rm = TRUE); tot <- nrow(df_tbi())
    value_box_custom("HTIC", sprintf("%s (%s)", n_fmt(n), pct(n, tot)), color = "#c1121f")
  })
  output$vb_tc_neurochir <- renderUI({
    n <- sum(df_tbi()$neurochir == "Oui", na.rm = TRUE); tot <- nrow(df_tbi())
    value_box_custom("Neurochirurgie", sprintf("%s (%s)", n_fmt(n), pct(n, tot)), color = "#9B5DE5")
  })
  output$vb_tc_deces    <- renderUI({
    n <- sum(df_tbi()$deces_rea_f == "Oui", na.rm = TRUE); tot <- nrow(df_tbi())
    value_box_custom("Décès réa", sprintf("%s (%s)", n_fmt(n), pct(n, tot)), color = "#6c757d")
  })
  
  output$plot_tbi_tcdb <- renderPlotly({
    d <- df_tbi() |>
      mutate(tcdb = factor(tcdb_scan_classe)) |>
      filter(!is.na(tcdb)) |>
      count(tcdb)
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = tcdb, y = n, fill = tcdb,
                       text = paste0("Classe Marshall ", tcdb, "<br>N = ", n))) +
      geom_col(width = 0.65) +
      labs(x = "Classe TCDB / Marshall", y = "Nombre de patients") +
      guides(fill = "none") +
      scale_fill_nejm() +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_tbi_gcs <- renderPlotly({
    d <- df_tbi() |> filter(!is.na(gcs_initial), !is.na(gcs_pire24))
    if (nrow(d) == 0) return(plotly_empty())
    d <- d |>
      mutate(evol = dplyr::case_when(gcs_pire24 < gcs_initial ~ "Aggravation",
                                     gcs_pire24 > gcs_initial ~ "Amélioration",
                                     TRUE ~ "Stable")) |>
      count(gcs_initial, gcs_pire24, evol)
    p <- ggplot(d, aes(x = gcs_initial, y = gcs_pire24, color = evol, size = n,
                       text = paste0("GCS admission : ", gcs_initial,
                                     "<br>Pire GCS H24 : ", gcs_pire24,
                                     "<br>", evol, " — ", n, " patient(s)"))) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#adb5bd") +
      geom_point(alpha = 0.75) +
      scale_color_manual(values = c("Aggravation" = "#E63946", "Stable" = "#457B9D",
                                    "Amélioration" = "#2A9D8F"), name = NULL) +
      scale_size_area(max_size = 9, guide = "none") +
      scale_x_continuous(breaks = 3:15, limits = c(2.5, 15.5)) +
      scale_y_continuous(breaks = 3:15, limits = c(2.5, 15.5)) +
      labs(x = "GCS à l'admission", y = "Pire GCS à H24") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_tbi_gcs_pire24 <- renderPlotly({
    d <- df_tbi() |>
      filter(!is.na(gcs_pire24)) |>
      count(gcs = factor(gcs_pire24, levels = 3:15))
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = gcs, y = n,
                       text = paste0("GCS pire H24 = ", gcs, "<br>N = ", n))) +
      geom_col(width = 0.8, fill = "#c1121f") +
      labs(x = "GCS pire à H24", y = "Nombre de patients") +
      scale_x_discrete(drop = FALSE) +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_tbi_htic_pic <- renderPlotly({
    d <- df_tbi() |>
      filter(!is.na(htic_yn)) |>
      count(htic_yn) |>
      mutate(pct = n / sum(n))
    n_capteur <- sum(df_tbi()$capteur_pic == "Oui", na.rm = TRUE)
    med_pic   <- median(df_tbi()$pic_max, na.rm = TRUE)
    p <- ggplot(d, aes(x = htic_yn, y = pct, fill = htic_yn,
                       text = paste0(htic_yn, "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(width = 0.55) +
      scale_fill_manual(values = c("Oui" = "#c1121f", "Non" = "#2A9D8F")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = NULL, y = "Proportion",
           subtitle = sprintf("Capteur PIC : %d patients | PIC max médiane : %.0f mmHg",
                              n_capteur, ifelse(is.na(med_pic), 0, med_pic))) +
      theme_dashboard() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  # Upset TC — ggplot statique (ggupset incompatible avec plotly)
  output$plot_tbi_htic_upset <- renderPlot({
    ttt_labels <- c(
      htic_ttt_sedation   = "Sédation",
      htic_ttt_curare     = "Curarisation",
      htic_ttt_normotherm = "Normothermie",
      htic_ttt_hypotherm  = "Hypothermie",
      htic_ttt_dve        = "DVE",
      htic_ttt_craniect   = "Craniectomie",
      htic_ttt_burst      = "Burst-suppression",
      htic_ttt_osmo       = "Osmothérapie",
      htic_ttt_hypocapnie = "Hypocapnie",
      htic_ttt_lombaire   = "DVL"
    )
    ttt_v <- names(ttt_labels)
    d <- df_tbi() |>
      filter(htic_yn == "Oui") |>
      rowwise() |>
      mutate(tx_set = list(unname(ttt_labels[ttt_v[which(c_across(all_of(ttt_v)) == 1)]]))) |>
      ungroup()
    if (nrow(d) == 0 || all(lengths(d$tx_set) == 0))
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Aucune donnée HTIC TC",
                                 size=5, color="#6c757d") + theme_void())
    ggplot(d, aes(x = tx_set, fill = centre)) +
      geom_bar(width = 0.7) +
      scale_x_upset(n_intersections = 12, n_sets = 10) +
      scale_fill_manual(values = pal_centre_auto, name = "Centre") +
      labs(x = NULL, y = "Nombre de patients") +
      theme_dashboard() +
      theme(axis.text.x = element_text(size = 8),
            legend.position = "right")
  }, res = 96)
  
  output$plot_tbi_mrs <- renderPlotly({
    d <- df_tbi() |>
      filter(!is.na(mrs_distance)) |>
      mutate(mrs = factor(mrs_distance, levels = as.character(0:6))) |>
      filter(!is.na(mrs)) |>
      count(mrs) |>
      mutate(pct = n / sum(n), grp = "TC")
    if (nrow(d) == 0) return(plotly_empty())
    # Shift plot : barre unique 100 % empilée, mRS 0 (meilleur) -> 6 (décès)
    p <- ggplot(d, aes(x = grp, y = pct, fill = mrs,
                       text = paste0("mRS ", mrs, "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = position_fill(reverse = TRUE), width = 0.5) +
      scale_fill_manual(values = pal_mrs, drop = FALSE, name = "mRS") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      coord_flip() +
      labs(x = NULL, y = "Proportion") +
      theme_dashboard() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  # ── HSA ─────────────────────────────────────────────────
  df_hsa <- reactive({ df_f() |> filter(diagnostic == "HSA") })
  
  output$vb_hsa_n          <- renderUI({
    value_box_custom("Patients HSA", n_fmt(nrow(df_hsa())), color = "#F4A261")
  })
  output$vb_hsa_vasospasme  <- renderUI({
    n <- sum(df_hsa()$hsa_vasospasme.factor == "Oui", na.rm = TRUE); tot <- nrow(df_hsa())
    value_box_custom("Vasospasme", sprintf("%s (%s)", n_fmt(n), pct(n, tot)), color = "#e76f51")
  })
  output$vb_hsa_clip_coil   <- renderUI({
    d <- df_hsa()
    n_clip <- sum(d$nc_clipping == 1, na.rm = TRUE)
    n_coil <- sum(grepl("coil|embol", as.character(d$hsa_anevrysme_traitement_type.factor),
                        ignore.case = TRUE), na.rm = TRUE)
    value_box_custom("Clipping / Coiling",
                     sprintf("%d / %d", n_clip, n_coil), color = "#e9c46a")
  })
  output$vb_hsa_deces       <- renderUI({
    n <- sum(df_hsa()$deces_rea_f == "Oui", na.rm = TRUE); tot <- nrow(df_hsa())
    value_box_custom("Décès réa", sprintf("%s (%s)", n_fmt(n), pct(n, tot)), color = "#6c757d")
  })
  
  output$plot_hsa_wfns <- renderPlotly({
    d <- df_hsa() |>
      filter(!is.na(hsa_wfns), !is.na(hsa_loc)) |>
      count(wfns = factor(hsa_wfns, levels = 1:5), loc = hsa_loc)
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = wfns, y = n, fill = loc,
                       text = paste0("WFNS ", wfns, " \u2014 ", loc, "<br>N = ", n))) +
      geom_col(position = "dodge", width = 0.7) +
      scale_x_discrete(drop = FALSE) +
      scale_fill_manual(values = pal_loc, drop = FALSE, name = NULL) +
      labs(x = "Grade WFNS", y = "Nombre de patients") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_hsa_fisher <- renderPlotly({
    d <- df_hsa() |>
      filter(!is.na(hsa_fisher), !is.na(hsa_loc)) |>
      count(fisher = factor(hsa_fisher, levels = 0:4), loc = hsa_loc)
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = fisher, y = n, fill = loc,
                       text = paste0("Fisher ", fisher, " \u2014 ", loc, "<br>N = ", n))) +
      geom_col(position = "dodge", width = 0.7) +
      scale_x_discrete(drop = FALSE) +
      scale_fill_manual(values = pal_loc, drop = FALSE, name = NULL) +
      labs(x = "Score de Fisher modifié", y = "Nombre de patients") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  # Traitement anévrysme (gardé depuis l'ancien plot_hsa_ttt_anev mais référencé dans le nouvel UI)
  output$plot_hsa_ttt_anev2 <- renderPlotly({
    d <- df_hsa() |>
      filter(!is.na(hsa_anevrysme_traitement_type.factor)) |>
      count(ttt = as.character(hsa_anevrysme_traitement_type.factor)) |>
      mutate(pct = n / sum(n))
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = reorder(ttt, n), y = pct, fill = ttt,
                       text = paste0(ttt, "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(width = 0.65) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      coord_flip() +
      labs(x = NULL, y = "Proportion") +
      theme_dashboard() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_hsa_vasospasme_dg <- renderPlotly({
    # Types de diagnostic du vasospasme (variables hsa_vasospasme_dg_type____)
    vsp_vars   <- grep("hsa_vasospasme_dg_type", names(df_hsa()), value = TRUE)
    vsp_labels <- setNames(vsp_vars, gsub("hsa_vasospasme_dg_type____", "Type ", vsp_vars))
    d <- df_hsa() |> filter(hsa_vasospasme.factor == "Oui")
    if (nrow(d) == 0 || length(vsp_vars) == 0) {
      # Fallback : juste prévalence oui/non
      dd <- df_hsa() |> filter(!is.na(hsa_vasospasme.factor)) |>
        count(vasospasme = as.character(hsa_vasospasme.factor)) |>
        mutate(pct = n / sum(n))
      p <- ggplot(dd, aes(x = vasospasme, y = pct, fill = vasospasme,
                          text = paste0(vasospasme, "<br>", round(100*pct,1), "% (N=", n, ")"))) +
        geom_col(width = 0.5) +
        scale_fill_manual(values = c("Oui" = "#e76f51", "Non" = "#2A9D8F")) +
        scale_y_continuous(labels = percent_format()) +
        labs(x = NULL, y = "Proportion") + theme_dashboard() + theme(legend.position = "none")
      return(ggplotly(p, tooltip = "text") |> layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa"))
    }
    cnt <- sapply(vsp_vars, function(v) sum(d[[v]] == 1, na.rm = TRUE))
    df_v <- data.frame(type = vsp_vars, n = cnt) |> filter(n > 0) |>
      mutate(pct = n / nrow(d))
    p <- ggplot(df_v, aes(x = reorder(type, n), y = pct,
                          text = paste0(type, "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(fill = "#e76f51", width = 0.65) +
      scale_y_continuous(labels = percent_format()) +
      coord_flip() + labs(x = NULL, y = "% parmi les vasospasmes") + theme_dashboard()
    ggplotly(p, tooltip = "text") |> layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_hsa_vasospasme <- renderPlotly({
    d <- df_hsa() |>
      filter(!is.na(hsa_vasospasme.factor)) |>
      count(vasospasme = as.character(hsa_vasospasme.factor))
    p <- plot_ly(d, labels = ~vasospasme, values = ~n, type = "pie",
                 marker = list(colors = c("#E63946", "#2A9D8F", "#dee2e6")),
                 textinfo = "label+percent",
                 hovertemplate = "%{label}<br>N = %{value}<extra></extra>") |>
      layout(paper_bgcolor = "#f8f9fa", showlegend = FALSE)
    p
  })
  
  output$plot_hsa_compli <- renderPlotly({
    d_rx <- df_hsa() |>
      filter(!is.na(centre)) |>
      mutate(endovasc = grepl("coil|embol|radio|endovas|interv",
                              as.character(hsa_anevrysme_traitement_type.factor),
                              ignore.case = TRUE)) |>
      filter(endovasc)
    if (nrow(d_rx) == 0) return(plotly_empty())
    # Types de complication (cases à cocher, choix multiples)
    compli_labels <- c(
      hsa_anevrysme_compli_type___1 = "Débord de coil",
      hsa_anevrysme_compli_type___2 = "Occlusion vasculaire",
      hsa_anevrysme_compli_type___3 = "Dissection",
      hsa_anevrysme_compli_type___4 = "Rupture",
      hsa_anevrysme_compli_type___5 = "Resaignement"
    )
    vars <- intersect(names(compli_labels), names(d_rx))
    if (length(vars) == 0) return(plotly_empty())
    den <- d_rx |> count(centre, name = "den")
    dd <- bind_rows(lapply(vars, function(v) {
      d_rx |>
        group_by(centre) |>
        summarise(n = sum(.data[[v]] == 1, na.rm = TRUE), .groups = "drop") |>
        mutate(type = unname(compli_labels[v]))
    })) |>
      left_join(den, by = "centre") |>
      mutate(pct  = ifelse(den > 0, n / den, NA_real_),
             type = factor(type, levels = unname(compli_labels)))
    if (nrow(dd) == 0) return(plotly_empty())
    p <- ggplot(dd, aes(x = centre, y = pct, fill = type,
                        text = paste0(centre, "<br>", type, "<br>",
                                      n, "/", den, " trait. endovasc. (", round(100*pct,1), "%)"))) +
      geom_col(position = "dodge", width = 0.8) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_fill_manual(values = c("Débord de coil"       = "#457B9D",
                                   "Occlusion vasculaire" = "#2A9D8F",
                                   "Dissection"           = "#E9C46A",
                                   "Rupture"              = "#E63946",
                                   "Resaignement"         = "#9B5DE5"),
                        drop = FALSE, name = "Complication") +
      labs(x = NULL, y = "Part des traitements endovasculaires") +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_hsa_delai <- renderPlotly({
    d_all <- df_hsa() |> filter(!is.na(delai_adm_h), delai_adm_h >= 0)
    if (nrow(d_all) == 0) return(plotly_empty())
    d      <- d_all |> filter(delai_adm_h <= 48)
    n_tard <- sum(d_all$delai_adm_h > 48)
    p <- ggplot(d, aes(x = delai_adm_h,
                       text = paste0("Délai = ", round(delai_adm_h, 0), " h"))) +
      geom_histogram(binwidth = 3, boundary = 0, fill = "#F4A261", color = "white") +
      scale_x_continuous(breaks = seq(0, 48, 6), limits = c(-1.5, 49.5)) +
      labs(x = "Délai symptômes → admission (heures)", y = "Nombre de patients",
           subtitle = sprintf("Fenêtre 0-48 h | %d patient(s) admis au-delà de 48 h non affichés", n_tard)) +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_hsa_pronostic_wfns <- renderPlotly({
    d <- df_hsa() |>
      filter(!is.na(mrs_distance), !is.na(hsa_wfns)) |>
      mutate(wfns_grp = factor(hsa_wfns, levels = 1:5, labels = paste("WFNS", 1:5)),
             mrs      = factor(mrs_distance, levels = as.character(0:6))) |>
      filter(!is.na(wfns_grp), !is.na(mrs)) |>
      count(wfns_grp, mrs) |>
      group_by(wfns_grp) |>
      mutate(pct = n / sum(n)) |>
      ungroup()
    if (nrow(d) == 0) return(plotly_empty())
    # mRS ordinal (shift) par grade WFNS
    p <- ggplot(d, aes(x = wfns_grp, y = pct, fill = mrs,
                       text = paste0(wfns_grp, "<br>mRS ", mrs, "<br>",
                                     round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = position_fill(reverse = TRUE), width = 0.75) +
      scale_fill_manual(values = pal_mrs, drop = FALSE, name = "mRS") +
      scale_y_continuous(labels = percent_format()) +
      coord_flip() +
      labs(x = NULL, y = "Proportion") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  # ── AVCi ────────────────────────────────────────────────
  df_avc <- reactive({ df_f() |> filter(diagnostic == "AVCi") })
  
  output$vb_avci_n          <- renderUI({
    value_box_custom("Patients AVCi", n_fmt(nrow(df_avc())), color = "#2A9D8F")
  })
  output$vb_avci_tpa        <- renderUI({
    n <- sum(df_avc()$aic_thrombolyse == 1, na.rm = TRUE); tot <- nrow(df_avc())
    value_box_custom("Thrombolyse (tPA)", sprintf("%s (%s)", n_fmt(n), pct(n, tot)), color = "#457B9D")
  })
  output$vb_avci_thrombecto <- renderUI({
    n <- sum(df_avc()$aic_thrombectomie == 1, na.rm = TRUE); tot <- nrow(df_avc())
    value_box_custom("Thrombectomie", sprintf("%s (%s)", n_fmt(n), pct(n, tot)), color = "#1D3557")
  })
  output$vb_avci_deces      <- renderUI({
    n <- sum(df_avc()$deces_rea_f == "Oui", na.rm = TRUE); tot <- nrow(df_avc())
    value_box_custom("Décès réa", sprintf("%s (%s)", n_fmt(n), pct(n, tot)), color = "#6c757d")
  })
  
  output$plot_avc_nihss <- renderPlotly({
    d <- df_avc() |> filter(!is.na(nihss))
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = nihss, text = paste0("NIHSS = ", nihss))) +
      geom_histogram(binwidth = 1, fill = "#2A9D8F", color = "white") +
      labs(x = "Score NIHSS", y = "N patients") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_avci_nihss_classe <- renderPlotly({
    d <- df_avc() |>
      filter(!is.na(nihss)) |>
      mutate(classe = case_when(
        nihss == 0            ~ "0 — Asymptomatique",
        nihss <= 4            ~ "1–4 — Mineur",
        nihss <= 15           ~ "5–15 — Modéré",
        nihss <= 20           ~ "16–20 — Sévère",
        TRUE                  ~ "≥21 — Très sévère"
      ),
      classe = factor(classe, levels = c("0 — Asymptomatique","1–4 — Mineur",
                                         "5–15 — Modéré","16–20 — Sévère","≥21 — Très sévère"))) |>
      count(classe) |>
      mutate(pct = n / sum(n))
    p <- ggplot(d, aes(x = classe, y = pct, fill = classe,
                       text = paste0(classe, "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(width = 0.65) +
      scale_fill_manual(values = c("#2A9D8F","#52b788","#E9C46A","#F4A261","#E63946")) +
      scale_y_continuous(labels = percent_format()) +
      coord_flip() + labs(x = NULL, y = "Proportion") +
      theme_dashboard() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_avc_ttt <- renderPlotly({
    tot <- nrow(df_avc())
    if (tot == 0) return(plotly_empty())
    d <- df_avc() |>
      summarise(
        Thrombolyse   = sum(aic_thrombolyse == 1, na.rm = TRUE),
        Thrombectomie = sum(aic_thrombectomie == 1, na.rm = TRUE)
      ) |>
      pivot_longer(everything(), names_to = "traitement", values_to = "n") |>
      mutate(pct = n / tot)
    p <- ggplot(d, aes(x = traitement, y = pct, fill = traitement,
                       text = paste0(traitement, "<br>", n, "/", tot,
                                     " (", round(100*pct,1), "%)"))) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = c("Thrombolyse" = "#2A9D8F", "Thrombectomie" = "#457B9D")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = NULL, y = "% de l'ensemble des AVCi") +
      theme_dashboard() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_avci_delai <- renderPlotly({
    d_all <- df_avc() |> filter(!is.na(delai_adm_h), delai_adm_h >= 0)
    if (nrow(d_all) == 0) return(plotly_empty())
    d      <- d_all |> filter(delai_adm_h <= 24)
    n_tard <- sum(d_all$delai_adm_h > 24)
    # Fenêtre 0-24 h avec repères thérapeutiques (« time is brain »)
    p <- ggplot(d, aes(x = delai_adm_h,
                       text = paste0("Délai = ", round(delai_adm_h, 1), " h"))) +
      geom_histogram(binwidth = 1, boundary = 0, fill = "#2A9D8F", color = "white") +
      geom_vline(xintercept = 4.5, linetype = "dashed", color = "#E63946") +
      geom_vline(xintercept = 6,   linetype = "dashed", color = "#1D3557") +
      scale_x_continuous(breaks = seq(0, 24, 3), limits = c(-0.5, 24.5)) +
      labs(x = "Délai symptômes → admission (heures)", y = "Nombre de patients",
           subtitle = sprintf("Fenêtre 0-24 h | repères 4,5 h (thrombolyse) et 6 h (thrombectomie) | %d patient(s) > 24 h non affichés", n_tard)) +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_avci_pronostic <- renderPlotly({
    d <- df_avc() |>
      filter(!is.na(mrs_distance)) |>
      mutate(reperfusion = case_when(
        aic_thrombectomie == 1 | aic_thrombolyse == 1 ~ "Reperfusion",
        TRUE ~ "Pas de reperfusion"),
        mrs = factor(mrs_distance, levels = as.character(0:6))) |>
      filter(!is.na(mrs)) |>
      count(reperfusion, mrs) |>
      group_by(reperfusion) |>
      mutate(pct = n / sum(n)) |>
      ungroup()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = reperfusion, y = pct, fill = mrs,
                       text = paste0(reperfusion, "<br>mRS ", mrs, "<br>",
                                     round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = position_fill(reverse = TRUE), width = 0.7) +
      scale_fill_manual(values = pal_mrs, drop = FALSE, name = "mRS") +
      scale_y_continuous(labels = percent_format()) +
      coord_flip() +
      labs(x = NULL, y = "Proportion") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  # ── AVCh ────────────────────────────────────────────────
  df_avch <- reactive({ df_f() |> filter(diagnostic == "AVCh") })
  
  output$vb_avch_n        <- renderUI({
    value_box_custom("Patients AVCh", n_fmt(nrow(df_avch())), color = "#E9C46A")
  })
  output$vb_avch_htic     <- renderUI({
    n <- sum(df_avch()$htic_yn == "Oui", na.rm = TRUE); tot <- nrow(df_avch())
    value_box_custom("HTIC", sprintf("%s (%s)", n_fmt(n), pct(n, tot)), color = "#e76f51")
  })
  output$vb_avch_neurochir <- renderUI({
    n <- sum(df_avch()$neurochir == "Oui", na.rm = TRUE); tot <- nrow(df_avch())
    value_box_custom("Neurochirurgie", sprintf("%s (%s)", n_fmt(n), pct(n, tot)), color = "#9B5DE5")
  })
  output$vb_avch_deces    <- renderUI({
    n <- sum(df_avch()$deces_rea_f == "Oui", na.rm = TRUE); tot <- nrow(df_avch())
    value_box_custom("Décès réa", sprintf("%s (%s)", n_fmt(n), pct(n, tot)), color = "#6c757d")
  })
  
  output$plot_avch_gcs <- renderPlotly({
    d <- df_avch() |>
      filter(!is.na(gcs_initial)) |>
      count(gcs = factor(gcs_initial, levels = 3:15))
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = gcs, y = n,
                       text = paste0("GCS = ", gcs, "<br>N = ", n))) +
      geom_col(fill = "#E9C46A", color = "#dee2e6", width = 0.8) +
      scale_x_discrete(drop = FALSE) +
      labs(x = "Score de Glasgow initial", y = "Nombre de patients") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_avch_htic <- renderPlotly({
    d   <- df_avch()
    tot <- nrow(d)
    if (tot == 0) return(plotly_empty())
    n_cap  <- sum(oui01(d$capteur_pic), na.rm = TRUE)
    n_htic <- sum(oui01(d$htic_yn),     na.rm = TRUE)
    # HTIC = sous-groupe des porteurs de capteur PIC -> dénominateur = n_cap
    dd <- data.frame(
      indicateur = factor(c("Capteur PIC","HTIC"), levels = c("Capteur PIC","HTIC")),
      count = c(n_cap, n_htic),
      den   = c(tot, n_cap),
      base  = c("de l'effectif AVCh","des patients avec capteur PIC")
    ) |>
      mutate(pct = ifelse(den > 0, 100*count/den, NA_real_),
             tip = paste0(indicateur, " : ", count, " patient(s)<br>",
                          ifelse(is.na(pct), "n.d.", paste0(round(pct,1), "% ", base))))
    p <- ggplot(dd, aes(x = indicateur, y = count, fill = indicateur, text = tip)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = c("Capteur PIC" = "#457B9D", "HTIC" = "#E63946")) +
      labs(x = NULL, y = "Nombre de patients") +
      theme_dashboard() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_avch_neurochir <- renderPlotly({
    d <- df_avch() |> filter(neurochir == "Oui")
    nms <- c("Évacuation hématome","DVE","Clipping","Craniect. hémisp.","Craniect. F. Post.","DVP/DVA","Autre")
    vrs <- c("nc_evacuation","nc_dve","nc_clipping","nc_craniect_hemi","nc_craniect_fp","nc_dvp","nc_autre")
    cnt <- sapply(vrs, function(v) sum(d[[v]] == 1, na.rm = TRUE))
    df_n <- data.frame(g = factor(nms, levels = rev(nms)), n = cnt)
    if (sum(cnt) == 0) return(plotly_empty())
    p <- ggplot(df_n, aes(x = g, y = n, text = paste0(g, "<br>N=", n))) +
      geom_col(fill = "#9B5DE5", width = 0.65) + coord_flip() +
      labs(x = NULL, y = "N patients") + theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_avch_delai <- renderPlotly({
    d_all <- df_avch() |> filter(!is.na(delai_adm_h), delai_adm_h >= 0)
    if (nrow(d_all) == 0) return(plotly_empty())
    d      <- d_all |> filter(delai_adm_h <= 48)
    n_tard <- sum(d_all$delai_adm_h > 48)
    p <- ggplot(d, aes(x = delai_adm_h,
                       text = paste0("Délai = ", round(delai_adm_h,0), " h"))) +
      geom_histogram(binwidth = 3, boundary = 0, fill = "#E9C46A", color = "white") +
      scale_x_continuous(breaks = seq(0, 48, 6), limits = c(-1.5, 49.5)) +
      labs(x = "Délai symptômes → admission (heures)", y = "Nombre de patients",
           subtitle = sprintf("Fenêtre 0-48 h | %d patient(s) admis au-delà de 48 h non affichés", n_tard)) +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_avch_pronostic <- renderPlotly({
    d <- df_avch() |>
      filter(!is.na(mrs_distance)) |>
      mutate(mrs = factor(mrs_distance, levels = as.character(0:6))) |>
      filter(!is.na(mrs)) |>
      count(mrs) |>
      mutate(pct = n / sum(n), grp = "AVCh")
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = grp, y = pct, fill = mrs,
                       text = paste0("mRS ", mrs, "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = position_fill(reverse = TRUE), width = 0.5) +
      scale_fill_manual(values = pal_mrs, drop = FALSE, name = "mRS") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      coord_flip() +
      labs(x = NULL, y = "Proportion") +
      theme_dashboard() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_avch_volume <- renderPlotly({
    d <- df_avch() |> filter(!is.na(hip_vol), !is.na(hip_30))
    if (nrow(d) == 0) return(plotly_empty())
    d$hip_30 <- factor(d$hip_30, levels = c("< 30 mL", "\u2265 30 mL"))
    p <- ggplot(d, aes(x = hip_30, y = hip_vol, fill = hip_30,
                       text = paste0(hip_30, "<br>Volume = ", round(hip_vol, 1), " mL"))) +
      geom_boxplot(alpha = 0.8, outlier.shape = 21) +
      scale_fill_manual(values = c("< 30 mL" = "#2A9D8F", "\u2265 30 mL" = "#E63946")) +
      labs(x = NULL, y = "Volume de l'hématome (mL)") +
      theme_dashboard() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  # ═══════ RÉANIMATION ══════════════════════════════════
  
  # HTIC & PIC
  output$plot_rea_capteur_htic <- renderPlotly({
    d <- df_f() |> filter(!is.na(diagnostic))
    if (nrow(d) == 0) return(plotly_empty())
    w <- d |>
      group_by(diagnostic) |>
      summarise(tot    = n(),
                n_cap  = sum(oui01(capteur_pic), na.rm = TRUE),
                n_htic = sum(oui01(htic_yn),     na.rm = TRUE),
                .groups = "drop")
    # HTIC = sous-groupe des porteurs de capteur PIC -> dénominateur = n_cap
    dd <- bind_rows(
      w |> transmute(diagnostic, indicateur = "Capteur PIC", count = n_cap,  den = tot,   base = "de l'effectif"),
      w |> transmute(diagnostic, indicateur = "HTIC",        count = n_htic, den = n_cap, base = "des patients avec capteur PIC")
    ) |>
      mutate(indicateur = factor(indicateur, levels = c("Capteur PIC","HTIC")),
             pct = ifelse(den > 0, 100 * count / den, NA_real_),
             tip = paste0(diagnostic, "<br>", indicateur, " : ", count, " patient(s)<br>",
                          ifelse(is.na(pct), "n.d.", paste0(round(pct, 1), "% ", base))))
    p <- ggplot(dd, aes(x = diagnostic, y = count, fill = indicateur, text = tip)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c("Capteur PIC" = "#457B9D", "HTIC" = "#E63946")) +
      labs(x = NULL, y = "Nombre de patients", fill = NULL) +
      theme_dashboard() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  output$plot_rea_pic_max <- renderPlotly({
    d <- df_f()|>filter(!is.na(pic_max),!is.na(diagnostic))
    if(nrow(d)==0) return(plotly_empty())
    p <- ggplot(d,aes(x=reorder(diagnostic,pic_max,median,na.rm=TRUE),
                      y=pic_max,fill=diagnostic,
                      text=paste0(diagnostic,"<br>PIC max=",pic_max," mmHg")))+
      geom_boxplot(alpha=0.8,outlier.shape=NA)+
      scale_fill_manual(values=pal_patho,drop=FALSE)+
      labs(x=NULL,y="PIC maximale (mmHg)")+theme_dashboard()+
      coord_cartesian(ylim = c(0,100)) +
      theme(axis.text.x=element_text(angle=30,hjust=1),legend.position="none")
    ggplotly(p,tooltip="text")|>layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  # Upset plot HTIC — NE PAS passer par ggplotly (ggupset utilise des geoms
  # spéciaux incompatibles avec la conversion plotly). On utilise renderPlot().
  output$plot_rea_htic_ttt <- renderPlot({
    ttt_labels <- c(
      htic_ttt_sedation   = "Sédation",
      htic_ttt_curare     = "Curarisation",
      htic_ttt_normotherm = "Normothermie active",
      htic_ttt_hypotherm  = "Hypothermie thérap.",
      htic_ttt_dve        = "DVE",
      htic_ttt_craniect   = "Craniectomie décomp.",
      htic_ttt_burst      = "Suppression métabol.",
      htic_ttt_osmo       = "Osmothérapie",
      htic_ttt_hypocapnie = "Hypocapnie permissive",
      htic_ttt_lombaire   = "DVL externe"
    )
    ttt_v <- names(ttt_labels)
    
    d <- df_f() |>
      filter(htic_yn == "Oui") |>
      rowwise() |>
      mutate(
        tx_set = list(unname(ttt_labels[ttt_v[which(c_across(all_of(ttt_v)) == 1)]]))
      ) |>
      ungroup()
    
    if (nrow(d) == 0 || all(lengths(d$tx_set) == 0)) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Aucune donnée",
                                 size=5, color="#6c757d") +
               theme_void())
    }
    
    ggplot(d, aes(x = tx_set, fill = centre)) +
      geom_bar(width = 0.7) +
      scale_x_upset(n_intersections = 12, n_sets = 10) +
      scale_fill_manual(values = pal_centre_auto, name = "Centre") +
      labs(x = NULL, y = "Nombre de patients") +
      theme_dashboard() +
      theme(
        axis.text.x = element_text(size = 8),
        legend.position = "right"
      )
  }, res = 96)
  
  # Neurochirurgie
  output$plot_rea_neurochir_yn <- renderPlotly({
    prop_bar_plot(df_f(),"neurochir","% avec neurochirurgie","diagnostic")
  })
  output$plot_rea_neurochir_types <- renderPlotly({
    d <- df_f()|>filter(neurochir=="Oui")
    nms <- c("Évacuation d'hématome","DVE","Clipping anévrysmal",
             "Craniectomie hémisp.","Craniectomie F.Post.","DVP/DVA","Autre")
    vrs <- c("nc_evacuation","nc_dve","nc_clipping","nc_craniect_hemi",
             "nc_craniect_fp","nc_dvp","nc_autre")
    cnt <- sapply(vrs,function(v) sum(d[[v]]==1,na.rm=TRUE))
    df_n <- data.frame(g=factor(nms,levels=rev(nms)),n=cnt)
    p <- ggplot(df_n,aes(x=g,y=n,text=paste0(g,"<br>N=",n)))+
      geom_col(fill="#9B5DE5",width=0.65)+coord_flip()+
      labs(x=NULL,y="N patients")+theme_dashboard()
    ggplotly(p,tooltip="text")|>layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  output$plot_rea_neurochir_delai <- renderPlotly({
    d <- df_f() |> filter(neurochir == "Oui", !is.na(delai_neurochir), !is.na(diagnostic))
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = reorder(diagnostic, delai_neurochir, median, na.rm = TRUE),
                       y = delai_neurochir, fill = diagnostic,
                       text = paste0(diagnostic, "<br>Délai = ", round(delai_neurochir, 1), " j"))) +
      geom_boxplot(alpha = 0.8, outlier.shape = NA) +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      coord_cartesian(ylim = c(0, 30)) +
      labs(x = NULL, y = "Délai admission → neurochirurgie (jours)") +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  output$plot_rea_neurochir_nb <- renderPlotly({
    d <- df_f() |> filter(neurochir == "Oui", !is.na(nb_neurochir), nb_neurochir >= 1, !is.na(diagnostic))
    if (nrow(d) == 0) return(plotly_empty())
    d <- d |>
      mutate(nb_cat = ifelse(nb_neurochir >= 4, "4+", as.character(nb_neurochir)),
             nb_cat = factor(nb_cat, levels = c("1","2","3","4+"))) |>
      filter(!is.na(nb_cat)) |>
      count(diagnostic, nb_cat)
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = nb_cat, y = n, fill = diagnostic,
                       text = paste0(diagnostic, "<br>", nb_cat, " geste(s) : ", n, " patient(s)"))) +
      geom_col(position = "dodge", width = 0.75) +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      labs(x = "Nombre de neurochirurgies par patient", y = "Nombre de patients", fill = NULL) +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  # Sédation & VM
  output$plot_rea_sed_yn <- renderPlotly({
    prop_bar_plot(df_f(),"sedation","% avec sédation","diagnostic")
  })
  output$plot_rea_sed_types <- renderPlotly({
    d <- df_f()|>filter(sedation=="Oui")
    mols <- c("Midazolam","Kétamine","Propofol","Dexmédétomidine","Clonidine")
    vars <- c("sed_midazolam","sed_ketamine","sed_propofol","sed_dexmede","sed_clonidine")
    cnt  <- sapply(vars,function(v) sum(d[[v]]==1,na.rm=TRUE))
    df_m <- data.frame(m=factor(mols,levels=rev(mols)),n=cnt)
    p <- ggplot(df_m,aes(x=m,y=n,text=paste0(m,"<br>N=",n)))+
      geom_col(fill="#457B9D",width=0.65)+coord_flip()+
      labs(x=NULL,y="N patients")+theme_dashboard()
    ggplotly(p,tooltip="text")|>layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  output$plot_rea_sed_duree <- renderPlotly({
    d <- df_f()|>filter(!is.na(duree_sedation),!is.na(diagnostic),sedation=="Oui")
    if(nrow(d)==0) return(plotly_empty())
    p <- ggplot(d,aes(x=reorder(diagnostic,duree_sedation,median,na.rm=TRUE),
                      y=duree_sedation,fill=diagnostic,
                      text=paste0(diagnostic,"<br>",duree_sedation," j")))+
      geom_boxplot(alpha=0.8,outlier.shape=NA)+
      scale_fill_manual(values=pal_patho,drop=FALSE)+
      labs(x=NULL,y="Durée sédation (j)")+theme_dashboard()+
      coord_cartesian(ylim = c(0,50)) +
      theme(axis.text.x=element_text(angle=30,hjust=1),legend.position="none")
    ggplotly(p,tooltip="text")|>layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  output$plot_rea_vm_duree <- renderPlotly({
    d <- df_f()|>filter(!is.na(duree_ventil),!is.na(diagnostic),ventilation=="Oui")
    if(nrow(d)==0) return(plotly_empty())
    p <- ggplot(d,aes(x=reorder(diagnostic,duree_ventil,median,na.rm=TRUE),
                      y=duree_ventil,fill=diagnostic,
                      text=paste0(diagnostic,"<br>",duree_ventil," j")))+
      geom_boxplot(alpha=0.8,outlier.shape=NA)+
      scale_fill_manual(values=pal_patho,drop=FALSE)+
      labs(x=NULL,y="Durée ventilation (j)")+theme_dashboard()+
      coord_cartesian(ylim = c(0,50)) +
      theme(axis.text.x=element_text(angle=30,hjust=1),legend.position="none")
    ggplotly(p,tooltip="text")|>layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  
  # LATA / ATA / EME / Décès
  output$plot_rea_lata       <- renderPlotly(prop_bar_plot(df_f(),"lata",       "% LATA","diagnostic"))
  output$plot_rea_ata        <- renderPlotly(prop_bar_plot(df_f(),"ata",        "% ATA", "diagnostic"))
  output$plot_rea_edme       <- renderPlotly(prop_bar_plot(df_f(),"edme",       "% EME", "diagnostic"))
  output$plot_rea_deces_patho<- renderPlotly(prop_bar_plot(df_f(),"deces_rea_f","% Décès en réa","diagnostic"))
  
  output$plot_rea_lata_strat <- renderPlotly({
    d <- df_f() |> filter(!is.na(lata), !is.na(diagnostic)) |>
      group_by(centre, diagnostic, lata) |>
      count() |>
      ungroup() |>
      group_by(centre, diagnostic) |>
      mutate (n_diag_par_centre = sum(n)) |>
      mutate (pct_lata_par_diag_par_centre = round(n / n_diag_par_centre * 100,0)) |>
      ungroup()
    
    p <- ggplot(d, aes(x = as_factor(centre), y = pct_lata_par_diag_par_centre,
                       fill = as_factor(lata), text = paste0(pct_lata_par_diag_par_centre,"%"))) +
      geom_bar(stat = "identity") +
      labs(x = NULL,y = "Pourcentage") +
      facet_grid(~ diagnostic) +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 30,hjust = 1),legend.position="none")
    ggplotly(p, tooltip="text") |> layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  
  # ═══════ DONNÉES MANQUANTES ═══════════════════════════
  
  missing_data <- reactive({
    d     <- df_f()
    strat <- input$missing_stratif
    all_v <- unlist(vars_missing)
    
    get_cat <- function(v) {
      names(vars_missing)[sapply(vars_missing, function(x) v %in% x)]
    }
    
    if (strat == "global") {
      bind_rows(lapply(all_v, function(v) {
        n_tot <- nrow(d)
        n_na  <- if(v %in% names(d)) sum(is.na(d[[v]])) else NA_integer_
        data.frame(
          categorie    = get_cat(v),
          variable     = v,
          label        = ifelse(!is.na(labels_missing[v]) && labels_missing[v]!="NA",
                                labels_missing[v], v),
          groupe       = "Global",
          n_total      = n_tot,
          n_manquant   = n_na,
          pct_manquant = if(!is.na(n_na)&&n_tot>0) round(100*n_na/n_tot,1) else NA_real_,
          stringsAsFactors = FALSE
        )
      }))
    } else {
      groupes <- sort(unique(na.omit(d[[strat]])))
      bind_rows(lapply(all_v, function(v) {
        bind_rows(lapply(groupes, function(g) {
          dg    <- d[d[[strat]]==g & !is.na(d[[strat]]),]
          n_tot <- nrow(dg)
          n_na  <- if(v %in% names(dg)) sum(is.na(dg[[v]])) else NA_integer_
          data.frame(
            categorie    = get_cat(v),
            variable     = v,
            label        = ifelse(!is.na(labels_missing[v]) && labels_missing[v]!="NA",
                                  labels_missing[v], v),
            groupe       = g,
            n_total      = n_tot,
            n_manquant   = n_na,
            pct_manquant = if(!is.na(n_na)&&n_tot>0) round(100*n_na/n_tot,1) else NA_real_,
            stringsAsFactors = FALSE
          )
        }))
      }))
    }
  })
  
  output$plot_missing_heatmap <- renderPlotly({
    d <- missing_data()
    if(nrow(d)==0) return(plotly_empty())
    p <- ggplot(d, aes(
      x    = groupe,
      y    = reorder(label, pct_manquant, mean, na.rm=TRUE),
      fill = pct_manquant,
      text = paste0(label,"\n",groupe,
                    "\nManquant : ",n_manquant,"/",n_total,
                    " (",pct_manquant,"%)")
    )) +
      geom_tile(color="white", linewidth=0.4) +
      geom_text(aes(label=ifelse(is.na(pct_manquant),"—",paste0(pct_manquant,"%"))),
                size=3.2, color="white", fontface="bold") +
      scale_fill_gradientn(
        colours  = c("#2A9D8F","#E9C46A","#E63946"),
        values   = rescale(c(0,30,100)),
        na.value = "#adb5bd",
        limits   = c(0,100),
        name     = "% manquant"
      ) +
      facet_grid(categorie ~ ., scales="free_y", space="free_y") +
      labs(x=NULL, y=NULL) + theme_dashboard() +
      theme(
        axis.text.x  = element_text(angle=35,hjust=1,size=10),
        axis.text.y  = element_text(size=10),
        strip.text.y = element_text(angle=0,hjust=0,face="bold",size=10),
        panel.spacing= unit(0.3,"lines"),
        legend.position = "right"
      )
    ggplotly(p,tooltip="text") |>
      layout(paper_bgcolor="#f8f9fa", plot_bgcolor="#f8f9fa",
             margin=list(l=220,r=80,t=20,b=100))
  })
  
  output$table_missing <- renderDT({
    d <- missing_data() |>
      select(Catégorie = categorie, Variable = label, Groupe = groupe,
             `N total`= n_total, `N manquant`= n_manquant,
             `% manquant`= pct_manquant) |>
      arrange(Catégorie, Variable, Groupe)
    datatable(d, rownames=FALSE, filter="top",
              extensions="Buttons",
              options=list(
                pageLength=20, scrollX=TRUE,
                language=list(url="//cdn.datatables.net/plug-ins/1.13.6/i18n/fr-FR.json"),
                dom="Bfrtip", buttons=c("copy","csv","excel")
              ),
              class="table-hover table-striped table-sm"
    ) |>
      formatStyle(
        "% manquant",
        background = styleInterval(c(10,30,60),
                                   c("#d4edda","#fff3cd","#ffd7a0","#f8d7da")),
        fontWeight = "bold"
      )
  })
  
  # ═══════ COMPARAISON INTER-CENTRES ════════════════════
  # Calcule l'indicateur choisi pour chaque centre (et chaque pathologie
  # si la stratification est demandée), à partir du dataset filtré.
  comp_data <- reactive({
    req(input$comp_var)
    key <- input$comp_var
    d   <- df_f() |> filter(!is.na(centre))
    if (nrow(d) == 0) return(data.frame())
    if (identical(input$comp_strat, "patho")) {
      d    <- d |> filter(!is.na(diagnostic))
      grps <- d |> distinct(centre, diagnostic)
      if (nrow(grps) == 0) return(data.frame())
      bind_rows(lapply(seq_len(nrow(grps)), function(i) {
        sub <- d |> filter(centre == grps$centre[i],
                           diagnostic == grps$diagnostic[i])
        data.frame(centre = grps$centre[i], diagnostic = grps$diagnostic[i],
                   valeur = compute_comp(sub, key), n = nrow(sub),
                   stringsAsFactors = FALSE)
      }))
    } else {
      cs <- sort(unique(d$centre))
      bind_rows(lapply(cs, function(ct) {
        sub <- d |> filter(centre == ct)
        data.frame(centre = ct, valeur = compute_comp(sub, key), n = nrow(sub),
                   stringsAsFactors = FALSE)
      }))
    }
  })
  
  output$comp_titre <- renderText({
    lbl <- names(comp_indicateurs)[comp_indicateurs == input$comp_var]
    paste0("Comparaison inter-centres — ", lbl)
  })
  
  output$plot_comp_centre <- renderPlotly({
    d <- comp_data()
    if (nrow(d) == 0 || all(is.na(d$valeur))) return(plotly_empty())
    key    <- input$comp_var
    is_pct <- comp_is_pct(key)
    ylab   <- names(comp_indicateurs)[comp_indicateurs == key]
    pal_c  <- setNames(
      colorRampPalette(c("#457B9D","#1D3557","#2A9D8F","#E9C46A","#E63946","#9B5DE5"))(length(centres_dispo)),
      centres_dispo
    )
    val_lbl <- if (is_pct) paste0(round(d$valeur, 1), "%") else round(d$valeur, 1)
    d$tip   <- paste0(d$centre,
                      if (identical(input$comp_strat, "patho")) paste0(" — ", d$diagnostic) else "",
                      "<br>", ylab, " : ", val_lbl,
                      "<br>N = ", d$n)
    p <- ggplot(d, aes(x = reorder(centre, valeur), y = valeur,
                       fill = centre, text = tip)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = pal_c, drop = FALSE) +
      labs(x = NULL, y = ylab, fill = NULL) +
      coord_flip() +
      theme_dashboard() + theme(legend.position = "none")
    if (identical(input$comp_strat, "patho"))
      p <- p + facet_wrap(~ diagnostic, scales = "free_x")
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$table_comp_centre <- renderDT({
    d <- comp_data()
    if (nrow(d) == 0) return(NULL)
    ylab     <- names(comp_indicateurs)[comp_indicateurs == input$comp_var]
    d$valeur <- round(d$valeur, 1)
    d <- if (identical(input$comp_strat, "patho")) {
      d |> select(Centre = centre, Pathologie = diagnostic,
                  !!ylab := valeur, N = n) |>
        arrange(Centre, Pathologie)
    } else {
      d |> select(Centre = centre, !!ylab := valeur, N = n) |>
        arrange(Centre)
    }
    datatable(d, rownames = FALSE,
              options = list(pageLength = 15, dom = "tip"),
              class = "table-hover table-striped table-sm")
  })
  
  # ═══════ SURVIE (KAPLAN-MEIER) ════════════════════════
  # Censure à la date de DERNIÈRES NOUVELLES réelles :
  #  - décès (réa ou constaté à distance) -> évènement, t = date_deces - admission
  #  - vivant AVEC évaluation à distance   -> censuré à evaluation_distance_date
  #  - vivant SANS évaluation à distance   -> censuré à la sortie (dernier contact connu)
  # Le statut vital au-delà de la réa n'étant connu que via l'évaluation à
  # distance, les patients sans suivi sont de vrais perdus de vue : on les
  # censure honnêtement à la sortie plutôt que de les supposer vivants.
  km_horizon <- 365
  surv_base <- reactive({
    df_f() |>
      mutate(
        suivi_dist    = (eval_distance %in% 1) & !is.na(evaluation_distance_date),
        date_contact  = as.Date(ifelse(suivi_dist, evaluation_distance_date, sortie_date),
                                origin = "1970-01-01"),
        delai_deces   = as.numeric(date_deces - date_admission),
        delai_contact = as.numeric(date_contact - date_admission),
        statut = ifelse(deces_total == 1 & !is.na(delai_deces) & delai_deces >= 0, 1L, 0L),
        statut = ifelse(is.na(statut), 0L, statut),
        tps    = ifelse(statut == 1, delai_deces, delai_contact)
      ) |>
      filter(!is.na(tps), tps >= 0)
  })
  
  # Convertit un survfit en data.frame (avec point de départ t=0) pour ggplot
  surv_to_df <- function(fit) {
    if (is.null(fit$strata)) {
      base <- data.frame(time = fit$time, surv = fit$surv,
                         n_risk = fit$n.risk, n_cens = fit$n.censor, strata = "Global")
    } else {
      lab  <- sub("^[^=]*=", "", rep(names(fit$strata), fit$strata))
      base <- data.frame(time = fit$time, surv = fit$surv,
                         n_risk = fit$n.risk, n_cens = fit$n.censor, strata = lab)
    }
    deb <- do.call(rbind, lapply(unique(base$strata), function(s)
      data.frame(time = 0, surv = 1,
                 n_risk = max(base$n_risk[base$strata == s]), n_cens = 0, strata = s)))
    out <- rbind(deb, base)
    out[order(out$strata, out$time), ]
  }
  
  km_plot <- function(by_var, pal_vec) {
    d <- surv_base()
    if (nrow(d) < 2 || all(d$statut == 0)) return(plotly_empty())
    d$grp <- d[[by_var]]
    d <- d |> filter(!is.na(grp))
    if (nrow(d) < 2) return(plotly_empty())
    fit <- survival::survfit(survival::Surv(tps, statut) ~ grp, data = d)
    sd  <- surv_to_df(fit)
    n_tot  <- nrow(d)
    n_dc   <- sum(d$statut == 1)
    n_nofu <- sum(d$statut == 0 & !d$suivi_dist)
    med_fu <- median(d$tps[d$statut == 0], na.rm = TRUE)
    # ── Courbe de survie ──
    p_curve <- ggplot(sd, aes(x = time, y = surv, color = strata, group = strata,
                              text = paste0(strata, "<br>J", round(time),
                                            "<br>Survie ", round(100 * surv, 1), "%",
                                            "<br>À risque : ", n_risk))) +
      geom_step(linewidth = 0.8) +
      scale_color_manual(values = pal_vec, drop = FALSE, name = NULL) +
      scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
      coord_cartesian(xlim = c(0, km_horizon)) +
      labs(x = NULL, y = "Survie",
           subtitle = sprintf("N=%d · décès=%d · suivi médian=%d j · %d sans suivi à distance",
                              n_tot, n_dc, ifelse(is.na(med_fu), 0, round(med_fu)), n_nofu)) +
      theme_dashboard()
    # ── Tableau du nombre à risque ──
    tp  <- seq(0, km_horizon, by = 90)
    sm  <- summary(fit, times = tp, extend = TRUE)
    risk <- data.frame(
      time   = sm$time,
      n_risk = sm$n.risk,
      strata = if (is.null(sm$strata)) "Global" else sub("^[^=]*=", "", as.character(sm$strata)),
      stringsAsFactors = FALSE)
    risk$strata <- factor(risk$strata, levels = rev(sort(unique(risk$strata))))
    p_tab <- ggplot(risk, aes(x = time, y = strata, color = strata,
                              text = paste0(strata, "<br>J", time, " : ", n_risk, " à risque"))) +
      geom_text(aes(label = n_risk), size = 3) +
      scale_color_manual(values = pal_vec, drop = FALSE, guide = "none") +
      scale_x_continuous(limits = c(0, km_horizon), breaks = tp) +
      labs(x = "Jours depuis l'admission", y = NULL, title = "Nombre à risque") +
      theme_dashboard() +
      theme(panel.grid = element_blank(), legend.position = "none",
            plot.title = element_text(size = 10))
    plotly::subplot(ggplotly(p_curve, tooltip = "text"),
                    ggplotly(p_tab,   tooltip = "text"),
                    nrows = 2, heights = c(0.74, 0.26), shareX = TRUE, titleY = TRUE) |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  }
  
  output$plot_km_patho  <- renderPlotly({ km_plot("diagnostic", pal_patho) })
  output$plot_km_centre <- renderPlotly({ km_plot("centre",     pal_centre_auto) })
  
  # ═══════ SANKEY : parcours des patients ═══════════════
  # Pire GCS H24 (catégorisé) -> diagnostic -> sortie de réa -> mRS dichotomisé
  output$plot_sankey <- renderPlotly({
    d <- df_f() |>
      filter(!is.na(gcs_pire24), !is.na(diagnostic)) |>
      mutate(
        gcs_cat = case_when(
          gcs_pire24 <= 8  ~ "GCS 3-8",
          gcs_pire24 <= 12 ~ "GCS 9-12",
          TRUE             ~ "GCS 13-15"),
        # Niveau 3 : lieu de sortie de réa ; décès sans lieu -> "Décès en réa"
        sortie = case_when(
          !is.na(sortie_rea.factor) ~ as.character(sortie_rea.factor),
          deces_total == 1          ~ "Décès en réa",
          TRUE                      ~ NA_character_),
        # Niveau 4 : devenir fonctionnel (décès = mRS 6)
        mrs_term = case_when(
          deces_total == 1                                ~ "mRS 6 (décès)",
          suppressWarnings(as.numeric(mrs_distance)) < 3  ~ "mRS 0-2",
          suppressWarnings(as.numeric(mrs_distance)) >= 3 ~ "mRS 3-5",
          TRUE ~ NA_character_)
      ) |>
      filter(!is.na(sortie), !is.na(mrs_term))
    if (nrow(d) == 0) return(plotly_empty())
    l1 <- d |> count(source = gcs_cat,    target = diagnostic)
    l2 <- d |> count(source = diagnostic, target = sortie)
    l3 <- d |> count(source = sortie,     target = mrs_term)
    links <- bind_rows(l1, l2, l3)
    nodes <- unique(c(l1$source, l1$target, l2$target, l3$target))
    idx   <- setNames(seq_along(nodes) - 1L, nodes)
    col_nodes <- dplyr::case_when(
      nodes == "mRS 0-2"                        ~ "#1A9850",
      nodes == "mRS 3-5"                        ~ "#F46D43",
      nodes %in% c("mRS 6 (décès)", "Décès en réa") ~ "#A50026",
      TRUE                                      ~ "#457B9D")
    plot_ly(
      type = "sankey", orientation = "h",
      node = list(label = nodes, pad = 15, thickness = 18,
                  color = col_nodes,
                  line = list(color = "white", width = 0.5)),
      link = list(source = unname(idx[links$source]),
                  target = unname(idx[links$target]),
                  value  = links$n)
    ) |>
      layout(paper_bgcolor = "#f8f9fa", font = list(size = 11),
             margin = list(l = 10, r = 10, t = 10, b = 10))
  })
  
  # ═══════ FUNNEL PLOT INTER-CENTRES ════════════════════
  # Évènements/effectif par centre pour l'indicateur en % sélectionné
  funnel_data <- reactive({
    key <- input$comp_var
    if (!comp_is_pct(key)) return(NULL)
    d  <- df_f() |> filter(!is.na(centre))
    ev <- switch(key,
                 deces_rea   = d$deces_rea_f == "Oui",
                 deces_total = d$deces_total == 1,
                 htic        = d$htic_yn == "Oui",
                 neurochir   = d$neurochir == "Oui",
                 sedation    = d$sedation == "Oui",
                 lata        = d$lata == "Oui")
    d$ev <- as.integer(ev)
    d |> filter(!is.na(ev)) |>
      group_by(centre) |>
      summarise(n = n(), events = sum(ev), rate = 100 * events / n, .groups = "drop")
  })
  
  output$plot_funnel_centre <- renderPlotly({
    key <- input$comp_var
    if (!comp_is_pct(key)) {
      msg <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, size = 4, color = "#6c757d",
                 label = "Funnel plot disponible uniquement pour les indicateurs en pourcentage") +
        theme_void()
      return(ggplotly(msg) |>
               layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa"))
    }
    fd <- funnel_data()
    if (is.null(fd) || nrow(fd) == 0) return(plotly_empty())
    p_bar <- 100 * sum(fd$events) / sum(fd$n)
    nseq  <- seq(1, max(fd$n), length.out = 200)
    se    <- sqrt(p_bar * (100 - p_bar) / nseq)
    lim   <- data.frame(
      n    = nseq,
      l95  = pmax(0,   p_bar - 1.96 * se),
      u95  = pmin(100, p_bar + 1.96 * se),
      l998 = pmax(0,   p_bar - 3.09 * se),
      u998 = pmin(100, p_bar + 3.09 * se)
    )
    ylab <- names(comp_indicateurs)[comp_indicateurs == key]
    p <- ggplot() +
      geom_line(data = lim, aes(x = n, y = u95),  linetype = "dashed", color = "#F4A261") +
      geom_line(data = lim, aes(x = n, y = l95),  linetype = "dashed", color = "#F4A261") +
      geom_line(data = lim, aes(x = n, y = u998), linetype = "dotted", color = "#E63946") +
      geom_line(data = lim, aes(x = n, y = l998), linetype = "dotted", color = "#E63946") +
      geom_hline(yintercept = p_bar, color = "#1D3557") +
      geom_point(data = fd, aes(x = n, y = rate, color = centre,
                                text = paste0(centre, "<br>", ylab, " : ", round(rate, 1),
                                              "%<br>N = ", n, " (", events, " évts)")),
                 size = 3) +
      scale_color_manual(values = pal_centre_auto, drop = FALSE, name = NULL) +
      coord_cartesian(ylim = c(0, 100)) +
      labs(x = "Effectif du centre (N)", y = ylab,
           subtitle = sprintf("Moyenne globale %.1f%% — limites 95%% (tirets) et 99,8%% (pointillés)", p_bar)) +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  # ═══════ GRAPHES AJOUTÉS (mRS pré-morbide, infections, fusions LAT/EME) ══
  oui01 <- function(x){ x <- as.character(x); ifelse(is.na(x), NA_integer_,
                                                     as.integer(x %in% c("Oui","1"))) }
  
  output$plot_rankin_pre <- renderPlotly({
    d <- df_f() |>
      filter(!is.na(rankin_pre), !is.na(diagnostic)) |>
      mutate(mrs = factor(rankin_pre, levels = 0:6)) |>
      filter(!is.na(mrs)) |>
      count(diagnostic, mrs) |>
      group_by(diagnostic) |>
      mutate(pct = n / sum(n)) |>
      ungroup()
    if (nrow(d) == 0) return(plotly_empty())
    p <- ggplot(d, aes(x = diagnostic, y = pct, fill = mrs,
                       text = paste0(diagnostic, "<br>mRS ", mrs,
                                     "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = position_fill(reverse = TRUE), width = 0.7) +
      scale_fill_manual(values = pal_mrs, drop = FALSE, name = "mRS") +
      scale_y_continuous(labels = percent_format()) +
      coord_flip() +
      labs(x = NULL, y = "Proportion") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_rea_infection <- renderPlotly({
    d <- df_f() |> filter(!is.na(diagnostic))
    if (nrow(d) == 0) return(plotly_empty())
    w <- d |>
      group_by(diagnostic) |>
      summarise(tot    = n(),
                n_inf  = sum(oui01(infection_rea),            na.rm = TRUE),
                n_pre  = sum(oui01(infection_rea_precoce),     na.rm = TRUE),
                n_inh  = sum(oui01(infection_rea_precoce_inh), na.rm = TRUE),
                n_tard = sum(oui01(infection_rea_tardive),     na.rm = TRUE),
                .groups = "drop")
    # Hiérarchie : précoce/tardive sous-groupes de l'infection ; inhalation sous-groupe du précoce
    dd <- bind_rows(
      w |> transmute(diagnostic, indicateur = "Infection (toute)",      count = n_inf,  den = tot,   base = "de l'effectif"),
      w |> transmute(diagnostic, indicateur = "Infection précoce",      count = n_pre,  den = n_inf, base = "des infections"),
      w |> transmute(diagnostic, indicateur = "Pneumonie d'inhalation", count = n_inh,  den = n_pre, base = "des infections précoces"),
      w |> transmute(diagnostic, indicateur = "Infection tardive",      count = n_tard, den = n_inf, base = "des infections")
    ) |>
      mutate(indicateur = factor(indicateur, levels = c("Infection (toute)","Infection précoce",
                                                        "Pneumonie d'inhalation","Infection tardive")),
             pct = ifelse(den > 0, 100 * count / den, NA_real_),
             tip = paste0(diagnostic, "<br>", indicateur, " : ", count, " patient(s)<br>",
                          ifelse(is.na(pct), "n.d.", paste0(round(pct, 1), "% ", base))))
    p <- ggplot(dd, aes(x = diagnostic, y = count, fill = indicateur, text = tip)) +
      geom_col(position = "dodge", width = 0.78) +
      scale_fill_manual(values = c("#1D3557","#457B9D","#E63946","#F4A261")) +
      labs(x = NULL, y = "Nombre de patients", fill = NULL) +
      theme_dashboard() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_rea_lat_combined <- renderPlotly({
    d <- df_f() |> filter(!is.na(diagnostic))
    if (nrow(d) == 0) return(plotly_empty())
    w <- d |>
      group_by(diagnostic) |>
      summarise(tot    = n(),
                n_lata = sum(oui01(lata), na.rm = TRUE),
                n_ata  = sum(oui01(ata),  na.rm = TRUE),
                .groups = "drop")
    # ATA = sous-groupe des LATA -> dénominateur = n_lata
    dd <- bind_rows(
      w |> transmute(diagnostic, indicateur = "LATA", count = n_lata, den = tot,    base = "de l'effectif"),
      w |> transmute(diagnostic, indicateur = "ATA",  count = n_ata,  den = n_lata, base = "des LATA")
    ) |>
      mutate(indicateur = factor(indicateur, levels = c("LATA","ATA")),
             pct = ifelse(den > 0, 100 * count / den, NA_real_),
             tip = paste0(diagnostic, "<br>", indicateur, " : ", count, " patient(s)<br>",
                          ifelse(is.na(pct), "n.d.", paste0(round(pct, 1), "% ", base))))
    p <- ggplot(dd, aes(x = diagnostic, y = count, fill = indicateur, text = tip)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c("LATA" = "#F4A261", "ATA" = "#E9C46A")) +
      labs(x = NULL, y = "Nombre de patients", fill = NULL) +
      theme_dashboard() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
  
  output$plot_rea_deces_eme <- renderPlotly({
    d <- df_f() |> filter(!is.na(diagnostic))
    if (nrow(d) == 0) return(plotly_empty())
    w <- d |>
      group_by(diagnostic) |>
      summarise(tot   = n(),
                n_dc  = sum(oui01(deces_rea_f), na.rm = TRUE),
                n_eme = sum(oui01(edme),        na.rm = TRUE),
                n_pmo = sum(oui01(pmo),         na.rm = TRUE),
                .groups = "drop")
    # Emboîtement : mort encéphalique sous-groupe des décès ; PMO sous-groupe des EME
    dd <- bind_rows(
      w |> transmute(diagnostic, indicateur = "Décès en réanimation", count = n_dc,  den = tot,   base = "de l'effectif"),
      w |> transmute(diagnostic, indicateur = "Mort encéphalique",    count = n_eme, den = n_dc,  base = "des décès en réanimation"),
      w |> transmute(diagnostic, indicateur = "PMO",                  count = n_pmo, den = n_eme, base = "des morts encéphaliques")
    ) |>
      mutate(indicateur = factor(indicateur, levels = c("Décès en réanimation","Mort encéphalique","PMO")),
             pct = ifelse(den > 0, 100 * count / den, NA_real_),
             tip = paste0(diagnostic, "<br>", indicateur, " : ", count, " patient(s)<br>",
                          ifelse(is.na(pct), "n.d.", paste0(round(pct, 1), "% ", base))))
    p <- ggplot(dd, aes(x = diagnostic, y = count, fill = indicateur, text = tip)) +
      geom_col(position = "dodge", width = 0.75) +
      scale_fill_manual(values = c("Décès en réanimation" = "#1D3557",
                                   "Mort encéphalique" = "#6c757d",
                                   "PMO" = "#2A9D8F")) +
      labs(x = NULL, y = "Nombre de patients", fill = NULL) +
      theme_dashboard() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") |>
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })
}

# ---- Lancement ---------------------------------------------
shinyApp(ui, server)