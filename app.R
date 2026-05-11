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

# ---- Chargement & préparation des données ------------------
df_raw <- readRDS("data.rds") %>%
  select(age, diagnostic.factor, rfstdtc, centre_inclusion.factor, sex.factor, age, imc, date_admission, date_symptomes, sortie_date,
         date_deces, sortie_date, gcs_total_admission, gcs_total_prerea_admission, deces_rea, deces_total, pronostic_dichotomie, 
         hta.factor, diabete.factor, tabac.factor, rankin_score, ventilation_rea.factor, ventilation_rea_duree, infection_rea.factor, 
         lata.factor, edme.factor, gos_sortie, gose_sortie, delai_admission, hsa_wfns, eval_distance_score_mrs, gcs_total_detail_admission,
         hsa_fisher_modifiee, aic_nihss, tcdb_scan_classe, gcs_total_pire_24_heures, htic.factor, htic_pic_max, capteur_pic.factor, neurochirurgie.factor, 
         sedation_rea.factor, sedation_rea_duree, ventilation_rea.factor, ventilation_rea_duree, ata.factor, deces_rea.factor, trachetomie.factor,
         infection_rea.factor, hsa_anevrysme_traitement_type.factor, hsa_vasospasme.factor, eval_distance, starts_with("htic_traitement___"), 
         starts_with("sedation_rea_type___"), starts_with("neurochirurgie_type___"), aic_thrombolyse, aic_thrombectomie)

# Conversion des dates clés
df_raw$date_admission  <- as.Date(df_raw$date_admission)
df_raw$date_symptomes  <- as.Date(df_raw$date_symptomes)
df_raw$sortie_date     <- as.Date(df_raw$sortie_date)
df_raw$date_deces      <- as.Date(df_raw$date_deces)

# Variables clés nettoyées
df <- df_raw %>%
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
    nihss         = as.numeric(aic_nihss),
    gcs_pire24    = as.numeric(gcs_total_pire_24_heures),
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
    nc_dvp           = as.numeric(neurochirurgie_type___7)
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
  d <- data %>%
    filter(!is.na(.data[[var]]), !is.na(.data[[by_col]])) %>%
    count(.data[[by_col]], val = .data[[var]]) %>%
    group_by(.data[[by_col]]) %>%
    mutate(pct = n / sum(n)) %>%
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
  ggplotly(p, tooltip = "text") %>%
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
        uiOutput("vb_centres"),
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
      col_widths = c(4, 4, 4),
      card(
        card_header("Antécédents — HTA"),
        plotlyOutput("plot_hta", height = "280px")
      ),
      card(
        card_header("Antécédents — Diabète"),
        plotlyOutput("plot_diabete", height = "280px")
      ),
      card(
        card_header("Antécédents — Tabac"),
        plotlyOutput("plot_tabac", height = "280px")
      )
    )
  ),


  # ── 3. RÉANIMATION ─────────────────────────────────────
  nav_panel("🏥 Réanimation", padding=20,
              div(style = "min-height:90px; margin-bottom:5px;",
              layout_columns(col_widths=c(3,3,3,3),
                             uiOutput("vb_rea_htic"), uiOutput("vb_rea_neurochir"),
                             uiOutput("vb_rea_vm"),   uiOutput("vb_rea_deces")
              )),
            navset_tab(
              
              nav_panel("HTIC", br(),
                        layout_columns(col_widths=c(6,6),
                                       card(card_header("Capteur de PIC — par pathologie"),
                                            plotlyOutput("plot_rea_capteur", height="300px")),
                                       card(card_header("HTIC — par pathologie"),
                                            plotlyOutput("plot_rea_htic", height="300px"))
                        ),
                        layout_columns(col_widths=c(6,6),
                                       card(card_header("PIC maximale (mmHg)"),
                                            plotlyOutput("plot_rea_pic_max", height="300px")),
                                       card(card_header("Traitements de l'HTIC"),
                                            plotlyOutput("plot_rea_htic_ttt", height="300px"))
                        )
              ),
              
              nav_panel("Neurochirurgie", br(),
                        layout_columns(col_widths=c(6,6),
                                       card(card_header("Recours neurochirurgie — par pathologie"),
                                            plotlyOutput("plot_rea_neurochir_yn", height="300px")),
                                       card(card_header("Types de gestes"),
                                            plotlyOutput("plot_rea_neurochir_types", height="300px"))
                        ),
                        layout_columns(col_widths=c(6,6),
                                       card(card_header("Neurochirurgie par centre"),
                                            plotlyOutput("plot_rea_neurochir_centre", height="300px")),
                                       card(card_header("Résumé"), uiOutput("info_neurochir"))
                        )
              ),
              
              nav_panel("Sédation & VM", br(),
                        layout_columns(col_widths=c(6,6),
                                       card(card_header("Sédation — par pathologie"),
                                            plotlyOutput("plot_rea_sed_yn", height="280px")),
                                       card(card_header("Molécules de sédation"),
                                            plotlyOutput("plot_rea_sed_types", height="280px"))
                        ),
                        layout_columns(col_widths=c(6,6),
                                       card(card_header("Durée de sédation (jours)"),
                                            plotlyOutput("plot_rea_sed_duree", height="280px")),
                                       card(card_header("Durée de ventilation mécanique (jours)"),
                                            plotlyOutput("plot_rea_vm_duree", height="280px"))
                        )
              ),
              
              nav_panel("LAT · EME · Décès", br(),
                        layout_columns(col_widths=c(3,3,3,3),
                                       uiOutput("vb_rea_lata"), uiOutput("vb_rea_ata"),
                                       uiOutput("vb_rea_edme"), uiOutput("vb_rea_deces2")
                        ), br(),
                        layout_columns(col_widths=c(6,6),
                                       card(card_header("LATA — par pathologie"),
                                            plotlyOutput("plot_rea_lata", height="300px")),
                                       card(card_header("ATA — par pathologie"),
                                            plotlyOutput("plot_rea_ata", height="300px"))
                        ),
                        layout_columns(col_widths=c(6,6),
                                       card(card_header("EME — par pathologie"),
                                            plotlyOutput("plot_rea_edme", height="300px")),
                                       card(card_header("Décès en réanimation — par pathologie"),
                                            plotlyOutput("plot_rea_deces_patho", height="300px"))
                        )
              )
            )
  ),
  
  # ─── ONGLET 4 : PAR PATHOLOGIE ───────────────────────────
  nav_panel(
    title = "🧠 Par pathologie",
    padding = 20,
    navset_tab(

      # TBI
      nav_panel("TC",
        br(),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Typologie des lésions (Marshall)"),
            plotlyOutput("plot_tbi_tcdb", height = "300px")
          ),
          card(
            card_header("GCS pré-hospitalier"),
            plotlyOutput("plot_tbi_gcs", height = "300px")
          )
        ),
          br(),
          DTOutput("table_tbi")
        ),

      # HSA
      nav_panel("HSA",
        br(),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Score WFNS à l'admission"),
            plotlyOutput("plot_hsa_wfns", height = "300px")
          ),
          card(
            card_header("Score de Fisher modifié"),
            plotlyOutput("plot_hsa_fisher", height = "300px")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Vasospasme"),
            plotlyOutput("plot_hsa_vasospasme", height = "280px")
          ),
          card(
            card_header("Traitement de l'anévrysme"),
            plotlyOutput("plot_hsa_ttt_anev", height = "280px")
          )
        )
      ),

      # AVC
      nav_panel("AVCi",
        br(),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("NIHSS à l'admission"),
            plotlyOutput("plot_avc_nihss", height = "300px")
          ),
          card(
            card_header("Thrombolyse & Thrombectomie"),
            plotlyOutput("plot_avc_ttt", height = "300px")
          )
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
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("mRs dichotomisé (< 3 ou > 2) par pathologie"),
        plotlyOutput("plot_pronostic", height = "340px")
      ),
      card(
        card_header("mRS détaillé par pathologie"),
        plotlyOutput("plot_mrs", height = "320px")
      ),
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("GCS à l'admission par pathologie"),
        plotlyOutput("plot_gcs", height = "320px")
      ),
      card(
        card_header("Durée de séjour en réanimation (jours)"),
        plotlyOutput("plot_duree_sejour", height = "340px")
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
  ))
)

# ---- SERVER ------------------------------------------------
server <- function(input, output, session) {

  # Reset filtres
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "filtre_diag",   selected = diagnostics_dispo)
    updateCheckboxGroupInput(session, "filtre_centre", selected = centres_dispo)
    updateNumericRangeInput(session, "filtre_pire_gcs", value = c(gcs_pire24_min, gcs_pire24_max))
    updateNumericRangeInput(session, "filtre_age", value = c(age_min, age_max))
    updateDateRangeInput(session, "filtre_dates",
                         start = date_min, end = date_max)
    updateSliderInput(session, "filtre_dates_2",
                         value = c(date_min,date_max))
  })

  # Dataset filtré réactif
  df_f <- reactive({
    req(input$filtre_diag, input$filtre_centre, input$filtre_dates)
    df %>%
      filter(
        diagnostic %in% input$filtre_diag,
        centre     %in% input$filtre_centre,
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
  })

  # ── Value Boxes ─────────────────────────────────────────
  output$vb_n <- renderUI({
    n <- nrow(df_f())
    value_box_custom("Patients inclus", n_fmt(n), color = "#1D3557")
  })
  output$vb_centres <- renderUI({
    n <- length(unique(na.omit(df_f()$centre)))
    value_box_custom("Centres actifs", n, color = "#457B9D")
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
    tot <- nrow(d %>% filter(!is.na(eval_distance)))
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
    value_box_custom("LATA", sprintf("%s (%s)",n_fmt(n),pct(n,tot)), "#F4A261")
  })
  output$vb_rea_ata      <- renderUI({
    n=sum(df_f()$ata=="Oui",na.rm=TRUE); tot=nrow(df_f())
    value_box_custom("ATA", sprintf("%s (%s)",n_fmt(n),pct(n,tot)), "#E9C46A")
  })
  output$vb_rea_edme     <- renderUI({
    n=sum(df_f()$edme=="Oui",na.rm=TRUE); tot=nrow(df_f())
    value_box_custom("EME", sprintf("%s (%s)",n_fmt(n),pct(n,tot)), "#6c757d")
  })
  output$vb_rea_deces2   <- renderUI({
    n=sum(df_f()$deces_rea_f=="Oui",na.rm=TRUE); tot=nrow(df_f())
    value_box_custom("Décès en réa", sprintf("%s (%s)",n_fmt(n),pct(n,tot)), "#E63946")
  })

  # ── Vue globale ─────────────────────────────────────────
  output$plot_patho_pie <- renderPlotly({
    d <- df_f() %>% count(diagnostic) %>% filter(!is.na(diagnostic))
    p <- plot_ly(d, labels = ~diagnostic, values = ~n, type = "pie",
                 marker = list(colors = unname(pal_patho[d$diagnostic])),
                 textinfo = "label+percent",
                 hovertemplate = "%{label}<br>N = %{value}<br>%{percent}<extra></extra>") %>%
      layout(showlegend = TRUE,
             legend = list(orientation = "h", y = -0.2),
             margin = list(t = 10, b = 10),
             paper_bgcolor = "#f8f9fa",
             plot_bgcolor  = "#f8f9fa")
    p
  })

  output$plot_centre_bar <- renderPlotly({
    d <- df_f() %>% count(centre, diagnostic) %>%
      filter(!is.na(centre), !is.na(diagnostic))
    p <- ggplot(d, aes(x = reorder(centre, -n), y = n, fill = diagnostic,
                       text = paste0(diagnostic, "<br>N = ", n))) +
      geom_col(position = "stack", width = 0.6) +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      labs(x = NULL, y = "Effectif", fill = NULL) +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

  output$plot_timeline <- renderPlotly({
    d <- df_f() %>%
      filter(!is.na(date_admission)) %>%
      mutate(mois = floor_date(date_admission, "month")) %>%
      count(mois, diagnostic)
    p <- ggplot(d, aes(x = mois, y = n, fill = diagnostic,
                       text = paste0(format(mois, "%b %Y"), "<br>", diagnostic, "<br>N = ", n))) +
      geom_col(position = "stack") +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      labs(x = NULL, y = "Inclusions / mois", fill = NULL) +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

output$plot_inclusion_cum <- renderPlotly({
  d <- df_f() %>%
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
                labs(x = "Date d'inclusion", y = "Nombre d'inclusions", col = "Centre d'inclusion")
  
  ggplotly(p) %>%
    layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
})

  # ── Démographie ─────────────────────────────────────────
  output$plot_age_box <- renderPlotly({
    d <- df_f() %>% filter(!is.na(age), !is.na(diagnostic))
    p <- ggplot(d, aes(x = reorder(diagnostic, age, median, na.rm = TRUE),
                       y = age, fill = diagnostic,
                       text = paste0(diagnostic, "<br>Âge = ", round(age, 1), " ans"))) +
      geom_boxplot(alpha = 0.8, outlier.shape = 21) +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      labs(x = NULL, y = "Âge (ans)", fill = NULL) +
      scale_y_continuous(limits = c(0,100)) +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

  output$plot_sexe <- renderPlotly({
    d <- df_f() %>%
      filter(!is.na(sexe), !is.na(diagnostic)) %>%
      count(diagnostic, sexe) %>%
      group_by(diagnostic) %>%
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
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

  antecedent_plot <- function(var, titre) {
    renderPlotly({
      d <- df_f() %>%
        filter(!is.na(.data[[var]]), !is.na(diagnostic)) %>%
        count(diagnostic, val = .data[[var]]) %>%
        group_by(diagnostic) %>%
        mutate(pct = n / sum(n))
      p <- ggplot(d, aes(x = diagnostic, y = pct, fill = val,
                         text = paste0(val, "<br>", round(100*pct,1), "% (N=", n, ")"))) +
        geom_col(position = "fill", width = 0.65) +
        scale_y_continuous(labels = percent_format()) +
        labs(x = NULL, y = "Proportion", fill = titre, title = titre) +
        theme_dashboard() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9))
      ggplotly(p, tooltip = "text") %>%
        layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
    })
  }
  output$plot_hta     <- antecedent_plot("hta", "HTA")
  output$plot_diabete <- antecedent_plot("diabete", "Diabète")
  output$plot_tabac   <- antecedent_plot("tabac", "Tabac")

  # ── Devenir ─────────────────────────────────────────────
  output$plot_pronostic <- renderPlotly({
    d <- df_f() %>%
      filter(!is.na(pronostic), !is.na(diagnostic)) %>%
      count(diagnostic, pronostic) %>%
      group_by(diagnostic) %>%
      mutate(pct = n / sum(n))
    p <- ggplot(d, aes(x = reorder(diagnostic, -pct), y = pct, fill = factor(pronostic, c("Bon pronostic", "Mauvais pronostic", "Décès en réanimation")),
                       text = paste0(pronostic, "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = "fill", width = 0.65) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_manual(values = c("Bon pronostic" = "#2A9D8F",
                                   "Mauvais pronostic" = "#F4A261",
                                   "Décès en réanimation" = "#E63946"
                                   )) +
      labs(x = NULL, y = "Proportion", fill = NULL) +
      theme_dashboard()
    
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

  output$plot_mrs <- renderPlotly({
    d <-  df_f() %>%
      filter(!is.na(mrs_distance) & !is.na(diagnostic)) %>%
      count(diagnostic, mrs_distance) %>%
      group_by(diagnostic) %>%
      mutate(pct = n / sum(n))
    
    p <- ggplot(d, aes(x = reorder(diagnostic, -pct), y = pct, fill = mrs_distance,
                       text = paste0(diagnostic, "<br>mRS ", mrs_distance,
                                     "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = "fill") +
      labs(x = NULL, y = NULL, fill = "Score mRS") +
      scale_fill_lancet() + 
      theme_dashboard() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      
    
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa", yaxis = NULL)
  })

  output$plot_duree_sejour <- renderPlotly({
    d <- df_f() %>% filter(!is.na(duree_sejour), !is.na(diagnostic), duree_sejour >= 0)
    p <- ggplot(d, aes(x = reorder(diagnostic, duree_sejour, median, na.rm = TRUE),
                       y = duree_sejour, fill = diagnostic,
                       text = paste0(diagnostic, "<br>Durée = ", duree_sejour, " j"))) +
      geom_boxplot(alpha = 0.8, outliers = FALSE) +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      scale_y_continuous(limits = c(0,100)) +
      labs(x = NULL, y = "Durée de séjour (jours)", fill = NULL) +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none")

    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

  output$plot_gcs <- renderPlotly({
    d <- df_f() %>% 
      filter(!is.na(gcs_initial), !is.na(diagnostic)) %>%
      count(diagnostic, gcs_initial) %>%
      group_by(diagnostic) %>%
      mutate(pct = n /sum(n))
    p <- ggplot(d, aes(x = gcs_initial, y = pct, fill = diagnostic,
                       text = paste0(diagnostic, "<br>GCS initial ", gcs_initial,
                                     "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = "dodge", width = 0.75) +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      scale_x_continuous(breaks = 3:15) +
      labs(x = "Score GCS", y = "Proportion", fill = NULL) +
      theme_dashboard()
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

  # ── Par pathologie : TBI ─────────────────────────────────
  df_tbi <- reactive({ df_f() %>% filter(grepl("TC", diagnostic, ignore.case = TRUE)) })

  output$plot_tbi_tcdb <- renderPlotly({
    d <- df_tbi() %>%
      mutate(tcdb = factor(tcdb_scan_classe)) %>%
      filter(!is.na(tcdb)) %>%
      count(tcdb)
    
    p <- ggplot(d, aes(x = tcdb, y = n, fill = tcdb, 
                    text = paste0("Classe Marshall ", tcdb, "<br> N =", n))) + 
      geom_col() +
      labs(x = "Classification de Marshall", y = "Nombre de patients") +
      guides(fill = FALSE) +
      scale_fill_nejm() +
      theme_dashboard()
    
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

  output$plot_tbi_gcs <- renderPlotly({
    d <- df_tbi() %>% 
      filter(!is.na(gcs_initial)) %>% 
      mutate(gcs_initial = as_factor(gcs_initial))
    
    p <- ggplot(d, aes(x = gcs_initial, fill = factor(gcs_initial),
                       text = paste0(..count..))) +
      geom_bar() +
      labs(x = "Score de Glasgow", y = "Nombre de patients") +
      guides(fill = FALSE) +
      theme_dashboard()
    
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

  # ── Par pathologie : HSA ─────────────────────────────────
  df_hsa <- reactive({ df_f() %>% filter(grepl("meningee|HSA|Hemorragie", diagnostic, ignore.case = TRUE)) })

  output$plot_hsa_wfns <- renderPlotly({
    d <- df_hsa() %>% filter(!is.na(hsa_wfns)) %>%
      count(wfns = factor(hsa_wfns))
    p <- plot_ly(d, x = ~wfns, y = ~n, type = "bar",
                 marker = list(color = "#F4A261"),
                 hovertemplate = "WFNS grade %{x}<br>N = %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "Grade WFNS"), yaxis = list(title = "N"),
             paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
    p
  })

  output$plot_hsa_fisher <- renderPlotly({
    d <- df_hsa() %>% filter(!is.na(hsa_fisher)) %>%
      count(fisher = factor(hsa_fisher))
    p <- plot_ly(d, x = ~fisher, y = ~n, type = "bar",
                 marker = list(color = "#E9C46A"),
                 hovertemplate = "Fisher modifié grade %{x}<br>N = %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "Score Fisher modifié"), yaxis = list(title = "N"),
             paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
    p
  })

  output$plot_hsa_vasospasme <- renderPlotly({
    d <- df_hsa() %>%
      filter(!is.na(hsa_vasospasme.factor)) %>%
      count(vasospasme = as.character(hsa_vasospasme.factor))
    p <- plot_ly(d, labels = ~vasospasme, values = ~n, type = "pie",
                 marker = list(colors = c("#E63946", "#2A9D8F", "#dee2e6")),
                 textinfo = "label+percent",
                 hovertemplate = "%{label}<br>N = %{value}<extra></extra>") %>%
      layout(paper_bgcolor = "#f8f9fa", showlegend= FALSE)
    p
  })

  output$plot_hsa_ttt_anev <- renderPlotly({
    d <- df_hsa() %>%
      filter(!is.na(hsa_anevrysme_traitement_type.factor)) %>%
      count(ttt = as.character(hsa_anevrysme_traitement_type.factor))
    p <- plot_ly(d, labels = ~ttt, values = ~n, type = "pie",
                 textinfo = "label+percent",
                 hovertemplate = "%{label}<br>N = %{value}<extra></extra>") %>%
      layout(paper_bgcolor = "#f8f9fa", showlegend= FALSE)
    p
  })

  # ── Par pathologie : AVC ischémique ─────────────────────
  df_avc <- reactive({ df_f() %>% filter(grepl("AVCi", diagnostic, ignore.case = TRUE)) })

  output$plot_avc_nihss <- renderPlotly({
    d <- df_avc() %>% filter(!is.na(nihss))
    p <- ggplot(d, aes(x = nihss, text = paste0("NIHSS = ", nihss))) +
      geom_histogram(binwidth = 1, fill = "#2A9D8F", color = "white") +
      labs(x = "Score NIHSS", y = "N patients") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

  output$plot_avc_ttt <- renderPlotly({
    d <- df_avc() %>%
      summarise(
        Thrombolyse   = sum(aic_thrombolyse == 1, na.rm = TRUE),
        Thrombectomie = sum(aic_thrombectomie == 1, na.rm = TRUE)
      ) %>%
      pivot_longer(everything(), names_to = "traitement", values_to = "n")
    p <- plot_ly(d, x = ~traitement, y = ~n, type = "bar",
                 marker = list(color = c("#2A9D8F", "#457B9D")),
                 hovertemplate = "%{x}<br>N = %{y}<extra></extra>") %>%
      layout(xaxis = list(title = NULL), yaxis = list(title = "N patients"),
             paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
    p
  })

  # ═══════ RÉANIMATION ══════════════════════════════════
  
  # HTIC & PIC
  output$plot_rea_capteur <- renderPlotly({
    prop_bar_plot(df_f(),"capteur_pic","% avec capteur PIC","diagnostic")
  })
  output$plot_rea_htic <- renderPlotly({
    prop_bar_plot(df_f(),"htic_yn","% avec HTIC","diagnostic")
  })
  output$plot_rea_pic_max <- renderPlotly({
    d <- df_f()%>%filter(!is.na(pic_max),!is.na(diagnostic))
    if(nrow(d)==0) return(plotly_empty())
    p <- ggplot(d,aes(x=reorder(diagnostic,pic_max,median,na.rm=TRUE),
                      y=pic_max,fill=diagnostic,
                      text=paste0(diagnostic,"<br>PIC max=",pic_max," mmHg")))+
      geom_boxplot(alpha=0.8,outlier.shape=21)+
      scale_fill_manual(values=pal_patho,drop=FALSE)+
      labs(x=NULL,y="PIC maximale (mmHg)")+theme_dashboard()+
      scale_y_continuous(limits = c(0,100)) +
      theme(axis.text.x=element_text(angle=30,hjust=1),legend.position="none")
    ggplotly(p,tooltip="text")%>%layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  output$plot_rea_htic_ttt <- renderPlotly({
    d <- df_f()%>%filter(htic_yn=="Oui")
    ttt_n <- c("Sédation","Curarisation","Normothermie active","Hypothermie thérap.",
               "DVE","Craniectomie décomp.","Suppression métabol.","Osmothérapie",
               "Hypocapnie permissive","DVL externe")
    ttt_v <- c("htic_ttt_sedation","htic_ttt_curare","htic_ttt_normotherm",
               "htic_ttt_hypotherm","htic_ttt_dve","htic_ttt_craniect",
               "htic_ttt_burst","htic_ttt_osmo","htic_ttt_hypocapnie","htic_ttt_lombaire")
    counts <- sapply(ttt_v, function(v) sum(d[[v]]==1,na.rm=TRUE))
    df_t <- data.frame(ttt=factor(ttt_n,levels=rev(ttt_n)),n=counts)
    p <- ggplot(df_t,aes(x=ttt,y=n,text=paste0(ttt,"<br>N=",n)))+
      geom_col(fill="#E63946",width=0.65)+coord_flip()+
      labs(x=NULL,y="N patients (HTIC+)")+theme_dashboard()
    ggplotly(p,tooltip="text")%>%layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  
  # Neurochirurgie
  output$plot_rea_neurochir_yn <- renderPlotly({
    prop_bar_plot(df_f(),"neurochir","% avec neurochirurgie","diagnostic")
  })
  output$plot_rea_neurochir_types <- renderPlotly({
    d <- df_f()%>%filter(neurochir=="Oui")
    nms <- c("Évacuation d'hématome","DVE","Clipping anévrysmal",
             "Craniectomie hémisp.","Craniectomie F.Post.","DVP/DVA","Autre")
    vrs <- c("nc_evacuation","nc_dve","nc_clipping","nc_craniect_hemi",
             "nc_craniect_fp","nc_dvp","nc_autre")
    cnt <- sapply(vrs,function(v) sum(d[[v]]==1,na.rm=TRUE))
    df_n <- data.frame(g=factor(nms,levels=rev(nms)),n=cnt)
    p <- ggplot(df_n,aes(x=g,y=n,text=paste0(g,"<br>N=",n)))+
      geom_col(fill="#9B5DE5",width=0.65)+coord_flip()+
      labs(x=NULL,y="N patients")+theme_dashboard()
    ggplotly(p,tooltip="text")%>%layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  output$plot_rea_neurochir_centre <- renderPlotly({
    pal_c <- setNames(
      colorRampPalette(c("#457B9D","#1D3557"))(length(centres_dispo)),
      centres_dispo
    )
    prop_bar_plot(df_f(),"neurochir","% avec neurochirurgie","centre",pal_c)
  })
  output$info_neurochir <- renderUI({
    d <- df_f(); n_nc <- sum(d$neurochir=="Oui",na.rm=TRUE); tot <- nrow(d)
    tagList(
      h5("Résumé", style="color:#1D3557;font-weight:700;margin-top:10px;"),
      tags$ul(
        tags$li(sprintf("Opérés : %d / %d (%.1f%%)", n_nc, tot, 100*n_nc/max(tot,1))),
        tags$li(sprintf("Médiane gestes/patient : %.1f",
                        median(na.omit(as.numeric(d$neurochirurgie_nombre))))),
        tags$li(sprintf("DVE : %d cas", sum(d$nc_dve==1,na.rm=TRUE))),
        tags$li(sprintf("Craniectomie : %d cas",
                        sum(d$nc_craniect_hemi==1|d$nc_craniect_fp==1,na.rm=TRUE)))
      )
    )
  })
  
  # Sédation & VM
  output$plot_rea_sed_yn <- renderPlotly({
    prop_bar_plot(df_f(),"sedation","% avec sédation","diagnostic")
  })
  output$plot_rea_sed_types <- renderPlotly({
    d <- df_f()%>%filter(sedation=="Oui")
    mols <- c("Midazolam","Kétamine","Propofol","Dexmédétomidine","Clonidine")
    vars <- c("sed_midazolam","sed_ketamine","sed_propofol","sed_dexmede","sed_clonidine")
    cnt  <- sapply(vars,function(v) sum(d[[v]]==1,na.rm=TRUE))
    df_m <- data.frame(m=factor(mols,levels=rev(mols)),n=cnt)
    p <- ggplot(df_m,aes(x=m,y=n,text=paste0(m,"<br>N=",n)))+
      geom_col(fill="#457B9D",width=0.65)+coord_flip()+
      labs(x=NULL,y="N patients")+theme_dashboard()
    ggplotly(p,tooltip="text")%>%layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  output$plot_rea_sed_duree <- renderPlotly({
    d <- df_f()%>%filter(!is.na(duree_sedation),!is.na(diagnostic),sedation=="Oui")
    if(nrow(d)==0) return(plotly_empty())
    p <- ggplot(d,aes(x=reorder(diagnostic,duree_sedation,median,na.rm=TRUE),
                      y=duree_sedation,fill=diagnostic,
                      text=paste0(diagnostic,"<br>",duree_sedation," j")))+
      geom_boxplot(alpha=0.8,outlier.shape=21)+
      scale_fill_manual(values=pal_patho,drop=FALSE)+
      labs(x=NULL,y="Durée sédation (j)")+theme_dashboard()+
      scale_y_continuous(limits = c(0,50)) +
      theme(axis.text.x=element_text(angle=30,hjust=1),legend.position="none")
    ggplotly(p,tooltip="text")%>%layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  output$plot_rea_vm_duree <- renderPlotly({
    d <- df_f()%>%filter(!is.na(duree_ventil),!is.na(diagnostic),ventilation=="Oui")
    if(nrow(d)==0) return(plotly_empty())
    p <- ggplot(d,aes(x=reorder(diagnostic,duree_ventil,median,na.rm=TRUE),
                      y=duree_ventil,fill=diagnostic,
                      text=paste0(diagnostic,"<br>",duree_ventil," j")))+
      geom_boxplot(alpha=0.8,outlier.shape=21)+
      scale_fill_manual(values=pal_patho,drop=FALSE)+
      labs(x=NULL,y="Durée ventilation (j)")+theme_dashboard()+
      scale_y_continuous(limits = c(0,50)) +
      theme(axis.text.x=element_text(angle=30,hjust=1),legend.position="none")
    ggplotly(p,tooltip="text")%>%layout(paper_bgcolor="#f8f9fa",plot_bgcolor="#f8f9fa")
  })
  
  # LATA / ATA / EME / Décès
  output$plot_rea_lata       <- renderPlotly(prop_bar_plot(df_f(),"lata",       "% LATA","diagnostic"))
  output$plot_rea_ata        <- renderPlotly(prop_bar_plot(df_f(),"ata",        "% ATA", "diagnostic"))
  output$plot_rea_edme       <- renderPlotly(prop_bar_plot(df_f(),"edme",       "% EME", "diagnostic"))
  output$plot_rea_deces_patho<- renderPlotly(prop_bar_plot(df_f(),"deces_rea_f","% Décès en réa","diagnostic"))
  
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
    ggplotly(p,tooltip="text") %>%
      layout(paper_bgcolor="#f8f9fa", plot_bgcolor="#f8f9fa",
             margin=list(l=220,r=80,t=20,b=100))
  })
  
  output$table_missing <- renderDT({
    d <- missing_data() %>%
      select(Catégorie=categorie, Variable=label, Groupe=groupe,
             `N total`=n_total, `N manquant`=n_manquant,
             `% manquant`=pct_manquant) %>%
      arrange(Catégorie, Variable, Groupe)
    datatable(d, rownames=FALSE, filter="top",
              extensions="Buttons",
              options=list(
                pageLength=20, scrollX=TRUE,
                language=list(url="//cdn.datatables.net/plug-ins/1.13.6/i18n/fr-FR.json"),
                dom="Bfrtip", buttons=c("copy","csv","excel")
              ),
              class="table-hover table-striped table-sm"
    ) %>%
      formatStyle(
        "% manquant",
        background = styleInterval(c(10,30,60),
                                   c("#d4edda","#fff3cd","#ffd7a0","#f8d7da")),
        fontWeight = "bold"
      )
  })
}

# ---- Lancement ---------------------------------------------
shinyApp(ui, server)
