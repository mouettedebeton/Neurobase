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

# ---- Chargement & préparation des données ------------------
df_raw <- readRDS("data.rds")

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
    diabete       = as.character(diabete.factor),
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
                             as.numeric(gcs_total_detail_admission),
                             as.numeric(gcs_total_admission)),
    # Scores spécifiques
    hsa_wfns      = as.numeric(hsa_wfns),
    hsa_fisher    = as.numeric(hsa_fisher_modifiee),
    nihss         = as.numeric(aic_nihss),
    gcs_pire24    = as.numeric(gcs_total_pire_24_heures),
    htic          = as.character(htic.factor),
    neurochir     = as.character(neurochirurgie.factor)
  )

# Plages de dates pour slider
date_min <- min(df$date_admission, na.rm = TRUE)
date_max <- max(df$date_admission, na.rm = TRUE)

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
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "#f8f9fa", color = NA),
      panel.background = element_rect(fill = "#f8f9fa", color = NA),
      panel.grid.major = element_line(color = "#dee2e6"),
      panel.grid.minor = element_blank(),
      axis.title       = element_text(color = "#495057", size = 11),
      axis.text        = element_text(color = "#6c757d"),
      legend.position  = "bottom",
      plot.title       = element_text(face = "bold", color = "#212529", size = 13),
      strip.text       = element_text(face = "bold", color = "#495057")
    )
}

# ---- Fonctions utilitaires ---------------------------------
value_box_custom <- function(title, value, icon = NULL, color = "#457B9D") {
  div(
    style = glue::glue("background:{color};color:white;border-radius:10px;padding:18px 20px;
                        box-shadow:0 2px 8px rgba(0,0,0,.12);height:100%;"),
    div(style = "font-size:0.85rem;opacity:0.85;margin-bottom:4px;", title),
    div(style = "font-size:2rem;font-weight:700;line-height:1;", value)
  )
}

n_fmt <- function(x) format(x, big.mark = " ", nsmall = 0)
pct   <- function(x, n) sprintf("%.1f%%", 100 * x / n)

# ---- UI ----------------------------------------------------
ui <- page_navbar(
  title = tags$span(
    tags$img(src = "https://cdn-icons-png.flaticon.com/512/3209/3209018.png",
             height = "28px", style = "margin-right:10px;vertical-align:middle;"),
    "Dashboard Neurolésés — ART"
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
    h5("🔧 Filtres globaux", style = "color:#1D3557;font-weight:700;"),
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
    dateRangeInput(
      "filtre_dates", "Période d'admission",
      start = date_min,
      end   = date_max,
      min   = date_min,
      max   = date_max,
      language = "fr",
      separator = "→"
    ),
    hr(),
    actionButton("reset_filters", "↺ Réinitialiser", class = "btn-outline-secondary btn-sm w-100")
  ),

  # ─── ONGLET 1 : VUE GLOBALE ──────────────────────────────
  nav_panel(
    title = "📊 Vue globale",
    padding = 20,
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      uiOutput("vb_n"),
      uiOutput("vb_centres"),
      uiOutput("vb_deces"),
      uiOutput("vb_age")
    ),
    br(),
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
      col_widths = c(12),
      card(
        card_header("Timeline des inclusions (par date d'admission)"),
        plotlyOutput("plot_timeline", height = "280px")
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

  # ─── ONGLET 3 : DEVENIR ──────────────────────────────────
  nav_panel(
    title = "📈 Devenir & Mortalité",
    padding = 20,
    layout_columns(
      col_widths = c(4, 4, 4),
      uiOutput("vb_deces2"),
      uiOutput("vb_edme"),
      uiOutput("vb_lata")
    ),
    br(),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Pronostic (dichotomie) par pathologie"),
        plotlyOutput("plot_pronostic", height = "340px")
      ),
      card(
        card_header("Durée de séjour en réanimation (jours)"),
        plotlyOutput("plot_duree_sejour", height = "340px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("GCS à l'admission par pathologie"),
        plotlyOutput("plot_gcs", height = "320px")
      ),
      card(
        card_header("mRS à distance par pathologie"),
        plotlyOutput("plot_mrs", height = "320px")
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
            card_header("Classification TDB (Marshall)"),
            plotlyOutput("plot_tbi_tcdb", height = "300px")
          ),
          card(
            card_header("Mécanisme du traumatisme"),
            plotlyOutput("plot_tbi_mecanisme", height = "300px")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("GCS pré-hospitalier"),
            plotlyOutput("plot_tbi_gcs", height = "300px")
          ),
          card(
            card_header("HTIC & Traitement"),
            plotlyOutput("plot_tbi_htic", height = "300px")
          )
        )
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

  # ─── ONGLET 5 : TABLES ───────────────────────────────────
  nav_panel(
    title = "📋 Tables",
    padding = 20,
    navset_tab(
      nav_panel("Table principale",
        br(),
        DTOutput("table_principale")
      ),
      nav_panel("Par pathologie",
        br(),
        layout_columns(
          col_widths = c(4),
          selectInput("select_diag_table", "Sélectionner une pathologie :",
                      choices = diagnostics_dispo, selected = diagnostics_dispo[1])
        ),
        DTOutput("table_patho")
      ),
      nav_panel("Par centre",
        br(),
        layout_columns(
          col_widths = c(4),
          selectInput("select_centre_table", "Sélectionner un centre :",
                      choices = centres_dispo, selected = centres_dispo[1])
        ),
        DTOutput("table_centre")
      )
    )
  )
)

# ---- SERVER ------------------------------------------------
server <- function(input, output, session) {

  # Reset filtres
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "filtre_diag",   selected = diagnostics_dispo)
    updateCheckboxGroupInput(session, "filtre_centre", selected = centres_dispo)
    updateDateRangeInput(session, "filtre_dates",
                         start = date_min, end = date_max)
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
        date_admission <= input$filtre_dates[2]
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
    n <- sum(d$deces_total == 1, na.rm = TRUE)
    tot <- nrow(d)
    val <- if (tot > 0) sprintf("%s (%s)", n_fmt(n), pct(n, tot)) else "—"
    value_box_custom("Décès total", val, color = "#E63946")
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
    p <- ggplot(d, aes(x = reorder(diagnostic, -pct), y = pct, fill = pronostic,
                       text = paste0(pronostic, "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = "fill", width = 0.65) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_manual(values = c("Bon pronostic" = "#2A9D8F",
                                   "Décès en réanimation" = "#E63946",
                                   "Mauvais pronostic" = "#F4A261")) +
      labs(x = NULL, y = "Proportion", fill = NULL) +
      theme_dashboard() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
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
    d <- df_f() %>% filter(!is.na(gcs_initial), !is.na(diagnostic))
    p <- ggplot(d, aes(x = gcs_initial, fill = diagnostic,
                       text = paste0("GCS = ", gcs_initial, "<br>", diagnostic))) +
      geom_histogram(binwidth = 1, position = "stack", color = "white") +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      scale_x_continuous(breaks = 3:15) +
      labs(x = "Score GCS", y = "N patients", fill = NULL) +
      theme_dashboard()
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

  output$plot_mrs <- renderPlotly({
    d <- df_f() %>%
      filter(!is.na(mrs_distance), !is.na(diagnostic)) %>%
      count(diagnostic, mrs_distance) %>%
      group_by(diagnostic) %>%
      mutate(pct = n / sum(n))
    p <- ggplot(d, aes(x = mrs_distance, y = pct, fill = diagnostic,
                       text = paste0(diagnostic, "<br>mRS ", mrs_distance,
                                     "<br>", round(100*pct,1), "% (N=", n, ")"))) +
      geom_col(position = "dodge", width = 0.75) +
      scale_fill_manual(values = pal_patho, drop = FALSE) +
      scale_y_continuous(labels = percent_format()) +
      labs(x = "mRS à distance", y = "Proportion", fill = NULL) +
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
    p <- plot_ly(d, x = ~tcdb, y = ~n, type = "bar",
                 marker = list(color = "#E63946"),
                 hovertemplate = "Marshall classe %{x}<br>N = %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "Classe Marshall (TDB)"),
             yaxis = list(title = "N patients"),
             paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
    p
  })

  output$plot_tbi_mecanisme <- renderPlotly({
    d <- df_tbi() %>%
      filter(!is.na(mecanisme_trauma.factor)) %>%
      count(mecanisme = as.character(mecanisme_trauma.factor))
    p <- plot_ly(d, labels = ~mecanisme, values = ~n, type = "pie",
                 textinfo = "label+percent",
                 hovertemplate = "%{label}<br>N = %{value}<extra></extra>") %>%
      layout(showlegend = TRUE, paper_bgcolor = "#f8f9fa")
    p
  })

  output$plot_tbi_gcs <- renderPlotly({
    d <- df_tbi() %>% filter(!is.na(gcs_initial))
    p <- ggplot(d, aes(x = gcs_initial,
                       text = paste0("GCS pré-hospit = ", gcs_initial))) +
      geom_histogram(binwidth = 1, fill = "#E63946", color = "white") +
      scale_x_continuous(breaks = 3:15) +
      labs(x = "Score GCS", y = "N patients") +
      theme_dashboard()
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "#f8f9fa", plot_bgcolor = "#f8f9fa")
  })

  output$plot_tbi_htic <- renderPlotly({
    d <- df_tbi() %>%
      filter(!is.na(htic)) %>%
      count(htic)
    p <- plot_ly(d, labels = ~htic, values = ~n, type = "pie",
                 marker = list(colors = c("#E63946", "#2A9D8F", "#dee2e6")),
                 textinfo = "label+percent",
                 hovertemplate = "%{label}<br>N = %{value}<extra></extra>") %>%
      layout(title = "HTIC diagnostiquée", paper_bgcolor = "#f8f9fa")
    p
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
      layout(paper_bgcolor = "#f8f9fa")
    p
  })

  output$plot_hsa_ttt_anev <- renderPlotly({
    d <- df_hsa() %>%
      filter(!is.na(hsa_anevrysme_traitement_type.factor)) %>%
      count(ttt = as.character(hsa_anevrysme_traitement_type.factor))
    p <- plot_ly(d, labels = ~ttt, values = ~n, type = "pie",
                 textinfo = "label+percent",
                 hovertemplate = "%{label}<br>N = %{value}<extra></extra>") %>%
      layout(paper_bgcolor = "#f8f9fa")
    p
  })

  # ── Par pathologie : AVC ischémique ─────────────────────
  df_avc <- reactive({ df_f() %>% filter(grepl("AVCi", diagnostic, ignore.case = TRUE)) })

  output$plot_avc_nihss <- renderPlotly({
    d <- df_avc() %>% filter(!is.na(nihss))
    p <- ggplot(d, aes(x = nihss, text = paste0("NIHSS = ", nihss))) +
      geom_histogram(binwidth = 2, fill = "#2A9D8F", color = "white") +
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

  # ── Tables ──────────────────────────────────────────────
  cols_table <- c("record_id", "centre", "diagnostic", "date_admission",
                  "age", "sexe", "imc", "gcs_initial",
                  "deces_total", "duree_sejour", "pronostic", "mrs_distance")

  dt_options <- list(
    pageLength = 15,
    language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/fr-FR.json"),
    scrollX = TRUE,
    dom = "Bfrtip",
    buttons = c("copy", "csv", "excel")
  )

  output$table_principale <- renderDT({
    df_f() %>%
      select(any_of(cols_table)) %>%
      mutate(date_admission = as.character(date_admission)) %>%
      datatable(extensions = "Buttons",
                options = dt_options,
                rownames = FALSE,
                filter = "top",
                class = "table-hover table-striped")
  })

  output$table_patho <- renderDT({
    req(input$select_diag_table)
    df_f() %>%
      filter(diagnostic == input$select_diag_table) %>%
      select(any_of(cols_table)) %>%
      mutate(date_admission = as.character(date_admission)) %>%
      datatable(extensions = "Buttons",
                options = dt_options,
                rownames = FALSE,
                filter = "top",
                class = "table-hover table-striped")
  })

  output$table_centre <- renderDT({
    req(input$select_centre_table)
    df_f() %>%
      filter(centre == input$select_centre_table) %>%
      select(any_of(cols_table)) %>%
      mutate(date_admission = as.character(date_admission)) %>%
      datatable(extensions = "Buttons",
                options = dt_options,
                rownames = FALSE,
                filter = "top",
                class = "table-hover table-striped")
  })
}

# ---- Lancement ---------------------------------------------
shinyApp(ui, server)
