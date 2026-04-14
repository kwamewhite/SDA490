# =============================================================================
# Canada Immigration Threat & Voting -- R Shiny App
#
# Tab 1: Cultural threat by province (CES 2019 choropleth + bar)
# Tab 2: Voting preferences by province (CES 2019 choropleth + bar)
# Tab 3: Threat by party (CES 2019 bar chart)
#
# Files needed in the same folder:
#   2019 Canadian Election Study - Online Survey v1.0.dta
# =============================================================================

# -- 0. Working directory ------------------------------------------------------
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  path <- rstudioapi::getActiveDocumentContext()$path
  if (nchar(path) > 0) setwd(dirname(path))
}

# -- 1. Packages ---------------------------------------------------------------
pkgs <- c("shiny", "shinydashboard", "leaflet", "haven",
          "dplyr", "scales", "ggplot2", "sf", "geodata")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, repos="https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only=TRUE))

# -- 2. Load CES 2019 ----------------------------------------------------------
ces <- haven::read_dta("2019 Canadian Election Study - Online Survey v1.0.dta")
cat("CES 2019 loaded: n =", nrow(ces), "\n")

# -- 3. Province lookup --------------------------------------------------------
prov_labels <- c(
  "14"="Alberta", "15"="British Columbia", "16"="Manitoba",
  "17"="New Brunswick", "18"="Newfoundland and Labrador",
  "19"="Northwest Territories", "20"="Nova Scotia", "21"="Nunavut",
  "22"="Ontario", "23"="Prince Edward Island", "24"="Quebec",
  "25"="Saskatchewan", "26"="Yukon"
)

prov_abbr <- c(
  "Alberta"="AB", "British Columbia"="BC", "Manitoba"="MB",
  "New Brunswick"="NB", "Newfoundland and Labrador"="NL",
  "Northwest Territories"="NT", "Nova Scotia"="NS", "Nunavut"="NU",
  "Ontario"="ON", "Prince Edward Island"="PEI", "Quebec"="QC",
  "Saskatchewan"="SK", "Yukon"="YT"
)

party_labels <- c(
  "1"="Liberal", "2"="Conservative", "3"="NDP",
  "4"="Bloc Quebecois", "5"="Green", "6"="People's Party"
)

party_colors <- c(
  "Conservative"    = "#1A4782",
  "Liberal"         = "#D71920",
  "NDP"             = "#F37021",
  "Bloc Quebecois"  = "#33B2CC",
  "Green"           = "#3D9B35",
  "People's Party"  = "#6B2D8B"
)

# -- 4. Build CES province dataset ---------------------------------------------
ces_clean <- ces %>%
  dplyr::mutate(
    province = prov_labels[as.character(as.numeric(cps19_province))],
    vote     = as.numeric(cps19_votechoice),
    vote_label = party_labels[as.character(vote)],
    
    # Nativism variables (recode 6=DK to NA)
    nav1 = ifelse(as.numeric(pes19_nativism1) == 6, NA, as.numeric(pes19_nativism1)),
    nav2 = ifelse(as.numeric(pes19_nativism2) == 6, NA, as.numeric(pes19_nativism2)),
    nav3 = ifelse(as.numeric(pes19_nativism3) == 6, NA, as.numeric(pes19_nativism3)),
    nav4 = ifelse(as.numeric(pes19_nativism4) == 6, NA, as.numeric(pes19_nativism4)),
    nav5 = ifelse(as.numeric(pes19_nativism5) == 6, NA, as.numeric(pes19_nativism5)),
    
    # Reverse nav3 (immigrants good for economy) so higher = more threat
    nav3_r = 6 - nav3,
    
    # Threat indices
    cultural_index = rowMeans(cbind(nav1, nav4), na.rm=TRUE),  # adapt to customs + culture harmed
    economic_index = nav3_r,                                     # immigrants bad for economy
    security_index = nav5,                                       # immigrants increase crime
    populist_index = nav2                                        # majority will over minority rights
  ) %>%
  dplyr::mutate(
    cultural_index = ifelse(is.nan(cultural_index), NA, cultural_index),
    prov_abbr      = prov_abbr[province]
  ) %>%
  dplyr::filter(!is.na(province))

# -- 5. Province summaries -----------------------------------------------------
# Threat by province
prov_threat <- ces_clean %>%
  dplyr::filter(!is.na(cultural_index)) %>%
  dplyr::group_by(province, prov_abbr) %>%
  dplyr::summarise(
    mean_cultural  = mean(cultural_index, na.rm=TRUE),
    mean_economic  = mean(economic_index, na.rm=TRUE),
    mean_security  = mean(security_index, na.rm=TRUE),
    mean_populist  = mean(populist_index, na.rm=TRUE),
    n              = dplyr::n(),
    .groups        = "drop"
  )

# Vote by province
prov_vote <- ces_clean %>%
  dplyr::filter(vote %in% 1:6) %>%
  dplyr::group_by(province, prov_abbr, vote_label) %>%
  dplyr::summarise(n = dplyr::n(), .groups="drop") %>%
  dplyr::group_by(province) %>%
  dplyr::mutate(pct = n / sum(n) * 100) %>%
  dplyr::ungroup()

prov_dominant <- prov_vote %>%
  dplyr::group_by(province, prov_abbr) %>%
  dplyr::slice_max(n, n=1, with_ties=FALSE) %>%
  dplyr::ungroup()

# Threat by party
threat_by_party <- ces_clean %>%
  dplyr::filter(vote %in% 1:6, !is.na(cultural_index)) %>%
  dplyr::group_by(vote_label) %>%
  dplyr::summarise(
    cultural  = mean(cultural_index, na.rm=TRUE),
    economic  = mean(economic_index, na.rm=TRUE),
    security  = mean(security_index, na.rm=TRUE),
    populist  = mean(populist_index, na.rm=TRUE),
    n         = dplyr::n(),
    .groups   = "drop"
  )

# -- 6. Load Canada map --------------------------------------------------------
canada_map <- tryCatch({
  m <- geodata::gadm(country="CAN", level=1, path=tempdir())
  m <- sf::st_as_sf(m)
  m$province <- gsub("Québec", "Quebec", m$NAME_1)
  m
}, error=function(e) { message("Map load failed: ", e$message); NULL })

# Helper: join sf to data
merge_sf <- function(sf_obj, data_df, by_col="province") {
  if (is.null(sf_obj)) return(NULL)
  dplyr::left_join(sf_obj, data_df, by=setNames(by_col, "province"))
}

sf_threat   <- merge_sf(canada_map, prov_threat)
sf_dominant <- merge_sf(canada_map, prov_dominant)

# -- 7. UI ---------------------------------------------------------------------
ui <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(
    title     = "Immigration Threat & Voting in Canada",
    titleWidth = 340
  ),
  shinydashboard::dashboardSidebar(
    width = 270,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Threat by Province",  tabName="tab_threat", icon=icon("map")),
      shinydashboard::menuItem("Voting by Province",  tabName="tab_vote",   icon=icon("vote-yea")),
      shinydashboard::menuItem("Threat by Party",     tabName="tab_party",  icon=icon("chart-bar"))
    ),
    tags$hr(style="border-color:#3d5166;"),
    
    # Tab 1 controls
    conditionalPanel(
      condition="input.tabs == 'tab_threat'",
      tags$div(style="padding:0 15px;",
               tags$p(tags$b("Threat Domain"), style="color:#3498db;font-size:13px;margin-bottom:6px;"),
               selectInput("threat_domain", "Show:",
                           choices=c(
                             "Cultural Threat (minorities adapt + culture harmed)" = "mean_cultural",
                             "Economic Threat (immigrants bad for economy)"        = "mean_economic",
                             "Security Threat (immigrants increase crime)"         = "mean_security",
                             "Populist Threat (majority over minority rights)"     = "mean_populist"
                           ), selected="mean_cultural")
      )
    ),
    
    # Tab 2 controls
    conditionalPanel(
      condition="input.tabs == 'tab_vote'",
      tags$div(style="padding:0 15px;",
               tags$p(tags$b("Party Filter"), style="color:#e74c3c;font-size:13px;margin-bottom:6px;"),
               selectInput("party_view", "Show:",
                           choices=c(
                             "Dominant party"    = "dominant",
                             "Conservative"      = "Conservative",
                             "Liberal"           = "Liberal",
                             "NDP"               = "NDP",
                             "Bloc Quebecois"    = "Bloc Quebecois",
                             "Green"             = "Green",
                             "People's Party"    = "People's Party"
                           ), selected="dominant")
      )
    ),
    
    # Tab 3 controls
    conditionalPanel(
      condition="input.tabs == 'tab_party'",
      tags$div(style="padding:0 15px;",
               tags$p(tags$b("Threat Domain"), style="color:#8e44ad;font-size:13px;margin-bottom:6px;"),
               radioButtons("party_threat", "Show:",
                            choices=c(
                              "Cultural Threat"  = "cultural",
                              "Economic Threat"  = "economic",
                              "Security Threat"  = "security",
                              "Populist Threat"  = "populist"
                            ), selected="cultural")
      )
    ),
    
    tags$hr(style="border-color:#3d5166;"),
    tags$div(
      style="padding:0 15px;color:#95a5a6;font-size:11px;line-height:1.8;",
      HTML("<b>Data:</b> CES 2019 Online Survey<br>
            <b>Nativism items:</b> pes19_nativism1–5<br>
            <b>Vote:</b> cps19_votechoice<br>
            Scale: 1 (low threat) → 5 (high threat)")
    )
  ),
  
  shinydashboard::dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background:#ecf0f1; }
      .box { border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,0.1); }
      .map-note { font-size:11px; color:#666; margin:5px 0 0 0; }
    "))),
    shinydashboard::tabItems(
      
      # Tab 1: Threat map
      shinydashboard::tabItem(tabName="tab_threat",
                              fluidRow(shinydashboard::box(
                                title=tags$b("Immigration Threat Perception by Province (CES 2019)"),
                                width=12, status="primary", solidHeader=TRUE,
                                leaflet::leafletOutput("map_threat", height=500),
                                tags$p("Darker = higher threat. Click province for details.", class="map-note")
                              )),
                              fluidRow(shinydashboard::box(
                                title=tags$b("Province Rankings -- Threat Perception"),
                                width=12, status="primary", solidHeader=TRUE,
                                plotOutput("bar_threat", height=300)
                              ))
      ),
      
      # Tab 2: Vote map
      shinydashboard::tabItem(tabName="tab_vote",
                              fluidRow(shinydashboard::box(
                                title=uiOutput("vote_map_title"),
                                width=12, status="danger", solidHeader=TRUE,
                                leaflet::leafletOutput("map_vote", height=500),
                                tags$p("Click province for vote breakdown.", class="map-note")
                              )),
                              fluidRow(shinydashboard::box(
                                title=uiOutput("vote_bar_title"),
                                width=12, status="danger", solidHeader=TRUE,
                                plotOutput("bar_vote", height=300)
                              ))
      ),
      
      # Tab 3: Threat by party
      shinydashboard::tabItem(tabName="tab_party",
                              fluidRow(shinydashboard::box(
                                title=tags$b("Immigration Threat Perception by Federal Party (CES 2019)"),
                                width=12, status="warning", solidHeader=TRUE,
                                plotOutput("bar_party", height=450),
                                tags$p(
                                  "Scale 1–5 (1=low threat, 5=high threat). CES 2019, unweighted means.
             Cultural = minorities should adapt + culture harmed by immigration.
             Economic = immigrants bad for economy. Security = immigrants increase crime.
             Populist = majority will over minority rights.",
                                  class="map-note"
                                )
                              ))
      )
    )
  )
)

# -- 8. Server -----------------------------------------------------------------
server <- function(input, output, session) {
  
  threat_col <- reactive({ input$threat_domain })
  
  threat_title <- reactive({
    switch(input$threat_domain,
           "mean_cultural" = "Mean Cultural Threat (minorities adapt + culture harmed)",
           "mean_economic" = "Mean Economic Threat (immigrants bad for economy)",
           "mean_security" = "Mean Security Threat (immigrants increase crime)",
           "mean_populist" = "Mean Populist Threat (majority over minority rights)"
    )
  })
  
  # -- Tab 1: Threat map -------------------------------------------------------
  output$map_threat <- leaflet::renderLeaflet({
    req(!is.null(sf_threat))
    col <- threat_col()
    vals <- sf_threat[[col]]
    pal  <- leaflet::colorNumeric(c("#d6eaf8","#1a5276"), domain=c(1,5), na.color="#cccccc")
    
    popup_html <- ifelse(
      is.na(sf_threat$province),
      paste0("<b>", sf_threat$NAME_1, "</b><br><i>No data</i>"),
      sprintf(
        "<div style='font-family:Arial;min-width:220px'>
         <b style='font-size:14px'>%s (%s)</b><hr style='margin:5px 0'>
         <b>Cultural Threat:</b> %.2f / 5<br>
         <b>Economic Threat:</b> %.2f / 5<br>
         <b>Security Threat:</b> %.2f / 5<br>
         <b>Populist Threat:</b> %.2f / 5<hr style='margin:5px 0'>
         <i style='font-size:11px;color:#888'>n = %d respondents</i></div>",
        sf_threat$province, sf_threat$prov_abbr,
        sf_threat$mean_cultural, sf_threat$mean_economic,
        sf_threat$mean_security, sf_threat$mean_populist,
        sf_threat$n
      )
    )
    
    leaflet::leaflet(sf_threat) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::setView(lng=-96.8, lat=58, zoom=3) %>%
      leaflet::addPolygons(
        fillColor=~pal(vals), fillOpacity=0.8, color="white", weight=1.5,
        highlightOptions=leaflet::highlightOptions(weight=3, color="#333", fillOpacity=0.95, bringToFront=TRUE),
        popup=popup_html,
        label=ifelse(is.na(sf_threat$province), sf_threat$NAME_1,
                     paste0(sf_threat$province, ": ", round(vals, 2))),
        labelOptions=leaflet::labelOptions(textsize="13px")
      ) %>%
      leaflet::addLegend("bottomright", pal=pal, values=~vals,
                         title="Threat Score<br>(1–5)", opacity=0.9,
                         labFormat=leaflet::labelFormat(digits=1))
  })
  
  output$bar_threat <- renderPlot({
    col <- threat_col()
    d   <- prov_threat
    d$y <- d[[col]]
    d   <- dplyr::arrange(d, desc(y))
    d$prov_abbr <- factor(d$prov_abbr, levels=d$prov_abbr)
    
    ggplot2::ggplot(d, ggplot2::aes(x=prov_abbr, y=y, fill=y)) +
      ggplot2::geom_col(width=0.7, show.legend=FALSE) +
      ggplot2::geom_text(ggplot2::aes(label=round(y,2)), vjust=-0.4, size=3, color="#333") +
      ggplot2::scale_fill_gradient(low="#d6eaf8", high="#1a5276") +
      ggplot2::scale_y_continuous(limits=c(0,5), expand=ggplot2::expansion(mult=c(0,0.1))) +
      ggplot2::labs(x=NULL, y="Mean Threat Score (1–5)", title=threat_title()) +
      ggplot2::theme_minimal(base_size=11) +
      ggplot2::theme(
        plot.title=ggplot2::element_text(face="bold", size=11, color="#1a4782"),
        panel.grid.major.x=ggplot2::element_blank(),
        axis.text.x=ggplot2::element_text(face="bold", size=10)
      )
  })
  
  # -- Tab 2: Vote map ---------------------------------------------------------
  output$vote_map_title <- renderUI({
    p <- input$party_view
    if (p=="dominant") { col <- "#c0392b"; label <- "Dominant Party by Province (CES 2019)"
    } else { col <- unname(party_colors[p]); if(is.na(col)) col <- "#888"
    label <- paste0(p, " Support by Province (CES 2019)") }
    tags$b(style=paste0("color:",col,";"), label)
  })
  
  output$vote_bar_title <- renderUI({
    p <- input$party_view
    if (p=="dominant") { col <- "#c0392b"; label <- "Province Rankings -- Dominant Party"
    } else { col <- unname(party_colors[p]); if(is.na(col)) col <- "#888"
    label <- paste0("Province Rankings -- ", p, " Support") }
    tags$b(style=paste0("color:",col,";"), label)
  })
  
  output$map_vote <- leaflet::renderLeaflet({
    req(!is.null(canada_map))
    
    if (input$party_view == "dominant") {
      d_sf   <- merge_sf(canada_map, prov_dominant)
      fcols  <- unname(party_colors[d_sf$vote_label]); fcols[is.na(fcols)] <- "#cccccc"
      popup_html <- ifelse(
        is.na(d_sf$province),
        paste0("<b>", d_sf$NAME_1, "</b><br><i>No data</i>"),
        sprintf(
          "<div style='font-family:Arial;min-width:210px'>
           <b style='font-size:14px'>%s (%s)</b><hr style='margin:5px 0'>
           <b>Dominant party:</b>
           <span style='background:%s;color:white;padding:1px 7px;border-radius:3px'>%s</span><br>
           <b>Support:</b> %.1f%%<br>
           <i style='font-size:11px;color:#888'>n = %d</i></div>",
          d_sf$province, d_sf$prov_abbr,
          unname(party_colors[d_sf$vote_label]), d_sf$vote_label,
          d_sf$pct, d_sf$n
        )
      )
      leaflet::leaflet(d_sf) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::setView(lng=-96.8, lat=58, zoom=3) %>%
        leaflet::addPolygons(
          fillColor=fcols, fillOpacity=0.8, color="white", weight=1.5,
          highlightOptions=leaflet::highlightOptions(weight=3, color="#333", fillOpacity=0.95, bringToFront=TRUE),
          popup=popup_html,
          label=ifelse(is.na(d_sf$province), d_sf$NAME_1,
                       paste0(d_sf$province, " — ", d_sf$vote_label, " (", round(d_sf$pct,1), "%)")),
          labelOptions=leaflet::labelOptions(textsize="13px")
        ) %>%
        leaflet::addLegend("bottomright", colors=unname(party_colors),
                           labels=names(party_colors), title="Dominant Party", opacity=0.9)
      
    } else {
      party <- input$party_view
      d_sf  <- merge_sf(canada_map, dplyr::filter(prov_vote, vote_label==party))
      pcol  <- unname(party_colors[party]); if(is.na(pcol)) pcol <- "#888"
      pal   <- leaflet::colorNumeric(c("#eeeeee", pcol), domain=c(0,100), na.color="#cccccc")
      popup_html <- ifelse(
        is.na(d_sf$province),
        paste0("<b>", d_sf$NAME_1, "</b><br><i>No data</i>"),
        sprintf(
          "<div style='font-family:Arial;min-width:200px'>
           <b style='font-size:14px'>%s (%s)</b><hr style='margin:5px 0'>
           <b><span style='background:%s;color:white;padding:1px 6px;border-radius:3px'>%s</span>
           support:</b> %.1f%%<br>
           <i style='font-size:11px;color:#888'>n = %d</i></div>",
          d_sf$province, d_sf$prov_abbr, pcol, party, d_sf$pct, d_sf$n
        )
      )
      leaflet::leaflet(d_sf) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::setView(lng=-96.8, lat=58, zoom=3) %>%
        leaflet::addPolygons(
          fillColor=~pal(pct), fillOpacity=0.8, color="white", weight=1.5,
          highlightOptions=leaflet::highlightOptions(weight=3, color="#333", fillOpacity=0.95, bringToFront=TRUE),
          popup=popup_html,
          label=ifelse(is.na(d_sf$province), d_sf$NAME_1,
                       paste0(d_sf$province, ": ", round(d_sf$pct,1), "%")),
          labelOptions=leaflet::labelOptions(textsize="13px")
        ) %>%
        leaflet::addLegend("bottomright", pal=pal, values=~pct,
                           title=paste0(party,"<br>Support"), opacity=0.9,
                           labFormat=leaflet::labelFormat(suffix="%", digits=0))
    }
  })
  
  output$bar_vote <- renderPlot({
    if (input$party_view == "dominant") {
      d <- dplyr::arrange(prov_dominant, desc(pct))
      d$prov_abbr <- factor(d$prov_abbr, levels=d$prov_abbr)
      ggplot2::ggplot(d, ggplot2::aes(x=prov_abbr, y=pct, fill=vote_label)) +
        ggplot2::geom_col(width=0.7) +
        ggplot2::geom_text(ggplot2::aes(label=paste0(round(pct,1),"%")), vjust=-0.4, size=3, color="#333") +
        ggplot2::scale_fill_manual(values=party_colors, name="Dominant party") +
        ggplot2::scale_y_continuous(labels=scales::percent_format(scale=1),
                                    expand=ggplot2::expansion(mult=c(0,0.12))) +
        ggplot2::labs(x=NULL, y="% Support", title="Dominant party support by province") +
        ggplot2::theme_minimal(base_size=11) +
        ggplot2::theme(panel.grid.major.x=ggplot2::element_blank(),
                       axis.text.x=ggplot2::element_text(face="bold", size=10),
                       legend.position="bottom", legend.text=ggplot2::element_text(size=9))
    } else {
      party <- input$party_view
      d <- dplyr::arrange(dplyr::filter(prov_vote, vote_label==party), desc(pct))
      if (nrow(d)==0) return(NULL)
      d$prov_abbr <- factor(d$prov_abbr, levels=d$prov_abbr)
      pcol <- unname(party_colors[party]); if(is.na(pcol)) pcol <- "#888"
      ggplot2::ggplot(d, ggplot2::aes(x=prov_abbr, y=pct)) +
        ggplot2::geom_col(fill=pcol, width=0.7) +
        ggplot2::geom_text(ggplot2::aes(label=paste0(round(pct,1),"%")), vjust=-0.4, size=3, color="#333") +
        ggplot2::scale_y_continuous(labels=scales::percent_format(scale=1),
                                    expand=ggplot2::expansion(mult=c(0,0.12))) +
        ggplot2::labs(x=NULL, y="% Support", title=paste0(party," support by province")) +
        ggplot2::theme_minimal(base_size=11) +
        ggplot2::theme(panel.grid.major.x=ggplot2::element_blank(),
                       axis.text.x=ggplot2::element_text(face="bold", size=10),
                       plot.title=ggplot2::element_text(color=pcol, face="bold"))
    }
  })
  
  # -- Tab 3: Threat by party --------------------------------------------------
  output$bar_party <- renderPlot({
    col   <- input$party_threat
    d     <- threat_by_party
    d$y   <- d[[col]]
    d     <- dplyr::arrange(d, desc(y))
    d$vote_label <- factor(d$vote_label, levels=d$vote_label)
    d$fcol <- unname(party_colors[as.character(d$vote_label)])
    d$fcol[is.na(d$fcol)] <- "#888888"
    
    title_map <- c(
      cultural = "Cultural Threat by Party\n(minorities should adapt + immigration harms culture)",
      economic = "Economic Threat by Party\n(immigrants bad for economy)",
      security = "Security Threat by Party\n(immigrants increase crime rates)",
      populist = "Populist Threat by Party\n(majority will over minority rights)"
    )
    
    ggplot2::ggplot(d, ggplot2::aes(x=vote_label, y=y, fill=vote_label)) +
      ggplot2::geom_col(width=0.65, show.legend=FALSE) +
      ggplot2::geom_text(ggplot2::aes(label=round(y,2)),
                         vjust=-0.4, size=5, fontface="bold", color="#333") +
      ggplot2::scale_fill_manual(values=party_colors) +
      ggplot2::scale_y_continuous(limits=c(1,5), expand=ggplot2::expansion(mult=c(0,0.1))) +
      ggplot2::geom_hline(yintercept=3, linetype="dashed", color="#aaa", linewidth=0.5) +
      ggplot2::annotate("text", x=Inf, y=3.1, label="Midpoint (3)", hjust=1.1, size=3, color="#aaa") +
      ggplot2::labs(
        x       = NULL,
        y       = "Mean Threat Score (1=low, 5=high)",
        title   = title_map[col],
        caption = "CES 2019 | Respondents who stated a vote intention (n varies by party)"
      ) +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(
        plot.title         = ggplot2::element_text(face="bold", size=13),
        plot.caption       = ggplot2::element_text(color="#888", size=10),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.text.x        = ggplot2::element_text(face="bold", size=12)
      )
  })
}

shinyApp(ui, server)