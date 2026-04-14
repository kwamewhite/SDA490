# =============================================================================
# Who's Afraid of Democracy? — Interactive Research Dashboard
# Threat Perception & Political Ideology in Canada (CES 2019)
#
# Tab 1: The Threat Landscape — map of threat by province
# Tab 2: Threat & Ideology — relationship between threat and ideology
# Tab 3: Who Feels Threatened? — threat by gender and age
#
# Run from the same folder as your data files
# =============================================================================

if (interactive() && requireNamespace("rstudioapi", quietly=TRUE)) {
  path <- rstudioapi::getActiveDocumentContext()$path
  if (nchar(path) > 0) setwd(dirname(path))
}

pkgs <- c("shiny","shinydashboard","leaflet","haven","dplyr",
          "ggplot2","sf","geodata","tidyr","scales")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, repos="https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only=TRUE))

# ── Data Setup ────────────────────────────────────────────────────────────────
ces <- haven::read_dta("2019 Canadian Election Study - Online Survey v1.0.dta")

prov_labels <- c(
  "14"="Alberta","15"="British Columbia","16"="Manitoba",
  "17"="New Brunswick","18"="Newfoundland and Labrador",
  "19"="Northwest Territories","20"="Nova Scotia","21"="Nunavut",
  "22"="Ontario","23"="Prince Edward Island","24"="Quebec",
  "25"="Saskatchewan","26"="Yukon"
)

ces_prov <- ces %>%
  mutate(
    province       = prov_labels[as.character(as.numeric(cps19_province))],
    nav1           = ifelse(as.numeric(pes19_nativism1)==6, NA, as.numeric(pes19_nativism1)),
    nav2           = ifelse(as.numeric(pes19_nativism2)==6, NA, as.numeric(pes19_nativism2)),
    nav3           = ifelse(as.numeric(pes19_nativism3)==6, NA, as.numeric(pes19_nativism3)),
    nav4           = ifelse(as.numeric(pes19_nativism4)==6, NA, as.numeric(pes19_nativism4)),
    nav5           = ifelse(as.numeric(pes19_nativism5)==6, NA, as.numeric(pes19_nativism5)),
    nav3_r         = 6 - nav3,
    cultural_index = rowMeans(cbind(nav1, nav4), na.rm=TRUE),
    economic_index = nav3_r,
    security_index = nav5,
    populist_index = nav2,
    ideology       = as.numeric(cps19_lr_scale_bef_1),
    gender         = case_when(
      as.numeric(cps19_gender)==1 ~ "Man",
      as.numeric(cps19_gender)==2 ~ "Woman",
      TRUE ~ NA_character_
    ),
    age_group = factor(case_when(
      as.numeric(cps19_age) < 35  ~ "18-34",
      as.numeric(cps19_age) < 50  ~ "35-49",
      as.numeric(cps19_age) < 65  ~ "50-64",
      as.numeric(cps19_age) >= 65 ~ "65+",
      TRUE ~ NA_character_
    ), levels=c("18-34","35-49","50-64","65+"))
  ) %>%
  filter(!is.na(province), !is.na(cultural_index))

# Province-level threat summaries
prov_threat <- ces_prov %>%
  group_by(province) %>%
  summarise(
    Cultural  = mean(cultural_index, na.rm=TRUE),
    Economic  = mean(economic_index, na.rm=TRUE),
    Security  = mean(security_index, na.rm=TRUE),
    Populist  = mean(populist_index, na.rm=TRUE),
    n         = n(),
    .groups   = "drop"
  )

# Canada map
canada_map <- tryCatch({
  m <- geodata::gadm(country="CAN", level=1, path=tempdir())
  m <- sf::st_as_sf(m)
  m$province <- gsub("Quebec","Quebec", gsub("Québec","Quebec", m$NAME_1))
  m
}, error=function(e) NULL)

sf_threat <- if (!is.null(canada_map))
  dplyr::left_join(canada_map, prov_threat, by="province") else NULL

# Ideology summary by threat level
make_ideology_summary <- function(threat_col) {
  ces_prov %>%
    filter(!is.na(ideology), ideology %in% 0:10, !is.na(.data[[threat_col]])) %>%
    mutate(threat_group = case_when(
      .data[[threat_col]] <= 2 ~ "Low Threat",
      .data[[threat_col]] == 3 ~ "Neutral",
      .data[[threat_col]] >= 4 ~ "High Threat"
    ),
    threat_group = factor(threat_group, levels=c("Low Threat","Neutral","High Threat"))
    ) %>%
    group_by(threat_group) %>%
    summarise(mean_ideology=mean(ideology, na.rm=TRUE), n=n(), .groups="drop")
}

DARK_RED <- "#5A0021"
CREAM    <- "#F5F0E8"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- shinydashboard::dashboardPage(
  skin = "red",
  shinydashboard::dashboardHeader(
    title     = "Who's Afraid of Democracy?",
    titleWidth = 310
  ),
  shinydashboard::dashboardSidebar(
    width = 270,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("The Threat Landscape",  tabName="tab_map",  icon=icon("map")),
      shinydashboard::menuItem("Threat & Ideology",     tabName="tab_ideo", icon=icon("chart-bar")),
      shinydashboard::menuItem("Who Feels Threatened?", tabName="tab_who",  icon=icon("users"))
    ),
    tags$hr(style="border-color:#7a1030;"),

    conditionalPanel(condition="input.tabs == 'tab_map'",
      tags$div(style="padding:0 15px;",
        tags$p(tags$b("Threat Domain"), style="color:#ffaaaa;font-size:13px;margin-bottom:6px;"),
        selectInput("map_domain", "Show on map:",
          choices=c(
            "Cultural Threat" = "Cultural",
            "Economic Threat" = "Economic",
            "Security Threat" = "Security",
            "Populist Threat" = "Populist"
          ), selected="Cultural")
      )
    ),

    conditionalPanel(condition="input.tabs == 'tab_ideo'",
      tags$div(style="padding:0 15px;",
        tags$p(tags$b("Threat Domain"), style="color:#ffaaaa;font-size:13px;margin-bottom:6px;"),
        radioButtons("ideo_domain", "Select threat:",
          choices=c(
            "Cultural"  = "cultural_index",
            "Economic"  = "economic_index",
            "Security"  = "security_index",
            "Populist"  = "populist_index"
          ), selected="cultural_index")
      )
    ),

    conditionalPanel(condition="input.tabs == 'tab_who'",
      tags$div(style="padding:0 15px;",
        tags$p(tags$b("Break down by"), style="color:#ffaaaa;font-size:13px;margin-bottom:6px;"),
        radioButtons("who_by", "Group:",
          choices=c("Gender"="gender","Age Group"="age_group"),
          selected="gender"),
        tags$br(),
        tags$p(tags$b("Threat Domain"), style="color:#ffaaaa;font-size:13px;margin-bottom:6px;"),
        radioButtons("who_domain", "Show:",
          choices=c(
            "Cultural"  = "cultural_index",
            "Economic"  = "economic_index",
            "Security"  = "security_index",
            "Populist"  = "populist_index"
          ), selected="cultural_index")
      )
    ),

    tags$hr(style="border-color:#7a1030;"),
    tags$div(
      style="padding:0 15px;color:#ccaaaa;font-size:11px;line-height:1.8;",
      HTML("<b>Data:</b> CES 2019 Online Survey<br>
            <b>n =</b> 37,000+ Canadians<br>
            <b>Threat scale:</b> 1 (low) to 5 (high)<br>
            <b>Ideology:</b> 0 (left) to 10 (right)<br><br>
            <b>Threat domains:</b><br>
            Cultural: minorities adapt +<br>
            &nbsp;&nbsp;culture harmed<br>
            Economic: immigrants bad<br>
            &nbsp;&nbsp;for economy (reversed)<br>
            Security: immigrants increase<br>
            &nbsp;&nbsp;crime<br>
            Populist: majority will over<br>
            &nbsp;&nbsp;minority rights")
    )
  ),

  shinydashboard::dashboardBody(
    tags$head(tags$style(HTML(paste0("
      .content-wrapper { background:", CREAM, "; }
      .box { border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,0.1); }
      .map-note { font-size:11px; color:#888; margin:6px 0 0 0; }
    ")))),

    shinydashboard::tabItems(

      # ── Tab 1 ───────────────────────────────────────────────────────────────
      shinydashboard::tabItem(tabName="tab_map",
        fluidRow(shinydashboard::box(
          title  = uiOutput("map_title"),
          width=12, status="danger", solidHeader=TRUE,
          leaflet::leafletOutput("map_threat", height=430),
          tags$p("Province colour = mean threat score (1–5). Click province for full breakdown.", class="map-note")
        )),
        fluidRow(shinydashboard::box(
          title  = tags$b("Province Rankings"),
          width=12, status="danger", solidHeader=TRUE,
          plotOutput("bar_prov", height=250)
        ))
      ),

      # ── Tab 2 ───────────────────────────────────────────────────────────────
      shinydashboard::tabItem(tabName="tab_ideo",
        fluidRow(
          shinydashboard::box(
            title  = uiOutput("ideo_title"),
            width=7, status="danger", solidHeader=TRUE,
            plotOutput("bar_ideo", height=360),
            tags$p("Mean ideology by threat level. Ideology: 0=far left, 10=far right.", class="map-note")
          ),
          shinydashboard::box(
            title  = tags$b("Correlation: All Domains vs Ideology"),
            width=5, status="danger", solidHeader=TRUE,
            plotOutput("bar_all_domains", height=280),
            tags$p("Higher bar = stronger relationship with right-wing ideology.", class="map-note")
          )
        ),
        fluidRow(shinydashboard::box(
          width=12, status="danger", solidHeader=FALSE,
          tags$div(style=paste0("background:",DARK_RED,";color:white;border-radius:8px;padding:16px 24px;"),
            tags$h4("Key Finding", style="color:#ffaaaa;font-weight:bold;margin-top:0;"),
            tags$p(HTML("<b>Cultural and populist threat</b> are the strongest predictors of right-wing ideology. Economic and security fears drop out once cultural threat is controlled for. The effect holds after controlling for age, income, and education."),
                   style="color:#f0d0d0;font-size:13px;margin:0;")
          )
        ))
      ),

      # ── Tab 3 ───────────────────────────────────────────────────────────────
      shinydashboard::tabItem(tabName="tab_who",
        fluidRow(
          shinydashboard::box(
            title  = uiOutput("who_title"),
            width=7, status="danger", solidHeader=TRUE,
            plotOutput("bar_who", height=360),
            tags$p("Mean threat score by group. Scale: 1 = low threat, 5 = high threat.", class="map-note")
          ),
          shinydashboard::box(
            title  = tags$b("Gender Finding"),
            width=5, status="danger", solidHeader=FALSE,
            tags$div(style=paste0("background:",DARK_RED,";color:white;border-radius:8px;padding:16px 24px;"),
              tags$h4("The mechanism differs by gender.", style="color:white;font-weight:bold;margin-top:0;"),
              tags$hr(style="border-color:#7a1030;margin:10px 0;"),
              tags$p(tags$b("Men: ", style="color:#ffaaaa;"),
                     "Cultural threat drives the rightward shift.",
                     style="color:#f0d0d0;font-size:13px;"),
              tags$p(tags$b("Women: ", style="color:#ffaaaa;"),
                     "Populist threat drives the rightward shift.",
                     style="color:#f0d0d0;font-size:13px;"),
              tags$hr(style="border-color:#7a1030;margin:10px 0;"),
              tags$p("Right-wing political messaging may work through different emotional channels depending on the audience.",
                     style="color:#ccaaaa;font-size:12px;font-style:italic;margin:0;")
            )
          )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  domain_label <- function(d) switch(d,
    "Cultural"       = "Cultural Threat",
    "Economic"       = "Economic Threat",
    "Security"       = "Security Threat",
    "Populist"       = "Populist Threat",
    "cultural_index" = "Cultural Threat",
    "economic_index" = "Economic Threat",
    "security_index" = "Security Threat",
    "populist_index" = "Populist Threat"
  )

  # Tab 1
  output$map_title <- renderUI({
    tags$b(paste("Immigration Threat by Province —", domain_label(input$map_domain), "(CES 2019)"))
  })

  output$map_threat <- leaflet::renderLeaflet({
    req(!is.null(sf_threat))
    col  <- input$map_domain
    vals <- sf_threat[[col]]
    pal  <- leaflet::colorNumeric(c("#fde0e0", DARK_RED), domain=c(1,5), na.color="#cccccc")

    popup_html <- ifelse(
      is.na(sf_threat$province),
      paste0("<b>",sf_threat$NAME_1,"</b><br><i>No data</i>"),
      sprintf(
        "<div style='font-family:Arial;min-width:230px'>
         <b style='font-size:14px'>%s</b><hr style='margin:5px 0'>
         <b>Cultural:</b> %.2f / 5<br>
         <b>Economic:</b> %.2f / 5<br>
         <b>Security:</b> %.2f / 5<br>
         <b>Populist:</b> %.2f / 5<hr style='margin:5px 0'>
         <i style='font-size:11px;color:#888'>n = %d respondents</i></div>",
        sf_threat$province,
        sf_threat$Cultural, sf_threat$Economic,
        sf_threat$Security, sf_threat$Populist,
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
                     paste0(sf_threat$province,": ", round(vals,2)," / 5")),
        labelOptions=leaflet::labelOptions(textsize="13px")
      ) %>%
      leaflet::addLegend("bottomright", pal=pal, values=~vals,
                         title="Threat Score<br>(1–5)", opacity=0.9,
                         labFormat=leaflet::labelFormat(digits=1))
  })

  output$bar_prov <- renderPlot({
    col <- input$map_domain
    d   <- prov_threat
    d$y <- d[[col]]
    d   <- dplyr::arrange(d, desc(y))
    abbr_map <- c(
      "Alberta"="AB","British Columbia"="BC","Manitoba"="MB",
      "New Brunswick"="NB","Newfoundland and Labrador"="NL",
      "Northwest Territories"="NWT","Nova Scotia"="NS","Nunavut"="NU",
      "Ontario"="ON","Prince Edward Island"="PEI","Quebec"="QC",
      "Saskatchewan"="SK","Yukon"="YT"
    )
    d$abbr <- abbr_map[d$province]
    d$abbr <- factor(d$abbr, levels=d$abbr)

    ggplot2::ggplot(d, ggplot2::aes(x=abbr, y=y, fill=y)) +
      ggplot2::geom_col(width=0.7, show.legend=FALSE) +
      ggplot2::geom_text(ggplot2::aes(label=round(y,2)), vjust=-0.4, size=3, color="#333") +
      ggplot2::scale_fill_gradient(low="#fde0e0", high=DARK_RED) +
      ggplot2::scale_y_continuous(limits=c(0,5), expand=ggplot2::expansion(mult=c(0,0.12))) +
      ggplot2::labs(x=NULL, y="Mean Threat Score (1–5)") +
      ggplot2::theme_minimal(base_size=11) +
      ggplot2::theme(panel.grid.major.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_text(face="bold", size=10))
  })

  # Tab 2
  output$ideo_title <- renderUI({
    tags$b(paste(domain_label(input$ideo_domain), "and Political Ideology (CES 2019)"))
  })

  output$bar_ideo <- renderPlot({
    d <- make_ideology_summary(input$ideo_domain)
    ggplot2::ggplot(d, ggplot2::aes(x=threat_group, y=mean_ideology, fill=mean_ideology)) +
      ggplot2::geom_col(width=0.6, show.legend=FALSE) +
      ggplot2::geom_text(ggplot2::aes(label=round(mean_ideology,1)),
                         vjust=-0.4, size=7, fontface="bold", color="#333") +
      ggplot2::scale_fill_gradient(low="#2c7bb6", high=DARK_RED) +
      ggplot2::scale_y_continuous(limits=c(0,10), expand=ggplot2::expansion(mult=c(0,0.1))) +
      ggplot2::geom_hline(yintercept=5, linetype="dashed", color="#aaa", linewidth=0.5) +
      ggplot2::annotate("text", x=3.4, y=5.2, label="Midpoint (5)", size=3.5, color="#aaa") +
      ggplot2::labs(x=NULL, y="Mean Ideology (0=Left, 10=Right)") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(panel.grid.major.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_text(size=13, face="bold"))
  })

  output$bar_all_domains <- renderPlot({
    d <- ces_prov %>%
      filter(ideology %in% 0:10) %>%
      summarise(
        Cultural = cor(cultural_index, ideology, use="complete.obs"),
        Economic = cor(economic_index, ideology, use="complete.obs"),
        Security = cor(security_index, ideology, use="complete.obs"),
        Populist = cor(populist_index, ideology, use="complete.obs")
      ) %>%
      tidyr::pivot_longer(everything(), names_to="domain", values_to="r") %>%
      dplyr::arrange(desc(r)) %>%
      dplyr::mutate(domain=factor(domain, levels=domain))

    ggplot2::ggplot(d, ggplot2::aes(x=domain, y=r, fill=r)) +
      ggplot2::geom_col(width=0.6, show.legend=FALSE) +
      ggplot2::geom_text(ggplot2::aes(label=round(r,2)),
                         vjust=-0.4, size=4.5, fontface="bold", color="#333") +
      ggplot2::scale_fill_gradient(low="#fde0e0", high=DARK_RED) +
      ggplot2::scale_y_continuous(limits=c(0,0.4), expand=ggplot2::expansion(mult=c(0,0.12))) +
      ggplot2::labs(x=NULL, y="Correlation (r)") +
      ggplot2::theme_minimal(base_size=11) +
      ggplot2::theme(panel.grid.major.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_text(face="bold", size=11))
  })

  # Tab 3
  output$who_title <- renderUI({
    group <- if (input$who_by=="gender") "Gender" else "Age Group"
    tags$b(paste(domain_label(input$who_domain), "by", group, "(CES 2019)"))
  })

  output$bar_who <- renderPlot({
    group_var  <- input$who_by
    threat_var <- input$who_domain

    d <- ces_prov %>%
      filter(!is.na(.data[[group_var]]), !is.na(.data[[threat_var]])) %>%
      group_by(group=.data[[group_var]]) %>%
      summarise(mean_threat=mean(.data[[threat_var]], na.rm=TRUE), n=n(), .groups="drop") %>%
      dplyr::mutate(group=as.character(group))

    fill_colors <- if (group_var=="gender") {
      c("Man"="#1A4782","Woman"=DARK_RED)
    } else {
      c("18-34"="#1A4782","35-49"=DARK_RED,"50-64"="#E8A838","65+"="#2ECC71")
    }

    ggplot2::ggplot(d, ggplot2::aes(x=group, y=mean_threat, fill=group)) +
      ggplot2::geom_col(width=0.6, show.legend=FALSE) +
      ggplot2::geom_text(ggplot2::aes(label=round(mean_threat,2)),
                         vjust=-0.4, size=6, fontface="bold", color="#333") +
      ggplot2::scale_fill_manual(values=fill_colors) +
      ggplot2::scale_y_continuous(limits=c(0,5), expand=ggplot2::expansion(mult=c(0,0.12))) +
      ggplot2::geom_hline(yintercept=3, linetype="dashed", color="#aaa", linewidth=0.5) +
      ggplot2::annotate("text", x=Inf, y=3.12, label="Midpoint (3)", hjust=1.1, size=3.5, color="#aaa") +
      ggplot2::labs(x=NULL, y="Mean Threat Score (1–5)") +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(panel.grid.major.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_text(face="bold", size=12))
  })
}

shinyApp(ui, server)
