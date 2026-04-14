# ══════════════════════════════════════════════════════════════════════════════
# Immigration Threat & Political Ideology — CES 2019 Analysis
# Monday Presentation
#
# Research Question:
#   Do different domains of immigration threat predict political ideology
#   (left-right scale), controlling for age, income, and education?
#
# Model:
#   Ideology (0-10) = Cultural Threat + Economic Threat + Security Threat
#                   + Age + Income + Education + ε
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Packages ───────────────────────────────────────────────────────────────
pkgs <- c("shiny", "shinydashboard", "dplyr", "tidyr",
          "ggplot2", "scales", "broom")
new  <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# ── 1. Load CES 2019 ───────────────────────────────────────────────────────────
# This downloads the CES 2019 web survey (~37,000 respondents)
ces2019_web <- haven::read_dta("2019 Canadian Election Study - Online Survey v1.0.dta")
ces <- ces2019_web   # cesR creates this object automatically

# ── 2. Explore variable names (run this first to confirm names) ────────────────
# Uncomment these lines to check what's available in your dataset:
# names(ces)[grepl("lr|left|right|ideol", names(ces), ignore.case=TRUE)]
# names(ces)[grepl("imm|immigr|nativ|asylum|refugee", names(ces), ignore.case=TRUE)]
# names(ces)[grepl("age|yob|income|educ", names(ces), ignore.case=TRUE)]

# ── 3. Recode variables ────────────────────────────────────────────────────────
# NOTE: CES 2019 variable names follow the pattern cps19_* (campaign period)
# If any variable name below throws an error, run the exploration code above
# and update the name accordingly.

ces_clean <- ces %>%
  dplyr::mutate(
    
    # ── Dependent variable: Left-Right Ideology (0 = far left, 10 = far right)
    ideology = dplyr::case_when(
      as.numeric(cps19_lr_scale_bef_1) %in% 0:10 ~ as.numeric(cps19_lr_scale_bef_1),
      TRUE ~ NA_real_
    ),
    
    # ── CULTURAL THREAT ────────────────────────────────────────────────────────
    # "Immigrants are generally good for Canada's economy"
    # pes19_immigjobs: 1=Strongly agree ... 5=Strongly disagree
    # Reverse so higher = more threat
    cultural_econ = dplyr::case_when(
      as.numeric(pes19_immigjobs) %in% 1:5 ~ 6 - as.numeric(pes19_immigjobs),
      TRUE ~ NA_real_
    ),
    
    # "Immigrants make Canada more open to new ideas and cultures"
    # pes19_immigjobs: 1=Strongly agree ... 5=Strongly disagree
    # Reverse so higher = more threat
    cultural_cult = dplyr::case_when(
      as.numeric(pes19_immigjobs) %in% 1:5 ~ 6 - as.numeric(pes19_immigjobs),
      TRUE ~ NA_real_
    ),
    
    # Cultural threat index = average of both items (0-4 scale, then 0-100)
    cultural_threat = rowMeans(
      dplyr::across(c(cultural_econ, cultural_cult)), na.rm = TRUE
    ),
    cultural_threat = scales::rescale(cultural_threat, to = c(0, 100)),
    
    # ── ECONOMIC THREAT ────────────────────────────────────────────────────────
    # "Immigrants take jobs away from other Canadians"
    # pes19_immigjobs: 1=Strongly agree ... 5=Strongly disagree
    # Keep as-is after reversing (agree = more economic threat)
    economic_threat = dplyr::case_when(
      as.numeric(pes19_immigjobs) %in% 1:5 ~ 6 - as.numeric(pes19_immigjobs),
      TRUE ~ NA_real_
    ),
    economic_threat = scales::rescale(economic_threat, to = c(0, 100)),
    
    # ── SECURITY / NATIVIST THREAT ─────────────────────────────────────────────
    # "Canada should admit fewer immigrants" (nativism section)
    # pes19_immigjobs: 1=Strongly agree ... 5=Strongly disagree
    # Reverse so higher = more restrictionist/threat
    security_threat = dplyr::case_when(
      as.numeric(pes19_immigjobs) %in% 1:5 ~ 6 - as.numeric(pes19_immigjobs),
      TRUE ~ NA_real_
    ),
    security_threat = scales::rescale(security_threat, to = c(0, 100)),
    
    # ── CONTROLS ───────────────────────────────────────────────────────────────
    # Age: calculated from birth year
    age = 2019 - as.numeric(cps19_yob),
    age = dplyr::case_when(age >= 18 & age <= 100 ~ age, TRUE ~ NA_real_),
    
    # Education: cps19_education
    # 1=No schooling ... 11=Professional degree or doctorate
    education = dplyr::case_when(
      as.numeric(cps19_education) %in% 1:12 ~ as.numeric(cps19_education),
      TRUE ~ NA_real_
    ),
    
    # Income: cps19_income_cat (income categories)
    income = dplyr::case_when(
      as.numeric(cps19_income_cat) %in% 1:9 ~ as.numeric(cps19_income_cat),
      TRUE ~ NA_real_
    ),
    
    # Gender: cps19_gender (1=Man, 2=Woman, 3=Other)
    gender = dplyr::case_when(
      as.numeric(cps19_gender) == 1 ~ "Man",
      as.numeric(cps19_gender) == 2 ~ "Woman",
      TRUE ~ NA_character_
    ),
    
    # Province
    province = as.character(cps19_province)
    
  ) %>%
  # Keep only complete cases for regression
  dplyr::filter(
    !is.na(ideology),
    !is.na(cultural_threat),
    !is.na(economic_threat),
    !is.na(security_threat),
    !is.na(age),
    !is.na(education),
    !is.na(income)
  )

cat("Clean sample size:", nrow(ces_clean), "\n")

# ── 4. Descriptive summaries ───────────────────────────────────────────────────

# Mean threat by ideology group
ces_clean <- ces_clean %>%
  dplyr::mutate(
    ideology_group = dplyr::case_when(
      ideology %in% 0:3  ~ "Left (0-3)",
      ideology %in% 4:6  ~ "Centre (4-6)",
      ideology %in% 7:10 ~ "Right (7-10)"
    ),
    ideology_group = factor(ideology_group,
                            levels = c("Left (0-3)","Centre (4-6)","Right (7-10)"))
  )

threat_by_ideo <- ces_clean %>%
  dplyr::group_by(ideology_group) %>%
  dplyr::summarise(
    cultural  = mean(cultural_threat,  na.rm = TRUE),
    economic  = mean(economic_threat,  na.rm = TRUE),
    security  = mean(security_threat,  na.rm = TRUE),
    n         = dplyr::n(),
    .groups   = "drop"
  ) %>%
  tidyr::pivot_longer(cols = c(cultural, economic, security),
                      names_to = "domain", values_to = "mean_threat") %>%
  dplyr::mutate(domain = factor(domain,
                                levels = c("cultural","economic","security"),
                                labels = c("Cultural Threat","Economic Threat","Security Threat")))

# Mean ideology by threat level (high vs low)
threat_dist_data <- ces_clean %>%
  tidyr::pivot_longer(
    cols = c(cultural_threat, economic_threat, security_threat),
    names_to = "domain", values_to = "threat_score"
  ) %>%
  dplyr::mutate(domain = dplyr::recode(domain,
                                       "cultural_threat"  = "Cultural Threat",
                                       "economic_threat"  = "Economic Threat",
                                       "security_threat"  = "Security Threat"
  ))

# ── 5. Regression model ────────────────────────────────────────────────────────
model <- lm(
  ideology ~ cultural_threat + economic_threat + security_threat
  + age + income + education,
  data = ces_clean
)

# Tidy model output for plotting
model_tidy <- broom::tidy(model, conf.int = TRUE) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    term = dplyr::recode(term,
                         "cultural_threat"  = "Cultural Threat",
                         "economic_threat"  = "Economic Threat",
                         "security_threat"  = "Security Threat",
                         "age"              = "Age",
                         "income"           = "Income",
                         "education"        = "Education"
    ),
    term      = factor(term, levels = rev(c(
      "Cultural Threat","Economic Threat","Security Threat",
      "Age","Income","Education"
    ))),
    significant = p.value < 0.05,
    domain    = dplyr::case_when(
      term %in% c("Cultural Threat","Economic Threat","Security Threat") ~ "Threat",
      TRUE ~ "Control"
    )
  )

cat("\n=== REGRESSION SUMMARY ===\n")
print(summary(model))

# ══════════════════════════════════════════════════════════════════════════════
# SHINY APP
# ══════════════════════════════════════════════════════════════════════════════
ui <- shinydashboard::dashboardPage(
  skin = "blue",
  
  shinydashboard::dashboardHeader(
    title     = "Immigration Threat & Ideology — CES 2019",
    titleWidth = 400
  ),
  
  shinydashboard::dashboardSidebar(
    width = 250,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Threat Distribution",  tabName = "tab_dist",   icon = icon("chart-bar")),
      shinydashboard::menuItem("Threat by Ideology",   tabName = "tab_ideo",   icon = icon("brain")),
      shinydashboard::menuItem("Regression Results",   tabName = "tab_reg",    icon = icon("calculator")),
      shinydashboard::menuItem("Coefficient Plot",     tabName = "tab_coef",   icon = icon("dot-circle"))
    ),
    tags$hr(style = "border-color:#3d5166;"),
    tags$div(
      style = "padding:0 12px; color:#95a5a6; font-size:11px; line-height:1.9;",
      HTML("<b>Dependent variable:</b><br>
            Ideology (0=left, 10=right)<br><br>
            <b>Threat domains:</b><br>
            Cultural, Economic, Security<br><br>
            <b>Controls:</b><br>
            Age, Income, Education<br><br>
            <b>Source:</b> CES 2019 Online<br>
            n ≈ 37,000")
    )
  ),
  
  shinydashboard::dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background:#f4f6f9; }
      .box { border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,.08); }
      .note { font-size:11px; color:#888; margin:6px 0 0 0; }
      .reg-table { font-size:13px; width:100%; border-collapse:collapse; }
      .reg-table th { background:#1A4782; color:white; padding:8px 12px; text-align:left; }
      .reg-table td { padding:7px 12px; border-bottom:1px solid #eee; }
      .reg-table tr:nth-child(even) { background:#f8f8f8; }
      .sig { color:#c0392b; font-weight:bold; }
    "))),
    
    shinydashboard::tabItems(
      
      # ── Tab 1: Threat Distribution ─────────────────────────────────────────
      shinydashboard::tabItem(tabName = "tab_dist",
                              shinydashboard::box(
                                title = tags$b(icon("chart-bar"), " Distribution of Three Threat Domains"),
                                width = 12, status = "primary", solidHeader = TRUE,
                                plotOutput("plot_dist", height = 480),
                                tags$p("Each panel shows distribution of one threat domain (0=no threat, 100=maximum threat). All three are derived from CES 2019 immigration attitude questions.", class = "note")
                              )
      ),
      
      # ── Tab 2: Threat by Ideology ──────────────────────────────────────────
      shinydashboard::tabItem(tabName = "tab_ideo",
                              shinydashboard::box(
                                title = tags$b(icon("brain"), " Mean Threat Scores by Ideological Group"),
                                width = 12, status = "warning", solidHeader = TRUE,
                                plotOutput("plot_ideo", height = 480),
                                tags$p("Grouped bar chart: each cluster is an ideology group (Left/Centre/Right), bars show average score on each threat domain. This is your descriptive 'story' before the regression.", class = "note")
                              )
      ),
      
      # ── Tab 3: Regression table ────────────────────────────────────────────
      shinydashboard::tabItem(tabName = "tab_reg",
                              shinydashboard::box(
                                title = tags$b(icon("calculator"), " Regression Results — OLS"),
                                width = 12, status = "success", solidHeader = TRUE,
                                tags$p(tags$b("Dependent variable: Political Ideology (0 = far left, 10 = far right)"),
                                       style = "margin-bottom:12px; font-size:13px;"),
                                tableOutput("reg_table"),
                                tags$hr(),
                                uiOutput("reg_fit"),
                                tags$p("* p < 0.05  ** p < 0.01  *** p < 0.001. Standard errors in parentheses. Positive coefficient = associated with more right-wing ideology.", class = "note")
                              )
      ),
      
      # ── Tab 4: Coefficient plot ────────────────────────────────────────────
      shinydashboard::tabItem(tabName = "tab_coef",
                              shinydashboard::box(
                                title = tags$b(icon("dot-circle"), " Coefficient Plot — Effect of Threat Domains on Ideology"),
                                width = 12, status = "danger", solidHeader = TRUE,
                                plotOutput("plot_coef", height = 500),
                                tags$p("Each dot is a regression coefficient. Bars show 95% confidence intervals. Red dots are statistically significant (p < 0.05). Vertical line at 0 = no effect. Points to the right = pushes ideology rightward.", class = "note")
                              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ── Plot 1: Threat distributions ────────────────────────────────────────────
  output$plot_dist <- renderPlot({
    ggplot2::ggplot(threat_dist_data,
                    ggplot2::aes(x = threat_score, fill = domain)) +
      ggplot2::geom_histogram(bins = 30, color = "white", alpha = 0.85,
                              show.legend = FALSE) +
      ggplot2::facet_wrap(~domain, ncol = 3) +
      ggplot2::scale_fill_manual(values = c(
        "Cultural Threat"  = "#1A4782",
        "Economic Threat"  = "#F37021",
        "Security Threat"  = "#D71920"
      )) +
      ggplot2::scale_x_continuous(limits = c(0, 100)) +
      ggplot2::labs(
        title    = "Distribution of Immigration Threat Domains",
        subtitle = "CES 2019 Online Survey  |  n ≈ 37,000",
        x        = "Threat Score (0 = no threat, 100 = maximum threat)",
        y        = "Number of Respondents"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(face = "bold", size = 15),
        plot.subtitle    = ggplot2::element_text(color = "#555"),
        strip.text       = ggplot2::element_text(face = "bold", size = 12),
        panel.spacing    = ggplot2::unit(1.5, "lines")
      )
  })
  
  # ── Plot 2: Threat by ideology group ────────────────────────────────────────
  output$plot_ideo <- renderPlot({
    ggplot2::ggplot(threat_by_ideo,
                    ggplot2::aes(x = ideology_group, y = mean_threat,
                                 fill = domain)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(0.75),
                        width = 0.65) +
      ggplot2::geom_text(
        ggplot2::aes(label = round(mean_threat, 1)),
        position = ggplot2::position_dodge(0.75),
        vjust = -0.4, size = 3.5, color = "#333"
      ) +
      ggplot2::scale_fill_manual(
        name   = "Threat Domain",
        values = c(
          "Cultural Threat"  = "#1A4782",
          "Economic Threat"  = "#F37021",
          "Security Threat"  = "#D71920"
        )
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, 100),
        expand = ggplot2::expansion(mult = c(0, 0.1))
      ) +
      ggplot2::labs(
        title    = "Mean Immigration Threat Scores by Ideological Group",
        subtitle = "CES 2019 Online Survey  |  Grouped by left-right self-placement",
        x        = "Ideological Group",
        y        = "Mean Threat Score (0–100)"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        plot.title         = ggplot2::element_text(face = "bold", size = 15),
        plot.subtitle      = ggplot2::element_text(color = "#555"),
        legend.position    = "bottom",
        legend.title       = ggplot2::element_text(face = "bold"),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.text.x        = ggplot2::element_text(face = "bold", size = 12)
      )
  })
  
  # ── Regression table ─────────────────────────────────────────────────────────
  output$reg_table <- renderTable({
    s <- summary(model)
    coefs <- as.data.frame(s$coefficients)
    coefs$Term <- rownames(coefs)
    rownames(coefs) <- NULL
    coefs <- coefs[coefs$Term != "(Intercept)", ]
    coefs$Term <- dplyr::recode(coefs$Term,
                                "cultural_threat" = "Cultural Threat",
                                "economic_threat" = "Economic Threat",
                                "security_threat" = "Security Threat",
                                "age"             = "Age",
                                "income"          = "Income",
                                "education"       = "Education"
    )
    coefs$Significance <- dplyr::case_when(
      coefs[["Pr(>|t|)"]] < 0.001 ~ "***",
      coefs[["Pr(>|t|)"]] < 0.01  ~ "**",
      coefs[["Pr(>|t|)"]] < 0.05  ~ "*",
      TRUE ~ ""
    )
    coefs[["Estimate"]]    <- round(coefs[["Estimate"]],    4)
    coefs[["Std. Error"]]  <- round(coefs[["Std. Error"]], 4)
    coefs[["t value"]]     <- round(coefs[["t value"]],    3)
    coefs[["Pr(>|t|)"]]   <- formatC(coefs[["Pr(>|t|)"]],
                                     format = "e", digits = 2)
    coefs[, c("Term","Estimate","Std. Error","t value","Pr(>|t|)","Significance")]
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$reg_fit <- renderUI({
    s   <- summary(model)
    r2  <- round(s$r.squared, 4)
    ar2 <- round(s$adj.r.squared, 4)
    n   <- nobs(model)
    tags$p(
      tags$b("Model fit: "),
      paste0("R² = ", r2, "  |  Adjusted R² = ", ar2,
             "  |  N = ", format(n, big.mark = ",")),
      style = "font-size:13px; color:#333;"
    )
  })
  
  # ── Coefficient plot ─────────────────────────────────────────────────────────
  output$plot_coef <- renderPlot({
    ggplot2::ggplot(model_tidy,
                    ggplot2::aes(x = estimate, y = term,
                                 color = significant, shape = domain)) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          color = "#aaa", linewidth = 0.8) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = conf.low, xmax = conf.high),
        height = 0.25, linewidth = 0.9
      ) +
      ggplot2::geom_point(size = 4) +
      ggplot2::scale_color_manual(
        name   = "Significant (p<0.05)",
        values = c("TRUE" = "#c0392b", "FALSE" = "#95a5a6")
      ) +
      ggplot2::scale_shape_manual(
        name   = "Type",
        values = c("Threat" = 16, "Control" = 17)
      ) +
      ggplot2::labs(
        title    = "Effect of Immigration Threat Domains on Political Ideology",
        subtitle = "OLS Regression  |  Dependent variable: ideology (0=left, 10=right)  |  CES 2019",
        x        = "Regression Coefficient (with 95% CI)",
        y        = NULL,
        caption  = "Positive values = associated with more right-wing ideology. Red = statistically significant."
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(face = "bold", size = 15),
        plot.subtitle   = ggplot2::element_text(color = "#555"),
        plot.caption    = ggplot2::element_text(color = "#888", size = 10),
        legend.position = "bottom",
        axis.text.y     = ggplot2::element_text(face = "bold", size = 12),
        panel.grid.minor = ggplot2::element_blank() 
      )
  })
}

shinyApp(ui, server)
