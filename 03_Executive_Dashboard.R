library(shiny)
source("04_Analysis_Pipeline.R", local = TRUE)
p <- run_analysis()
d <- p$data
m <- p$models
f <- p$forecast
percent <- function(x) ifelse(is.na(x), "Undefined", sprintf("%.1f%%", 100 * x))

ui <- navbarPage(
  "Grade 7-10 Cohort Survival",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#1F536F"),
  tabPanel("Regional profile",
           h2("Where should retention receive closer review?"),
           p("Public DepEd cohort survival estimates published by the PSA. Analysis uses aggregate data."),
           selectInput("region", "Selected region", choices = sort(unique(d$regional$region))),
           fluidRow(column(6, wellPanel(h4("Latest national rate"), h3(textOutput("national")))),
                    column(6, wellPanel(h4("Latest rate in the selected region"), h3(textOutput("local"))))),
           fluidRow(column(6, plotOutput("groups", height = 440)),
                    column(6, plotOutput("region_series", height = 440)))),
  tabPanel("Decline model",
           h2("Can we flag a decline of at least three percentage points?"),
           p("Test results use 2021–2024. The slider explores the trade-off between missed declines and extra flags."),
           sliderInput("threshold", "Flag probability cutoff", min = .05, max = .95,
                       value = score_cutoff, step = .05),
           p(textOutput("flag_count")),
           fluidRow(column(6, h4("Logistic model and majority baseline"), tableOutput("scores")),
                    column(6, h4("Confusion matrix"), tableOutput("matrix"))),
           h4("Regression test errors"), tableOutput("linear"),
           h4("Adjusted odds ratios"), tableOutput("odds")),
  tabPanel("National forecast",
           h2("National Grade 7-10 cohort survival"),
           p("Historical projection from the latest observed source year. The band comes from historical errors."),
           plotOutput("forecast", height = 420),
           fluidRow(column(6, tableOutput("future")), column(6, tableOutput("forecast_errors")))),
  tabPanel("Recommended action",
           h2("Review low or falling regional rates"),
           tags$ul(tags$li("Check the latest reported levels and verify abrupt changes with regional education offices."),
                   tags$li("Inspect records flagged by the model as a secondary check."),
                   tags$li("Use enrollment, transfers, attendance and completion records to investigate the cause.")),
           p("The estimates summarize cohorts. They do not identify individual learners or the effect of an intervention."),
           p(tags$a("PSA OpenSTAT data and footnotes", href = source_url)),
           h4("Data quality"), tableOutput("audit")),
  footer = div(class = "container-fluid text-muted py-3", "PC25 R Language w/Lab | Week 17 | BSIT 4.9")
)

server <- function(input, output, session) {
  regional_series <- reactive({
    req(input$region)
    subset(d$regional, region == input$region)
  })
  scored <- reactive({
    req(input$threshold)
    classification(m$actual, m$probability >= input$threshold)
  })
  output$national <- renderText(sprintf("%.1f%% (%s)", tail(d$national$survival_pct, 1), max(d$national$year)))
  output$local <- renderText({
    local <- subset(d$clean, region == input$region & sex == "Both Sexes" & !is.na(survival_pct))
    local <- local[which.max(local$year), ]
    sprintf("%.1f%% (%s)", local$survival_pct, local$year)
  })
  output$groups <- renderPlot(plot_groups(d$regional), alt = "Latest cohort survival by region and sex")
  output$region_series <- renderPlot({
    ggplot(regional_series(), aes(year, survival_pct, color = sex)) +
      geom_line(linewidth = .8) + geom_point(size = 1.5) +
      scale_color_manual(values = c(Female = "#1F536F", Male = "#AB512F")) +
      labs(title = input$region, x = "Reporting year", y = "Cohort survival (%)", color = NULL) + plot_theme()
  }, alt = "Historical cohort survival for the selected region")
  output$flag_count <- renderText({
    x <- scored()$matrix
    sprintf("%s flags. %s of %s observed declines missed.", sum(x[, 2]), x[2, 1], sum(x[2, ]))
  })
  output$scores <- renderTable(data.frame(Metric = names(scored()$metrics),
                                          Logistic = percent(scored()$metrics),
                                          Baseline = percent(m$majority_result$metrics)))
  output$matrix <- renderTable({
    x <- scored()$matrix
    data.frame(Observed = c("No decline flag", "Decline of at least 3 pp"),
               Predicted_0 = x[, 1], Predicted_1 = x[, 2])
  }, digits = 0)
  output$linear <- renderTable(m$compare, digits = 3)
  output$odds <- renderTable(m$odds[m$odds$Term %in% c("prior10", "previous_change_pp", "sexMale"), ], digits = 3)
  output$forecast <- renderPlot(plot_forecast(d$national, f), alt = "National cohort survival and three-year projection")
  output$future <- renderTable(f$future, digits = 1)
  output$forecast_errors <- renderTable(data.frame(Baseline = rownames(f$errors), f$errors, row.names = NULL), digits = 2)
  output$audit <- renderTable(d$audit, digits = 0)
}

shinyApp(ui, server)
