library(shiny)
library(ggplot2)
library(data.table)
library(readr)
library(tidyquant)
library(lubridate)

bad_hashes <- c('cf99ce6498b53400492284bfc9747fa4', 'd2846dafdec6b55aa53b4d39c1d97074')

process_stats <- function(stats1) {
  stats1 <- stats1[!(Hash %in% bad_hashes)][, `:=`(N=1L:.N, Count=.N, HighScore=cummax(Score)), by='Scenario']
  setorder(stats1, Scenario, N)
  stats1[, IsNewSession := c(TRUE, diff(DateTime) > 270), by='Scenario']
  stats1[, Session := cumsum(1L * IsNewSession), by='Scenario']
  stats1[, NSession := 1L:.N, by=c('Scenario', 'Session')]
  stats1[, IsNewRecord := c(FALSE, diff(HighScore) > 0), by='Scenario']
  stats1[, Week := as.integer(lubridate::isoweek(DateTime) - min(lubridate::isoweek(DateTime)))]
  stats1[, WeekGroup := as.factor(Week)]
  stats1[, NGroup := as.factor(N)]
  stats1
}

stats <- data.table(read_csv("stats.csv", col_types = cols(DateTime = col_datetime(format = "%Y-%m-%dT%H:%M:%S"))))
stats <- process_stats(stats)
scenarios <- unique(stats$Scenario)

measureVars <- c(
  "N", "NSession", "Session", "DateTime", "Score", "Shots", "Hits", "Accuracy", "IsNewRecord",
  "Kills", "Deaths", "FightTime", "AvgTTK", "DamageDone", "DamageTaken", "DistanceTraveled",
  "HighScore", "Score*Accuracy", "Score/Accuracy", "Score/Accuracy^2")
colorVars <- c("(None)", "Hash", "IsNewSession", "Week", "WeekGroup", "NGroup", measureVars)

ui <- fluidPage(
  titlePanel("Kovaak Stats"),

  sidebarLayout(
    sidebarPanel(
      h3('Upload'),
      fileInput("csvFile",
                "Upload stats.csv",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),

      h3('Filter'),
      sliderInput("minAttempts",
                  "Min. attempts",
                  step = 10L, min = 20L, max = max(stats$N),
                  value = 100L),
      sliderInput("weekRange",
                  "Weeks",
                  step = 1L, min = 0L, max = max(stats$Week),
                  value = c(0L, max(stats$Week))),
      textAreaInput("scenarioLines",
                    "Scenario List",
                    value="",
                    placeholder="One scenario per line, case-insensitive. Ignores other filters if filled."),
      actionLink('fillScenarioLines', 'Fill Scenario List'),

      h3('Plotting'),
      sliderInput("facetCols",
                  "Plot columns",
                  step = 1L, min = 1L, max = 8L,
                  value = 3L),
      checkboxInput("includeMA",
                    "Include moving averages (SMA50, SMA30, SMA10, SMA5)",
                    value = TRUE),
      checkboxInput("jitterRels",
                    "Jitter points in Score by Accuracy and Custom Plot"),
      sliderInput("weekBucketSize",
                  "Week group size",
                  step = 1L, min = 1L, max = max(stats$Week) + 1,
                  value = 1L),
      sliderInput("nBucketSize",
                  "Attempt # group (NGroup) size",
                  step = 5L, min = 5L, max = as.integer(max(stats$N) / 5) * 5 + 5,
                  value = 1L),
      sliderInput("pointSize",
                  "Point Size",
                  step = 0.1, min = 0.0, max = 10.0,
                  value = 3.0),
      sliderInput("pointOpacity",
                  "Point Opacity",
                  step = 0.05, min = 0.0, max = 1.0,
                  value = 1.0),
      width = 3
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h2(textOutput("stats_updated")),
      tabsetPanel(
        type = "tabs",
        tabPanel("Scenarios", dataTableOutput("highScores")),
        tabPanel("Attempts by Week", plotOutput("attempts", height='800px')),
        tabPanel("Individual Attempts", dataTableOutput("info")),
        tabPanel("Score by Attempt", plotOutput("scorePlot", height="800px")),
        tabPanel("Accuracy by Attempt", plotOutput("accPlot", height="800px")),
        tabPanel("Score by Date", plotOutput("scoreDatePlot", height="800px")),
        tabPanel("Accuracy by Date", plotOutput("accDatePlot", height="800px")),
        tabPanel("Score by Accuracy", plotOutput("relPlot", height="800px")),
        tabPanel("Custom Plot",
                 fluidRow(column(9,
                                 column(12,
                                        column(4, selectInput("xVar", "X Variable", measureVars, "Accuracy")),
                                        column(4, selectInput("xFunc", "X Function", c("X", "log X", "1 / X", "60 / X"), "X")),
                                        column(4, textInput("xLabel", "X Custom Label", placeholder="(default)")),
                                        ),
                                 column(12,
                                        column(4, selectInput("yVar", "Y Variable", measureVars, "Score")),
                                        column(4, selectInput("yFunc", "Y Function", c("Y", "log Y", "1 / Y", "60 / Y"), "Y")),
                                        column(4, textInput("yLabel", "Y Custom Label", placeholder="(default)"))
                                        ),
                                 column(12,
                                        column(4, selectInput("colorVar", "Color Variable", colorVars, "WeekGroup")),
                                        column(4, selectInput("sizeVar", "Size Variable", colorVars, "(None)"))
                                        )
                                ),
                          column(3,
                                 selectInput("trends", "Trendlines", c("None", "Line", "Curve")),
                                 selectInput("customGeom", "Geometry", c("Point", "Line", "Path")),
                                 checkboxInput("flipAxes", "Flip Axes")
                                 ),
                          column(12, plotOutput("customRelPlot", height="800px"))))
      )
    )
  )
)

server <- function(input, output, session) {
  included <- reactive({
    included.
  })

  stats. <- reactive({
    if (!is.null(input$csvFile)) {
      stats <- data.table(read_csv(input$csvFile$datapath, col_types = cols(DateTime = col_datetime(format = "%Y-%m-%dT%H:%M:%S"))))
      stats <- process_stats(stats)
      scenarios <- unique(stats$Scenario)
      old_stats <- NULL
      updateSliderInput(session, "minAttempts", max=max(stats$N))
      updateSliderInput(session, "weekRange", max=max(stats$Week))
    }

    stats[, WeekGroup := as.factor(as.integer(Week / input$weekBucketSize) * input$weekBucketSize)]
    stats[, NGroup := as.factor(as.integer(N / input$nBucketSize) * input$nBucketSize)]
    if (nchar(input$scenarioLines) > 0) {
      parts <- tolower(strsplit(input$scenarioLines, "\n")[[1]])
      included <- stats[, tolower(Scenario) %in% parts]
    } else {
      included <- stats[, Count >= input$minAttempts & input$weekRange[1] <= Week & Week <= input$weekRange[2]]
    }

    stats[, I := included][I == TRUE]
  })

  observeEvent(input$fillScenarioLines, {
    updateTextAreaInput(session, 'scenarioLines', value=paste(unique(stats.()$Scenario), collapse="\n"))
  })

  mas <- reactive({
    if(input$includeMA) list(
      geom_ma(ma_fun=SMA, n=50, size=1, linetype='solid', col='blue'),
      geom_ma(ma_fun=SMA, n=30, size=1, linetype='solid', col='blue', alpha=0.5),
      geom_ma(ma_fun=SMA, n=10, size=1, linetype='solid', col='blue', alpha=0.25),
      geom_ma(ma_fun=SMA, n=5, size=1, linetype='solid', col='blue', alpha=0.1))
    else list()
  })

  fw <- reactive({ facet_wrap(Scenario ~ ., scales = 'free', ncol = input$facetCols) })

  jitter <- reactive({
    list({
      if (input$jitterRels) geom_jitter(width=0.01, height=0.01, alpha=input$pointOpacity, size=input$pointSize)
      else geom_point(alpha=input$pointOpacity, size=input$pointSize)
    })
  })

  pt <- reactive({ geom_point(alpha=input$pointOpacity, size=input$pointSize, stroke=0, shape=20) })

  output$attempts <- renderPlot({
    qplot(WeekGroup, Scenario, data=stats.()[, .(Attempts = .N), by=.(Scenario, WeekGroup)], col=Attempts, fill=Attempts, geom='tile') +
      geom_text(aes(label=Attempts), col='white')
  })

  output$info <- renderDataTable({ stats.() })

  output$highScores <- renderDataTable({
    stats.()[, .(HighScore=round(max(Score), 2), Attempts=.N), by=Scenario][order(tolower(Scenario))][Attempts >= input$minAttempts]
  })

  output$scorePlot <- renderPlot({
    ggplot(aes(N, Score, col=WeekGroup), data=stats.()) +
      geom_line(aes(N, HighScore), col='red') +
      pt() + mas() + fw()
  })

  output$accPlot <- renderPlot({
    ggplot(aes(N, Accuracy, col=WeekGroup), data=stats.()) +
      pt() + mas() + fw()
  })

  output$scoreDatePlot <- renderPlot({
    ggplot(aes(DateTime, Score, col=WeekGroup), data=stats.()) +
      geom_line(aes(DateTime, HighScore), col='red') +
      pt() + fw()
  })

  output$accDatePlot <- renderPlot({
    ggplot(aes(DateTime, Accuracy, col=WeekGroup), data=stats.()) +
      pt() + fw()
  })

  output$relPlot <- renderPlot({
    ggplot(aes(Accuracy, Score, col=WeekGroup), data=stats.()) +
      jitter() + fw()
  })

  output$customRelPlot <- renderPlot({
    X <- input$xVar
    Y <- input$yVar
    if (input$xFunc == "1 / X") X <- paste0("1 / (", X, ")")
    if (input$xFunc == "60 / X") X <- paste0("60 / (", X, ")")
    if (input$yFunc == "1 / Y") Y <- paste0("1 / (", Y, ")")
    if (input$yFunc == "60 / Y") Y <- paste0("60 / (", Y, ")")

    gpt <- {
      if (input$sizeVar == '(None)') jitter()
      else if (input$jitterRels) geom_jitter(width=0.01, height=0.01, alpha=input$pointOpacity)
      else geom_point(alpha=input$pointOpacity)
    }
    col_part <- ifelse(input$colorVar == '(None)', 1L, input$colorVar)
    ggplot({ if(input$sizeVar == '(None)') aes_string(X, Y, col=col_part) else aes_string(X, Y, col=col_part, size=input$sizeVar) }, data=stats.()) +
      fw() +
      { if(input$customGeom == 'Point') gpt else if (input$customGeom == 'Path') geom_path(alpha=input$pointOpacity) else geom_line(alpha=input$pointOpacity) } +
      { if(input$xFunc == "log X") scale_x_log10() else list() } +
      { if(input$yFunc == "log Y") scale_y_log10() else list() } +
      { if(input$xLabel != '') xlab(input$xLabel) else list() } +
      { if(input$yLabel != '') ylab(input$yLabel) else list() } +
      { if(input$trends == 'None') list() else if (input$trends == 'Line') geom_smooth(method='lm', se=FALSE, size=1) else geom_smooth(method='loess', se=FALSE, size=1) } +
      { if(input$flipAxes) coord_flip() else list() }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
