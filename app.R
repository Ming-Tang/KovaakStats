library(shiny)
library(ggplot2)
library(data.table)
library(readr)
library(tidyquant)
library(lubridate)

if (file.exists("old_stats.csv")) {
  old_stats <- data.table(read_csv("old_stats.csv", col_types = cols(DateTime = col_datetime(format = "%Y-%m-%dT%H:%M:%S"))))
} else {
  old_stats <- NULL
}

stats <- data.table(read_csv("stats.csv", col_types = cols(DateTime = col_datetime(format = "%Y-%m-%dT%H:%M:%S"))))

stats <- rbind(old_stats, stats, fill=TRUE)

stats <- stats[, .(N=1L:.N, DateTime, Score, Accuracy, AvgTTK, Count=.N, HighScore=cummax(Score)), by='Scenario']
stats[, Week := as.integer(lubridate::isoweek(DateTime) - min(lubridate::isoweek(DateTime)))]
stats[, WeekGroup := as.factor(Week)]

scenarios <- unique(stats$Scenario)

ui <- fluidPage(
    titlePanel("Kovaak Stats"),

    sidebarLayout(
        sidebarPanel(
            h2('Filter'),
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
                          value="", placeholder="One scenario per line, case-insensitive. Ignores other filters if filled."),

            h2('Plotting'),
            sliderInput("facetCols",
                        "Plot columns",
                        step = 1L, min = 1L, max = 8L,
                        value = 3L),
            checkboxInput("includeMA",
                          "Include moving averages (SMA50, SMA30, SMA10, SMA5)",
                          value = TRUE),
            checkboxInput("jitterRels", "Jitter points in Score by Accuracy"),
            sliderInput("weekBucketSize", "Color by week group size",
                        step = 1L, min = 1L, max = max(stats$Week) + 1, value = 1L),
            sliderInput("pointSize", "Point Size",
                        step = 0.1, min = 0.0, max = 5.0, value = 2),
            sliderInput("pointOpacity", "Point Opacity",
                        step = 0.05, min = 0.0, max = 1.0, value = 1.0),
            width = 3
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel("Scenarios", dataTableOutput("highScores")),
                tabPanel("Attempts by Week", plotOutput("attempts"), height='800px'),
                tabPanel("Individual Attempts", dataTableOutput("info")),
                tabPanel("Score by Attempt", plotOutput("scorePlot", height="800px")),
                tabPanel("Accuracy by Attempt", plotOutput("accPlot", height="800px")),
                tabPanel("Score by Date", plotOutput("scoreDatePlot", height="800px")),
                tabPanel("Accuracy by Date", plotOutput("accDatePlot", height="800px")),
                tabPanel("Score by Accuracy", plotOutput("relPlot", height="800px")),
                tabPanel("Unadjusted Score by Accuracy", plotOutput("relPlotUnadjusted", height="800px"))
            )
        )
    )
)

server <- function(input, output) {
    included <- reactive({
        stats[, WeekGroup := as.factor(as.integer(Week / input$weekBucketSize) * input$weekBucketSize)]
        if (nchar(input$scenarioLines) > 0) {
            parts <- tolower(strsplit(input$scenarioLines, "\n")[[1]])
            print(parts)
            stats[, tolower(Scenario) %in% parts]
        } else {
            stats[, Count >= input$minAttempts & input$weekRange[1] <= Week & Week <= input$weekRange[2]]
        }
    })

    mas <- reactive({ if(input$includeMA) list(
      geom_ma(ma_fun=SMA, n=50, size=1, linetype='solid', col='blue'),
      geom_ma(ma_fun=SMA, n=30, size=1, linetype='solid', col='blue', alpha=0.5),
      geom_ma(ma_fun=SMA, n=10, size=1, linetype='solid', col='blue', alpha=0.25),
      geom_ma(ma_fun=SMA, n=5, size=1, linetype='solid', col='blue', alpha=0.1)
    ) else list() })

    fw <- reactive({ facet_wrap(Scenario ~ ., scales = 'free', ncol = input$facetCols) })

    jitter <- reactive({
        if (input$jitterRels) geom_jitter(width=0.01, height=0.05, alpha=input$pointOpacity, size=input$pointSize, stroke=0)
        else geom_point(alpha=input$pointOpacity, size=input$pointSize, stroke=0)
    })

    pt <- reactive({ geom_point(alpha=input$pointOpacity, size=input$pointSize, stroke=0) })

    output$attempts <- renderPlot({
        qplot(WeekGroup, Scenario, data=stats[, I := included()][I == TRUE][, .(Attempts = .N), by=.(Scenario, WeekGroup)], col=Attempts, fill=Attempts, geom='tile') +
            geom_text(aes(label=Attempts), col='white')
    })

    output$info <- renderDataTable({ stats[, I := included()][I == TRUE] })

    output$highScores <- renderDataTable({
        stats[, I := included()][I == TRUE][, .(HighScore=round(max(Score), 2), Attempts=.N), by=Scenario][order(tolower(Scenario))][Attempts >= input$minAttempts]
    })

    output$scorePlot <- renderPlot({
        ggplot(aes(N, Score, col=WeekGroup), data=stats[, `:=`(I=included())][I == TRUE]) +
            geom_line(aes(N, HighScore), col='red') +
            pt() + mas() + fw()
    })

    output$accPlot <- renderPlot({
        ggplot(aes(N, Accuracy, col=WeekGroup), data=stats[, `:=`(I=included())][I == TRUE]) +
            pt() + mas() + fw()
    })

    output$scoreDatePlot <- renderPlot({
        ggplot(aes(DateTime, Score, col=WeekGroup), data=stats[, `:=`(I=included())][I == TRUE]) +
            geom_line(aes(DateTime, HighScore), col='red') +
            pt() + fw()
    })

    output$accDatePlot <- renderPlot({
        ggplot(aes(DateTime, Accuracy, col=WeekGroup), data=stats[, `:=`(I=included())][I == TRUE]) +
            pt() + fw()
    })

    output$relPlot <- renderPlot({
        ggplot(aes(Accuracy, Score, col=WeekGroup), data=stats[, `:=`(I=included())][I == TRUE]) +
            jitter() + fw()
    })

    output$relPlotUnadjusted <- renderPlot({
        ggplot(aes(Accuracy, Score / Accuracy, col=WeekGroup), data=stats[, `:=`(I=included())][I == TRUE]) +
            jitter() + fw()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
