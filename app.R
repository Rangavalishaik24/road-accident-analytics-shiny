# Professional Shiny Dashboard: Road Traffic Accident Analysis
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(ggplot2)
library(viridis)
library(DT)

# 1. DATA PREPARATION
df <- read.csv("cleaned.csv", stringsAsFactors = TRUE)
# 2. USER INTERFACE
ui <- dashboardPage(
  dashboardHeader(title = "Road Accident Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Visualizations", tabName = "viz", icon = icon("chart-line")),
      menuItem("Statistics", tabName = "stats", icon = icon("balance-scale")),
      menuItem("Actionable Solutions", tabName = "solutions", icon = icon("lightbulb")),
      menuItem("Data Export", tabName = "download", icon = icon("download"))
    ),
    hr(),
    div(style = "padding: 10px; color: white;", h4("Global Filters")),
    selectInput("severityFilter", "Filter by Severity:", 
                choices = levels(df$Accident_severity), 
                selected = levels(df$Accident_severity), multiple = TRUE),
    selectInput("weatherFilter", "Filter by Weather:", 
                choices = levels(df$Weather_conditions), 
                selected = levels(df$Weather_conditions), multiple = TRUE)
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-blue .main-header .logo { background-color: #2C3E50; font-weight: bold; }
      .skin-blue .main-header .navbar { background-color: #34495E; }
      .skin-blue .main-sidebar { background-color: #2C3E50; }
      .skin-blue .main-sidebar .sidebar-menu>li.active>a { background-color: #1ABC9C; }
      .box { border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); }
      .value-box { border-radius: 8px; transition: transform .2s; }
      .value-box:hover { transform: scale(1.02); }
      .dataTables_wrapper { font-size: 13px; }
    "))),
    
    tabItems(
      # TAB 1: OVERVIEW
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("totalAccidents"),
                valueBoxOutput("fatalAccidents"),
                valueBoxOutput("experienceCategories")
              ),
              fluidRow(
                box(title = tagList(icon("database"), " Dataset Structure"), 
                    width = 6, status = "primary", solidHeader = TRUE, 
                    DTOutput("structureTable")),
                box(title = tagList(icon("list-alt"), " Summary Statistics"), 
                    width = 6, status = "primary", solidHeader = TRUE, 
                    DTOutput("summaryTable"))
              )
      ),
      
      # TAB 2: VISUALIZATIONS
      tabItem(tabName = "viz",
              fluidRow(
                box(title = "Distribution of Accident Severity", width = 6, status = "success", solidHeader = TRUE, plotOutput("plot1")),
                box(title = "Accidents by Age Band and Sex", width = 6, status = "success", solidHeader = TRUE, plotOutput("plot2"))
              ),
              fluidRow(
                box(title = "Top 10 Causes of Accidents", width = 6, status = "warning", solidHeader = TRUE, plotOutput("plot3")),
                box(title = "Accidents by Weather Condition", width = 6, status = "warning", solidHeader = TRUE, plotOutput("plot4"))
              ),
              fluidRow(
                box(title = "Bar Plot: Severity vs Age Band", width = 6, status = "info", solidHeader = TRUE, plotOutput("plot5")),
                box(title = "Scatter Plot: Experience vs Severity", width = 6, status = "info", solidHeader = TRUE, plotOutput("plot6"))
              )
      ),
      
      # TAB 3: STATISTICS
      tabItem(tabName = "stats",
              fluidRow(
                box(title = "Chi-Square Test (Experience vs. Severity)", width = 12, status = "danger", solidHeader = TRUE,
                    verbatimTextOutput("chiTest"))
              )
      ),
      
      # TAB 4: ACTIONABLE SOLUTIONS (Restored Card Layout)
      tabItem(tabName = "solutions",
              fluidRow(
                column(width = 12, align = "center",
                       h2("Strategic Recommendations & Safety Roadmap", 
                          style = "color: #2C3E50; font-weight: bold; margin-bottom: 20px;"),
                       hr(style = "border-top: 1px solid #BDC3C7; width: 50%;"))
              ),
              fluidRow(
                box(title = tagList(icon("shield-alt"), " Severity Intervention"), width = 4, status = "danger", solidHeader = TRUE, 
                    p("Implement high-resolution surveillance and improved emergency protocols in identified high-severity zones.")),
                box(title = tagList(icon("user-graduate"), " Youth Safety Programs"), width = 4, status = "primary", solidHeader = TRUE,
                    p("Develop mandatory VR hazard perception tests and defensive driving courses for the 18-30 demographic.")),
                box(title = tagList(icon("exclamation-triangle"), " Behavioral Enforcement"), width = 4, status = "warning", solidHeader = TRUE,
                    p("Strict enforcement of lane discipline and AI-based detection systems to monitor mobile phone usage."))
              ),
              fluidRow(
                box(title = tagList(icon("cloud-sun-rain"), " Smart Infrastructure"), width = 4, status = "info", solidHeader = TRUE,
                    p("Upgrade road drainage systems and install automated weather-responsive signage to mitigate risks.")),
                box(title = tagList(icon("id-badge"), " Graduated Licensing"), width = 4, status = "success", solidHeader = TRUE,
                    p("Introduce a tiered licensing system where drivers with < 2 years experience have night-driving restrictions.")),
                box(title = tagList(icon("microchip"), " Vehicle Standards"), width = 4, status = "navy", solidHeader = TRUE,
                    p("Promote the adoption of ADAS (Advanced Driver Assistance Systems) for public and private vehicle fleets."))
              ),
              fluidRow(
                box(width = 12, background = "black", style = "text-align: center; border-radius: 10px;",
                    h3(style = "color: #1ABC9C; font-style: italic; margin: 10px;", 
                       "\"Data-driven policy is the cornerstone of a zero-accident future.\""))
              )
      ),
      
      # TAB 5: DATA EXPORT
      tabItem(tabName = "download",
              fluidRow(
                box(title = "Export Analytical Data", width = 12, status = "primary", solidHeader = TRUE,
                    downloadButton("downloadData", "Download CSV Dataset", class = "btn-success"))
              )
      )
    )
  )
)

# 3. SERVER LOGIC
server <- function(input, output) {
  
  filteredData <- reactive({
    if (is.null(input$severityFilter) | is.null(input$weatherFilter)) return(df)
    df %>% 
      filter(Accident_severity %in% input$severityFilter) %>%
      filter(Weather_conditions %in% input$weatherFilter)
  })
  
  # KPI Boxes
  output$totalAccidents <- renderValueBox({ valueBox(nrow(filteredData()), "Total Incidents", icon = icon("car-crash"), color = "purple") })
  output$fatalAccidents <- renderValueBox({ 
    val <- sum(filteredData()$Accident_severity == "2" | filteredData()$Accident_severity == "Fatal", na.rm = TRUE)
    valueBox(val, "High Severity", icon = icon("skull-crossbones"), color = "red") 
  })
  output$experienceCategories <- renderValueBox({ valueBox(length(unique(filteredData()$Driving_experience)), "Exp. Levels", icon = icon("id-card"), color = "blue") })
  
  # Tables
  output$structureTable <- renderDT({
    data.frame(Feature = names(filteredData()), 
               Type = sapply(filteredData(), function(x) if(is.numeric(x)) "Numeric" else "Category"),
               Sample = sapply(filteredData(), function(x) as.character(x[1]))) %>%
      datatable(options = list(pageLength = 6, dom = 'tp'), rownames = FALSE, style = 'bootstrap', class = 'cell-border stripe')
  })
  
  output$summaryTable <- renderDT({
    target <- filteredData()
    sum_list <- lapply(names(target), function(n) {
      val <- target[[n]]
      if (is.numeric(val)) {
        ins <- paste("Avg:", round(mean(val, na.rm=T), 2))
      } else {
        top <- names(sort(table(val), decreasing = T)[1])
        ins <- paste("Top:", if(length(top)==0) "N/A" else top)
      }
      return(data.frame(Column = n, Insight = ins))
    })
    do.call(rbind, sum_list) %>% datatable(options = list(pageLength = 6, dom = 'tp'), rownames = FALSE, style = 'bootstrap', class = 'cell-border stripe')
  })
  
  # Plots
  output$plot1 <- renderPlot({ ggplot(filteredData(), aes(x = Accident_severity, fill = Accident_severity)) + geom_bar(color = "black", alpha = 0.8) + theme_minimal() + scale_fill_brewer(palette = "Set2") })
  output$plot2 <- renderPlot({ ggplot(filteredData(), aes(x = Age_band_of_driver, fill = Sex_of_driver)) + geom_bar(position = "dodge", color = "black") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) })
  output$plot3 <- renderPlot({ 
    top_causes <- filteredData() %>% count(Cause_of_accident) %>% top_n(10, n)
    ggplot(top_causes, aes(x = reorder(Cause_of_accident, n), y = n)) + geom_col(fill = "#1ABC9C") + coord_flip() + theme_minimal() 
  })
  output$plot4 <- renderPlot({ ggplot(filteredData(), aes(x = Weather_conditions)) + geom_bar(fill = "#E67E22") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) })
  output$plot5 <- renderPlot({ 
    ggplot(filteredData(), aes(x = Age_band_of_driver, fill = Accident_severity)) + 
      geom_bar(position = "dodge", alpha = 0.85, color = "white", linewidth = 0.3) +
      scale_fill_viridis_d(option = "plasma") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face = "bold"))
  })
  output$plot6 <- renderPlot({ ggplot(filteredData(), aes(x = Driving_experience, y = Accident_severity)) + geom_jitter(color = "#9B59B6", alpha = 0.3) + theme_minimal() })
  
  output$chiTest <- renderPrint({
    contingency <- table(filteredData()$Driving_experience, filteredData()$Accident_severity)
    if(nrow(contingency) > 1) chisq.test(contingency, simulate.p.value = TRUE) else "Insufficient data for test."
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("accident_analysis_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filteredData(), file, row.names = FALSE) }
  )
}

shinyApp(ui = ui, server = server)
