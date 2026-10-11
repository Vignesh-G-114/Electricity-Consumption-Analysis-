
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

# Sample Electricity Consumption Dataset
electricity <- data.frame(
  Month = month.name[1:12],
  Units = c(250, 280, 310, 350, 400, 450,
            480, 460, 390, 340, 300, 270),
  Year = rep(2026, 12)
)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Electricity Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard",
               icon = icon("dashboard")),
      menuItem("Consumption Details", tabName = "details",
               icon = icon("bolt"))
    ),
    
    hr(),
    
    sliderInput(
      "rate", "Cost Per Unit (₹)",
      min = 1, max = 20, value = 8, step = 0.5
    ),
    
    sliderInput(
      "limit", "High Consumption Limit",
      min = 100, max = 600, value = 400, step = 10
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f2f6fc; }
      .box, .small-box { border-radius: 10px; }
    "))),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          valueBoxOutput("totalUnits", width = 4),
          valueBoxOutput("averageUnits", width = 4),
          valueBoxOutput("estimatedBill", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Monthly Electricity Consumption",
            width = 8,
            status = "primary",
            plotOutput("consumptionPlot", height = 300)
          ),
          
          box(
            title = "Monthly Electricity Bill",
            width = 4,
            status = "success",
            plotOutput("billPlot", height = 300)
          )
        ),
        
        fluidRow(
          box(
            title = "Consumption Analysis",
            width = 6,
            status = "warning",
            tableOutput("summaryTable")
          ),
          
          box(
            title = "Usage Alert",
            width = 6,
            status = "danger",
            textOutput("alert")
          )
        )
      ),
      
      tabItem(
        tabName = "details",
        
        fluidRow(
          box(
            title = "Monthly Consumption Data",
            width = 12,
            status = "primary",
            DTOutput("dataTable"),
            br(),
            downloadButton("downloadData", "Download CSV")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Calculate consumption and bill
  data <- reactive({
    d <- electricity
    d$Bill <- d$Units * input$rate
    d$Status <- ifelse(
      d$Units >= input$limit,
      "High Consumption",
      "Normal"
    )
    d
  })
  
  # Total consumption
  output$totalUnits <- renderValueBox({
    valueBox(
      paste(sum(data()$Units), "Units"),
      "Total Consumption",
      icon = icon("bolt"),
      color = "blue"
    )
  })
  
  # Average consumption
  output$averageUnits <- renderValueBox({
    valueBox(
      round(mean(data()$Units), 1),
      "Average Monthly Units",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  # Estimated annual bill
  output$estimatedBill <- renderValueBox({
    valueBox(
      paste0("₹", format(
        sum(data()$Bill), big.mark = ",",
        scientific = FALSE
      )),
      "Estimated Annual Bill",
      icon = icon("rupee-sign"),
      color = "purple"
    )
  })
  
  # Monthly consumption chart
  output$consumptionPlot <- renderPlot({
    ggplot(data(), aes(x = Month, y = Units)) +
      geom_col(fill = "steelblue") +
      geom_hline(
        yintercept = input$limit,
        linetype = "dashed",
        color = "red"
      ) +
      labs(x = "Month", y = "Units Consumed") +
      theme_minimal() +
      theme(axis.text.x = element_text(
        angle = 45, hjust = 1
      ))
  })
  
  # Monthly bill chart
  output$billPlot <- renderPlot({
    ggplot(data(), aes(x = Month, y = Bill)) +
      geom_col(fill = "darkgreen") +
      labs(x = "Month", y = "Estimated Bill (₹)") +
      theme_minimal() +
      theme(axis.text.x = element_text(
        angle = 45, hjust = 1
      ))
  })
  
  # Summary statistics
  output$summaryTable <- renderTable({
    d <- data()
    
    data.frame(
      Metric = c(
        "Highest Consumption",
        "Lowest Consumption",
        "Average Consumption",
        "Highest Usage Month"
      ),
      Value = c(
        max(d$Units),
        min(d$Units),
        round(mean(d$Units), 1),
        as.character(d$Month[which.max(d$Units)])
      )
    )
  })
  
  # High consumption alert
  output$alert <- renderText({
    d <- data()
    high <- d[d$Units >= input$limit, ]
    
    if (nrow(high) == 0) {
      return("All months are below the selected consumption limit.")
    }
    
    paste(
      "High consumption detected in:",
      paste(high$Month, collapse = ", "),
      ". Consider reducing electricity usage."
    )
  })
  
  # Detailed table
  output$dataTable <- renderDT({
    datatable(
      data(),
      options = list(pageLength = 12),
      rownames = FALSE
    )
  })
  
  # Download CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("electricity_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)