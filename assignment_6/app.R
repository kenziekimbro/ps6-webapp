library(shiny)
library(tidyverse)

mentalhealth <- read_delim("depression_anxiety_data.csv")

ui <- fluidPage(
    titlePanel("Mental Health in College Students Data"),
        tabsetPanel(
            tabPanel("Introduction", 
                p("This dataset looks at anxiety and depression in college students from The University of Lahore."),
                p("There are ", nrow(mentalhealth), "students in the dataset."),
                p("The dataset contains ", nrow(mentalhealth)*ncol(mentalhealth), "observations and ", ncol(mentalhealth), "variables."),
                p("Here is a small random sample of the data, with 5 select variables:"),
                tableOutput("sample")),
            tabPanel("Graph", 
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons("color", "Choose color",
                                      choices = c("skyblue", "pink", "red", "black"),
                                      selected = c("black")
                       )),
                       mainPanel(plotOutput("plot")))),
            tabPanel("Table", 
                     sidebarLayout(
                       sidebarPanel(
                         checkboxGroupInput("elements", "Choose element(s):",
                                            choices = c("School Year" = "school_year",
                                              "Gender" = "gender",
                                              "Suicidal" = "suicidal"))),
                       mainPanel(tableOutput("table"),
                                 textOutput("text"))))
  )
        )

server <- function(input, output) {
  
#Introduction Page
    output$sample <- renderTable({
      mentalhealth %>% 
        select("id", "school_year", "gender", "depression_severity", "anxiety_severity") %>% 
        sample_n(10)
    })

#Table
    mh <- reactive({ 
      mentalhealth %>%
      select(input$elements)
    })
    
    output$table <- renderTable({
    mentalhealth %>%
      filter(!is.na(phq_score)) %>% 
      filter(!is.na(gad_score)) %>% 
      group_by(mh()) %>%
      summarise(Anxiety = mean(gad_score, na.rm = TRUE),
                Depression = mean(phq_score, na.rm = TRUE), .groups = "drop")
      })
    output$text <- renderText({
      paste("The table is displaying the average anxiety and depression scores based on", input$elements, ".")
    })
    
#Graph
    output$plot <- renderPlot({
      mentalhealth %>% 
        filter(!is.na(phq_score)) %>% 
        filter(!is.na(gad_score)) %>% 
        group_by(gender) %>% 
        ggplot(aes(x = phq_score, y = gad_score, color = gender)) +
        geom_point(col = input$color)  +
        labs(title = "Anxiety and Depression in College Students",
             x = "Depression",
             y = "Anxiety")
      
    }) 
}

shinyApp(ui = ui, server = server)
