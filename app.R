library(shiny)
library(shinydashboard)
library(googlesheets4)
library(dplyr)
library(tidyr)

# read in data ----
url <- "1KzLoJSakrpr3nY2KiFMOd9jn1DEwKKNbw2QibnW_aXg"
gs4_deauth()
full_table <- read_sheet(url, col_types = "c") %>%
    rename("Genre" = 4, "Location(s)" = 5)

genres <- c("Scifi",
            "Fantasy",
            "Romance",
            "Biography",
            "Horror/Thriller",
            "Crime",
            "Comedy",
            "Non-fiction",
            "Poetry",
            "Education",
            "Graphic Novel",
            "Fiction",
            "Classics")


# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(title = "Book Recs"),
    sidebar = dashboardSidebar(
        tags$a(href="https://forms.gle/aHPtwTNvrL6L1wb17", "Add a book", target="_blank"),
        checkboxGroupInput(inputId = "genre",
                           label = "Genre",
                           choices = genres,
                           selected = genres)
    ),
    body = dashboardBody(
        checkboxGroupInput(inputId = "visible_cols",
                           label = NULL,
                           choices = colnames(full_table),
                           selected = colnames(full_table),
                           inline = TRUE),
        tableOutput(outputId = "book_table")
    ),
    title = "Book Recs",
    skin = "purple"
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    display_table <- reactiveVal(full_table)
    
    # update visible columns in the display table ----
    observeEvent(input$visible_cols, {
        full_table %>%
            select(input$visible_cols) %>%
            display_table()
    })
    
    # book_table ----
    output$book_table <- renderTable({
        display_table()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
