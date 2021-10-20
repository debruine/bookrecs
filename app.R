library(shiny)
library(shinydashboard)
library(googlesheets4)

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
        checkboxGroupInput(inputId = "genre",
                           label = "Genre",
                           choices = genres,
                           selected = genres)
    ),
    body = dashboardBody(
        tags$a(href="https://forms.gle/aHPtwTNvrL6L1wb17", "Add a book", target="_blank"),
        tableOutput(outputId = "book_table")
    ),
    title = "Book Recs",
    skin = "purple"
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # book_table ----
    output$book_table <- renderTable({
        url <- "1KzLoJSakrpr3nY2KiFMOd9jn1DEwKKNbw2QibnW_aXg"
        gs4_deauth()
        read_sheet(url, col_types = "c")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
