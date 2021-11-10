library(shiny)
library(shinydashboard)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(DT)

# read in data ----
url <- "1KzLoJSakrpr3nY2KiFMOd9jn1DEwKKNbw2QibnW_aXg"
gs4_deauth()
full_table <- read_sheet(url, col_types = "c") %>%
    rename("Genre" = 4, "Location(s)" = 5) %>%
    mutate(id = row_number()) %>%
    select(-Timestamp) %>%
    mutate(`Number of pages` = gsub("[^0-9]", "", `Number of pages`) %>% as.integer())

full_colnames <- colnames(full_table) %>%
    setdiff("id")

min_pages <- min(full_table$`Number of pages`, na.rm = TRUE)
max_pages <- max(full_table$`Number of pages`, na.rm = TRUE)

genre_table <- full_table %>%
    select(id, Genre) %>%
    mutate(Genre = strsplit(Genre, split = "\\s*,\\s*")) %>%
    unnest(cols = Genre)

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


languages <- unique(full_table$Language)

# ui -----
# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(title = "Book Recs"),
    ## sidebar ----
    sidebar = dashboardSidebar(
        tags$a(href="https://forms.gle/aHPtwTNvrL6L1wb17", "Add a book", target="_blank"),
        ### genre ----
        checkboxGroupInput(inputId = "genre",
                           label = "Genre",
                           choices = genres,
                           selected = genres),
        actionButton(inputId = "genre_select_all",
                     label = "Select All"),
        actionButton(inputId = "genre_unselect_all",
                     label = "Unselect All"),
        
        ### page_limits ----
        sliderInput(inputId = "page_limits",
                    label = "Number of Pages",
                    min = min_pages,
                    max = max_pages,
                    value = c(min_pages, max_pages),
                    step = 100)
    ),
    ## body ----
    body = dashboardBody(
        checkboxGroupInput(inputId = "visible_cols",
                           label = NULL,
                           choices = full_colnames,
                           selected = full_colnames,
                           inline = TRUE),
        DTOutput(outputId = "book_table")
    ),
    title = "Book Recs",
    skin = "purple"
)

# server ----
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    message("start of app code")
    display_table <- reactiveVal(full_table)
    
    # update the display table ----
    observe({
        message("update display_table")
        
        filtered_genres <- genre_table %>%
            filter(Genre %in% input$genre)
        
        full_table %>%
            semi_join(filtered_genres, by = "id") %>%
            filter(
                (`Number of pages` >= input$page_limits[[1]] &
                 `Number of pages` <= input$page_limits[[2]]) |
                  is.na(`Number of pages`)  
            ) %>%
            select(c(input$visible_cols, "id")) %>%
            display_table()
    })
    
    # book_table ----
    output$book_table <- renderDT({
        message("render book_table")
        
        display_table() %>%
            select(-id)
    }, rownames = FALSE)
    
    # genre_select_all ----
    observeEvent(input$genre_select_all, {
        message("genre_select_all")
        
        updateCheckboxGroupInput(session = session, 
                                 inputId = "genre",
                                 selected = genres)
    })
    
    # genre_unselect_all ----
    observeEvent(input$genre_unselect_all, {
        message("genre_unselect_"ll")
        
        updateCheckboxGroupInput(session = session, 
                                 inputId = "genre",
                                 selected = character(0))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)






































mmm
