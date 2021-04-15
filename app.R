#loading the libraries
library(shiny)
library(tidyverse)

#reading the csv file and transforming some data
netflix <- read_csv("netflix_titles.csv")
netflixYear <- unique(netflix$release_year)
netflixRating <- sort(unique(netflix$rating))



ui <- fluidPage(

    # Application title
    titlePanel("Netflix Data"),

    sidebarLayout(
        sidebarPanel(
            #year selection
            sliderInput(inputId = "years", 
                        label = "Select Release Year",
                        min = 1980,
                        max = max(netflixYear),
                        value = c(1990, 2010),
                        sep = ""
                        ),
            #rating selection
            selectInput(
                inputId = "rating",
                label = "Select ratings",
                choices = netflixRating,
                selected = sample(netflixRating, 1),
                multiple = TRUE
            )
        ),
        mainPanel(
            plotOutput("releases"),
            plotOutput("ratings")
        )
    )
)

# server logic
server <- function(input, output) {
    output$releases <- renderPlot({
        netflix %>%
            filter(release_year > input$years[1] & release_year < input$years[2]) %>%
            group_by(release_year) %>% 
            summarize(count = n()) %>% 
            arrange(desc(count)) %>% 
            ggplot(aes(reorder(release_year, count), count, fill = release_year)) +
            geom_bar(stat = "identity", width = 0.9) +
            xlab("Year") +
            ylab("Number of movies") +
            ggtitle("Number of releases") +
            coord_flip()
    })
    output$ratings <- renderPlot ({
        netflix %>%
            #choosing only the non-empty values from the list selected by the user
            filter(!is.na(rating) & rating %in% input$rating) %>% 
            group_by(rating) %>% 
            summarize(count = n()) %>% 
            arrange(desc(count)) %>% 
            ggplot(aes(reorder(rating, count), count, fill = rating)) +
            geom_bar(stat = "identity", width = 0.8) +
            coord_flip() +
            xlab("TV ratings") +
            ylab("No of movies") +
            ggtitle("Ratings with the biggest number of releases")
    })
}

# running the app
shinyApp(ui = ui, server = server)
