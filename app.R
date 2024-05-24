library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Charger les données
diamonds <- read_csv("data/diamonds.csv")

# Interface utilisateur
ui <- navbarPage("Diamonds Analysis",
                 tabPanel("Data",
                          sidebarLayout(
                              sidebarPanel(
                                  # Checkbox pour afficher toutes les lignes
                                  checkboxInput("show_all", "Afficher toutes les lignes", value = FALSE),
                                  # SelectInput pour filtrer par coupe
                                  selectInput("cut_filter", "Filtrer par coupe", 
                                              choices = c("All", unique(diamonds$cut)), selected = "All")
                              ),
                              mainPanel(
                                  # Affichage du tableau de données
                                  tableOutput("data_table"),
                                  # Affichage du résumé statistique
                                  verbatimTextOutput("summary_stats")
                              )
                          )
                 ),
                 tabPanel("Visualisation",
                          navlistPanel(
                              tabPanel("Histogramme des prix",
                                       # Slider pour ajuster le nombre de bins de l'histogramme
                                       sliderInput("bins", "Nombre de bins :", min = 1, max = 50, value = 30),
                                       # Affichage de l'histogramme
                                       plotOutput("hist_plot")
                              ),
                              tabPanel("Boîte à moustaches de la profondeur",
                                       # SelectInput pour choisir la couleur de la boîte à moustaches
                                       selectInput("box_color", "Couleur de la boîte :", 
                                                   choices = c("red", "blue", "green"), selected = "red"),
                                       # Affichage de la boîte à moustaches
                                       plotOutput("box_plot")
                              ),
                              tabPanel("Nuage de points prix vs carat",
                                       # Input pour ajuster le titre du nuage de points
                                       textInput("scatter_title", "Titre du graphique :", value = "Prix en fonction du Carat"),
                                       # Affichage du nuage de points
                                       plotOutput("scatter_plot")
                              )
                          )
                 )
)


# Fonction serveur
server <- function(input, output) {
    
    # RenderTable pour afficher le tableau de données
    output$data_table <- renderTable({
        data <- diamonds
        # Filtrage des données selon le choix de coupe
        if (input$cut_filter != "All") {
            data <- filter(data, cut == input$cut_filter)
        }
        # Affichage de toutes les lignes ou seulement les 5 premières
        if (!input$show_all) {
            data <- head(data, 5)
        }
        data
    })
    
    # RenderPrint pour afficher le résumé statistique
    output$summary_stats <- renderPrint({
        data <- diamonds
        # Filtrage des données selon le choix de coupe
        if (input$cut_filter != "All") {
            data <- filter(data, cut == input$cut_filter)
        }
        # Calcul des statistiques descriptives
        summary_stats <- data %>%
            summarise(
                count = n(),
                mean_price = mean(price, na.rm = TRUE),
                sd_price = sd(price, na.rm = TRUE),
                mean_depth = mean(depth, na.rm = TRUE),
                sd_depth = sd(depth, na.rm = TRUE)
            )
        print(summary_stats)
    })
    
    # RenderPlot pour afficher l'histogramme des prix
    output$hist_plot <- renderPlot({
        ggplot(diamonds, aes(x = price)) +
            geom_histogram(bins = input$bins, fill = "blue", color = "black") +
            labs(title = "Histogramme des prix des diamants", x = "Prix", y = "Fréquence")
    })
    
    # RenderPlot pour afficher la boîte à moustaches de la profondeur
    output$box_plot <- renderPlot({
        ggplot(diamonds, aes(y = depth)) +
            geom_boxplot(fill = input$box_color) +
            labs(title = "Boîte à moustaches de la profondeur", y = "Profondeur")
    })
    
    # RenderPlot pour afficher le nuage de points prix vs carat
    output$scatter_plot <- renderPlot({
        ggplot(diamonds, aes(x = carat, y = price)) +
            geom_point(color = "blue") +
            labs(title = input$scatter_title, x = "Carat", y = "Prix")
    })
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
