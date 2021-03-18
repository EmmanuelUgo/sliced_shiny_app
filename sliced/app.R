
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidymodels)
library(waiter)
library(sever)
library(shinyjs)
library(shinycssloaders)
library(Cairo)


movies_df <- read_csv("games.txt")

movies_df <- movies_df %>%
    mutate(avg_peak_perc = parse_number(avg_peak_perc),
           sign_check = str_starts(gamename,"<")) %>%
    filter(!is.na(gain), !is.na(avg_peak_perc),sign_check == FALSE) 

theme_set(ggthemes::theme_fivethirtyeight(base_family = "verdana, sans-serif"))

ui <- dashboardPage(skin = "black",
    dashboardHeader(title = "Sliced"),
    
    ## Creating an input option to select games
    dashboardSidebar(
        sidebarUserPanel("A Shiny Dashboard", 
                         subtitle = " by Emmanuel Ugochukwu",
                         image = "steam_logo.png"),
        
        selectInput("v_game",
                    "Choose Games",
                    choices = movies_df %>%
                        distinct(gamename) %>%
                        arrange(gamename),
                    multiple = TRUE,
                    selected = c("Rust","ABZU"))),
    
    
    dashboardBody(useShinyjs(),
                  use_sever(),
                  use_waiter(), 
                  use_steward(),
                  waiter_show_on_load(spin_google()),
                  
        fluidPage(theme = shinythemes::shinytheme("paper"),
                  
        fluidRow(box(withSpinner(plotOutput("p_value"),type = 5), width = 12,
                 downloadButton("v_p_value_download"))),
        fluidRow(box(withSpinner(plotOutput("p_estimate"),type = 5), width = 12,
                     downloadButton("p_estimate_download"))))
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
## some UI design
    sever()
    Sys.sleep(2)
    waiter_hide()
    
## Creating a reactive df
    games_reactive <- reactive({
            movies_df %>%
            filter(gamename %in% input$v_game) %>%
            count(gamename,year, wt = mean(avg_peak_perc, na.rm = TRUE), name = "avg_perc") %>%
            filter(!is.na(avg_perc))
            # group_by(gamename,year) %>%
            # summarise(avg_perc = mean(avg_peak_perc, na.rm = TRUE)) %>%
            # ungroup() 
    })
    

 ## plot for p - value
    output$p_value <- renderPlot({
        req(input$v_game %in% movies_df$gamename)
        games_reactive() %>%
            nest(data = c(year,avg_perc)) %>%
            mutate(model = map(data, ~lm(avg_perc ~ year, data = .x))) %>%
            mutate(tidy_df = map(model, tidy)) %>%
            unnest(tidy_df) %>%
            filter(term == "year") %>%
            mutate(p.value = p.adjust(p.value)) %>%
            ggplot(aes(gamename,p.value))+
            geom_point(na.rm = TRUE)+
            geom_hline(yintercept = 0)+
            geom_hline(yintercept = 0.05)+
            geom_hline(yintercept = 0.95)+
            geom_hline(yintercept = 1)+
            coord_flip()+
            labs(
                title = "P-Values for each game",
                x = NULL,
                y = "P-Value", 
                caption = "Plot: @EmmanuelUgo | Source: Steam"
            ) +
            theme(
                legend.position = "none",
                axis.text.y = element_text(size = 15)
            )
    })
    
    ## Plot for percentage estimate
    output$p_estimate <- renderPlot({
        req(input$v_game %in% movies_df$gamename)
        games_reactive() %>%
            nest(data = c(year,avg_perc)) %>%
            mutate(model = map(data, ~lm(avg_perc ~ year, data = .x))) %>%
            mutate(tidy_df = map(model, tidy)) %>%
            unnest(tidy_df) %>%
            filter(term == "year") %>%
            mutate(gamename = str_wrap(gamename, 20),
                   gamename = fct_reorder(gamename,estimate),
                   estimate = estimate/100,
                   std.error = std.error/100)  %>%
            ggplot(aes(gamename,estimate))+
            geom_hline(yintercept = 0, col = "midnightblue")+
            geom_errorbar(aes(ymin = estimate - std.error,
                              ymax = estimate + std.error,
                              width = 0.2, col = gamename))+
            geom_point(na.rm = TRUE)+
            scale_y_continuous(labels = scales::percent)+
            coord_flip()+
            labs(
                title = "How does the average gain of these games move for each year?",
                x = NULL,
                caption = "Plot: @EmmanuelUgo | Source: Steam"
            ) +
            theme(
                legend.position = "none",
                axis.text.y = element_text(size = 13, family = "italics")
            )
    })
    
    ## Download Region
    
    output$v_p_value_download <- downloadHandler(
        filename = function(){paste0("p_value-",Sys.time(), ".png",sep = "")},
        contentType = "image/png",
        
        content =  function(file){
            ggsave(filename = file, width = 30,  height = 18,  units = "cm",device = "png",
                   type = "cairo-png", dpi = 72)})
    
    output$p_estimate_download <- downloadHandler(
        filename = function(){paste0("estimate-",Sys.time(), ".png",sep = "")},
        contentType = "image/png",
        
        content =  function(file){
            ggsave(filename = file, width = 30,  height = 18,  units = "cm",device = "png",
                   type = "cairo-png", dpi = 72)})

}

# Run the application 
shinyApp(ui = ui, server = server)
