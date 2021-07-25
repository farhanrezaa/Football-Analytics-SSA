# Next Step :
#1. Change Image Size
#2. Give more explanation on UI
#3. Make code shorter!
#4. Make sure there is no copyright violated
library(shiny)
library(tidyverse)
library(worldfootballR)
library(dplyr)
library(ggradar)
library(fmsb)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(h1("Self Service Football Analytics")),
    uiOutput("img"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("player_link_1", "Player 1 Link (Get from fbref.com/en/players)", "https://fbref.com/en/players/f25c8e3a/Naby-Keita"),
            textInput("player_link_2", "Player 2 Link (Get from fbref.com/en/players)", "https://fbref.com/en/players/77e84962/Thiago-Alcantara"),
            textInput("season_1", "Season of Player 1", "2020-2021"),
            textInput("season_2", "Season of Player 2", "2020-2021")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tab",
                        tabPanel("Table Summary", dataTableOutput(outputId = "player_stats_summary")),
                        tabPanel("Chart Summary", plotOutput("radar_chart", click = "plot_click"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ct_passing_stats_1 <- reactive({
        fb_player_season_stats(input$player_link_1, stat_type = 'passing') %>% 
            select(player_name, Season, Comp, Att_Total, Cmp_Total, Att_Long, Cmp_Long,
                   Final_Third, KP, Prog) %>% 
            filter(Season == input$season_1)
    })
    ct_passing_stats_2 <- reactive({
        fb_player_season_stats(input$player_link_2, stat_type = 'passing') %>% 
            select(player_name, Season, Comp, Att_Total, Cmp_Total, Att_Long, Cmp_Long,
                   Final_Third, KP, Prog) %>% 
            filter(Season == input$season_2)
    })
    
    
    ct_standard_stats_1 = reactive({
        fb_player_season_stats(input$player_link_1, stat_type = 'standard') %>% 
            mutate(goals_plus_assist = G_minus_PK + Ast,
               xG_plus_xA = xG_Expected + xA_Expected) %>% 
            select(player_name, Season, Comp, Min_Time, G_minus_PK, Ast, goals_plus_assist, xG_plus_xA,
               xG_Expected, xA_Expected, npxG_Expected) %>% 
            filter(Season == input$season_1)
    })
    ct_standard_stats_2 = reactive({
        fb_player_season_stats(input$player_link_2, stat_type = 'standard') %>% 
            mutate(goals_plus_assist = G_minus_PK + Ast,
                   xG_plus_xA = xG_Expected + xA_Expected) %>% 
            select(player_name, Season, Comp, Min_Time, G_minus_PK, Ast, goals_plus_assist, xG_plus_xA,
                   xG_Expected, xA_Expected, npxG_Expected) %>% 
            filter(Season == input$season_2)
    })
    
    
    ct_shooting_stats_1 = reactive({
        fb_player_season_stats(input$player_link_1, stat_type = 'shooting') %>% 
            select(player_name, Season, Comp, Sh_Standard, SoT_Standard) %>% 
            filter(Season == input$season_1)
    })
    ct_shooting_stats_2 = reactive({
        fb_player_season_stats(input$player_link_2, stat_type = 'shooting') %>% 
            select(player_name, Season, Comp, Sh_Standard, SoT_Standard) %>% 
            filter(Season == input$season_2)
    })
    
    
    ct_defense_stats_1 = reactive({
        fb_player_season_stats(input$player_link_1, stat_type = 'defense') %>% 
            filter(Season == input$season_1) %>% 
            select(player_name, Season, Comp, Tkl_Tackles, TklW_Tackles, Press_Pressures, Succ_Pressures,
                   `Att 3rd_Tackles`, `Mid 3rd_Tackles`, `Att 3rd_Pressures`, Int, Clr)
    })
    ct_defense_stats_2 = reactive({
        fb_player_season_stats(input$player_link_2, stat_type = 'defense') %>% 
            filter(Season == input$season_2) %>% 
            select(player_name, Season, Comp, Tkl_Tackles, TklW_Tackles, Press_Pressures, Succ_Pressures,
                   `Att 3rd_Tackles`, `Mid 3rd_Tackles`, `Att 3rd_Pressures`, Int, Clr)
    })
    
    ct_possession_stats_1 = reactive({
        fb_player_season_stats(input$player_link_1, stat_type = 'possession') %>% 
            filter(Season == input$season_1) %>% 
            select(player_name, Season, Comp, `Att 3rd_Touches`, `Att Pen_Touches`, Att_Dribbles,
                   Succ_Dribbles)
    })
    
    ct_possession_stats_2 = reactive({
        fb_player_season_stats(input$player_link_2, stat_type = 'possession') %>% 
            filter(Season == input$season_2) %>% 
            select(player_name, Season, Comp, `Att 3rd_Touches`, `Att Pen_Touches`, Att_Dribbles,
                   Succ_Dribbles)
    })
    
    ct_misc_stats_1 = reactive({
        fb_player_season_stats(input$player_link_1, stat_type = 'misc') %>%  
            filter(Season == input$season_1) %>% 
            select(player_name, Season, Comp, Recov, Won_Aerial_Duels, Lost_Aerial_Duels)
    })
    
    ct_misc_stats_2 = reactive({
        fb_player_season_stats(input$player_link_2, stat_type = 'misc') %>%  
            filter(Season == input$season_2) %>% 
            select(player_name, Season, Comp, Recov, Won_Aerial_Duels, Lost_Aerial_Duels)
    })
    
    overall_summary_1 = reactive({
        full_join(ct_passing_stats_1(), ct_standard_stats_1(), by = c('player_name', 'Season', 'Comp')) %>% 
        full_join(ct_shooting_stats_1(), by = c('player_name', 'Season', 'Comp')) %>% 
        full_join(ct_defense_stats_1(), by = c('player_name', 'Season', 'Comp')) %>% 
        full_join(ct_possession_stats_1(), by = c('player_name', 'Season', 'Comp')) %>% 
        full_join(ct_misc_stats_1(), by = c('player_name', 'Season', 'Comp'))
    })
    
    overall_summary_2 = reactive({
        full_join(ct_passing_stats_2(), ct_standard_stats_2(), by = c('player_name', 'Season', 'Comp')) %>% 
            full_join(ct_shooting_stats_2(), by = c('player_name', 'Season', 'Comp')) %>% 
            full_join(ct_defense_stats_2(), by = c('player_name', 'Season', 'Comp')) %>% 
            full_join(ct_possession_stats_2(), by = c('player_name', 'Season', 'Comp')) %>% 
            full_join(ct_misc_stats_2(), by = c('player_name', 'Season', 'Comp'))
    })
    
    summary_stats_1 = reactive({
        overall_summary_1() %>% 
        select(player_name, Comp, Min_Time, 
               xA_Expected, Att_Dribbles, npxG_Expected, G_minus_PK, Ast,
               KP, Sh_Standard, `Att Pen_Touches`, xG_plus_xA) %>% 
        group_by(player_name) %>% 
        summarise(total_minute = sum(Min_Time, na.rm = TRUE),
                  npGoal = sum(G_minus_PK, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                  xA = sum(xA_Expected, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                  Dribble = sum(Att_Dribbles, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                  npxG = sum(npxG_Expected, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                  key_pass = sum(KP, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                  shoot = sum(Sh_Standard, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                  pen_touch = sum(`Att Pen_Touches`, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                  xG_xA = sum(xG_plus_xA, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE))
    })
    
    summary_stats_2 = reactive({
        overall_summary_2() %>% 
            select(player_name, Comp, Min_Time, 
                   xA_Expected, Att_Dribbles, npxG_Expected, G_minus_PK, Ast,
                   KP, Sh_Standard, `Att Pen_Touches`, xG_plus_xA) %>% 
            group_by(player_name) %>% 
            summarise(total_minute = sum(Min_Time, na.rm = TRUE),
                      npGoal = sum(G_minus_PK, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                      xA = sum(xA_Expected, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                      Dribble = sum(Att_Dribbles, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                      npxG = sum(npxG_Expected, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                      key_pass = sum(KP, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                      shoot = sum(Sh_Standard, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                      pen_touch = sum(`Att Pen_Touches`, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE),
                      xG_xA = sum(xG_plus_xA, na.rm = TRUE)*90/sum(Min_Time, na.rm = TRUE))
    })
    
    player_comparison = reactive({
        rbind(summary_stats_1(), summary_stats_2()) %>% 
        select(player_name, npGoal, xA, Dribble, npxG, key_pass, shoot, pen_touch, xG_xA)
    })
    
    player_comparison_chart = reactive({
        rbind(summary_stats_1(), summary_stats_2()) %>% 
            select(npGoal, xA, Dribble, npxG, key_pass, shoot, pen_touch, xG_xA)
    })
    
    areas <- c(rgb(1, 0, 0, 0.25),
               rgb(0, 1, 0, 0.25),
               rgb(0, 0, 1, 0.25),
               rgb(1, 1, 0, 0.35))

    output$player_stats_summary <- renderDataTable({player_comparison()})
    output$radar_chart <- renderPlot({
        # Create Radar Chart for Comparing Players
        radarchart(player_comparison_chart(),
                   title = 'Compare Player Using Radar Chart',
                   maxmin = FALSE,
                   cglty = 1,       # Grid line type
                   cglcol = "gray", # Grid line color
                   pcol = 2:4,      # Color for each line
                   plwd = 2,        # Width for each line
                   plty = 1,        # Line type for each line
                   pfcol = areas)
        legend("topright",
               legend = c('Player 1', 'Player 2'),
               bty = "n", pch = 20, col = areas,
               text.col = "grey25", pt.cex = 2)
    })
    output$img <- renderUI({
        tags$img(src = "https://www.actsoft.com/wp-content/uploads/2019/10/football-play-speed-tracking-fleet-data-analytics-e1569945248768.jpg")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
