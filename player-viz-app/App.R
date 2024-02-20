# Set directory ----
# setwd("C:/Users/David/OneDrive/Desktop/dataClass/04-ff-analysis/02-szn-long/2023/02-main-analysis")

# Load Shiny & other packages ----
library(shiny)
library(tidyverse)
library(ggimage)
library(scales)
library(nflreadr)
library(nflfastR)
library(gt)
library(gtExtras)
library(DT)
# library(rsconnect)

# Source helpers ----
source("helpers.R")
# source("player-viz-app/helpers.R")

# Load data ----
playerScores <- read.csv("data/8. Player Scores.csv")
playerFinishes <- read.csv("data/9. Player Finishes.csv")
schedule <- read.csv("data/11. NFL Schedule.csv")

# Add Games Played Column
playerFinishes <- playerFinishes %>%
  mutate(GamesPlayed = 16 - MissedGames)

# Add Opponents Column to Player Scores
playerScores <- playerScores %>%
  left_join(playerFinishes %>% select(Player, Pos, Team), by = "Player") %>%
  left_join(schedule, by = c("Team" = "Team", "Week" = "Week")) %>%
  left_join(teams_colors_logos %>% select(team_abbr, team_logo_espn), 
                                          by = c("Opponent" = "team_abbr"))

# Add Names Columns
#playerFinishes <- playerFinishes %>% 
#  mutate(SimpleName = vFullName(Player), 
#         FirstName = vSingleName(Player, 1), 
#         LastName = vSingleName(Player, 2))

# Arrange PlayerFinshes DF by Total
playerFinishes <- playerFinishes %>% arrange(desc(Total))

# Function to display player team, number, and position 
playerInfo <- function(playerName = "") {
  if (!is.na(playerName)) {
    if (is.na((playerFinishes %>% filter(Player == playerName))[, 13])) {
      return(paste0(
        (playerFinishes %>% filter(Player == playerName))[, 11],
        " | ", 
        (playerFinishes %>% filter(Player == playerName))[, 2]))
    }
    else {
      return(paste0(
        (playerFinishes %>% filter(Player == playerName))[, 11],
        " | #", 
        (playerFinishes %>% filter(Player == playerName))[, 13],
        " ", 
        (playerFinishes %>% filter(Player == playerName))[, 2]))
    }
  }
}


# Define ui ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Fantasy Football Player Analysis 2023 (PPR)"),
  
  # Navbar layout ----
  navbarPage(title = "", 
             
    # First Page ----
    tabPanel(h5("Player Analysis"),
    
      # First row ----
      fluidRow(
        
        # First Column ----
        column(2, 
        
              # Sidebar panel ----
              wellPanel(
                
                # Player search ----
                textInput(inputId = "PlayerInput", label = h4("Player Search"),
                          value = ""))
              ),
                
        # Second Column ----
        column(6, h2(textOutput(outputId = "PlayerNameOutput")), 
                 h4(textOutput(outputId = "PlayerInfoOutput")), 
                 gt_output(outputId = "PlayerFinishOutput")),
        
        # Third Column ----
        column(4, htmlOutput(outputId = "TeamLogo"))
        
        ), 
              
      # Second row ----
      fluidRow(
                
        # First column ----
        column(2, 
               
               # Sidebar panel ----
               wellPanel(
                 
                 
                 # Week range slider ----
                 sliderInput(inputId = "Weeks", label = h4("Weeks"), min = 1,
                             max = 17, value = c(1,17)), 
                 
                 # Ylim slider ----
                 sliderInput(inputId = "Ylims", label = h4("Score Limits"), 
                             min = 0, max = 56, value = c(0, 56), step = 8), 
                 
                 # Opponents-on-graph check box ----
                 checkboxInput(inputId = "Opps", "Show opponents",
                               value = FALSE))
        ), 
                
        # Second column ----
        column(3,  h3("Overview of Scores"), 
               plotOutput(outputId = "BoxPlot")), 
                
        # Third column ----
        column(5, h3("Scores by Week"), 
               plotOutput(outputId = "LinePlot")), 
                
        # Fourth column ----
        column(2)
        
        ), 
      
      # Third row ----
      fluidRow(
        
        # First column ----
        column(2), 
        
        # Second column ----
        column(3, h3("Summary Statistics"), 
               gt_output(outputId = "ScoresStats")), 
        
        # Third column ----
        column(5, h3("Data Table"), 
               dataTableOutput(outputId = "WeeklyScores"))
        
      )
      
      ), 
    
    # Second Page ----
    tabPanel(h5("Player Comparison"), 
      
      # First Row ----
      fluidRow(
        
        # First column ----
        column(2, 
               
               # Sidebar panel ----
               wellPanel(
                 
                 # Player search ----
                 textInput(inputId = "PlayerOneInput", 
                           label = h4("Player Search"), value = ""))
              ),
        
        # Second Column ----
        column(1, htmlOutput(outputId = "PlayerOneTeamLogo")), 
        
        # Third Column ----
        column(3, h2(textOutput(outputId = "PlayerOneNameOutput"), align = "left"), 
               h4(textOutput(outputId = "PlayerOneInfoOutput"), align = "left")),
        
        # Forth Column ----
        column(1, h2("vs.", align = "center")),
        
        # Fifth Column ----
        column(3, h2(textOutput(outputId = "PlayerTwoNameOutput"), align = "right"), 
               h4(textOutput(outputId = "PlayerTwoInfoOutput"), align = "right")),
        
        # Sixth Column ----
        column(1, htmlOutput(outputId = "PlayerTwoTeamLogo")),
        
        # Seventh Column ----
        column(1)
        
        ), 
      
      # Second row ----
      fluidRow(
        
        # First column ----
        column(2, 
               
               # Sidebar panel ----
               wellPanel(

                 # Player search ----
                 textInput(inputId = "PlayerTwoInput", 
                           label = h4("Player Search"), value = ""))
        ),
        
        # Second column ----
        column(1), 
        
        # Third column ----
        column(7, gt_output(outputId = "PlayerCompFinishOutput")), 
        
        # Fourth column ----
        column(2)
        
      ),
    
      # Third row ----
      fluidRow(
        
        # First column ----
        column(2, 
               
               # Sidebar panel ----
               wellPanel(
                 
                 # Week range slider ----
                 sliderInput(inputId = "WeeksComp", label = h4("Weeks"), min = 1,
                             max = 17, value = c(1,17)), 
                 
                 # Ylim slider ----
                 sliderInput(inputId = "YlimsComp", label = h4("Score Limits"), 
                             min = 0, max = 56, value = c(0, 56), step = 8), 
                 
                 # Opponents-on-graph check box ----
                 checkboxInput(inputId = "OppsComp", "Show opponents",
                               value = FALSE))
               
        ), 
        
        # Second column ----
        column(3, h3("Overview of Scores"), 
               plotOutput(outputId = "PlayerCompBoxPlot")), 
        
        # Third column ----
        column(5, h3("Scores by Week"), 
               plotOutput(outputId = "PlayerCompLinePlot")), 
        
        # Fourth column ----
        column(2)
        
        ), 
      
      # Fourth row ----
      fluidRow(
        
        # First column ----
        column(2), 
        
        # Second column ----
        column(3, h3("Summary Statistics"), 
               gt_output(outputId = "CompStats")), 
        
        # Fourth column ----
        column(5, h3("Data Table"), 
               dataTableOutput(outputId = "PlayerCompWeeklyScores"))
        
      )
      
      ), 
    
    # Third Page ----
    tabPanel(h5("About"),
             
      # First row ----
      fluidRow(
        
        # First column ----
        column(12, align = "left", 
               h5("App by: David Harler Jr."), 
               br(), 
               h5("Fantasy Football Data from: FantasyPros"), 
               h5("ADP from: Sleeper"), 
               h5("Team Logos from: nflfastR"))
        
      )
    )
  )
)
  

# Define server logic ----
server <- function(input, output) {
  
  # Player Name Heading ----
  output$PlayerNameOutput <- renderText({ 
    input$PlayerInput
  })
  
  # Player One Name Heading ----
  output$PlayerOneNameOutput <- renderText({ 
    input$PlayerOneInput
  })
  
  # Player Two Name Heading ----
  output$PlayerTwoNameOutput <- renderText({ 
    input$PlayerTwoInput
  })
  
  # Reactive Conductor to Hold Current Player's Name
  #currPlayer <- reactive({
  #  playerSearch(input$PlayerInput)
  #})
  
  # Reactive Conductor to Hold Current Player One's Name
  #currP1 <- reactive({
  #  playerSearch(input$PlayerOneInput)
  #})
  
  # Reactive Conductor to Hold Current Player Two's Name
  #currP2 <- reactive({
  #  playerSearch(input$PlayerTwoInput)
  #})
  
  # Player Info Subheading, utilizing the Player Info Helper Function ----
  output$PlayerInfoOutput <- renderText({
    playerInfo(input$PlayerInput)
  })
  
  # Player One Info Subheading, utilizing the Player Info Helper Function ----
  output$PlayerOneInfoOutput <- renderText({
    playerInfo(input$PlayerOneInput)
  })
  
  # Player Two Info Subheading, utilizing the Player Info Helper Function ----
  output$PlayerTwoInfoOutput <- renderText({
    playerInfo(input$PlayerTwoInput)
  })
  
  # Team Logo, utilizing HTML ----
  output$TeamLogo <- renderText({
    c('<img src="', 
      (playerFinishes %>% filter(Player == input$PlayerInput))$team_logo_espn, 
      '"width="100" height="100">')
  })
  
  # Player One Team Logo, utilizing HTML ----
  output$PlayerOneTeamLogo <- renderText({
    c('<center><img src="', 
      (playerFinishes %>% filter(Player == input$PlayerOneInput))$team_logo_espn, 
      '"width="100" height="100">')
  })
  
  # Player Two Team Logo, utilizing HTML ----
  output$PlayerTwoTeamLogo <- renderText({
    c('<center><img src="', 
      (playerFinishes %>% filter(Player == input$PlayerTwoInput))$team_logo_espn, 
      '"width="100" height="100">')
  })
  
  # Player Season Overview gtTable ----
  output$PlayerFinishOutput <- render_gt(
    playerFinishes %>% filter(Player == input$PlayerInput) %>%
      select(Finish, Total, PPG, PosADP, RoundDrafted, MissedGames, bye_week) %>%
      gt() %>% opt_align_table_header("center") %>%
      cols_align("center") %>% 
      cols_width(Finish ~ px(80), Total ~ px(80), PPG ~ px(80), 
                 PosADP ~ px(80), RoundDrafted ~ px(80), 
                 MissedGames ~ px(80), bye_week ~ px(80)) %>%
    cols_label(PosADP = "ADP", RoundDrafted = "Round Drafted", 
               MissedGames = "Games Missed", bye_week = "Bye Week", 
               Finish = "Season Rank"), 
    align = "left"
  )
  
  # Player Comp Season Overview gtTable ----
  output$PlayerCompFinishOutput <- render_gt(
    playerFinishes %>% filter(Player == input$PlayerOneInput |
                                Player == input$PlayerTwoInput) %>%
      select(Player, Finish, Total, PPG, PosADP, RoundDrafted, MissedGames, bye_week) %>%
      gt() %>% opt_align_table_header("center") %>%
      cols_align("center") %>% 
      cols_width(Finish ~ px(80), Total ~ px(80), PPG ~ px(80), 
                 PosADP ~ px(80), RoundDrafted ~ px(80), MissedGames ~ px(80), 
                 bye_week ~ px(80), Player ~ px(160)) %>%
      cols_label(PosADP = "ADP", RoundDrafted = "Round Drafted", 
                 MissedGames = "Games Missed", bye_week = "Bye Week", 
                 Finish = "Season Rank"), 
    align = "center"
  )
  
  # Reactive conductor to hold the current Player's Position ----
  currPos <- reactive({
    (playerFinishes %>% filter(Player == input$PlayerInput))[1, 2]  
  })
  
  # Reactive conductor to recalculate the player's rank based on user inputs ----
  currPosRankDF <- reactive({
    playerScores %>% 
      filter(Pos == currPos() &
               Week >= input$Weeks[1] & 
               Week <= input$Weeks[2] &
               Score >= input$Ylims[1] &
               Score <= input$Ylims[2]) %>%
      group_by(Player) %>%
      summarise("Total" = round(sum(Score, na.rm = TRUE), 1)) %>%
      left_join(playerFinishes %>% select(Player, Pos), by = "Player") %>%
      mutate(PosRank = paste0(Pos, as.character(round(rank(-Total), 0)))) %>%
      filter(Player == input$PlayerInput)
  })
  
  # Reactive conductor to hold the player's current rank ----
  currPosRank <- reactive({
    currPosRankDF()[1, 4]
    })
  
  # Reactive conductor to hold Player One's Position
  currPosP1 <- reactive({
    (playerFinishes %>% filter(Player == input$PlayerOneInput))[1, 2]  
  })
  
  # Reactive conductor to hold Player Two's Position
  currPosP2 <- reactive({
    (playerFinishes %>% filter(Player == input$PlayerTwoInput))[1, 2]  
  })
  
  # Reactive conductor to recalculate Player One's rank based on the selected time span ----
  currP1RankDF <- reactive({
    playerScores %>% 
      filter(Pos == currPosP1() &
               Week >= input$WeeksComp[1] & 
               Week <= input$WeeksComp[2] &
               Score >= input$YlimsComp[1] &
               Score <= input$YlimsComp[2]) %>%
      group_by(Player) %>%
      summarise("Total" = round(sum(Score, na.rm = TRUE), 1)) %>%
      left_join(playerFinishes %>% select(Player, Pos), by = "Player") %>%
      mutate(PosRank = paste0(Pos, as.character(round(rank(-Total), 0)))) %>%
      filter(Player == input$PlayerOneInput)
  })
  
  # Reactive conductor to recalculate Player Two's rank based on the selected time span ----
  currP2RankDF <- reactive({
    playerScores %>% 
      filter(Pos == currPosP2() &
               Week >= input$WeeksComp[1] & 
               Week <= input$WeeksComp[2] &
               Score >= input$YlimsComp[1] &
               Score <= input$YlimsComp[2]) %>%
      group_by(Player) %>%
      summarise("Total" = round(sum(Score, na.rm = TRUE), 1)) %>%
      left_join(playerFinishes %>% select(Player, Pos), by = "Player") %>%
      mutate(PosRank = paste0(Pos, as.character(round(rank(-Total), 0)))) %>%
      filter(Player == input$PlayerTwoInput)
  })
  
  # Reactive conductor to hold the Player One's current rank ----
  currP1Rank <- reactive({
    currP1RankDF()[1, 4]
  })
  
  # Reactive conductor to hold the Player Two's current rank ----
  currP2Rank <- reactive({
    currP2RankDF()[1, 4]
  })
  
  # gtTable to display the player's summary statistics ----
  output$ScoresStats <- render_gt(
    playerScores %>% filter(Player == input$PlayerInput & 
                              Week >= input$Weeks[1] & 
                              Week <= input$Weeks[2] &
                              Score >= input$Ylims[1] &
                              Score <= input$Ylims[2]) %>%
      summarise("PPG" = round(mean(Score, na.rm = TRUE), 1), 
                "Floor" = round(min(Score, na.rm = TRUE), 1), 
                "Median" = round(median(Score, na.rm = TRUE), 1), 
                "Ceiling" = round(max(Score, na.rm = TRUE), 1), 
                "Stdev" = round(sd(Score, na.rm = TRUE), 1), 
                "Total" = round(sum(Score, na.rm = TRUE), 1)) %>% 
      pivot_longer(cols = everything()) %>% 
      rbind(data.frame(name = c("Rank"), value = c(as.character(currPosRank())))) %>%
      gt() %>% 
      tab_options(column_labels.hidden = TRUE) %>%
      cols_align("left") %>% 
      cols_width(name ~ px(120), value ~ px(120)), 
    align = "left"
  )
  
  # gtTable to display the summary statistics for Players 1 & 2 ----
  output$CompStats <- render_gt(
    
    left_join(
      playerScores %>% filter(Player == input$PlayerOneInput & 
                                Week >= input$WeeksComp[1] & 
                                Week <= input$WeeksComp[2] &
                                Score >= input$YlimsComp[1] &
                                Score <= input$YlimsComp[2]) %>%
      summarise("PPG" = round(mean(Score, na.rm = TRUE), 1), 
                "Floor" = round(min(Score, na.rm = TRUE), 1), 
                "Median" = round(median(Score, na.rm = TRUE), 1), 
                "Ceiling" = round(max(Score, na.rm = TRUE), 1), 
                "Stdev" = round(sd(Score, na.rm = TRUE), 1), 
                "Total" = round(sum(Score, na.rm = TRUE), 1)) %>% 
        pivot_longer(cols = everything()) %>% 
        rbind(data.frame(name = c("Rank"), value = c(as.character(currP1Rank())))) %>%
        rename(p1values = "value"), 
      playerScores %>% filter(Player == input$PlayerTwoInput & 
                                Week >= input$WeeksComp[1] & 
                                Week <= input$WeeksComp[2] &
                                Score >= input$YlimsComp[1] &
                                Score <= input$YlimsComp[2]) %>%
      summarise("PPG" = round(mean(Score, na.rm = TRUE), 1), 
                "Floor" = round(min(Score, na.rm = TRUE), 1), 
                "Median" = round(median(Score, na.rm = TRUE), 1), 
                "Ceiling" = round(max(Score, na.rm = TRUE), 1), 
                "Stdev" = round(sd(Score, na.rm = TRUE), 1), 
                "Total" = round(sum(Score, na.rm = TRUE), 1)) %>% 
        pivot_longer(cols = everything()) %>% 
        rbind(data.frame(name = c("Rank"), value = c(as.character(currP2Rank())))) %>%
        rename(p2values = "value"), 
      by = "name") %>%
      gt() %>% 
      tab_options() %>%
      cols_align("left") %>% 
      cols_width(name ~ px(120), p1values ~ px(120), p2values ~ px(120)) %>%
      cols_label(name = "", p1values = input$PlayerOneInput, 
                 p2values = input$PlayerTwoInput), 
    align = "left"
  )

  # Box Plot of Player's Scores ----
  output$BoxPlot <- renderPlot({
    
    p1 <- playerScores %>% filter(Player == input$PlayerInput & 
                                    Week >= input$Weeks[1] & 
                                    Week <= input$Weeks[2]) %>% 
      ggplot(mapping = aes(x = Player, y = Score), na.rm = TRUE) +
      geom_point(color = (playerFinishes %>% 
                            filter(Player == input$PlayerInput))$team_color) + 
      geom_boxplot(color = (playerFinishes %>% 
                              filter(Player == input$PlayerInput))$team_color, 
                   alpha = 0.5) +
      ylim(input$Ylims[1], input$Ylims[2]) +
      xlab("") + ylab("PPR Points") +
      theme(plot.margin = margin(7.25, 12, 18.75, 12, "pt"),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 16), 
            axis.title.y = element_text(size = 18, face = "plain"), 
            axis.ticks = element_blank(), 
            panel.border = element_rect(color = "black", 
                                        fill = NA, 
                                        linewidth = 1))
    
    p1
    
  })
  
  # Box Plot for Player Comp ----
  output$PlayerCompBoxPlot <- renderPlot({
    
    p3 <- playerScores %>% filter((Player == input$PlayerOneInput | 
                                   Player == input$PlayerTwoInput) & 
                                   Week >= input$WeeksComp[1] & 
                                   Week <= input$WeeksComp[2]) %>% 
      ggplot(na.rm = TRUE) +
      geom_point(mapping = aes(x = Player, y = Score, color = Player)) +
      geom_boxplot(mapping = aes(x = Player, y = Score, color = Player), alpha = 0.5) +
      scale_color_manual(values = 
                          c((playerFinishes %>% filter(Player == input$PlayerOneInput))$team_color,
                            (playerFinishes %>% filter(Player == input$PlayerTwoInput))$team_color), 
                        breaks = c(input$PlayerOneInput, input$PlayerTwoInput)) + 
      scale_fill_manual(values = 
                          c((playerFinishes %>% filter(Player == input$PlayerOneInput))$team_color,
                            (playerFinishes %>% filter(Player == input$PlayerTwoInput))$team_color), 
                        breaks = c(input$PlayerOneInput, input$PlayerTwoInput)) + 
      ylim(input$YlimsComp[1], input$YlimsComp[2]) +
      xlab("") + ylab("PPR Points") +
      theme(plot.margin = margin(7.25, 12, 18.75, 12, "pt"),
            axis.text.x = element_text(size = 18, face = "plain"),
            axis.text.y = element_text(size = 16), 
            axis.title.y = element_text(size = 18, face = "plain"), 
            axis.ticks = element_blank(), 
            panel.border = element_rect(color = "black", 
                                        fill = NA, 
                                        linewidth = 1), 
            legend.position = "none")
    
    p3
    
  })
  
  # Line Chart of Player's Scores ----
  output$LinePlot <- renderPlot({
    
    if (!input$Opps) {
      p2 <- playerScores %>% 
        filter(Player == input$PlayerInput & 
                 Week >= input$Weeks[1] & 
                 Week <= input$Weeks[2]) %>% 
        ggplot(mapping = aes(x = Week, y = Score), na.rm = TRUE) +
        geom_point(color = (playerFinishes %>% 
                              filter(Player == input$PlayerInput))$team_color) + 
        geom_line(color = (playerFinishes %>% 
                             filter(Player == input$PlayerInput))$team_color,
                  linewidth = 0.65) +
        ylim(input$Ylims[1], input$Ylims[2]) +
        scale_x_continuous(breaks = integer_breaks()) + 
        xlab("Weeks") + ylab("") +
        theme(plot.margin = margin(8, 12, 0, 12, "pt"), 
              axis.text.x = element_text(size = 16), 
              axis.text.y = element_text(size = 16),
              axis.title.x = element_text(size = 18, face = "plain"), 
              axis.ticks = element_blank(), 
              panel.border = element_rect(color = "black", 
                                          fill = NA, 
                                          linewidth = 1))
      
      p2
    }
    
    else {
      p2 <- playerScores %>% 
        filter(Player == input$PlayerInput & 
                 Week >= input$Weeks[1] & 
                 Week <= input$Weeks[2]) %>% 
        ggplot(mapping = aes(x = Week, y = Score), na.rm = TRUE) +
        geom_image(aes(image = team_logo_espn), size = .125) + 
        geom_line(color = (playerFinishes %>% 
                             filter(Player == input$PlayerInput))$team_color,
                  linewidth = 0.65) +
        ylim(input$Ylims[1], input$Ylims[2]) +
        scale_x_continuous(breaks = integer_breaks()) + 
        xlab("Weeks") + ylab("") +
        theme(plot.margin = margin(8, 12, 0, 12, "pt"), 
              axis.text.x = element_text(size = 16), 
              axis.text.y = element_text(size = 16),
              axis.title.x = element_text(size = 18, face = "plain"), 
              axis.ticks = element_blank(), 
              panel.border = element_rect(color = "black", 
                                          fill = NA, 
                                          linewidth = 1))
      
      p2
    }
    
  })
  
  # Line Chart for Player Comp ----
  output$PlayerCompLinePlot <- renderPlot({
    
    if (!input$OppsComp) {
      p4 <- playerScores %>% 
        filter((Player == input$PlayerOneInput | Player == input$PlayerTwoInput) &
                 Week >= input$WeeksComp[1] & 
                 Week <= input$WeeksComp[2]) %>% 
        ggplot(mapping = aes(x = Week, y = Score, color = Player), na.rm = TRUE) +
        geom_point() + 
        geom_line(linewidth = 0.65) +
        ylim(input$YlimsComp[1], input$YlimsComp[2]) +
        scale_x_continuous(breaks = integer_breaks()) + 
        scale_color_manual(values = 
                             c((playerFinishes %>% filter(Player == input$PlayerOneInput))$team_color,
                               (playerFinishes %>% filter(Player == input$PlayerTwoInput))$team_color), 
                           breaks = c(input$PlayerOneInput, input$PlayerTwoInput)) + 
        xlab("Weeks") + ylab("") +
        theme(legend.key.size = unit(2, 'cm'),
              plot.margin = margin(8, 12, 0, 12, "pt"), 
              axis.text.x = element_text(size = 16), 
              axis.text.y = element_text(size = 16),
              axis.title.x = element_text(size = 18, face = "plain"), 
              axis.ticks = element_blank(), 
              panel.border = element_rect(color = "black", 
                                          fill = NA, 
                                          linewidth = 1))
      
      p4
    }
    
    else {
      p4 <- playerScores %>% 
        filter((Player == input$PlayerOneInput | Player == input$PlayerTwoInput) & 
                 Week >= input$WeeksComp[1] & 
                 Week <= input$WeeksComp[2]) %>% 
        ggplot(mapping = aes(x = Week, y = Score), na.rm = TRUE) +
        geom_image(aes(image = team_logo_espn), size = .125) + 
        geom_line(mapping = aes(color = Player), linewidth = 0.65) +
        ylim(input$YlimsComp[1], input$YlimsComp[2]) +
        scale_x_continuous(breaks = integer_breaks()) + 
        scale_color_manual(values = 
                             c((playerFinishes %>% filter(Player == input$PlayerOneInput))$team_color,
                               (playerFinishes %>% filter(Player == input$PlayerTwoInput))$team_color), 
                           breaks = c(input$PlayerOneInput, input$PlayerTwoInput)) + 
        xlab("Weeks") + ylab("") +
        theme(legend.key.size = unit(2, 'cm'), 
              plot.margin = margin(8, 12, 0, 12, "pt"), 
              axis.text.x = element_text(size = 16), 
              axis.text.y = element_text(size = 16),
              axis.title.x = element_text(size = 18, face = "plain"), 
              axis.ticks = element_blank(), 
              panel.border = element_rect(color = "black", 
                                          fill = NA, 
                                          linewidth = 1))
      
      p4
      
    }
    
  })
  
  
  # Data Table of Player's Scores ----
  output$WeeklyScores = renderDataTable({
    if (!input$Opps){
      playerScores %>% 
        filter(Player == input$PlayerInput & 
                 Week >= input$Weeks[1] & 
                 Week <= input$Weeks[2] &
                 Score >= input$Ylims[1] &
                 Score <= input$Ylims[2]) %>%
        select(Week, Score)
    }
    else {
      playerScores %>% 
        filter(Player == input$PlayerInput & 
                 Week >= input$Weeks[1] & 
                 Week <= input$Weeks[2] &
                 Score >= input$Ylims[1] &
                 Score <= input$Ylims[2]) %>%
        select(Week, Score, Opponent)
    }
  }, options = list(pageLength = 25, dom = "ft")
  )
  
  # Data Table for Player Comp ----
  output$PlayerCompWeeklyScores = renderDataTable(
    playerScores %>% filter((Player == input$PlayerOneInput | 
                               Player == input$PlayerTwoInput) &
                              Week >= input$WeeksComp[1] &
                              Week <= input$WeeksComp[2] &
                              Score >= input$YlimsComp[1] &
                              Score <= input$YlimsComp[2]) %>%
      select(Player, Week, Score) %>%
      spread(key = Player, value = Score), 
    options = list(pageLength = 25, dom = "ft")
  )


  
}

# Create Shiny app ----
shinyApp(ui, server)






