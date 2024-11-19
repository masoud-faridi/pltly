library(shiny)  
library(plotly)  
library(shinyWidgets)  
library(dplyr)  

# Prepare the data  
df_1 <- iris %>%  
  mutate(grp = if_else(Petal.Width > mean(Petal.Width), 1, 0)) %>%   
  mutate(grp2 = if_else(Sepal.Length > mean(Sepal.Length), 10, 9)) %>%   
  mutate(grp3 = if_else(Petal.Length > mean(Petal.Length), 100, 99)) %>%  
  mutate(id = 1:dim(iris)[1])  

ui <- fluidPage(  
  shinyWidgets::pickerInput('grp_levels',  
                            "Select Group Levels (grp)",   
                            multiple = TRUE,  
                            choices = sort(unique(df_1$grp), decreasing = TRUE),   
                            selected = sort(unique(df_1$grp), decreasing = TRUE),  # Select both groups by default  
                            options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),  
  shinyWidgets::pickerInput('grp2_levels',  
                            "Select Group Levels (grp2)",   
                            multiple = TRUE,  
                            choices = sort(unique(df_1$grp2), decreasing = TRUE),   
                            selected = sort(unique(df_1$grp2), decreasing = TRUE),  # Select both groups by default  
                            options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),  
  shinyWidgets::pickerInput('grp3_levels',  
                            "Select Group Levels (grp3)",   
                            multiple = TRUE,  
                            choices = sort(unique(df_1$grp3), decreasing = TRUE),   
                            selected = sort(unique(df_1$grp3), decreasing = TRUE),  # Select both groups by default  
                            options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)), 
  plotlyOutput("plotly_sunburst")  
)  

server <- function(input, output, session) {  
  output$plotly_sunburst <- renderPlotly({  
    # Initial plot with lines always shown  
    plot <- df_1 %>% plot_ly(alpha = 0.6) %>%  
      add_lines(x = ~id, y = ~Petal.Width, name = "Petal Width")  
    
    # Filter data based on first selected group levels  
    if (length(input$grp_levels) > 0) {  
      filtered_data_grp <- df_1 %>% filter(grp %in% input$grp_levels)  
      # Add markers only for the selected groups from grp  
      plot <- plot %>%  
        add_markers(data = filtered_data_grp, x = ~id, y = ~Petal.Width,   
                    color = I("blue"),  # Color for grp markers  
                    marker = list(size = 6),   
                    name = "Group Points (grp)")  
    }  
    
    # Filter data based on second selected group levels  
    if (length(input$grp2_levels) > 0) {  
      filtered_data_grp2 <- df_1 %>% filter(grp2 %in% input$grp2_levels)  
      # Add markers only for the selected groups from grp2  
      plot <- plot %>%  
        add_markers(data = filtered_data_grp2, x = ~id, y = ~Petal.Width,   
                    color = I("red"),  # Color for grp2 markers  
                    marker = list(size = 6),   
                    name = "Group Points (grp2)")  
    }  
    if (length(input$grp3_levels) > 0) {  
      filtered_data_grp3 <- df_1 %>% filter(grp3 %in% input$grp3_levels)  
      # Add markers only for the selected groups from grp2  
      plot <- plot %>%  
        add_markers(data = filtered_data_grp3, x = ~id, y = ~Petal.Width,   
                    color = I("green"),  # Color for grp2 markers  
                    marker = list(size = 6),   
                    name = "Group Points (grp3)")  
    }  
    
    plot  
  })  
  
  observeEvent(c(input$grp_levels, input$grp2_levels), {  
    # When groups are selected, update the plot accordingly  
    plotlyProxy("plotly_sunburst", session) %>%  
      plotlyProxyInvoke("restyle", list(visible = "TRUE"), list(1, 2))  # Update markers for both sets  
  })  
}  

shinyApp(ui, server)