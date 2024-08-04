library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# Define UI
ui <- fluidPage(
  titlePanel("Energy Consumption Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Plot Options"),
      selectInput("plot_type", "Choose a plot:",
                  choices = c("Box Plot", "City Bar Chart", 
                              "Scatter Plot", "Energy consumption Bar Chart", "Heating Fuel Bar Chart"))  
    ),
    mainPanel(
      plotOutput("selected_plot")
    )
  )
)

# Define Server
server <- function(input, output) {
  # Load data reactively
  data <- reactive({
    readRDS("data/merge.rds")
  })
  
  output$selected_plot <- renderPlot({
    req(data())  # Ensure data is loaded
    
    # Switch case to handle different plot types
    switch(input$plot_type,
           "Box Plot" = {
             ggplot(data(), aes(x = in.building_america_climate_zone, y = day_total_energy)) +
               geom_boxplot(fill = "skyblue", color = "orange") +
               labs(title = "Distribution of Total Energy Consumption Across Building Climate Zones",
                    x = "Building Climate Zone",
                    y = "Total Energy Consumption") +
               theme_minimal()
           },
           "City Bar Chart" = {
             average_energy_by_city <- data() %>%
               group_by(in.weather_file_city) %>%
               summarize(avg_energy = mean(day_total_energy, na.rm = TRUE))
             
             ggplot(average_energy_by_city, aes(x = in.weather_file_city, y = avg_energy)) +
               geom_bar(stat = "identity", fill = "maroon") +
               labs(title = "Average Energy Consumption by City",
                    x = "City",
                    y = "Average Energy Consumption") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           },
           "Scatter Plot" = {
             ggplot(data(), aes(x = median_Dry_Bulb_Temperature, y = day_total_energy, color = median_Dry_Bulb_Temperature)) +
               geom_point(size = 3) +
               scale_color_gradient(low = "pink", high = "blue", name = "Temperature") +
               labs(title = "Total Energy Consumption vs. Dry Bulb Temperature",
                    x = "Temperature",
                    y = "Total Energy Consumption") +
               theme_minimal()
           },
           "Energy consumption Bar Chart" = {
             ggplot(data(), aes(x = date, y = day_total_energy)) +
               geom_bar(stat = "identity", fill = "purple") +
               labs(title = "Energy Consumption Over Time (Bar Chart)",
                    x = "Date",
                    y = "Total Energy Consumption (kWh)") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           },
           "Heating Fuel Bar Chart" = {
             filtered_data_heating <- data() %>%
               filter(in.heating_fuel != "None") %>%
               group_by(in.heating_fuel) %>%
               summarize(day_total_energy = sum(day_total_energy, na.rm = TRUE))
             
             ggplot(filtered_data_heating, aes(x = in.heating_fuel, y = day_total_energy,
                                               fill = in.heating_fuel)) +
               geom_bar(stat = "identity") +
               labs(title = "Total Energy Consumption by Heating Fuel",
                    x = "Heating Fuel",
                    y = "Total Energy Consumption (kWh)") +
               scale_y_continuous(labels = scales::comma) +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)


#URL for Shiny App
#https://greeshmashinyapp.shinyapps.io/shiny/
