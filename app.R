#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Mortality  Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tableOutput("table")
    ),
    # Show a plot of the generated distribution
    mainPanel(h3("Draw boxes around points to see the raw data."),
              plotOutput("scatterPlot", brush = "plot_brush")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(WHO)
  library(tidyverse)
  library(data.table)
  
  # codes <- get_codes()
  # hw_codes <- codes[grep('hrh', codes$label, ignore.case=T),]
  # glimpse(codes)
  child_codes <- c('imr', 'nmr', 'u5mr')
  for (i in child_codes){
    #gets the data from WHO
    assign(paste0(i, '_data'), as_tibble(data.table(get_data(i))[datasource == 'DHS',]))
    x <- paste0(i, '_data')
    # setnames(get(eval(x)), paste(i,colnames(get(eval(x))), sep="_"))
    assign(eval(x), get(eval(x)) %>%
             separate('value', into = c('mort_mean', 'stdev'), sep = " ", fill = "right", convert = T))
    if(!exists("rawData")){
      rawData <- as_tibble(data.table(get(eval(x))))
    }
    else rawData <- rbind(rawData, get(eval(x)))
  }
  #gets healthcare worker data
  hw <- data.table(get_data("HRH_29"))
  names(hw) <- paste0( "hw_", names(hw))
  #joins the data down to a dense matrix
  rawData <- left_join(hw, rawData, by = c("hw_country"="country", "hw_year"="year"))
  group.colors <- c("Infant mortality rate (deaths per 1000 live births)"="#333BFF", "Neonatal mortality rate (deaths per 1000 live births)"="#CC6600", "Under-five mortality rate (deaths per 1000 live births)"="#9633FF")
  final_data <- rawData[,c("gho","hw_country", "hw_value", "mort_mean")]
  final_data <- filter(final_data, !is.na(mort_mean)) %>%
    filter(mort_mean != 'No') %>%
    as.tibble() %>%
    group_by(hw_country, gho) %>%
    arrange(hw_country)
  final_data$mort_mean <- as.numeric(final_data$mort_mean)
  final_data <- final_data %>% summarise(mort_mean = mean(mort_mean), hw_mean = mean(hw_value))
  final_data <- left_join(final_data, group.colors)
  
  # log.model <-lm(log(imr_mean) ~ hw_mean, final_data)
  # log.model.df <- data.frame(x = final_data$hw_mean,
  #                            y = exp(fitted(log.model)))
  imr <- final_data[grep("Infant", final_data$gho), "mort_mean"][['mort_mean']]
  nmr <- final_data[grep("Neonatal", final_data$gho), "mort_mean"][['mort_mean']]
  u5mr <- final_data[grep("Under", final_data$gho), "mort_mean"][['mort_mean']]
  hw <- unique(final_data$hw_mean)
  
  
  imr.log.model <- lm(log(imr) ~ hw, final_data)
  imr.log.model.df <- data.frame(x = final_data[grep("Infant", final_data$gho), "hw_mean"][["hw_mean"]],
                                 y = exp(fitted(imr.log.model)), z = "#333BFF")

  nmr.log.model <-lm(log(nmr) ~ hw, final_data)
  nmr.log.model.df <- data.frame(x = final_data[grep("Infant", final_data$gho), "hw_mean"][["hw_mean"]],
                                 y = exp(fitted(nmr.log.model)), z = "#CC6600")
  u5mr.log.model <-lm(log(u5mr) ~ hw, final_data)
  u5mr.log.model.df <- data.frame(x = final_data[grep("Infant", final_data$gho), "hw_mean"][["hw_mean"]],
                                  y = exp(fitted(u5mr.log.model)), z = "#9633FF")
  
  output$scatterPlot <- renderPlot({
    # draw the scatter plot
    sp <- ggplot(final_data, mapping = aes(x=hw_mean, y=mort_mean, color=color.group))
    sp +
      geom_point() +
      labs(x="Health Workers per 1000", y="mortality per 1000 live births") +
      geom_smooth(data = imr.log.model.df, aes(x, y, color=z), size = 1, linetype = 1) +
      geom_smooth(data = nmr.log.model.df, aes(x, y, color=z), size = 1, linetype = 1) +
      geom_smooth(data = u5mr.log.model.df, aes(x, y, color=z), size = 1, linetype = 1)
    # + scale_colour_manual(values=group.colors)
  })
  
  output$table <- renderTable({brushedPoints(final_data, input$plot_brush, xvar = "hw_mean", yvar = "mort_mean")})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

