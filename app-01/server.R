library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    
    # Get subset of wells according to cutoff criteria
    x <- subset(well, subset = c(prod_date >= min(input$tsteps) &
                                 prod_date <= max(input$tsteps) &
                                 h_td_md > input$depth &
                                 (h_well_type == "OW" |
                                  h_well_type == "GW" |
                                  h_well_type == "D")))
    
    # Sum well counts by field
    y <- sqldf("select distinct(w_field_num), count(p_api)
               from x
               group by w_field_num")
    
    # Change column names
    names(y) <- c("field", "count")
    
    # Drop any field that has less than field.cutoff fraction of the wells in
    # the Basin.
    wcount <- y[which(y$count/sum(y$count) >= input$field.cutoff/100),]
    
    # Add row for Field 999
    wcount <- rbind(wcount, c(999, sum(y$count)-sum(wcount$count)))
    
    barplot(height = wcount$count,
            names.arg = as.character(wcount$field),
            ylim = c(0, 3.5e3),
            ylab = "Well Count",
            xlab = "Field Number",
            main = "Well Counts by Field")
  })
})