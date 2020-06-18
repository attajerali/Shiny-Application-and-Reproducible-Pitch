#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(HistData)
data(GaltonFamilies)
library(ggplot2) 
library(dplyr)
# Define server logic required to our application
# 1st step: pass inches to cm (1 inches=2.54 cm)
gf <- GaltonFamilies
gf <- gf %>% mutate(father=father*2.54,
                    mother=mother*2.54,
                    childHeight=childHeight*2.54)
# Our linear model
model1 <- lm(childHeight ~ father + mother + gender, data=gf)
shinyServer(function(input, output) {
    output$pText <- renderText({
        paste("Father's height is",
              strong(round(input$inFh, 1)),
              "cm, and mother's height is",
              strong(round(input$inMh, 1)),
              "cm, then:")
    })
    output$pred <- renderText({
        df <- data.frame(father=input$inFh,
                         mother=input$inMh,
                         gender=factor(input$inGen, levels=levels(gf$gender)))
        ch <- predict(model1, newdata=df)
        kid <- ifelse(
            input$inGen=="female",
            "Daugther",
            "Son"
        )
        paste0(em(strong(kid)),
               "'s predicted height is going to be around ",
               em(strong(round(ch))),
               " cm"
        )
    })
    output$Plot <- renderPlot({
        kid <- ifelse(
            input$inGen=="female",
            "Daugther",
            "Son"
        )
        df <- data.frame(father=input$inFh,
                         mother=input$inMh,
                         gender=factor(input$inGen, levels=levels(gf$gender)))
        ch <- predict(model1, newdata=df)
        xvals <- c("Father", kid, "Mother")
        df <- data.frame(
            x = factor(xvals, levels = xvals, ordered = TRUE),
            y = c(input$inFh, ch, input$inMh))
        ggplot(df, aes(x=x, y=y, color=c("red", "green", "blue"), fill=c("red", "green", "blue"))) +
            geom_bar(stat="identity", width=0.5) +
            xlab("") +
            ylab("Height (cm)") +
            theme_minimal() +
            theme(legend.position="none")
    })
})