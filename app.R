library(shiny)

##############
ui <- fluidPage(
titlePanel("Central Limit Theorem"), 
sidebarLayout(
    sidebarPanel(
        sliderInput(inputId = "num", label = "Sample Size", value = 3, min = 1, max = 70),
        radioButtons(inputId = "dist", label="Distribution type:",
                 choices = list("Normal" = 1, "Uniform" = 2, "Log-normal" = 3, "Exponential" = 4))
        ),
    mainPanel(
        plotOutput(outputId = "pl"))
        )
)


###############
server <- function(input, output) {
    output$pl = renderPlot({
        n = input$num
        xbar = rep(0,500)
        set.seed(0)
        for (i in 1:500)
        {
            if (input$dist == 1) {xbar[i] = mean(rnorm(n));} #generating norm values
            if (input$dist == 2) {xbar[i] = mean(runif(n));} #generating norm values
            if (input$dist == 3) {xbar[i] = mean(rlnorm(n));} #generating norm values
            if (input$dist == 4) {xbar[i] = mean(rexp(n));} #generating exp values
        }
        hist(xbar,probability = T,main = "Histogram and Density")
        lines(density(xbar),col="red")
    })
}

###############
shinyApp(ui = ui, server = server)
