# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in the RStudio IDE.

library(shiny)

# Define the user interface
ui <- fluidPage(
    withMathJax(),

    titlePanel("Coin tosses and Brownian motion"),

    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "n",
                         label = "Sample size n:",
                         value = 25),
            numericInput(inputId = "horizon",
                         label = "Zoom factor:",
                         value = 1),
            numericInput(inputId = "trajectories", 
                         label = "Number of trajectories:",
                         value = 1),
            actionButton(inputId = "go",
                         label = "Update")
        ),

        mainPanel(
           plotOutput("trajectories"),
           uiOutput("formula")
        )
    )
)

# Define server logic
server <- function(input, output) {
    invoer <- eventReactive(input$go, {
        list(n = input$n,
             horizon = input$horizon,
             trajectories = input$trajectories)
    })


    output$trajectories <- renderPlot({
        data <- invoer()
        if (data$trajectories < 2) {
            X <- sample(x = c(-1, 1), 
                        size = data$n * data$horizon, 
                        replace = TRUE)
            S <- c(0, cumsum(X))
            Bn <- S / sqrt(data$n)
            plot(x = (0:(data$n*data$horizon))/data$n, y = Bn, type = "l",
                 xlab = "time t",
                 ylab = "trajectory",
                 cex.lab = 1.3)
        } else {
            X <- matrix(sample(x = c(-1, 1), 
                               size = data$n * data$horizon * data$trajectories, 
                               replace = TRUE),
                        ncol = data$trajectories)
            S <- rbind(numeric(data$trajectories), apply(X = X, MARGIN = 2, FUN = cumsum))
            Bn <- S / sqrt(data$n)
            matplot(x = (0:(data$n*data$horizon))/data$n, y = Bn, type = "l", lty = 1,
                 xlab = "time t",
                 ylab = "trajectory",
                 cex.lab = 1.3)
        }
        abline(h = 0, lty = "dashed")
    })
    
    output$formula <- renderUI({
        withMathJax("Independent and identically distributed random variables \\(Y_1, Y_2, \\ldots\\) such that
        $$
            P(Y_i = +1) = P(Y_i = -1) = \\frac{1}{2}
        $$
        Partial sums:
        $$
            S_n = Y_1 + \\cdots + Y_n
        $$
        Trajectory of approximation to Brownian motion:
        $$
            B_{n,t} = \\frac{S_{nt}}{\\sqrt{n}}
        $$")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)