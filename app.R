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
    withMathJax(),

    # Application title
    titlePanel("Donsker's theorem"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "n",
                         label = "Sample size n:",
                         value = 25),
            numericInput(inputId = "horizon",
                         label = "Horizon:",
                         value = 1),
            numericInput(inputId = "trajectories", 
                         label = "Number of trajectories:",
                         value = 1),
            actionButton(inputId = "go",
                         label = "Update")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("trajectories"),
           uiOutput("formula")
        )
    )
)

# Define server logic required to draw a histogram
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
                 ylab = expression(B[n](t) == S[nt]/sqrt(n)))
        } else {
            X <- matrix(sample(x = c(-1, 1), 
                               size = data$n * data$horizon * data$trajectories, 
                               replace = TRUE),
                        ncol = data$trajectories)
            S <- rbind(numeric(data$trajectories), apply(X = X, MARGIN = 2, FUN = cumsum))
            Bn <- S / sqrt(data$n)
            matplot(x = (0:(data$n*data$horizon))/data$n, y = Bn, type = "l", lty = 1,
                 xlab = "time t",
                 ylab = expression(B[n](t) == S[nt]/sqrt(n)))
        }
        title("Approximated Brownian motion trajectory", cex.main = 2)
        abline(h = 0, lty = "dashed")
    })
    
    output$formula <- renderUI({
        withMathJax("Independent and identically distributed random variables \\(X_1, X_2, \\ldots\\) such that
        $$
            P(X_i = 1) = P(X_i = -1) = \\frac{1}{2}
        $$
        Partial sums:
        $$
            S_n = X_1 + \\cdots + X_n
        $$
        Trajectory of approximation to Brownian motion:
        $$
            B_n(t) = \\frac{S_{[nt]}}{\\sqrt{n}}
        $$")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
