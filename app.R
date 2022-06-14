# soiltestcorr demo ----

# LIBRARIES ----

# Shiny
library(shiny)
library(shinydashboard)
library(bslib)

# Widgets
library(plotly)

# Core
library(tidyverse)

# SoilTestCorr
library(soiltestcorr)


# LOAD DATASETS ----
data_1 <- soiltestcorr::data_test %>% rename(x=STV, y=RY)
data_2 <- soiltestcorr::freitas1966 %>% rename(x=STK, y=RY)

data_list = list("Test dataset" = data_1,
                 "Freitas et al. 1966" = data_2)

method_list = list("mod_alcc", "cate_nelson_1965", "cate_nelson_1971",
                   "linear_plateau", "quadratic_plateau", "mitscherlich")

# 1.0 USER INTERFACE ----
ui <- navbarPage(

    title = "soiltestcorr-shinyapp",

    theme = bslib::bs_theme(version = 4, bootswatch = "yeti"),

    ## Main tab----
    tabPanel(
        title = "Main",

        sidebarLayout(
      ### Side Panel ----
            sidebarPanel(
                width = 3.5,
                h2("Select Options"),
                # Example datasets
                p("Use the example datasets:"),
                shiny::selectInput(
                    inputId = "dataset_choice",
                    label   = "Choose a dataset",
                    choices = c("Test dataset", "Freitas et al. 1966")
                ),

                # Load user data
                p("Or you can upload your own data:"),
                shiny::fileInput("file1", "Load your data (*.csv). \n 
                                 x = soil test, y = yield",
                                           accept = c(
                                               "text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")    ),
                tags$hr(),

                # Method choice
                shiny::selectInput(
                    inputId = "method_choice",
                    label   = "Choose a correlation method",
                    choices = c("mod_alcc", "cate_nelson_1965", "cate_nelson_1971",
                                   "linear_plateau", "quadratic_plateau", "mitscherlich")
                ),
                # Target RY
                sliderInput("target", "Target Yield (%)", value = 90, min = 70, max = 99),
                # Text input
                shiny::textInput("xtitle", "x_title", "Soil test value (units)"),
                shiny::textInput("ytitle", "y_title", "Relative yield (%)"),

                tags$hr()
                

                            ),
      ### Main Panel ----
            mainPanel(
                h1("Soil Test Correlation Plot"),
                #plotlyOutput("corrplot", height = 700, width = 1200) # plotly
                plotOutput("corrplot", height = 600, width = 900) # ggplot

            )
        )

    ),

## Data Tab ----

tabPanel(
    title = "Data",
    sidebarLayout(
        sidebarPanel(
            width = 3.5,
            h2("Dataset"),
            # Load user data
            ),
        mainPanel(
            tableOutput("data_table")
        )
    )
),

## Results ----

tabPanel(
    title = "Results",
    sidebarLayout(
        sidebarPanel(
            width = 3.5,
            h2("Results"),
            # Load user data
        ),
        mainPanel(
            h1("Summary table"),
            tableOutput("results_summary"),
            h2("Abbreviations"),
            tags$hr(),
            p("mod_alcc() function:"),
            p("mod_alcc: Modified Arcsine-log Calibration Curve"), 
            p("n: sample size"), 
            p("r: Pearson's Correlation Coefficient"), 
            p("target: relative yield to estimate the CSTV"), 
            p("CSTV: critical soil test value"), 
            p("LL: lower limit of CSTV"), 
            p("UL: lower limit of CSTV"), 
            p("confidence: 1-alpha, to estimate confidence interval"), 
            p("CSTV90: critical soil test value for 90% or relative yield"), 
            p("CSTV100: critical soil test value for 100% or relative yield"), 
            p("n.90x2: number of observations with soil test values above two-times the CSTV90. They may represent a risk of leverage"), 
            p("n.100: number of observations with soil test values above the CSTV100. They may represent a risk of leverage"), 
            tags$hr(),
            p("cate_nelson_1965() function:"), 
            p("CRYV: target to estimate the CSTV"), 
            p("quadrants.qI/II/III/IV: number of observations on each quadrant"), 
            p("quadrants.positive: number of well-classified observations"), 
            p("quadrants.negative: number of observations that don't match yield expectations based on their soil test values"), 
            p("R2: coefficient of determination of the classification model"), 
            tags$hr(),
            p("cate_nelson_1971() function:"), 
            p("CRYV: lowest soil test value that minimizes the residual sum of squares of the classification model"),
            tags$hr(),
            p("linear_plateau() & quadratic_plateau functions:"), 
            p("intercept: relative yield when soil test value (or x-variable) = 0"), 
            p("slope: linear change rate of relative yield per unit of soil test values (or x-variable)"), 
            p("LL_cstv: lower limit of CSTV at the plateau level"), 
            p("UL_cstv: upper limit of CSTV at the plateau level"),
            p("STVt: Soil test value at the target level"),
            p("AIC: Aikaike Information Criteria score"),
            p("AICc: corrected-Aikaike Information Criteria score. Adjusted by the number of parameters"),
            tags$hr(),
            p("mitscherlich() function:"), 
            p("a: asymptote of the response function"),
            p("b: x-intercept of the response function"),
            p("c: rate or curvature paramater"),
        )
    )
),

## Code ----

tabPanel(
   title = "Code",
   sidebarLayout(
      # Side Panel
      sidebarPanel(
         width = 3.5,
         #h2("soiltestcorr"),
         h2("Installation"),
         p("You can install soiltestcorr from CRAN:"),
         code('install.packages("soiltestcorr")'),
         br(),
         br(),
         p("Or the development version from GitHub:"),
         code('install_github("adriancorrendo/soiltestcorr")'),
         tags$hr(),
         p("Explore more about soiltestcorr functions at"),
         p("Online documentation") %>%
            a(
               href = 'https://adriancorrendo.github.io/soiltestcorr/',
               target = "_blank",
               class = "btn btn-lg btn-primary"
            ) %>%
            div(class = "text-center")
            ),
      # Main panel
      mainPanel(
         h1("Code"),
         p("You can grab the following code to your R session and run it by yourself!"),
         p("Don't forget to visit the ",
           a("Vignettes", href = "https://adriancorrendo.github.io/soiltestcorr/articles/Introduction_to_soiltestcorr.html"),
         " for more details."),
         tags$hr(),
         code("# Load the package"), br(),
         code('library(soiltestcorr)'), br(), br(),
         code("# Load your data"), br(),
         code('my_data <- read.csv(file = "your_filename.csv", header = TRUE)'), br(),
         tags$hr(),
         code("# Define target and confidence"), br(),
         code("target <- 90  # as relative yield percentage (%), may be used in all functions except cate_nelson_1971()"), br(),
         code("confidence <- 0.95 # needed for mod_alcc only"), br(),
         tags$hr(),
         code("# Modified Arcsine-log Calibration Curve"), br(),
         code("# Fit mod_alcc()"), br(),
         code("fit_modalcc <- mod_alcc(data = my_data, ry = y, stv = x, target = target, confidence = 0.95, tidy = TRUE)"), br(),
         code("fit_modalcc"),
         tags$hr(),
         code("# Cate & Nelson 1965, Graphical version"), br(),
         code("# Fit cate_nelson_1965()"), br(),
         code("fit_cate_nelson_1965 <- cate_nelson_1965(data = my_data, ry = y, stv = x, target = target, tidy = TRUE)"), br(),
         code("fit_cate_nelson_1965"),
         tags$hr(),
         code("# Cate & Nelson 1971, Statistical version"), br(),
         code("# Fit cate_nelson_1971()"), br(),
         code("fit_cate_nelson_1971 <- cate_nelson_1971(data = my_data, ry = y, stv = x, tidy = TRUE)"), br(),
         code("fit_cate_nelson_1971"),
         tags$hr(),
         code("# Linear-plateau response"), br(),
         code("# Fit linear_plateau()"), br(),
         code("fit_linear_plateau <- linear_plateau(data = my_data, ry = y, stv = x, tidy = TRUE)"), br(),
         code("fit_linear_plateau"),
         tags$hr(),
         code("# Quadratic-plateau response"), br(),
         code("# Fit quadratic_plateau()"), br(),
         code("fit_quadratic_plateau <- quadratic_plateau(data = my_data, ry = y, stv = x, tidy = TRUE)"), br(),
         code("fit_quadratic_plateau"),
         tags$hr(),
         code("# Mistcherlich response"), br(),
         code("# Fit mitscherlich"), br(),
         code("fit_mitscherlich <- mitscherlich(data = my_data, ry = y, stv = x, type = 1, tidy = TRUE)"), br(),
         code("fit_mitscherlich"),
         tags$hr(),
      )
   )
),

## About tab ----

tabPanel(
    title = "About",
    titlePanel("About soiltestcorr"),
    sidebarLayout(
        # Side Panel
        sidebarPanel(
           width = 1.2,
           img(src = "soiltestcorr_logo.png", height = 120, width = 100),
        ),
        # Main Panel
        mainPanel(
            h1("Description"),
            p("Soil test correlation is foundational to the Fertilizer 
              Recommendation Support Tool",
              a("(FRST)",
                href = "http://www.soiltestfrst.org/"), 
              "being developed for the US. In this context, the soiltestcorr-package 
              offers a collection of functions intended to contribute to reproducible 
              correlation analysis between crop relative yield and soil test values. 
              This web-application tool was specifically designed to facilitate the use of the 
              soiltestcorr functionalities for users with no background in R-programming. 
              Both developments stemmed from ongoing work with the ",
              a("FRST",
                href = "http://www.soiltestfrst.org/"),
              " and Feed the Future Innovation Lab for Collaborative Research on Sustainable
              Intensification (",
              a("SIIL",
                href = "https://digitalconsortium.wixsite.com/dgfsc"),
              ") projects."),
            tags$hr(),
            h2("Citation"),
            p("Correndo A, Pearce A, Osmond D, Ciampitti I (2022). soiltestcorr: Soil Test Correlation and Calibration. R package version 2.1.2.",
              a("https://cloud.r-project.org/web/packages/soiltestcorr/",
                href = "https://cloud.r-project.org/web/packages/soiltestcorr/")),
            h3("Development"),
            p("This application was designed by Adrian Correndo (C) (2022) using the shiny R-package."),
            p("Chang et al. (2021). _shiny: Web  Application Framework for R_. R package version 1.7.1,",
              a("<https://CRAN.R-project.org/package=shiny>", 
                href = "https://CRAN.R-project.org/package=shiny"))
            
            
        )
    )
)

)




# 2.0 SERVER ----

server <- function(input, output) {
   
   # Call Reactive vars ----
   rv <- reactiveValues()
   
   observe({
      rv$method <- method_list %>% pluck(input$method_choice)
      
   })
   
   observe({
      
      inFile <- input$file1
      
      if (is.null(inFile)){
         rv$data_set <- data_list %>% pluck(input$dataset_choice )
      }
      
      else {rv$data_set <- read.csv(inFile$datapath, #header = input$header
      )}
      
   })

    # Data Table ----
    output$data_table <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1

        if (is.null(inFile))
            return(NULL)


        read.csv(inFile$datapath, #header = input$header
                 ) %>% 
           rename(x=STV, y=RY) %>%
            mutate(obs = seq(1,nrow(.),by=1), .before = 1,
                   obs = as.integer(obs)) 
    })

    # Correlation plot ----
    output$corrplot <-


        #renderPlotly({
        renderPlot({

        if (input$method_choice == "mod_alcc") {
            plot <-
            soiltestcorr::mod_alcc(data = rv$data_set,
                                   ry = y, stv = x,
                                   target = input$target, confidence = 0.95,
                                   plot = TRUE)+
               theme(axis.text = element_text(size = rel(1.25)),
                     axis.title = element_text(size = rel(1.75)))+
               labs(x=paste(input$xtitle), y = paste(input$ytitle)) }

        if (input$method_choice == "cate_nelson_1965") {
            plot <-
            soiltestcorr::cate_nelson_1965(data = rv$data_set,
                                           ry = y, stv = x,
                                           target = input$target,
                                           plot = TRUE)+
               theme(axis.text = element_text(size = rel(1.25)),
                     axis.title = element_text(size = rel(1.75)))+
                labs(x=paste(input$xtitle), y = paste(input$ytitle))     }

        if (input$method_choice == "cate_nelson_1971") {
            plot <-
            soiltestcorr::cate_nelson_1971(data = rv$data_set,
                                           ry = y, stv = x,
                                           plot = TRUE)+
               theme(axis.text = element_text(size = rel(1.25)),
                     axis.title = element_text(size = rel(1.75)))+
                labs(x=paste(input$xtitle), y = paste(input$ytitle))     }

        if (input$method_choice == "linear_plateau") {
            plot <-
            soiltestcorr::linear_plateau(data = rv$data_set,
                                         ry = y, stv = x,
                                         target = input$target,
                                         plot = TRUE) +
               theme(axis.text = element_text(size = rel(1.25)),
                     axis.title = element_text(size = rel(1.75)))+
                labs(x=paste(input$xtitle), y = paste(input$ytitle))    }

        if (input$method_choice == "quadratic_plateau") {
            plot <-
            soiltestcorr::quadratic_plateau(data = rv$data_set,
                                         ry = y, stv = x,
                                         target = input$target,
                                         plot = TRUE)+
               theme(axis.text = element_text(size = rel(1.25)),
                     axis.title = element_text(size = rel(1.75)))+
                labs(x=paste(input$xtitle), y = paste(input$ytitle))     }

        if (input$method_choice == "mitscherlich") {
            plot <-
            soiltestcorr::mitscherlich(data = rv$data_set,
                                         ry = y, stv = x,
                                         type = 1, target = input$target,
                                         plot = TRUE)+
               theme(axis.text = element_text(size = rel(1.25)),
                     axis.title = element_text(size = rel(1.75)))+
                labs(x=paste(input$xtitle), y = paste(input$ytitle))      }
        # Plotly output
        #plotly::ggplotly(plot)
        # Or ggplot
        plot


    })

    # Results summary ----
    output$results_summary <-

       renderTable({

            if (input$method_choice == "mod_alcc") {
                results_tidy <-
                    soiltestcorr::mod_alcc(data = rv$data_set,
                                           ry = y, stv = x,
                                           target = input$target, confidence = 0.95,
                                           tidy = TRUE) %>%
                    dplyr::select(-SMA,-Curve) %>%
                    mutate(.before = 1, Method = paste(input$method_choice))  }

            if (input$method_choice == "cate_nelson_1965") {
                results_tidy <-
                    soiltestcorr::cate_nelson_1965(data = rv$data_set,
                                                   ry = y, stv = x,
                                                   target = input$target,
                                                   tidy = TRUE) %>%
                    mutate(.before = 1, Method = paste(input$method_choice))     }

            if (input$method_choice == "cate_nelson_1971") {
                results_tidy <-
                    soiltestcorr::cate_nelson_1971(data = rv$data_set,
                                                   ry = y, stv = x,
                                                   tidy = TRUE) %>%
                    mutate(.before = 1, Method = paste(input$method_choice))     }

            if (input$method_choice == "linear_plateau") {
                results_tidy <-
                    soiltestcorr::linear_plateau(data = rv$data_set,
                                                 ry = y, stv = x,
                                                 target = input$target,
                                                 tidy = TRUE)  %>%
                    mutate(.before = 1, Method = paste(input$method_choice))    }

            if (input$method_choice == "quadratic_plateau") {
                results_tidy <-
                    soiltestcorr::quadratic_plateau(data = rv$data_set,
                                                    ry = y, stv = x,
                                                    target = input$target,
                                                    tidy = TRUE) %>%
                    #mutate(.after = UL_ctstv, CI_type = "Wald Conf. Interval") %>%
                    mutate(.before = 1, Method = paste(input$method_choice))    }

            if (input$method_choice == "mitscherlich") {
                results_tidy <-
                    soiltestcorr::mitscherlich(data = rv$data_set,
                                               ry = y, stv = x,
                                               type = 1, target = input$target,
                                               tidy = TRUE) %>%
                    mutate(.before = 1, Method = paste(input$method_choice))  }

            results_tidy

         })

}




# Run the application
shinyApp(ui = ui, server = server)
