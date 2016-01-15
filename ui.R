require(rCharts)
shinyUI(
  navbarPage(
    "CPT Visualization",
    tabPanel(
      "Data and Settings",
      fluidRow(
        column(
          9,
          textOutput(outputId="table_tittle"),
          tableOutput(outputId="csvfile"),
          textOutput(outputId="table_tittle2"),
          tableOutput(outputId="csvfile_summary"),
          textOutput(outputId="method_chosen"),
          textOutput(outputId="method_chosen2")
        ),
        column(
          3,
          wellPanel(
            fileInput(
              'file1', "Choose .csv File",
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')
            ),
            tags$hr(),
            checkboxInput(inputId='header',label='Header',value=TRUE),
            checkboxInput(inputId='example',label='Use example data',value=F)
          ),
          wellPanel(
            selectInput(
              inputId="p_val_methods", 
              label="Methods for Hypothesis Testing",
              choices=c("Dynamic","NBExact","Ttest",
                        "NBLRT", "PoisLRT", "BinLRT",
                        "PoisExact","FishersExact"),
              selected = "NBLRT"
            ),
            sliderInput(
              "alpha.penalty", 
              label="Penalized Regression Parameter",
              min=0, max=1, value=0, step = 0.1
            ),
            tags$small(
              "0 = Ridge Regression,  1 = Lasso"
            ),
            tags$br(),
            tags$small("(0, 1) = Mixture of L1 and L2 penalty")
          )
        )
      )
    ),
    tabPanel(
      "Rate Ratio Plot",
      showOutput(outputId="myChart", lib="dimple") # to see lib: d1$lib
        ),
    tabPanel(
      "Manhattan Plot",
#       showOutput(outputId="manhattanplot", lib="dimple")
       plotOutput("manhattanplot")
    ),
    navbarMenu(
      "More",
      tabPanel(
        "About",
        fluidRow(
          column(
            6,
            tags$small("to add")
          ),
          column(
            3,
            tags$small(
              "To add ",
              "To add  ",
              "To add"
            )
          )
        )
      )
    )
  )
)