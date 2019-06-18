
shinyUI(fluidPage(

    # Application title
    titlePanel("Infrastructure Modeling"),

    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "choose csv file", multiple = FALSE, accept = c(".csv")),
            uiOutput("cluster_x"),
            uiOutput("cluster_y"),
            uiOutput("cluster_z"),
            uiOutput("cluster_select")
        ),

       
        mainPanel(
            tabsetPanel(
            tabPanel("Data", tableOutput("data")),
            tabPanel("correlation", plotOutput("correlation", height = "800px")),
            tabPanel("Find Optimum CLusters", plotOutput("findOptimum")),
            tabPanel("PCA", plotOutput("pca", height = "800px")),
            tabPanel("Visualize Clusters", plotlyOutput("clusters", width = "1200px", height = "800px")),
            tabPanel("Final Correlation", plotOutput("finalcorr", width = "1200px", height = "800px"))
        )
        )
    )
))
