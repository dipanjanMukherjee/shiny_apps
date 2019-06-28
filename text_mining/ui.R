
shinyUI(dashboardPage(
    skin = 'purple',
    dashboardHeader(title = "Text Mining Toolkit"),
    dashboardSidebar(sidebarMenu(
        menuItem("Data", tabName = "data", icon = icon("dashboard")),
        menuItem("Visualization", tabName = "visualization", icon = icon("th")),
        menuItem("Clustered Documents", tabName = "cluster", icon = icon("file-alt")),
        menuItem("Topics", tabName = "topics", icon = icon("comments"))
    )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "data",
                    fluidRow(
                        box(fileInput("file1", "choose ccsv file", multiple = FALSE, accept = c(".csv"))),
                        box(uiOutput("col_select"),
                            downloadButton("corpus", "Download Corpus"))
                    )),
            
            tabItem(tabName = "visualization", 
                    fluidRow(
                        box(plotlyOutput("termFreqPlot")),
                        box(plotOutput("wordcloud")),
                        box(plotOutput("heatmap"))
                    )),
            tabItem(tabName = "cluster",
                    fluidRow(
                        box(plotlyOutput("cluster_plot"), height = 800),
                        box(downloadButton("clustered_data", "Download Clustered Documents"))
                    )),
            tabItem(tabName = "topics",
                    fluidRow(
                        box(plotlyOutput("topics"), width = 800, height = 800),
                        box(downloadButton("topic_data", "Download Topics"))
                    ))
        )
    )

))
