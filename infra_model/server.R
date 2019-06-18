
shinyServer(function(input, output) {

    dataframe <- reactive({
        if(is.null(input$file1)){
            return(NULL)
        }
        data <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
        data
    })
    
    output$column_select <- renderUI({
        if(!is.null(input$file1)){
            mydata <- dataframe()
            mydata$hostname <- NULL
            shiny::selectInput('columns', 'select columns for clustering', choices = names(mydata), multiple = TRUE)
        }
    })

    output$cluster_x <- renderUI({
        if(!is.null(input$file1)){
            mydata <- dataframe()
            mydata$hostname <- NULL
            shiny::selectInput('x', 'sekect x axis for xluster plot', choices = names(mydata), selected = "disk_usage")
        }
    })
    
    output$cluster_y <- renderUI({
        if(!is.null(input$file1)){
            mydata <- dataframe()
            mydata$hostname <- NULL
            shiny::selectInput('y', 'sekect y axis for xluster plot', choices = names(mydata), selected = "input_output_rate")
        }
    })
    
    output$cluster_z <- renderUI({
        if(!is.null(input$file1)){
            mydata <- dataframe()
            mydata$hostname <- NULL
            shiny::selectInput('z', 'sekect z axis for xluster plot', choices = names(mydata), selected = "gpu_memory")
        }
    })
    
    output$cluster_select <- renderUI({
        if(!is.null(input$file1)){
            shiny::numericInput("centroids", "number of centroids", 2, min = 1, max = 10)
        }
    })
    
    performkmeans <- reactive({
        df <- dataframe()
        set.seed(123)
        df$page_rate <- NULL
        df$cpu_usage <- NULL
        
        k2 <- stats::kmeans(df[,1:ncol(df)-1], centers = input$centroids, nstart = 25)
        end <- cbind(df, cluster = k2$cluster)
        end
    })
    
    output$data <- renderTable({
        dataframe()
    })
    
    output$correlation <- renderPlot({
        df <- dataframe()
        cols <- colnames(df)
        graphics::plot(df[,1:ncol(df) - 1], main = "simple scatterplot matrix")
    })
    
    output$summary <- renderText({
        km <- performkmeans()
        km$centers
    })
    
    output$clusters <- renderPlotly({
        df <- performkmeans()
        colors <- c()
        
        if(input$centroids == 2){
            df$cluster[which(df$input_output_rate <= 64 )] <- 'Low I/O'
            df$cluster[which(df$input_output_rate > 64 )] <- 'High I/O'
        }
        else if(input$centroids == 2){
            df$cluster[which(df$gpu_memory > 50 & df$input_output_rate <= 64 )] <- 'Low I/O - High gpU'
            df$cluster[which(df$gpu_memory <= 50 & df$input_output_rate <= 64 )] <- 'Low I/O - low gpu'
            df$cluster[which(df$gpu_memory > 50 & df$input_output_rate > 64 )] <- 'high I/O - high gpu'
            df$cluster[which(df$gpu_memory <= 50 & df$input_output_rate > 64 )] <- 'high I/O - low gpu'
        }
        
        withProgress(message = 'Making plot', detail = 'this may take a while...', value = 0,{
            for(i in 1:15){
                incProgress(1/15)
            }
            
            xAXisInputColumn <- df[[input$x]]
            yAXisInputColumn <- df[[input$y]]
            zAXisInputColumn <- df[[input$z]]
            
            p <- plotly::plot_ly(df, x = xAXisInputColumn, y = yAXisInputColumn, z = zAXisInputColumn,
                                 text = ~paste(input$x, ":", xAXisInputColumn, '<br>', input$y, ":", yAXisInputColumn, '<br>', input$z, ":", zAXisInputColumn),
                                 color = ~cluster, hoverinfo = 'text', symbol = ~hostname) %>% 
                                layout(scene = list(xaxis = list(title = input$x), yaxis = list(title = input$y), zaxis = list(title = input$z)))
            
            p
        })
    })
    
    output$pca <- renderPlot({
        df <- dataframe()
        df$hostname <- NULL
        autoplot(stats::prcomp(df), data = df, loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE)
    })
    
    output$findOptimum <- renderPlot({
        df <- dataframe()
        df$hostname <- NULL
        set.seed(123)
        
        #function to compute total within-cluster sum of square
        wss <- function(k){
            stats::kmeans(df, k, nstart = 10)$tot.withinss
        }
        
        #compute and plot wss for k=1 to k=15
        k.values <- 1:15
        
        #extract wss for 2-15 clusters
        wss_values <- purrr::map_dbl(k.values, wss)
        
        graphics::plot(k.values, wss_values, type = "b", pch = 19, frame = FALSE, xlab = "number of clusters k",
                       ylab = "total within clusters sum of squares")
        
        
    })
    
    output$finalcorr <- renderPlot({
        df <- performkmeans()
        df$page_rate <- NULL
        df$cpu_usage <- NULL
        df$hostname <- NULL
        graphics::plot(x = df[,1:ncol(df) - 1], col = c("red","blue","green","yellow")[df$cluster],
                       main = "simple scatterplot matrix after dimensionality reduction and clustering")
    })
})
