
shinyServer(function(input, output) {

  dataframeCreation <- reactive({
    if(is.null(input$file1))
      return(NULL)
    data <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    data
  })
  
  output$col_select <- renderUI({
    if(!is.null(input$file1)){
      mydata <- dataframeCreation()
      selectInput('columns', 'Select text columns for topics', choices = names(mydata), multiple = TRUE)
    }
  })
  
  datasetInput <- reactive({
    df <- dataframeCreation()
    df <- df[, input$columns]
    df
  })
  
  dtmCreation <- reactive({
    df <- datasetInput()
    corpus <- tm::Corpus(tm::VectorSource(df))
    corpus <- tm::tm_map(corpus, removePunctuation)
    corpus <- tm::tm_map(corpus, stripWhitespace)
    corpus <- tm::tm_map(corpus, removeNumbers)
    
    skipStopWords <-function(x) removeWords(x, stopwords("english"))
    corpus <- tm::tm_map(corpus, skipStopWords)
    
    dtm <- tm::DocumentTermMatrix(corpus)
    dtm <- tm::removeSparseTerms(dtm, sparse = 0.999)
    
    dtm
    
  })
  
  dtm_to_matrix <- reactive({
    dtm <- dtmCreation()
    dtm_m <- as.matrix(dtm)
    dtm_m
  })
  
  find_rowTotals <- function(dtm){
    rowTotals <- apply(dtm, 1, sum)
  }
  
  dtm_without_zero <- reactive({
    dtm_m <- dtmCreation()
    rowTotals <- find_rowTotals(dtm_m)
    dtm_new <- dtm_m[rowTotals > 0,]
    rownames(dtm_new) <- 1:nrow(dtm_new)
    dtm_new
  })
  
  df_without_zero <- function(){
    dtm <- dtm_without_zero
    df <- datasetInput()
    rowTotals <- find_rowTotals(dtm)
    non_relevant <- as.vector(which(rowTotals == 0))
    df_new <- filter(df, rownames(df) %in% non_relevant)
    
  }
  
  performScalingWeighting <- reactive({
    dtm <- dtm_without_zero()
    dtm_tfIdf <- tm::weightTfIdf(t(dtm), normalize = FALSE)
    dtm_tfIdf_m <- as.matrix(dtm_tfIdf)
    
    rownames(dtm_tfIdf_m) <- 1:nrow(dtm_tfIdf_m)
    
    dtm_scale <- scale(dtm_tfIdf_m)
    dtm_scale
  })
  
  performLSA <- reactive({
    set.seed(123)
    dtm_scale <- performScalingWeighting()
    
    LSASPace <- lsa::lsa(t(dtm_scale), dims = dimcalc_raw())
    print(dim(LSASPace$tk))
    print(dim(LSASPace$dk))
    print(length(LSASPace$sk))
    LSASPace
  })
  
  performClustering <- reactive({
    set.seed(123)
    lsaSpace <- performLSA()
    dtm_m <- dtm_without_zero()
    dtm_scale <- performScalingWeighting()
    
    nclus <- length(lsaSpace$sk)
    
    print(nclus)
    
    clusters <- stats::kmeans(t(dtm_scale), centers = 10, nstart = 18)
    
    clusters
  })
  
  addCLusteringColumn <- reactive({
    tryCatch({
      orig <- dataframeCreation()
      
      clustered_data <- performClustering()
      
      end <- cbind(orig, cluster = clustered_data$cluster)
      
      #autolabel <- get_autolabels()
      
      #end <- cbind(end, topic = autolabel)
      
      end
    },
    error = function(cond){
      message(cond)
    })
  })

  output$cluster_plot <- renderPlotly({
    withProgress(message = "plot creation in progres", detail = "this may take a while...", value = 0, 
                 {
                   for(i in 1:10){
                     incProgress(1/10)
                   }
                   
                   data <- addCLusteringColumn()
                   txt_columns <- colnames(data)
                   
                   print(input$columns)
                   sel_columns <- input$columns
                   
                   print(txt_columns)
                   
                   data <- plyr::count(data, txt_columns)
                   
                   p2 <- ggplot2::ggplot(data, aes(x = reorder(cluster, -freq), y = freq, fill = text_summary)) +
                     geom_bar(stat = "identity")
                   
                   plotly::ggplotly(p2, height = 800)
                 }
                 )
  })
  
  addTopicColumn <- reactive({
    tryCatch({
      end <- addCLusteringColumn()
      autolabel <- get_autolabels()
      
      df_with_topic <- cbind(end, topic = autolabel)
      
      df_with_topic
    },
    error = function(cond){
      message(cond)
    })
  })
  
  output$corpus <- downloadHandler(
    filename = function(){
      paste(input$columns, ".csv", sep = "-")
    },
    content = function(file){
      write.csv(dtm_to_matrix(), file, row.names = FALSE)
    }
  )
  
  output$clustered_data <- downloadHandler(
    filename = function(){
      paste(input$columns, "-clustered.csv", sep = "-")
    },
    content = function(file){
      write.csv(addCLusteringColumn(), file, row.names = FALSE)
    }
  )
  
  output$topic_data <- downloadHandler(
    filename = function(){
      paste(input$columns, "-topics.csv", sep = "-")
    },
    content = function(file){
      write.csv(addTopicColumn(), file, row.names = FALSE)
    }
  )
  
  output$wordcloud <- renderPlot({
    if(!is.null(input$columns)){
      withProgress(message = "plot creation in progress", detail = "this may take a while...", value = 0,
                   {
                     for(i in 1:10){
                       incProgress(1/10)
                     }
                     
                     set.seed(1234)
                     d <- wordfreq()
                     
                     word <- wordcloud::wordcloud(words = d$word, freq = d$freq, min.freq = 10, max.words = 200,
                                                  random.order = FALSE, rot.per = 0.35,
                                                  colors = RColorBrewer::brewer.pal(8, "Dark2"))
                     
                     word
                   })
    }
  })
  
  output$termFreqPlot <- plotly::renderPlotly({
    if(!is.null(input$columns)){
      withProgress(message = "plot creation in progress", detail = "this may take a while...", value = 0,
                   {
                     for(i in 1:10){
                       incProgress(1/10)
                     }
                     
                     d <- wordfreq()
                     
                     p <- ggplot2::ggplot(subset(d, freq > 1), aes(x = reorder(word, freq), y = freq))
                     p <- p + geom_bar(stat = "identity", fill = "purple", col = "purple")
                     p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
                     p <- p + coord_flip()
                     p <- p + ggtitle("term frequency plot") + xlab("term") + ylab("frequency")
                     p
                     plotly::ggplotly(p)
                     
                     
                   })
    }
  })
  
  wordfreq <- reactive({
    df <- datasetInput()
    
    corpus <- tm::Corpus(tm::VectorSource(df))
    corpus <- tm::tm_map(corpus, removePunctuation)
    corpus <- tm::tm_map(corpus, stripWhitespace)
    corpus <- tm::tm_map(corpus, removeNumbers)
    
    skipStopWords <-function(x) removeWords(x, stopwords("english"))
    corpus <- tm::tm_map(corpus, skipStopWords)
    
    tdm <- tm::TermDocumentMatrix(corpus)
    tdm <- tm::removeSparseTerms(tdm, sparse = 0.999)
    
    m <- as.matrix(tdm)
    
    v<- sort(rowSums(m), decreasing = TRUE)
    
    d <- data.frame(word = names(v), freq = v, stringsAsFactors = FALSE)
    
    d
  })
  
  correlation <- reactive({
    dtm <- dtmCreation()
    freqTerms <- tm::findFreqTerms(dtm, lowfreq = 1)
    c <- as.vector(freqTerms)
    
    assocs <- tm::findAssocs(dtm, c, 0.9)
    assocs <- as.matrix(assocs)
    
    assocs_v <- c("")
    assocs_v_read <- c("")
    
    for(i in 1:length(assocs)){
      #for(j in 1:length(assocs[[i]])){
        assocs_v_read <- c(assocs_v, names(assocs[[i]]))
      #}
    }
    
    assocs_v_read <- assocs_v_read[!is.na(assocs_v)]
    allterms <- as.vector(dtm$dimname$Terms)
    corTerms <- as.vector(assocs_v_read)
    retainterms <- unique(c(corTerms, freqTerms))
    
    dtm_hm <- dtm[,(dtm$dimnames$Terms) %in% retainterms]
    dtm_m <- as.matrix(dtm_hm)
    
    cor_hm <- cor(dtm_m)
    cor_hm
    
  })
  
  output$heatmap <- renderPlot({
    withProgress(message = "plot creation in progress", detail = "this may take a while...", value = 0,
                 {
                   for(i in 1:10){
                     incProgress(1/10)
                   }
                   
                   cor_hm <- correlation()
                   heat <- gplots::heatmap.2(cor_hm,
                                             dendrogram = 'none',
                                             na.rm = TRUE,
                                             trace = 'none',
                                             col = grDevices::colorRampPalette(c("green","yellow"))(256), density.info = 'none')
                 })
  })
  
  get_autolabels <- reactive({
    finalList <- vector()
    dtm <- as.matrix(dtm_without_zero())
    for(i in 1:nrow(dtm)){
      df_label <- data.frame(unlist(dtm[i, order(dtm[i,], decreasing = TRUE)]))
      
      if(any(df_label[1,1] > 0)){
        if(any(df_label[2,1] > 0)){
          if(any(df_label[3,1] > 0)){
            val <- paste(rownames(df_label)[1:3], collapse = "-")
          }
          else{
            val <- paste(rownames(df_label)[1:2], collapse = "-")
          }
        }
        else{
          val <- paste(rownames(df_label)[1], collapse = "-")
        }}
        finalList[i] <- val
      }
      a1 <- finalList
      a1[1:length(a1)]
    
  })
  
  output$topics <- renderPlotly({
    withProgress(message ="plot creation in progress", detail = "this may take a while...", value = 0,
                 {
                   for(i in 1:10){
                     incProgress(1/10)
                   }
                   
                   data <- addTopicColumn()
                   
                   data <- plyr::count(data, "topic")
                   
                   a <- list(showticklabels = FALSE)
                   
                   b <- list(range = c(300,0))
                   
                   p <- plotly::plot_ly(data, x =~ topic, y =~ freq, type = "scatter", mode = "markers",
                                        marker = list(size =~ freq, opacity = 0.5, color = ~ freq,
                                                      colors = "Red")) %>% layout(xaxis = a, yaxis = b) %>%
                     layout(autosize = F, width = 1700, height = 800)
                 })
  })
  
  
  
})
