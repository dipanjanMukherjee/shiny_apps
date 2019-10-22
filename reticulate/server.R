#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reticulate)
#use_virtualenv('C:/Users/DipanjanMukhopadhyay/Documents/poc/poc', required = TRUE)
sns = import("seaborn", delay_load = TRUE)
plt = import('matplotlib.pyplot', delay_load = TRUE)

source_python("C:/Users/DipanjanMukhopadhyay/Documents/poc/poc/poc.py")
#use_python("C:/Program Files (x86)Python/Python37/")
py_discover_config()



#plt <- import('matplotlib.pyplot')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderImage({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
        draw_hist(x, bins)

        #plt.show(histplt)
        #sns$plt.hist(df["sepal_length"], normed=True, bins=30)
        #df = sns$load_dataset('iris')
        #sns$distplot(df["sepal_length"])
        #plt$show(x)
        list(src = 'myplot.png')
    })

})
