library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggrepel)

COFFEE_DIR <- 'data/coffees/'
ROAST_DIR <- 'data/roasts/'
coffee_files <- str_remove_all(list.files(COFFEE_DIR), '.csv')
roast_files <- str_remove_all(list.files(ROAST_DIR), '.csv')

## Cache which coffee was roasted at each date
roast_coffees <- rep('', length(roast_files))
for (i in 1:length(roast_files))
    roast_coffees[i] <- read.csv(paste0(ROAST_DIR, roast_files[i], '.csv'))$name[1]

## Linearly map from [min1, max1] to [min2, max2]
mapInterval <- function(x, min1, max1, min2, max2) {
    return((x-min1) * (max2-min2) / (max1-min1) + min2)
}

## Convert from C to F
toF <- function(degree.celcius) {
    return(degree.celcius * 9/5 + 32)
}


ui <- fluidPage(
    sidebarLayout(
        mainPanel(plotOutput('plot'),
                  tableOutput('info')),
        sidebarPanel(uiOutput('roast'),
                     selectInput('coffee', 'Filter By Coffee', 
                                 c('Select a coffee'='', coffee_files),
                                 multiple=TRUE),
                     radioButtons('unit', 'Unit', choices=c('Celcius', 'Fahrenheit')),
                     conditionalPanel('input.unit == "Celcius"',
                                      sliderInput('tempRangeC', 'Temperature',
                                                  -250, 250, c(0, 225))),
                     conditionalPanel('input.unit != "Celcius"',
                                      sliderInput('tempRangeF', 'Temperature',
                                                  toF(-250), toF(250),
                                                  toF(c(0, 225)))),
                     sliderInput('heatRange', 'Heat',
                                 0, 12, c(0, 12)))))


server <- function(input, output) {
    output$roast <- renderUI({
        files <- roast_files
        if (length(input$coffee) > 0)
            files <- roast_files[roast_coffees %in% input$coffee]
        
        selectInput('roast', 'Roast', files)
    })
    output$plot <- renderPlot({
        if (!file.exists(paste0(ROAST_DIR, input$roast, '.csv')))
            return()
        
        df <- read.csv(paste0(ROAST_DIR, input$roast, '.csv'),
                       stringsAsFactors=FALSE) %>%
            mutate(RoR=c(0, diff(temp)))
        tempRange <- input$tempRangeC

        ## Convert to F
        if (input$unit == 'Fahrenheit') {
            df <- df %>% mutate(temp=toF(temp),
                                RoR=c(0, diff(temp)))
            tempRange <- input$tempRangeF
        }
        
        df.coffee <- read.csv(paste0(COFFEE_DIR, df$name[1], '.csv'))
        
        ggplot(df) +
            aes(x=time) +
            geom_line(aes(y=temp, color='Temperature'), size=2) +
            geom_line(aes(y=mapInterval(heat, input$heatRange[1], input$heatRange[2],
                                        tempRange[1], tempRange[2]),
                          color='Gas'), size=2) +
            geom_line(aes(y=RoR, color='Rate of Rise'), size=2) +
            geom_label_repel(aes(y=temp, label=notes), size=3.5, direction='x') +
            scale_y_continuous(name=paste0('Temperature (', input$unit, ')'),
                               sec.axis=sec_axis(trans=function(x)
                                   mapInterval(x, tempRange[1], tempRange[2],
                                               input$heatRange[1], input$heatRange[2]),
                                   breaks=seq(input$heatRange[1], input$heatRange[2]),
                                   name='Heat')) +
            coord_cartesian(ylim=tempRange) +
            ggtitle(paste0(df.coffee$name[1], ', ', df$date[1])) +
            scale_color_discrete(name='Color',
                                 breaks=c('Temperature', 'Gas', 'Rate of Rise')) +
            xlab('Time') + theme_bw(base_size=18) +
            theme(legend.position='bottom')
    })
    
    output$info <- renderTable({
        if (!file.exists(paste0(ROAST_DIR, input$roast, '.csv')))
            return(data.frame())
        
        df <- read.csv(paste0(ROAST_DIR, input$roast, '.csv'),
                       stringsAsFactors=FALSE)
        if (!file.exists(paste0(COFFEE_DIR, df$name[1], '.csv')))
            return(data.frame())
        
        return(read.csv(paste0(COFFEE_DIR, df$name[1], '.csv')))
        
    })
}

shinyApp(ui = ui, server = server)
