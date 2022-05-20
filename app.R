library(shiny)
library(tidyverse)
library(ggrepel)

ROAST_DIR <- 'data/roasts/'
roast_files <- rev(str_remove_all(list.files(ROAST_DIR), '.csv'))
coffees <- read_csv('data/coffees.csv') %>% arrange(origin, name)
roasts <- read_csv(paste0(ROAST_DIR, roast_files, '.csv')) %>%
    filter(time==0) %>%
    select(name, date) %>%
    rename(id=name) %>%
    left_join(coffees) %>%
    mutate(date=roast_files,
           roast_name=paste0(name, ' (', date, ')'))

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
                     selectInput('origin', 'Filter By Origin',
                                 c('Select a coffee'='', unique(coffees$origin)),
                                 multiple=TRUE),
                     selectInput('coffee', 'Filter By Coffee',
                                 c('Select a coffee'='', coffees$name),
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
        files <- roasts
        if (length(input$origin) > 0)
            files <- files %>% filter(origin %in% input$origin)
        if (length(input$coffee) > 0)
            files <- files %>% filter(name %in% input$coffee)
        selectInput('roast', 'Roast', files$roast_name, selected=files$roast_name[1])
    })
    output$plot <- renderPlot({
        roast <- roasts %>% filter(roast_name == input$roast)
        if (!file.exists(paste0(ROAST_DIR, roast$date, '.csv')))
            return()
        
        df <- read_csv(paste0(ROAST_DIR, roast$date, '.csv')) %>%
            mutate(RoR=c(0, diff(temp)))
        tempRange <- input$tempRangeC
        
        ## Convert to F
        if (input$unit == 'Fahrenheit') {
            df <- df %>% mutate(temp=toF(temp),
                                RoR=c(0, diff(temp)/diff(time)))
            tempRange <- input$tempRangeF
        }

        if (!(df$name[1] %in% coffees$id)) {
            print(paste0('Coffee: ', df$name[1], ' not found.'))
            return()
        }
        df.coffee <- coffees %>% filter(id == df$name[1])
        
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
        roasts %>%
            filter(roast_name == input$roast) %>%
            select(-id, -date, -roast_name) %>%
            mutate(url=paste0('<a href="', url, '" target="_blank">', url, '</a>')) %>%
            return()
    }, sanitize.text.function = function(x) x)
}

shinyApp(ui = ui, server = server)
