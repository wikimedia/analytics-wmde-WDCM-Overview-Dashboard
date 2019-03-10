### ---------------------------------------------------------------------------
### --- WDCM Overview Dashboard, v. Beta 0.1
### --- Script: server.R, v. Beta 0.1
### ---------------------------------------------------------------------------

### --- Setup
### --- general
library(shiny)
library(RMySQL)
library(XML)
library(data.table)
library(DT)
library(stringr)
library(tidyr)
library(dplyr)
library(reshape2)
### --- compute
library(parallelDist)
library(smacof)
### --- visualization
library(wordcloud)
library(RColorBrewer)
library(visNetwork)
library(rbokeh)
library(networkD3)
library(ggplot2)
library(ggrepel)
library(scales)
library(ggvis)

### --- Server (Session) Scope
### --------------------------------

### --- functions
get_WDCM_table <- function(url_dir, filename, row_names) {
  read.csv(paste0(url_dir, filename), 
           header = T, 
           stringsAsFactors = F,
           check.names = F)
}

# - projectType() to determine project type
projectType <- function(projectName) {
  unname(sapply(projectName, function(x) {
    if (grepl("commons", x, fixed = T)) {"Commons"
    } else if (grepl("mediawiki|meta|species|wikidata", x)) {"Other"
    } else if (grepl("wiki$", x)) {"Wikipedia"
    } else if (grepl("quote$", x)) {"Wikiquote"
    } else if (grepl("voyage$", x)) {"Wikivoyage"
    } else if (grepl("news$", x)) {"Wikinews"
    } else if (grepl("source$", x)) {"Wikisource"
    } else if (grepl("wiktionary$", x)) {"Wiktionary"
    } else if (grepl("versity$", x)) {"Wikiversity"
    } else if (grepl("books$", x)) {"Wikibooks"
    } else {"Other"}
  }))
}

### --- Config File
params <- xmlParse('wdcmConfig_wdcmOverviewDashboard.xml')
params <- xmlToList(params)

### --- shinyServer
shinyServer(function(input, output, session) {
  
  ### --- DATA
  
  withProgress(message = 'Downloading data', detail = "Please be patient.", value = 0, {
  
    ### --- fetch wdcm2_project
    wdcmProject <- get_WDCM_table(params$etl_dir, 'wdcm_project.csv')
    wdcmProject$projectype <- projectType(wdcmProject$eu_project)
    incProgress(1/6, detail = "Please be patient.")
    # - determine how many project types are present
    # - and assign Brewer colors
    lengthProjectColor <- length(unique(wdcmProject$projectype))
    projectTypeColor <- brewer.pal(lengthProjectColor, "Set1")
    
    ### --- fetch wdcm2_project_category_2dmap
    wdcm2_project_category_2dmap <-
      get_WDCM_table(params$ml_dir,
                     'wdcm_project_category_2dmap.csv',
                     row_names = T)
    wdcm2_project_category_2dmap[, 1] <- NULL
    colnames(wdcm2_project_category_2dmap)[3] <- "eu_project"
    wdcm2_project_category_2dmap <- left_join(wdcm2_project_category_2dmap,
                                              wdcmProject,
                                              by = "eu_project")
    labelSet <- unlist(lapply(unique(wdcm2_project_category_2dmap$projecttype), function(x){
    w <- which(wdcm2_project_category_2dmap$projecttype %in% x)
    lS <- arrange(wdcm2_project_category_2dmap[w, ], desc(eu_count))[1:5, ]
    lS$eu_project
    }))
    labelSetSmall1 <- unlist(lapply(unique(wdcm2_project_category_2dmap$projecttype), function(x){
      w <- which(wdcm2_project_category_2dmap$projecttype %in% x)
      lS <- arrange(wdcm2_project_category_2dmap[w, ], desc(eu_count))[1, ]

      lS$eu_project
      }))
    labelSetSmall3 <- unlist(lapply(unique(wdcm2_project_category_2dmap$projecttype), function(x){
      w <- which(wdcm2_project_category_2dmap$projecttype %in% x)
      lS <- arrange(wdcm2_project_category_2dmap[w, ], desc(eu_count))[1:3, ]
      lS$eu_project
      }))
    wdcm2_project_category_2dmapReduceLabels <- wdcm2_project_category_2dmap
    wdcm2_project_category_2dmapReduceLabels$eu_project[which(!(wdcm2_project_category_2dmapReduceLabels$eu_project %in% labelSet))] <- ""
    colnames(wdcm2_project_category_2dmap)[c(3, 5,6)] <- c('Project', 'Usage', 'Project Type')
    wdcm2_project_category_2dmap$projectTypeColor <- sapply(wdcm2_project_category_2dmap$`Project Type`, function(x) {
      projectTypeColor[which(sort(unique(wdcm2_project_category_2dmap$`Project Type`)) %in% x)]
      })
  
  ### --- fetch wdcm2_category
  wdcmCategory <- get_WDCM_table(params$etl, 'wdcm_category.csv')
  incProgress(5/6, detail = "Please be patient.") 
  
  ### ---fetch wdcm2_category_project_2dmap
  wdcm2_category_project_2dmap <- 
    get_WDCM_table(params$ml_dir, 
                   'wdcm2_category_project_2dmap.csv', 
                   row_names = T)
  colnames(wdcm2_category_project_2dmap)[1] <- 'id'
  wdcm2_category_project_2dmap[, 1] <- NULL
  wdcm2_category_project_2dmap <- left_join(wdcm2_category_project_2dmap,
                                            wdcmCategory,
                                            by = "category")
  colnames(wdcm2_category_project_2dmap)[3:4] <- c('Category', 'Usage')
  
  ### --- fetch wdcm2_project_category
  wdcmProjectCategory <- get_WDCM_table(params$etl_dir, 'wdcm_project_category.csv', row_names = F)
  wdcmProjectCategory$type <- projectType(wdcmProjectCategory$eu_project)
  colnames(wdcmProjectCategory) <- c('Project', 'Category', 'Usage', 'Project Type')
  
  ### --- Fetch update info
  update <- read.csv(params$updatePath, 
                     header = T,
                     check.names = F,
                     stringsAsFactors = F,
                     row.names = 1)
  
  })
  
  ### --- OUTPUTS
  
  
  ### --- output: updateString
  output$updateString <- renderText({
    date <- update[max(which(grepl("Orchestra END", update$Step))), ]$Time
    date <- paste0(date, " UTC")
    return(paste('<p style="font-size:80%;"align="right"><b>Last update: </b><i>', date, '</i></p>', sep = ""))
  })
  
  ### ----------------------------------
  ### --- TAB: Overview
  ### ----------------------------------
  
  ### --- output$overviewPlot
  output$overviewPlot <- renderPlot({
    ggplot(wdcm2_project_category_2dmapReduceLabels, aes(x = D1, y = D2,
                                                         color = projecttype,
                                                         label = eu_project)) +
      geom_point(aes(size = eu_count), shape = 21) +
      scale_size(name = "Usage", 
                 breaks = waiver(), 
                 labels = comma,
                 limits = NULL, 
                 range = c(.5, 30), 
                 trans = "identity", 
                 guide = "legend") + 
      scale_colour_manual(values = projectTypeColor, name = "Project Type") +
      geom_text_repel(size = 5, fontface = 'bold', segment.size = .25, show.legend = FALSE) +
      theme_bw() +
      theme(axis.text.x = element_blank()) +
      theme(axis.text.y = element_blank()) +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(axis.ticks.x = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(panel.background = element_rect(color = "white", fill = "white")) +
      theme(panel.border = element_blank()) +
      theme(panel.grid = element_blank()) + 
      theme(legend.text = element_text(size = 14)) +
      theme(legend.title = element_text(size = 15))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$overviewPlotDynamic
  output$overviewPlotDynamic <- renderRbokeh({
    outFig <- figure(width = 1400, height = 900, logo = NULL) %>%
      ly_points(D1, D2, 
                data = wdcm2_project_category_2dmap,
                size = log(Usage),
                color = 'Project Type', 
                hover = list(Project, Usage)) %>% 
      x_axis(visible = F) %>% 
      y_axis(visible = F) %>% 
      theme_grid(which = c("x", "y"), 
                 grid_line_color = "white") %>% 
      theme_plot(outline_line_alpha = 0) %>% 
      set_palette(discrete_color = pal_color(projectTypeColor))
    outFig
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$usageTendencyPlot
  output$usageTendencyPlot <- renderPlot({
    ggplot(wdcm2_category_project_2dmap, aes(x = D1, 
                                             y = D2, 
                                             label = Category)) +
      scale_color_discrete(guide = FALSE) +
      geom_point(aes(size = Usage), fill = "cadetblue1", 
                 color = "cadetblue4", shape = 21) +
      scale_size(name = "Usage", 
                 breaks = waiver(), 
                 labels = comma,
                 limits = NULL, 
                 range = c(2, 20), 
                 trans = "identity", 
                 guide = "legend") + 
      theme_bw() +
      geom_text_repel(size = 4, show.legend = FALSE) +
      theme(axis.text.x = element_blank()) +
      theme(axis.text.y = element_blank()) +
      theme(axis.title.x = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(axis.ticks.x = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(panel.border = element_blank()) +
      theme(panel.grid = element_blank()) + 
      theme(legend.text = element_text(size = 10)) +
      theme(legend.title = element_text(size = 12))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectRankFrequencyPlot
  output$projectRankFrequencyPlot <- renderPlot({
    rank <- order(wdcmProject$eu_count)
    frequency <- wdcmProject$eu_count[rank]
    project <- wdcmProject$eu_project[rank]
    dataSet <- data.frame(Rank = rank,
                          Frequency = frequency, 
                          Project = project,
                          stringsAsFactors = F)
    dataSet$Project[which(!(dataSet$Project %in% labelSetSmall1))] <- ""
    dataSet <- dataSet[order(-dataSet$Frequency), ]
    ggplot(dataSet, aes(x = Rank, 
                        y = Frequency,
                        label = Project)) +
      geom_path(size = .25, color = "darkblue") + 
      geom_point(size = 1, color = "darkblue") + 
      geom_point(size = .65, color = "white") + 
      scale_y_continuous(labels = comma) +
      xlab("Project Usage Rank") + ylab("Project Usage") + 
      geom_text_repel(size = 3, segment.size = .15, show.legend = FALSE) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 9)) +
      theme(axis.text.y = element_text(size = 9)) +
      theme(axis.title.x = element_text(size = 12)) +
      theme(axis.title.y = element_text(size = 12)) +
      theme(axis.ticks.x = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(panel.border = element_blank()) +
      theme(panel.grid = element_blank()) + 
      theme(legend.text = element_text(size = 10)) +
      theme(legend.title = element_text(size = 12))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectLogRankLogFrequencyPlot
  output$projectLogRankLogFrequencyPlot <- renderPlot({
    rank <- order(wdcmProject$eu_count)
    frequency <- wdcmProject$eu_count[rank]
    project <- wdcmProject$eu_project[rank]
    dataSet <- data.frame(Rank = log(rank),
                          Frequency = log(frequency), 
                          Project = project,
                          stringsAsFactors = F)
    dataSet$Project[which(!(dataSet$Project %in% labelSetSmall3))] <- ""
    dataSet <- dataSet[order(-dataSet$Frequency), ]
    ggplot(dataSet, aes(x = Rank, 
                        y = Frequency,
                        label = Project)) +
      geom_path(size = .25, color = "red") + 
      geom_smooth(size = .25, method = "lm", color = "red") + 
      geom_point(size = 1, color = "red") + 
      geom_point(size = .65, color = "white") + 
      scale_y_continuous(labels = comma) +
      xlab("log(Project Usage Rank)") + ylab("log(Project Usage)") + 
      geom_text_repel(size = 3, segment.size = .15, show.legend = FALSE) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 9)) +
      theme(axis.text.y = element_text(size = 9)) +
      theme(axis.title.x = element_text(size = 12)) +
      theme(axis.title.y = element_text(size = 12)) +
      theme(axis.ticks.x = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(panel.border = element_blank()) +
      theme(panel.grid = element_blank()) + 
      theme(legend.text = element_text(size = 10)) +
      theme(legend.title = element_text(size = 12))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectCategoryCross
  output$projectCategoryCross <- renderPlot({
    dataSet <- wdcmProjectCategory
    dataSet$Project <- NULL
    dataSet <- dataSet %>% 
      group_by(`Project Type`, Category) %>%
      summarise(Usage = sum(Usage)) %>% 
      as.data.frame()
    dataSet$`Project Type` <- factor(dataSet$`Project Type`, 
                                     levels = unique(dataSet$`Project Type`))
    dataSet$Category <- factor(dataSet$Category, 
                               levels = unique(dataSet$Category))
    ggplot(dataSet, aes(x = Category, 
                        y = Usage,
                        fill = `Project Type`,
                        color = `Project Type`)) +
      geom_bar(stat = "identity", width = .35) +
      scale_fill_manual(values = projectTypeColor, name = "Project Type") +
      scale_color_manual(values = projectTypeColor) +
      scale_y_continuous(labels = comma) +
      xlab("Category") + ylab("Usage") + 
      facet_grid(`Project Type` ~ ., scales = "free_y") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 11, hjust = 1)) +
      theme(axis.text.y = element_text(size = 9)) +
      theme(axis.title.x = element_text(size = 12)) +
      theme(axis.title.y = element_text(size = 12)) +
      theme(axis.ticks.x = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(panel.background = element_rect(fill = "white")) +
      theme(panel.border = element_blank()) +
      theme(panel.grid = element_blank()) + 
      theme(legend.position = "none") +
      theme(strip.background = element_blank()) +
      theme(strip.text = element_text(size = 11))
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectVolume
  output$projectVolume <- renderPlot({
    minQ <- input$volumeSlider[1]/100
    maxQ <- input$volumeSlider[2]/100
    wSel <- which(wdcmProject$eu_count <= quantile(wdcmProject$eu_count, maxQ) & 
                    wdcmProject$eu_count >= quantile(wdcmProject$eu_count, minQ))
    dataSet <- wdcmProject[wSel, ] %>% 
      arrange(desc(eu_count)) %>% as.data.frame()
    colnames(dataSet) <- c('Project', 'Usage', 'Project Type')
    if (dim(dataSet)[1] > 30) {
      dataSet <- dataSet[1:30, ]
    }
    dataSet$Project <- factor(dataSet$Project)
    dataSet <- dataSet[order(dataSet$Usage), ]
    ggplot(dataSet, aes(x = reorder(Project, Usage), 
                        y = Usage,
                        fill = `Project Type`)) +
      geom_bar(stat = "identity", width = .2) + 
      xlab("Project") + ylab("Usage") + 
      scale_fill_manual(values = projectTypeColor[which(sort(unique(wdcmProject$projectype)) %in% dataSet$`Project Type`)], 
                        name = "Project Type") +
      scale_y_continuous(labels = comma) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 11, hjust = 1)) +
      theme(axis.text.y = element_text(size = 9)) +
      theme(axis.title.x = element_text(size = 12)) +
      theme(axis.title.y = element_text(size = 12)) +
      theme(axis.ticks.x = element_blank()) +
      theme(axis.ticks.y = element_blank()) +
      theme(panel.border = element_blank()) +
      theme(panel.grid = element_blank())
  }) %>% withProgress(message = 'Generating plot',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectCategoryDataTable
  output$projectCategoryDataTable <- DT::renderDataTable({
    datatable(wdcmProjectCategory,
    options = list(
      pageLength = 20,
      width = '100%',
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
    rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectDataTable
  output$projectDataTable <- DT::renderDataTable({
    dataSet <- wdcmProject
    colnames(dataSet) <- c('Project', 'Usage', 'Project Type')
    datatable(dataSet,
              options = list(
                pageLength = 20,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  })

### --- END shinyServer




