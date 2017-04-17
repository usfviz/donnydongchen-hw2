rm(list = ls())
cat("\014") 
########################################################################
# load required libraries (install if libraries not found)
if ("shiny" %in% rownames(installed.packages())==F) {
  install.packages("shiny", repos="http://cran.us.r-project.org")}
library(shiny)
if ("ggplot2" %in% rownames(installed.packages())==F) {
  install.packages("ggplot2", repos="http://cran.us.r-project.org")}
library(ggplot2)
if ("ggvis" %in% rownames(installed.packages())==F) {
  install.packages("ggvis", repos="http://cran.us.r-project.org")}
library(ggvis)
if ("dplyr" %in% rownames(installed.packages())==F) {
  install.packages("dplyr", repos="http://cran.us.r-project.org")}
library(dplyr)

# set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

########################################################################
# Data Preprocessing
df_le <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", skip = 4, header = T)[-109,1:59]
df_fr <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", skip = 4, header = T)[-109,1:59]
df_p <- read.csv("population.csv", na.strings = c(NA, "", " "), header = T)[-109,1:59]
df_meta <- read.csv("Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", header = T)[,1:2]
df_le <- subset(df_le,select=-c(Indicator.Name, Indicator.Code))
df_fr <- subset(df_fr,select=-c(Indicator.Name, Indicator.Code))
df_p <- subset(df_p,select=-c(Indicator.Name, Indicator.Code))

colnames(df_le) <- c("Country.Name","Country.Code", c(1960:2014))
colnames(df_fr) <- c("Country.Name","Country.Code", c(1960:2014))
colnames(df_p) <- c("Country.Name","Country.Code", c(1960:2014))

df_meta_le <- merge(df_meta, df_le, by = "Country.Code")
df_meta_fr <- merge(df_meta, df_fr, by = "Country.Code")
df_meta_p <- merge(df_meta, df_p, by = "Country.Code")

region <- df_meta_p[,"Region"]
assign("region", region, envir = .GlobalEnv)
country <- df_meta_p[,"Country.Name"]
assign("country", region, envir = .GlobalEnv)

unique_region <- unique(c(as.character(df_meta_p$Region)))[-5]

########################################################################
# Shiny UI
ui <- fluidPage(
  headerPanel('Life Expectancy and Fertility Rate'),
  fluidRow(
    column(12, mainPanel(uiOutput("ggvis_ui"), ggvisOutput("ggvis")),
    column(10, sliderInput("year", "Select Year", 1960, 2014, 1, animate = TRUE))),
    column(5, checkboxGroupInput("region", "Select Regions", choices = unique_region))
)
)

# Shiny Server
server <- function(input, output) {
  
  show_country <- function(x){
    if(is.null(x)) return(NULL)
    paste0 ( x$Country.Name )
  }
    
  region_select <- reactive(is.null(input$region))
    
  data <- reactive({
    if (is.null(input$region)){
    Fertility.Rate <- df_meta_fr[, as.character(input$year)]
    Life.Expectancy <- df_meta_le[, as.character(input$year)]
    Population <- df_meta_p[, as.character(input$year)]
    data <- data.frame(cbind(Fertility.Rate, Life.Expectancy, Population))
    data$Country.Name <- df_meta_fr$Country.Name
    data}
    else{
    # input <- data.frame(region="North America", year=2014)  
      Fertility.Rate <- df_meta_fr[df_meta_fr[,"Region"]%in%input$region, as.character(input$year)]
      Life.Expectancy <- df_meta_le[df_meta_le[,"Region"]%in%input$region, as.character(input$year)]
      Population <- df_meta_p[df_meta_p[,"Region"]%in%input$region, as.character(input$year)]
    data <- data.frame(cbind(Fertility.Rate, Life.Expectancy, Population))
    data$Country.Name <- df_meta_fr[df_meta_fr[,"Region"]%in%input$region,]$Country.Name
    data
    }}
    )
    
    data %>% 
      ggvis(~Life.Expectancy, ~Fertility.Rate, size = ~Population, 
            # fill = ~region, 
            key := ~Country.Name) %>%
      layer_points() %>%
      add_tooltip(show_country, "hover") %>%
      bind_shiny("ggvis", "ggvis_ui")
}

shinyApp(ui = ui, server = server)

# shiny::runGitHub(repo = "donnydongchen/-viz", username = "donnydongchen", subdir = "viz_hw2_donny_chen.R")