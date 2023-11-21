options(java.parameters = "-Xmx8000m")
options(shiny.maxRequestSize=30*1024^2)#30MB


######################################################
#packages
######################################################
library(shiny)
library(shinyFiles)
library(shinyWidgets)
library(chron)
library(openxlsx2)
library(timeDate)
library(stringr)
library(dplyr)
library(lubridate)
#library(rgdal)
library(sf)
library(shinybusy)
library(readr)


source("./source.R", local=TRUE)
######################################################

######################################################
# Define User Interface
######################################################
fluidPage(
  titlePanel(title=div(h1("Preparation du tableur vigie-chiro pour analyse_integration donnees Hobo"),h6("Office National des Forets / V. Parmain / Mars 2020/mise à jour Novembre 2023")),
             windowTitle = "Prepa .csv"),

  add_busy_bar(color = "red", height = "8px", timeout = 1000),
    
  sidebarLayout(
    sidebarPanel(
      fileInput("filecsv", h6("Choisir le fichier .csv a transformer:"), multiple = FALSE, accept = ".csv",
                buttonLabel = "Parcourir...", placeholder = "Pas de fichier"),
      actionButton("ValidTab", label = "Valider"),
      
      fileInput("filetxt", h6("Choisir le fichier .txt issu du Hobo:"), multiple = FALSE, accept = ".txt",
                buttonLabel = "Parcourir...", placeholder = "Pas de fichier"),
      actionButton("ValidHobo", label = "Valider"),
      
      #radioButtons("radio", label = h6("Systeme de coordonnees"), choices = list("RGF93" = 1, "WGS84" = 2),selected = 1),
      selectInput("coordsys", label = h6("Choisir le systeme de coordonnees"),
                  choices = list("RGF93", "WGS84"),selected = "RGF93"),
      
      numericInput("X", label = h6("X"), value = 1),
      numericInput("Y", label = h6("Y"), value = 1),
      
      actionButton("convert", label = "Convertir"),
      
      textInput("op", h6("Observateur"), value = "Enter text..."),
      
      #shinyDirButton('directory', 'Folder select', 'Please select a folder'),
      textInput("Chemin", h6("Chemin"), value = "Enter text..."),
      
      actionButton("action", label = "Valider"),
      
      actionButton("transform", label = "Transformation"),
      
      downloadButton('download',"telecharger")
      
      #downloadLink("downloadData", "Download"),
      
    ),
    
    
    mainPanel(
      tableOutput("contents1"),
      tableOutput("contents11"),
      verbatimTextOutput("contents2"),
      tableOutput("contents3"),
      tableOutput("contents33"),
      verbatimTextOutput("contents4"),
      verbatimTextOutput("contents5"),
      tableOutput("contents6"),
      tableOutput("contents7"),
      tableOutput("Contacts8")
      #shiny::verbatimTextOutput("directorypath"),
      #shinyDirButton("dir", "Input directory", "Upload"),
      #verbatimTextOutput("dir", placeholder = TRUE)  # added a placeholder
    )
  )
)




