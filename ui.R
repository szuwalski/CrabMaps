#layers for density by species
#layer for bottom temperature
#layer for bathymetry
#layer for SST
#make densities based on length (double ended slider for what range of lengths to plot)
# relate transparency to transparency
#overlay the length frequency distribution at the top as another option

shinyUI(fluidPage(
  titlePanel("Eastern Bering Sea crab"),
 
sidebarLayout(
    sidebarPanel(
#      checkboxGroupInput("sex", 
#        label = h3("Sex"), 
#        choices = list("Male" = 1, 
#           "Female" = 2),
#        selected = 1),
    
#      checkboxGroupInput("var", 
#       label = h3("Species"), 
#        choices = list("Opilio" = 1, 
#           "Tanner E" = 2, "Red king" = 3,
#		"Tanner W"=4,"Hair"=5),
#       selected = 1),
      helpText("Spatial distribution of log(density) from summer surveys"),
      selectInput("var", 
                  label = "Species",
                  choices = c("Opilio", "Red king",
                              "Tanner E", "Tanner W","Hair"),
                  selected = "Red king"),
      
      selectInput("sex", 
        label = "Sex",
        choices = c("Male", "Female"),
        selected = "Male"),

      selectInput("scale", 
        label = "Scale to",
        choices = c("Species", "All stocks"),
        selected = "Species"),  

      sliderInput("slider", 
        label = "Year",
        min = 1975, max = 2014, value = 1982),

      sliderInput("slider2", 
        label = "Lengths to display",
        min = 0, max = 245, value = c(25,205))
    ),
  
    mainPanel(plotOutput("map"))
  )
))

 
