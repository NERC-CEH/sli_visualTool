library(shiny)
library(bslib)

#theming with bslib
UKCEH_theme <- bs_theme(
  bg = "#fff",
  fg = "#292C2F",
  primary = "#0483A4",
  secondary = "#EAEFEC",
  success = "#37a635",
  info = "#34b8c7",
  warning = "#F49633",
  base_font = font_link(family = "Montserrat",href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600&display=swap")
  ,"accordion-button-bg" = "#EAEFEC"
)

## more specific theming options: https://rstudio.github.io/bslib/articles/theming/index.html#theming-variables



#increase the font weight of the headings (h1, h2, h3, h4, h5, h6)
UKCEH_theme <- bs_add_variables(UKCEH_theme,
                                # low level theming
                                "headings-font-weight" = 600)

#titlePanel replacement
UKCEH_titlePanel <- function(title = "UKCEH Shiny app", windowTitle = title){
  
  # 
  # div(
  #   div(
  #     style = "display: flex; justify-content: space-between; align-items: center;",
  #     
  #     a(href="JavaScript: location.reload(true);" ,img(src="https://www.ceh.ac.uk/sites/default/files/images/theme/ukceh_logo_long_720x170_rgb.png",
  #         style="height: 50px;vertical-align:middle;")),
  #     
  #     a(href="JavaScript: location.reload(true);" ,img(src="https://jncc.gov.uk/images/logo.png",
  #                                                      style="height: 50px;vertical-align:middle;")),
  #   ),
  #   
  #   div(
  #   
  #   h2(  
  #       title,
  #       style ='vertical-align:middle; display:inline;padding-left:40px;'
  #     ),
  #   ),
  #   
  #   tagList(tags$head(tags$title(paste0(windowTitle," | UK Centre for Ecology & Hydrology")),
  #                     tags$link(rel="shortcut icon", href="https://brandroom.ceh.ac.uk/themes/custom/ceh/favicon.ico"))),
  #   style = "padding: 30px;"
  # )
  
  
  ## option 2
  titlePanel(
    tags$div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      
      # Left Logo
      tags$a(href="JavaScript: location.reload(true);",
             #tags$img(src = "https://www.ceh.ac.uk/sites/default/files/images/theme/ukceh_logo_long_720x170_rgb.png", height = "50px")
             tags$img(src = "UKCEH_Logo_Master_Black.png", height = "50px")
             
      )#
      ,
      tags$span(
        tags$strong("", .noWS="outside", style = "padding-left:10px;"),
        tags$p("Beta", .noWS="outside", style = "font-size:24px; background-color: lightblue;")
      ),
      
      # Title in the center
      
      tags$h1(title, style = "flex-grow: 1; text-align: middle;padding-left:40px;"), # can use middle or left text-align, and add  padding-left:40px;
      

      # Right Logo
      tags$img(src = "https://jncc.gov.uk/images/logo.png", height = "50px")
    )
  )
}



