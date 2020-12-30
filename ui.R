

# Create UI
ui <- dashboardPage(
  
  # ----------------------------------------------------------------------Header
  dashboardHeader(
    title = "SWATShiny"
    #titleWidth = 300
  ),
  
  # ---------------------------------------------------------------------Sidebar
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Calibration", tabName = "cali", icon = icon("align-justify"), 
               startExpanded = TRUE, selected = TRUE,
               menuSubItem("Manual", tabName = "manual", selected = TRUE),
               menuSubItem("Automatic", tabName = "auto"))
    ),
    sidebarMenu(
      menuItem("Visualization", tabName = "visual",icon = icon("align-justify"),
               startExpanded = TRUE, selected = TRUE,
               menuSubItem("watout.dat", tabName = "watout", selected = TRUE),
               menuSubItem("output.rch", tabName = "rch", selected = TRUE),
               menuSubItem("output.hru", tabName = "hru", selected = TRUE),
               menuSubItem("output.sub", tabName = "outputsub"))
    )
  ),

  # ------------------------------------------------------------------------Body
  dashboardBody(
    
    # Tab item for General Setting
    tabItems(
      
      tabItem(tabName = "watout",
              watoutUI("watoutUI")
              ),
      
      tabItem(tabName = "hru",
              hruUI("hruUI")
              )
    )
  ) 
 #------------------------------------------------------------------------------ 
)