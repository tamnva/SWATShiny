
# maximum upload file 500 MB
# options(shiny.maxRequestSize = 500*1024^2)

# Creating server
server <- function(input, output, session) {
  hruServer("hruUI")
  
  watoutServer("watoutUI")
}