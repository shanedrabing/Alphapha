# IMPORTS


library(shiny)


# CONSTANTS


# ...


# FUNCTIONS


# ...


# APP


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            h3("Alphalpha", style = "text-align:center"),
        ),
        column(tabsetPanel(type = "pills",
            tabPanel("HTML", br(), htmlOutput("html"))
        ), width = 10)
    )
)

# ui <- fluidPage(
#     br(),
#     sidebarLayout(
#         sidebarPanel(h3("Wikipedia Trees", style = "text-align:center"), br(), tabsetPanel(type = "tabs",
#             tabPanel("Input",
#                 textAreaInput("text_in", NULL, DEFAULT_INPUT, "100%", "60vh", resize = "vertical")
#             ),
#             tabPanel("Settings",
#                 br(), p("General Options", style = "text-align:center;font-weight:bold;"),
#                 checkboxInput("simplify", "Simplify?", TRUE),
#                 fixedRow(column(4, p("Recursive Search")), column(8,
#                     sliderInput("crawl", NULL, 1, 5, 1, animate = FALSE)
#                 )),
#                 br(), p("GraphViz Options", style = "text-align:center;font-weight:bold;"),
#                 checkboxInput("rooted", "Rooted tree?", TRUE),
#                 fixedRow(column(4, p("Layout")), column(8,
#                     selectInput("layout", NULL, OPTIONS_LAYOUT)
#                 )),
#                 fixedRow(column(4, p("Splines")), column(8,
#                     selectInput("splines", NULL, OPTIONS_SPLINES),
#                 )),
#                 fixedRow(column(4, p("Overlap")), column(8,
#                     selectInput("overlap", NULL, OPTIONS_OVERLAP),
#                 )),
#                 fixedRow(column(4, p("Rankdir")), column(8,
#                     selectInput("rankdir", NULL, OPTIONS_RANKDIR)
#                 )),
#                 br(), p("Danger Zone", style = "text-align:center;font-weight:bold;"),
#                 checkboxInput("explore", "Explore?", FALSE)
#             )
#         ), width = 2),
#         column(tabsetPanel(type = "pills",
#             tabPanel("HTML", br(), htmlOutput("html")),
#             tabPanel("SVG", br(), DiagrammeR::grVizOutput("plot", "95%", "80vh")),
#             tabPanel("Text", br(), verbatimTextOutput("text")),
#             tabPanel("Table", br(), tableOutput("table"))
#         ), width = 10)
#     )
# )

server <- function(input, output) {
}

shinyApp(ui, server)
