# IMPORTS


library(dplyr)
library(lubridate)
library(shiny)
library(rvest)
library(tibble)


# CONSTANTS


FORM_YAHOO_CGI <- "https://query1.finance.yahoo.com/v7/finance/download/%s?"

TEXT_REFERENCE <- trimws("
^GSPC      ::  S&P 500
^IXIC      ::  NASDAQ Composite
^DJI       ::  Dow Jones Industrial Average
^VIX       ::  CBOE Volatility
 DX-Y.NYB  ::  US Dollar Index
 GC=F      ::  Gold Spot
 CL=F      ::  Crude Oil
")


# FUNCTIONS


as_unix_time <- function(date) {
    date %>%
        as.POSIXct(origin = "1970-01-01", tz="UTC") %>%
        as.numeric()
}

clean_names <- function(x) {
    names(x) <- names(x) %>%
        gsub("[^0-9A-Za-z]+", "_", .) %>%
        trimws("both", "_") %>%
        gsub("^(\\d)", "x\\1", .) %>%
        tolower()

    return(x)
}

get_content <- function(url) {
    url %>%
        httr::GET() %>%
        httr::content(na = "null")
}

get_df <- function(url) {
    url %>%
        get_content() %>%
        clean_names()
}

concat <- function(..., sep = "", collapse = "") {
    paste(..., sep = sep, collapse = collapse)
}

api_kwargs <- function(kwargs) {
    paste(names(kwargs), kwargs, sep = "=", collapse = "&")
}

api_format <- function(cgi, kwargs) {
    concat(cgi, api_kwargs(kwargs))
}

api_yahoo <- function(symbol, date_start, date_end, interval=c("1d", "1wk", "1mo")) {
    kwargs <- c(period1 = as_unix_time(date_start),
                period2 = as_unix_time(date_end),
                interval = interval,
                events = "history")

    FORM_YAHOO_CGI %>%
        sprintf(symbol) %>%
        api_format(kwargs)
}


# DEBUGGING


if (FALSE) {
    # parameters
    input <- list()
    input$symbol_x <- "AAPL"
    input$symbol_y <- "DX-Y.NYB"
    input$date_start <- "2023-01-01"
    input$date_end <- "2023-03-01"
    input$interval <- "1d"
    input$element_a <- "close"
    input$offset_a <- 0
    input$element_b <- "open"
    input$offset_b <- 0
}


# APP


# frontend
ui <- navbarPage(
    "Alphalpha",
    tabPanel("Change Correlation", sidebarLayout(
        sidebarPanel(width = 2,
            textInput("symbol_x", "Symbols", "^VIX"),
            textInput("symbol_y", NULL, "QQQ"),
            selectInput("interval", "Interval", c("1d", "1wk", "1mo")),
            dateInput("date_start", "Start and End",
                      value = today() - 365, max = today()),
            dateInput("date_end", NULL,
                      max = today()),
            selectInput("element_a", "Minuend and Offset", c("open", "high", "low", "close", "adj_close", "volume"), "close"),
            numericInput("offset_a", NULL, 0, 0, step = 1),
            selectInput("element_b", "Subtrahend and Offset", c("open", "high", "low", "close", "adj_close", "volume"), "close"),
            numericInput("offset_b", NULL, 1, 0, step = 1)
        ),
        column(width = 10,
            column(width = 7,
                plotOutput("change_plot", "100%", "85vh")
            ),
            column(width = 5,
                h3("Model Summary"),
                verbatimTextOutput("change_text"),
                h3("Symbol Reference"),
                verbatimTextOutput("change_symbols")
            )
        )
    )),
    tabPanel("...",
    )
)

# backend
server <- function(input, output) {
    f_symbol_x <- reactive({
        input$symbol_x %>%
            trimws()
    })

    f_symbol_y <- reactive({
        input$symbol_y %>%
            trimws()
    })

    f_df_x <- reactive({
        # scrape
        f_symbol_x() %>%
            api_yahoo(input$date_start, input$date_end, input$interval) %>%
            get_df()
    })

    f_df_y <- reactive({
        # scrape
        f_symbol_y() %>%
            api_yahoo(input$date_start, input$date_end, input$interval) %>%
            get_df()
    })

    f_series_x <- reactive({
        # react
        df_x <- f_df_x()

        # transform
        minuend_x <- df_x %>%
            pull(input$element_a) %>%
            lead(input$offset_a)
        subtrahend_x <- df_x %>%
            pull(input$element_b) %>%
            lead(input$offset_b)
        data.frame(date = df_x$date, series_x = minuend_x - subtrahend_x)
    })

    f_series_y <- reactive({
        # react
        df_y <- f_df_y()

        # transform
        minuend_y <- df_y %>%
            pull(input$element_a) %>%
            lead(input$offset_a)
        subtrahend_y <- df_y %>%
            pull(input$element_b) %>%
            lead(input$offset_b)
        data.frame(date = df_y$date, series_y = minuend_y - subtrahend_y)
    })

    output$change_plot <- renderPlot(res = 90, {
        # react
        series_x <- f_series_x()
        series_y <- f_series_y()

        # plot
        series_x %>%
            inner_join(series_y) %>%
            with({
                op <- par(mar = c(4.5, 4.5, 0.5, 0.5))

                plot(series_x, series_y, type = "n",
                     xlab = f_symbol_x(), ylab = f_symbol_y())

                abline(h = 0, v = 0, col = "grey70")
                points(series_x, series_y, pch = 1)

                m <- lm(series_y ~ series_x)
                abline(m, lwd = 2, col = 2)

                par(op)
            })
    })

    output$change_text <- renderText(sep = "\n", {
        # react
        series_x <- f_series_x()
        series_y <- f_series_y()

        # modeling
        series_x %>%
            inner_join(series_y) %>%
            mutate(X = series_x,
                   Y = series_y) %>%
            lm(Y ~ X, .) %>%
            summary() %>%
            capture.output() %>%
            "["(-(1:4))
    })

    output$change_symbols <- renderText(sep = "\n", {
        TEXT_REFERENCE
    })
}

# run app
shinyApp(ui, server)
