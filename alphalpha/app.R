# IMPORTS


library(dplyr)
library(httr)
library(lubridate)
library(readr)
library(rvest)
library(shiny)
library(tibble)


# CONSTANTS


OHLC <- c("open", "high", "low", "close", "adj_close", "volume")

CGI_YAHOO <- "https://query1.finance.yahoo.com/v7/finance/download/%s?"
CGI_FRED <- "https://fred.stlouisfed.org/graph/fredgraph.csv?"

TEXT_REFERENCE <- trimws("
^GSPC         ::  S&P 500
^IXIC         ::  NASDAQ Composite
^DJI          ::  Dow Jones Industrial Average
^VIX          ::  CBOE Volatility
^TNX          ::  Treasury Yield 10 Years
 DX-Y.NYB     ::  US Dollar Index
 GC=F         ::  Gold
 HG=F         ::  Copper
 CL=F         ::  Crude Oil
 FRED:UNRATE  ::  Unemployment Rate
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
        GET() %>%
        content(na = "null")
}

get_yahoo <- function(url) {
    url %>%
        get_content() %>%
        clean_names()
}

get_fred <- function(url) {
    url %>%
        GET() %>%
        content() %>%
        setNames(c("date", "close"))
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

    CGI_YAHOO %>%
        sprintf(symbol) %>%
        api_format(kwargs)
}

api_fred <- function(sid) {
    api_format(CGI_FRED, c(id = sid))
}

mysplit <- function(x, split) {
    unlist(strsplit(x, split))
}

parse_symbol <- function(symbol) {
    pair <- symbol %>%
        trimws() %>%
        mysplit(":")

    if (length(pair) == 1)
        pair <- c("YAHOO", pair)

    pair[1:2]
}

handle_get <- function(
    pair, date_start = NULL, date_end = NULL, interval = NULL) {

    origin <- pair[1]
    symbol <- pair[2]

    if (origin == "YAHOO") {
        symbol %>%
            api_yahoo(date_start, date_end, interval) %>%
            get_yahoo()
    } else if (origin == "FRED") {
        symbol %>%
            api_fred() %>%
            get_fred()
    }
}


# DEBUGGING


if (FALSE) {
    # parameters
    input <- list()
    input$symbol_x <- "AAPL"
    input$symbol_y <- "FRED:LNS12300025"
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

    # change correlation
    tabPanel("Correlation", sidebarLayout(
        sidebarPanel(width = 2,
            # symbols
            textInput("symbol_x", "Symbols", "^VIX"),
            textInput("symbol_y", NULL, "QQQ"),

            # interval
            selectInput("interval", "Interval", c("1d", "1wk", "1mo")),

            # dates
            dateInput("date_start", "Start and End",
                      value = today() - 365, max = today()),
            dateInput("date_end", NULL,
                      max = today()),

            # variables
            selectInput("element_a", "Minuend and Offset", OHLC, "close"),
            numericInput("offset_a", NULL, 0, 0, step = 1),

            selectInput("element_b", "Subtrahend and Offset", OHLC, "close"),
            numericInput("offset_b", NULL, 1, 0, step = 1)
        ),
        column(width = 10,
            column(width = 7,
                plotOutput("plot_c", "100%", "85vh")
            ),
            column(width = 5,
                h3("Model Summary"),
                verbatimTextOutput("text_c_m"),
                h3("Symbol Reference"),
                verbatimTextOutput("text_c_s")
            )
        )
    )),

    # delta histogram
    tabPanel("Histogram", sidebarLayout(
        sidebarPanel(width = 2,
            # symbols
            textInput("symbol_h", "Symbol", "AAPL"),

            # probability
            numericInput("prob", "Tail Probability", 0.1, 0, 0.5, 0.005),

            # interval
            selectInput("interval_h", "Interval", c("1d", "1wk", "1mo")),

            # dates
            dateInput("date_start_h", "Start and End",
                      value = today() - 365, max = today()),
            dateInput("date_end_h", NULL,
                      max = today()),

            # variables
            selectInput("element_h", "Variable and Offset", OHLC, "close"),
            numericInput("offset_h", NULL, 20, 0, step = 1)
        ),
        column(width = 10,
            column(width = 10,
                plotOutput("plot_h", "100%", "85vh")
            ),
            column(width = 2,
                h3("Percentiles"),
                verbatimTextOutput("text_h")
            )
        )
    ))
)

# backend
server <- function(input, output) {
    f_df_x <- reactive({
        # scrape
        input$symbol_x %>%
            parse_symbol() %>%
            handle_get(input$date_start, input$date_end, input$interval)
    })

    f_df_y <- reactive({
        # scrape
        input$symbol_y %>%
            parse_symbol() %>%
            handle_get(input$date_start, input$date_end, input$interval)
    })

    f_df_h <- reactive({
        # scrape
        input$symbol_h %>%
            parse_symbol() %>%
            handle_get(input$date_start_h, input$date_end_h, input$interval_h)
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

    f_series_h <- reactive({
        # react
        df_h <- f_df_h()

        # transform
        minuend_h <- df_h %>%
            pull(input$element_h)
        subtrahend_h <- df_h %>%
            pull(input$element_b) %>%
            lag(input$offset_h)

        data.frame(date = df_h$date, series_h = minuend_h - subtrahend_h)
    })

    output$plot_c <- renderPlot(res = 90, {
        # react
        series_x <- f_series_x()
        series_y <- f_series_y()

        # plot
        series_x %>%
            inner_join(series_y) %>%
            with({
                op <- par(mar = c(4.5, 4.5, 0.5, 0.5))

                plot(series_x, series_y, type = "n",
                     xlab = trimws(input$symbol_x),
                     ylab = trimws(input$symbol_y))

                abline(h = 0, v = 0, col = "grey70")
                points(series_x, series_y, pch = 1)

                m <- lm(series_y ~ series_x)
                abline(m, lwd = 2, col = 2)

                par(op)
            })
    })

    output$plot_h <- renderPlot(res = 90, {
        # react
        series_h <- f_series_h()

        # plot
        series_h %>%
            with({
                op <- par(mar = c(5.5, 4, 3.5, 0.5))

                title <- "Delta Histogram\n(%s, %s, %s, %s, %s, %s, %s)" %>%
                    sprintf(trimws(input$symbol_h),
                            input$prob,
                            input$interval_h,
                            input$date_start_h,
                            input$date_end_h,
                            input$element_h,
                            input$offset_h)

                tbl <- table(round(series_h))
                key <- as.numeric(names(tbl))
                val <- as.numeric(tbl)

                qua <- c(input$prob, 1 - input$prob)
                rng <- quantile(na.omit(series_h), qua)
                col <- ifelse(rng < 0, 2, 3)

                # initiate plot
                plot(key, val, type = "n", xaxt = "n",
                     main = title,
                     xlab = "", ylab = "Count")

                abline(v = 0, col = 4, lwd = 4)
                axis(1, c(min(key), 0, max(key)))

                abline(v = rng, col = col, lwd = 4)
                axis(1, rng, round(rng, 2), las = 2)

                segments(key, 0, key, val, lwd = 2, lend = 2)
                points(key, val, pch = 16)

                par(op)
            })
    })

    output$text_h <- renderText(sep = "\n", {
        # react
        series_h <- f_series_h()

        # data frame
        series_h %>%
            with({
                p <- seq(0, 1, 0.04)
                q <- quantile(series_h, p, na.rm = TRUE)

                p %>%
                    cbind(q) %>%
                    apply(1, function(x) {
                        sprintf("%3.0f%% :: %7.2f", 100 * x[1], x[2])
                    })
            })
    })

    output$text_c_m <- renderText(sep = "\n", {
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

    output$text_c_s <- renderText(sep = "\n", {
        TEXT_REFERENCE
    })
}

# run app
shinyApp(ui, server)
