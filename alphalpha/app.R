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


# FUNCTIONS (GENERAL)


concat <- function(..., sep = "", collapse = "") {
    paste(..., sep = sep, collapse = collapse)
}

mysplit <- function(x, split) {
    unlist(strsplit(x, split))
}

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


# FUNCTIONS (API)


api_kwargs <- function(kwargs) {
    paste(names(kwargs), kwargs, sep = "=", collapse = "&")
}

api_format <- function(cgi, kwargs) {
    concat(cgi, api_kwargs(kwargs))
}

api_yahoo <- function(symbol, d_c_start, d_c_end, interval=c("1d", "1wk", "1mo")) {
    kwargs <- c(period1 = as_unix_time(d_c_start),
                period2 = as_unix_time(d_c_end),
                interval = interval,
                events = "history")

    CGI_YAHOO %>%
        sprintf(symbol) %>%
        api_format(kwargs)
}

api_fred <- function(sid) {
    api_format(CGI_FRED, c(id = sid))
}


# FUNCTIONS (REQUESTS)


mycontent <- function(x, ..., progress = FALSE, show_col_types = FALSE) {
    content(x, ..., progress = progress, show_col_types = show_col_types)
}

get_yahoo <- function(url) {
    url %>%
        GET() %>%
        mycontent(na = "null") %>%
        clean_names()
}

get_fred <- function(url) {
    url %>%
        GET() %>%
        mycontent() %>%
        setNames(c("date", "close"))
}


# FUNCTIONS (ALPHALPHA)


parse_symbol <- function(symbol) {
    pair <- symbol %>%
        trimws() %>%
        mysplit(":")

    if (length(pair) == 1)
        pair <- c("YAHOO", pair)

    pair[1:2]
}

handle_get <- function(
    pair, d_c_start = NULL, d_c_end = NULL, interval = NULL) {

    origin <- pair[1]
    symbol <- pair[2]

    if (origin == "YAHOO") {
        symbol %>%
            api_yahoo(d_c_start, d_c_end, interval) %>%
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
    input$sym_c0 <- "AAPL"
    input$sym_c1 <- "FRED:LNS12300025"
    input$d_c_start <- "2023-01-01"
    input$d_c_end <- "2023-03-01"
    input$intr_c <- "1d"
    input$minu <- "close"
    input$subt <- "open"
    input$offs_c <- 0
}


# FRONTEND


ui <- navbarPage("Alphalpha",
    # HEAD


    header = tags$head(tags$style(HTML("
        .form-group {
            margin-bottom: 10px;
        }
        "))
    ),


    # CORRELATION


    tabPanel("Correlation", sidebarLayout(
        sidebarPanel(width = 2,
            # symbols
            textInput("sym_c0", "Symbols", "^VIX"),
            textInput("sym_c1", NULL, "QQQ"),

            # operation
            selectInput("op_c", "Operation", c("Change", "Difference")),

            # interval
            selectInput("intr_c", "Interval", c("1d", "1wk", "1mo")),

            # dates
            dateInput("d_c_start", "Start and End",
                      value = today() - 365, max = today()),
            dateInput("d_c_end", NULL,
                      max = today()),

            # variables
            selectInput("minu", "Minuend", OHLC, "close"),
            selectInput("subt", "Subtrahend and Offset", OHLC, "open"),
            numericInput("offs_c", NULL, 0, 0, step = 1)
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


    # HISTOGRAM


    tabPanel("Histogram", sidebarLayout(
        sidebarPanel(width = 2,
            # symbols
            textInput("sym_h", "Symbol", "AAPL"),

            # operation
            selectInput("op_h", "Operation", c("Difference", "Change")),

            # probability
            numericInput("prob", "Tail Probability", 0.1, 0, 0.5, 0.005),

            # interval
            selectInput("intr_h", "Interval", c("1d", "1wk", "1mo")),

            # dates
            dateInput("d_h_start", "Start and End",
                      value = today() - 365, max = today()),
            dateInput("d_h_end", NULL,
                      max = today()),

            # variables
            selectInput("elem_h", "Variable and Offset", OHLC, "close"),
            numericInput("offs_h", NULL, 20, 0, step = 1)
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


# BACKEND


server <- function(input, output) {


    # CORRELATION


    f_df_x <- reactive({
        # scrape
        input$sym_c0 %>%
            parse_symbol() %>%
            handle_get(input$d_c_start, input$d_c_end, input$intr_c)
    })

    f_df_y <- reactive({
        # scrape
        input$sym_c1 %>%
            parse_symbol() %>%
            handle_get(input$d_c_start, input$d_c_end, input$intr_c)
    })

    f_series_x <- reactive({
        # react
        df_x <- f_df_x()

        # transform
        minuend_x <- df_x %>%
            pull(input$minu)
        subtrahend_x <- df_x %>%
            pull(input$subt) %>%
            lag(input$offs_c)

        # difference is default
        result <- minuend_x - subtrahend_x
        if (input$op_c == "Change") {
            result <- 100 * (minuend_x - subtrahend_x) / subtrahend_x
        }

        data.frame(date = df_x$date, series_x = result)
    })

    f_series_y <- reactive({
        # react
        df_y <- f_df_y()

        # transform
        minuend_y <- df_y %>%
            pull(input$minu)
        subtrahend_y <- df_y %>%
            pull(input$subt) %>%
            lag(input$offs_c)

        # difference is default
        result <- minuend_y - subtrahend_y
        if (input$op_c == "Change") {
            result <- 100 * (minuend_y - subtrahend_y) / subtrahend_y
        }

        data.frame(date = df_y$date, series_y = result)
    })

    output$plot_c <- renderPlot(res = 90, {
        # react
        series_x <- f_series_x()
        series_y <- f_series_y()

        # plot
        series_x %>%
            inner_join(series_y, "date") %>%
            with({
                op <- par(mar = c(4.5, 4.5, 3.5, 0.5))

                title <- "%s Correlation\n(%s, %s, %s, %s, %s, %s, %s, %s)" %>%
                    sprintf(input$op_c,
                            trimws(input$sym_c0),
                            trimws(input$sym_c1),
                            input$intr_c,
                            input$d_c_start,
                            input$d_c_end,
                            input$minu,
                            input$subt,
                            input$offs_c)

                plot(series_x, series_y, type = "n",
                     main = title,
                     xlab = trimws(input$sym_c0),
                     ylab = trimws(input$sym_c1))

                abline(h = 0, v = 0, col = "grey70")
                points(series_x, series_y, pch = 1)

                m <- lm(series_y ~ series_x)
                abline(m, lwd = 2, col = 2)

                par(op)
            })
    })

    output$text_c_m <- renderText(sep = "\n", {
        # react
        series_x <- f_series_x()
        series_y <- f_series_y()

        # modeling
        series_x %>%
            inner_join(series_y, "date") %>%
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


    # HISTOGRAM


    f_df_h <- reactive({
        # scrape
        input$sym_h %>%
            parse_symbol() %>%
            handle_get(input$d_h_start, input$d_h_end, input$intr_h)
    })

    f_series_h <- reactive({
        # react
        df_h <- f_df_h()

        # transform
        minuend_h <- df_h %>%
            pull(input$elem_h)
        subtrahend_h <- df_h %>%
            pull(input$elem_h) %>%
            lag(input$offs_h)

        # difference is default
        result <- minuend_h - subtrahend_h
        if (input$op_h == "Change") {
            result <- 100 * (minuend_h - subtrahend_h) / subtrahend_h
        }

        data.frame(date = df_h$date, series_h = result)
    })

    output$plot_h <- renderPlot(res = 90, {
        # react
        series_h <- f_series_h()

        # plot
        series_h %>%
            with({
                op <- par(mar = c(5.5, 4, 3.5, 0.5))

                title <- "%s Histogram\n(%s, %s, %s, %s, %s, %s, %s)" %>%
                    sprintf(input$op_h,
                            trimws(input$sym_h),
                            input$prob,
                            input$intr_h,
                            input$d_h_start,
                            input$d_h_end,
                            input$elem_h,
                            input$offs_h)

                ex <- optimize(function(ex) {
                    tbl <- table(round((10 ^ ex) * series_h))
                    abs(30 - length(tbl))
                }, c(-2, 2))$minimum

                tbl <- table(round((10 ^ ex) * series_h))
                key <- round(as.numeric(names(tbl)) / (10 ^ ex), 2)
                val <- as.numeric(tbl)

                qua <- c(input$prob, 1 - input$prob)
                rng <- quantile(series_h, qua, na.rm = TRUE)
                col <- ifelse(rng < 0, 2, 3)

                # initiate plot
                plot(key, val, type = "n", xaxt = "n",
                     main = title,
                     xlab = "", ylab = "Count")

                abline(v = 0, col = 4, lwd = 4)
                axis(1, c(min(key), 0, max(key)),
                     round(c(min(series_h, na.rm = TRUE), 0, max(series_h, na.rm = TRUE)), 1))

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
                p <- seq(0, 1, 0.05)
                q <- quantile(series_h, p, na.rm = TRUE)

                p %>%
                    cbind(q) %>%
                    apply(1, function(x) {
                        sprintf("%3.0f%% :: %7.2f", 100 * x[1], x[2])
                    })
            })
    })

}


# RUN APP


shinyApp(ui, server)
