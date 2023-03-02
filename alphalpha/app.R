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
INTERVALS <- c("1d", "1wk", "1mo")

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

CACHE <- list()


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

api_yahoo <- function(symbol, date_c0, date_c1, interval=INTERVALS) {
    kwargs <- c(period1 = as_unix_time(date_c0),
                period2 = as_unix_time(date_c1),
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
    pair, date_c0 = NULL, date_c1 = NULL, interval = NULL) {

    # split pair
    origin <- pair[1]
    symbol <- pair[2]

    # key for caching
    key <- c(origin = origin,
             symbol = symbol,
             date_c0 = date_c0,
             date_c1 = date_c1,
             interval = interval) %>%
           api_kwargs()

    # scrape api
    if (is.null(CACHE[[key]])) {
        if (origin == "YAHOO") {
            CACHE[[key]] <<- symbol %>%
                api_yahoo(date_c0, date_c1, interval) %>%
                get_yahoo()
        } else if (origin == "FRED") {
            CACHE[[key]] <<- symbol %>%
                api_fred() %>%
                get_fred()
        }
    }

    # assign cache to globals
    assign("CACHE", CACHE, .GlobalEnv)
    CACHE[[key]]
}

handle_series <- function(df, k0, k1, off, op, name) {
    # transform
    x <- df %>%
        pull(k0)

    y <- df %>%
        pull(k1) %>%
        lag(off)

    # difference is default
    if (op == "Difference") {
        z <- x - y
    } else if (op == "Change") {
        z <- 100 * (x - y) / y
    }

    data.frame(df$date, z) %>%
        setNames(c("date", name))
}


# DEBUGGING


if (FALSE) {
    # parameters
    input <- list()
    input$sym_c0 <- "AAPL"
    input$sym_c1 <- "FRED:LNS12300025"
    input$date_c0 <- "2023-01-01"
    input$date_c1 <- "2023-03-01"
    input$intr_c <- "1d"
    input$var_c0 <- "close"
    input$var_c1 <- "open"
    input$off_c <- 0
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
            selectInput("intr_c", "Interval", INTERVALS),

            # dates
            dateInput("date_c0", "Start and End",
                      value = today() - 365, max = today()),
            dateInput("date_c1", NULL,
                      max = today()),

            # variables
            selectInput("var_c0", "Minuend", OHLC, "close"),
            selectInput("var_c1", "Subtrahend and Offset", OHLC, "open"),
            numericInput("off_c", NULL, 0, 0, step = 1)
        ),
        column(width = 10,
            column(width = 7,
                plotOutput("plot_c", "100%", "85vh")
            ),
            column(width = 5,
                h3("Model Summary"),
                verbatimTextOutput("text_c0"),
                h3("Symbol Reference"),
                verbatimTextOutput("text_c1")
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
            numericInput("pr_h", "Tail Probability", 0.1, 0, 0.5, 0.005),

            # interval
            selectInput("intr_h", "Interval", INTERVALS),

            # dates
            dateInput("date_h0", "Start and End",
                      value = today() - 365, max = today()),
            dateInput("date_h1", NULL,
                      max = today()),

            # variables
            selectInput("var_h", "Variable and Offset", OHLC, "close"),
            numericInput("off_h", NULL, 20, 0, step = 1)
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


    f_series_c_x <- reactive({
        input$sym_c0 %>%
            parse_symbol() %>%
            handle_get(input$date_c0, input$date_c1, input$intr_c) %>%
            handle_series(input$var_c0, input$var_c1,
                          input$off_c, input$op_c, "series_c_x")
    })

    f_series_c_y <- reactive({
        input$sym_c1 %>%
            parse_symbol() %>%
            handle_get(input$date_c0, input$date_c1, input$intr_c) %>%
            handle_series(input$var_c0, input$var_c1,
                          input$off_c, input$op_c, "series_c_y")
    })

    output$plot_c <- renderPlot(res = 90, {
        # react
        series_c_x <- f_series_c_x()
        series_c_y <- f_series_c_y()

        # plot
        series_c_x %>%
            inner_join(series_c_y, "date") %>%
            with({
                op <- par(mar = c(4.5, 4.5, 3.5, 0.5))

                title <- "%s Correlation\n(%s, %s, %s, %s, %s, %s, %s, %s)" %>%
                    sprintf(input$op_c,
                            trimws(input$sym_c0),
                            trimws(input$sym_c1),
                            input$intr_c,
                            input$date_c0,
                            input$date_c1,
                            input$var_c0,
                            input$var_c1,
                            input$off_c)

                plot(series_c_x, series_c_y, type = "n",
                     main = title,
                     xlab = trimws(input$sym_c0),
                     ylab = trimws(input$sym_c1))

                abline(h = 0, v = 0, col = "grey70")
                points(series_c_x, series_c_y)

                fit <- lm(series_c_y ~ series_c_x)
                coef <- fit$coefficients

                abline(fit, lwd = 2, col = 2)

                legend(min(series_c_x, na.rm = TRUE),
                       max(series_c_y, na.rm = TRUE),
                       c(sprintf("%.3fx + %.3f", coef[1],  coef[2]),
                         sprintf("Rsq: %.3f", summary(fit)$r.squared)),
                       bty = "n", text.col = 2, text.font = 2,
                       xjust = 0, yjust = 1)

                par(op)
            })
    })

    output$text_c0 <- renderText(sep = "\n", {
        # react
        series_c_x <- f_series_c_x()
        series_c_y <- f_series_c_y()

        # modeling
        series_c_x %>%
            inner_join(series_c_y, "date") %>%
            mutate(X = series_c_x,
                   Y = series_c_y) %>%
            lm(Y ~ X, .) %>%
            summary() %>%
            capture.output() %>%
            "["(-(1:4))
    })

    output$text_c1 <- renderText(sep = "\n", TEXT_REFERENCE)


    # HISTOGRAM


    f_series_h <- reactive({
        input$sym_h %>%
            parse_symbol() %>%
            handle_get(input$date_h0, input$date_h1, input$intr_h) %>%
            handle_series(input$var_h, input$var_h,
                          input$off_h, input$op_h, "series_h")
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
                            input$pr_h,
                            input$intr_h,
                            input$date_h0,
                            input$date_h1,
                            input$var_h,
                            input$off_h)

                ex <- optimize(function(ex) {
                    tbl <- table(round((10 ^ ex) * series_h))
                    abs(30 - length(tbl))
                }, c(-2, 2))$minimum

                tbl <- table(round((10 ^ ex) * series_h))
                key <- round(as.numeric(names(tbl)) / (10 ^ ex), 2)
                val <- as.numeric(tbl)

                qua <- c(input$pr_h, 1 - input$pr_h)
                rng <- quantile(series_h, qua, na.rm = TRUE)
                col <- ifelse(rng < 0, 2, 3)

                # initiate plot
                plot(key, val, type = "n", xaxt = "n",
                     main = title,
                     xlab = "", ylab = "Count")

                segments(key, 0, key, val, lwd = 20, lend = 2)

                abline(v = rng, col = "white", lwd = 8)
                abline(v = rng, col = col, lwd = 4)

                axis(1, c(min(key), 0, max(key)),
                     round(c(min(series_h, na.rm = TRUE), 0, max(series_h, na.rm = TRUE)), 1))
                axis(1, rng, round(rng, 2), las = 2)

                par(op)
            })
    })

    output$text_h <- renderText(sep = "\n", {
        with(f_series_h(), {
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
