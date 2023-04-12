# IMPORTS


library(dplyr)
library(httr)
library(lubridate)
library(readr)
library(readxl)
library(rvest)
library(shiny)
library(tibble)
library(zoo)


# CONSTANTS


# globals
NAAIM <- NULL
CACHE <- list()

# colors
RED <- rgb(1, 0, 0)
GREEN <- rgb(0, 1, 0)
BLUE <- rgb(0, 0, 1)
GREY90 <- "grey90"
GREY70 <- "grey70"
TRANSPARENT <- "#00000000"

# inputs
OHLC <- c("open", "high", "low", "close", "adj_close", "volume")
INTERVALS <- c("1d", "1wk", "1mo")
OPERATION <- c("Change", "Difference")

# cgi urls
CGI_YAHOO <- "https://query1.finance.yahoo.com/v7/finance/download/%s?"
CGI_FRED <- "https://fred.stlouisfed.org/graph/fredgraph.csv?"

# user agents
AGENTS <- c("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36 Edge/12.1",
            "Mozilla/5.0 (X11; CrOS x86_64 8172.45.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.64 Safari/537.36)",
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9",
            "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
            "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:15.0) Gecko/20100101 Firefox/15.0.1")

# hints
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

# time
UNIX_DAY <- 86400
UNIX_1900_1970 <- 25567

# xaxis
MONTHS <- expand.grid(1850:2030, 1:12) %>%
    apply(1, paste, collapse = "-") %>%
    ym()

MONTHS_FMT <- MONTHS %>%
    lapply(function(x) sprintf("%04d-%02d", year(x), month(x)))

YEARS <- 1850:2030 %>%
    expand.grid(1) %>%
    apply(1, paste, collapse = "-") %>%
    ym()

YEARS_5 <- seq(1850, 2030, 5) %>%
    expand.grid(1) %>%
    apply(1, paste, collapse = "-") %>%
    ym()

YEARS_5_FMT <- YEARS_5 %>%
    year() %>%
    paste()

YEARS_0 <- YEARS %>%
    setdiff(YEARS_5)

# yaxis
PRICES <- 0:9 %>%
    lapply("*", 10 ^ (0:3)) %>%
    unlist() %>%
    unique() %>%
    sort()

PRICES_1 <- PRICES %>%
    paste() %>%
    "["(startsWith(., "1")) %>%
    as.integer()

PRICES_0 <- PRICES %>%
    setdiff(PRICES_1)


# FUNCTIONS (GENERAL)


concat <- function(..., sep = "", collapse = "") {
    paste(..., sep = sep, collapse = collapse)
}

mysplit <- function(x, split) {
    unlist(strsplit(x, split))
}

as_unix_time <- function(date, origin = "1970-01-01") {
    date %>%
        as.POSIXct(origin = origin, tz="UTC") %>%
        as.numeric()
}

as_unix_day <- function(date, origin = "1970-01-01") {
    day_orig <- origin %>%
        as_unix_time() %>%
        "/"(UNIX_DAY)

    day_date <- date %>%
        as_unix_time(origin) %>%
        "/"(UNIX_DAY)

    day_date - day_orig
}

clean_names <- function(x) {
    names(x) <- names(x) %>%
        gsub("[^0-9A-Za-z]+", "_", .) %>%
        trimws("both", "_") %>%
        gsub("^(\\d)", "x\\1", .) %>%
        tolower()

    return(x)
}

breakpoints <- function(x) {
    lower <- x %>%
        sub("\\((.+),.*", "\\1", .) %>%
        as.numeric()

    upper <- x %>%
        sub("[^,]*,([^]]*)\\]", "\\1", .) %>%
        as.numeric()

    data.frame(lower, upper)
}


# FUNCTIONS (TA)


sma <- function(x, n) {
    rollmean(x, n, NA, align = "right")
}

relerr <- function(a, b) {
    100 * (b - a) / a
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

ua_random <- function() {
    user_agent(sample(AGENTS, 1))
}


# FUNCTIONS (REQUESTS)


mycontent <- function(x, ..., progress = FALSE, show_col_types = FALSE) {
    content(x, ..., progress = progress, show_col_types = show_col_types)
}

get_yahoo <- function(url) {
    url %>%
        GET(ua_random()) %>%
        mycontent(na = "null") %>%
        clean_names()
}

get_fred <- function(url) {
    url %>%
        GET(ua_random()) %>%
        mycontent() %>%
        setNames(c("date", "close"))
}

get_naaim <- function() {
    if (is.null(NAAIM)) {
        url <- "https://www.naaim.org/programs/naaim-exposure-index/" %>%
            read_html() %>%
            html_node("a[href*='/uploads/']") %>%
            html_attr("href")

        url %>%
            download.file("./tmp.xlsx", mode = "wb", quiet = TRUE)

        NAAIM <<- "./tmp.xlsx" %>%
            read_excel() %>%
            clean_names() %>%
            mutate(date = as_date(date)) %>%
            arrange(date)

        file.remove("./tmp.xlsx")
    }

    assign("NAAIM", NAAIM, .GlobalEnv)
    NAAIM
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
            selectInput("op_c", "Operation", OPERATION),

            # interval
            selectInput("intr_c", "Interval", INTERVALS),

            # dates
            dateInput("date_c0", "Start and End",
                      value = today() - 365, max = today()),
            dateInput("date_c1", NULL,
                      max = today()),

            # variables
            selectInput("var_c0", "Minuend", OHLC, "close"),
            selectInput("var_c1", "Subtrahend and Offset", OHLC, "close"),
            numericInput("off_c", NULL, 1, 0, step = 1)
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


    # CHANGE


    tabPanel("Change", sidebarLayout(
        sidebarPanel(width = 2,
            # symbols
            textInput("sym_y", "Symbol", "NVDA"),

            # interval
            selectInput("intr_y", "Interval", INTERVALS, "1d"),

            # offset
            numericInput("off_y", "Offset", 200, 0, step = 1),

            # window
            numericInput("len_y", "MA Length", 90, 0, step = 1),

            # dates
            dateInput("date_y0", "Start and End",
                      value = today() - round(10 * 365.25), max = today()),
            dateInput("date_y1", NULL,
                      max = today()),
        ),
        column(width = 10,
            column(width = 10,
                plotOutput("plot_y0", "100%", "85vh")
            ),
            column(width = 2,
                plotOutput("plot_y1", "100%", "85vh")
            )
        )
    )),


    # HISTOGRAM


    tabPanel("Histogram", sidebarLayout(
        sidebarPanel(width = 2,
            # symbols
            textInput("sym_h", "Symbol", "AAPL"),

            # operation
            selectInput("op_h", "Operation", OPERATION),

            # probability
            numericInput("pr_h", "Tail Probability", 0.1, 0, 0.5, 0.0025),

            # interval
            selectInput("intr_h", "Interval", INTERVALS),

            # dates
            dateInput("date_h0", "Start and End",
                      value = today() - 3 * 365, max = today()),
            dateInput("date_h1", NULL,
                      max = today()),

            # variables
            selectInput("var_h0", "Minuend", OHLC, "close"),
            selectInput("var_h1", "Subtrahend and Offset", OHLC, "open"),
            numericInput("off_h", NULL, 0, 0, step = 1)
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
    )),


    # LOG-LOG


    tabPanel("Log-Log", sidebarLayout(
        sidebarPanel(width = 2,
            # symbols
            textInput("sym_l", "Symbol", "DHR"),

            # log view
            selectInput("log_lv", "Log View", c("xy", "y", "x", "none"), "y"),

            # log model
            selectInput("log_lm", "Log Model", c("xy", "y", "x", "none")),

            # interval
            selectInput("intr_l", "Interval", INTERVALS, "1wk"),

            # dates
            dateInput("date_l0", "Start and End",
                      value = today() - round(40 * 365.25), max = today()),
            dateInput("date_l1", NULL,
                      max = today()),

            # model origin
            dateInput("date_lo", "Model Origin",
                      value = "1943-02-02", max = today())
        ),
        column(width = 10,
            column(width = 10,
                plotOutput("plot_l0", "100%", "85vh")
            ),
            column(width = 2,
                plotOutput("plot_l1", "100%", "85vh")
            )
        )
    )),


    # SENTIMENT


    tabPanel("Sentiment", sidebarLayout(
        sidebarPanel(width = 2,
            # symbols
            textInput("sym_s", "Symbol", "^GSPC"),

            # indicator
            numericInput("ma_s0", "MA Symbol", 90, 0, step = 1),
            numericInput("ma_s1", "MA NAAIM", 13, 0, step = 1),

            # interval
            selectInput("intr_s", "Interval", INTERVALS),

            # dates
            dateInput("date_s0", "Start and End",
                      value = today() - 365, max = today()),
            dateInput("date_s1", NULL,
                      max = today()),
        ),
        column(width = 10,
            plotOutput("plot_s", "100%", "85vh")
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

                abline(h = 0, v = 0, col = GREY70)
                points(series_c_x, series_c_y)

                fit <- lm(series_c_y ~ series_c_x)
                coef <- fit$coefficients

                abline(fit, lwd = 2, col = RED)

                legend(min(series_c_x, na.rm = TRUE),
                       max(series_c_y, na.rm = TRUE),
                       c(sprintf("%.3fx + %.3f", coef[2],  coef[1]),
                         sprintf("Rsq: %.3f", summary(fit)$r.squared)),
                       bty = "n", text.col = RED, text.font = 2,
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


    # CHANGE


    f_df_y <- reactive({
        input$sym_y %>%
            parse_symbol() %>%
            handle_get(ymd("1900-01-01"), today(), input$intr_y)
    })

    output$plot_y0 <- renderPlot(res = 90, {
        df <- f_df_y()

        with(df, {
            op <- par(mar = c(3, 4, 3.5, 0.5))

            title <- "Change Plot\n(%s, %s, %s, %s, %s, %s)" %>%
                sprintf(trimws(input$sym_y),
                        input$intr_y,
                        input$off_y,
                        input$len_y,
                        input$date_y0,
                        input$date_y1)

            major <- seq(-100, 1000, 50)
            minor <- setdiff(seq(-100, 1000, 10), major)

            x <- relerr(lag(close, input$off_y), close)
            i <- (input$date_y0 <= date) & (date < input$date_y1)

            plot(date[i], x[i], type = "n",
                 ylab = "Percent Change",
                 main = title)

            abline(h = minor, v = YEARS_0, col = GREY90)
            abline(h = major, v = YEARS_5, col = GREY70)
            abline(h = 0, v = input$date_y1, col = "lightblue3", lwd = 2)

            lines(date, x)

            if (input$len_y != 0) {
                m <- rollmean(x, input$len_y, NA, align = "right")
                lines(date, m, col = RED, lwd = 2)
            }

            par(op)
        })
    })

    output$plot_y1 <- renderPlot(res = 90, {
        df <- f_df_y()

        with(df, {
            op <- par(mar = c(3, 2, 3.5, 0.5))

            i <- (ymd(input$date_y0) <= date) & (date < ymd(input$date_y1))

            error <- relerr(lag(close, input$off_y), close)[i]
            last <- tail(error, 1)

            med <- median(error, na.rm = TRUE)
            mu <- mean(error, na.rm = TRUE)
            sig <- sd(error, na.rm = TRUE)

            tbl <- error %>%
                cut(20) %>%
                table()

            brk <- breakpoints(names(tbl))

            plot(c(0, max(tbl)), c(min(brk$lower), max(brk$upper)),
                 type = "n", las = 3, main = "Distribution")
            rect(0, brk$lower, tbl, brk$upper, col = TRANSPARENT)

            abline(h = last, col = RED, lwd = 2)

            form <- "p %.3f\n\u03BC\u0303 %.1f\n\u03BC %.1f\n\u03C3 %.1f"
            legend(max(tbl, na.rm = TRUE) / 2, max(error, na.rm = TRUE),
                   sprintf(form, ecdf(error)(last), med, mu, sig),
                   xjust = 0.5, box.col = NA, bg = NA)

            par(op)
        })
    })


    # HISTOGRAM


    f_series_h <- reactive({
        input$sym_h %>%
            parse_symbol() %>%
            handle_get(input$date_h0, input$date_h1, input$intr_h) %>%
            handle_series(input$var_h0, input$var_h1,
                          input$off_h, input$op_h, "series_h")
    })

    output$plot_h <- renderPlot(res = 90, {
        # react
        series_h <- f_series_h()

        # plot
        with(series_h, {
            op <- par(mar = c(5.5, 4.5, 3.5, 0.5))

            title <- "%s Histogram\n(%s, %s, %s, %s, %s, %s, %s, %s)" %>%
                sprintf(input$op_h,
                        trimws(input$sym_h),
                        input$pr_h,
                        input$intr_h,
                        input$date_h0,
                        input$date_h1,
                        input$var_h0,
                        input$var_h1,
                        input$off_h)

            qua <- c(input$pr_h, 1 - input$pr_h)
            rng <- quantile(series_h, qua, na.rm = TRUE)
            col <- ifelse(rng < 0, RED, BLUE)

            tbl <- series_h %>%
                cut(30) %>%
                table()

            brk <- breakpoints(names(tbl))

            plot(c(min(brk$lower), max(brk$upper)), c(0, max(tbl)),
                 type = "n", las = 3, main = title,
                 xlab = "", ylab = "Count")
            rect(brk$lower, 0, brk$upper, tbl, col = TRANSPARENT)

            abline(v = rng, col = col, lwd = 2)
            axis(1, rng, round(rng, 2), las = 2, hadj = 1.8)

            par(op)
        })
    })

    output$text_h <- renderText(sep = "\n", {
        # react
        series_h <- f_series_h()

        with(series_h, {
            p <- seq(0, 1, 0.05)
            q <- quantile(series_h, p, na.rm = TRUE)

            p %>%
                cbind(q) %>%
                apply(1, function(x) {
                    sprintf("%3.0f%% :: %7.2f", 100 * x[1], x[2])
                })
        })
    })


    # LOG-LOG


    f_df_l <- reactive({
        input$sym_l %>%
            parse_symbol() %>%
            handle_get(input$date_l0, input$date_l1, input$intr_l)
    })

    f_m <- reactive({
        df <- f_df_l()
        origin <- paste(input$date_lo)

        if (input$log_lm == "xy") {
            fit <- lm(log(close) ~ log(as_unix_day(date, origin)), df)
        } else if (input$log_lm == "y") {
            fit <- lm(log(close) ~ as_unix_day(date, origin), df)
        } else if (input$log_lm == "x") {
            fit <- lm(close ~ log(as_unix_day(date, origin)), df)
        } else {
            fit <- lm(close ~ as_unix_day(date, origin), df)
        }

        if (grepl("y", input$log_lm)) {
            close_fit <- exp(predict(fit, df))
        } else {
            close_fit <- predict(fit, df)
        }

        list(fit = fit, close_fit = close_fit)
    })

    output$plot_l0 <- renderPlot(res = 90, {
        df <- f_df_l()
        m <- f_m()
        fit <- m$fit
        close_fit <- m$close_fit

        with(df, {
            op <- par(mar = c(3, 4, 3.5, 0.5))

            lv <- ifelse(input$log_lv == "none", "", input$log_lv)


            m <- coef(fit)[2]
            b <- coef(fit)[1]
            r <- summary(fit)$r.squared

            p <- coef(summary(fit))[2, 4]
            sig <- case_when(p < 0.001 ~ "***",
                             p < 0.01 ~ "**",
                             p < 0.05 ~ "*",
                             p < 0.1 ~ ".",
                             TRUE ~ "")

            eq <- sprintf("%.3fx + %.3f", m, b)
            rsq <- sprintf("(Rsq %.4f) %s", r, sig)

            title <- "Log-Log Plot\n(%s, %s, %s, %s, %s, %s, %s)" %>%
                sprintf(trimws(input$sym_l),
                        input$log_lv,
                        input$log_lm,
                        input$intr_l,
                        input$date_l0,
                        input$date_l1,
                        input$date_lo)

            plot(date, close, type = "n", log = lv,
                 xaxt = "n", las = 1,
                 xlab = "", ylab = "",
                 main = title)

            abline(h = PRICES_0, v = YEARS_0, col = GREY90)
            abline(h = PRICES_1, v = YEARS_5, col = GREY70)
            axis(1, YEARS_5, YEARS_5_FMT)

            legend(min(date, na.rm = TRUE), max(close, na.rm = TRUE),
                   c(eq, rsq), lwd = c(2, 0), col = RED)

            lines(date, close)
            lines(date, close_fit, col = RED, lwd = 2)

            par(op)
        })
    })

    output$plot_l1 <- renderPlot(res = 90, {
        df <- f_df_l()
        m <- f_m()
        fit <- m$fit
        close_fit <- m$close_fit

        with(df, {
            op <- par(mar = c(3, 2, 3.5, 0.5))

            error <- (close - close_fit) / close_fit
            last <- tail(error, 1)

            tbl <- error %>%
                cut(20) %>%
                table()

            brk <- breakpoints(names(tbl))

            plot(c(0, max(tbl)), c(min(brk$lower), max(brk$upper)),
                 type = "n", las = 3, main = "Distribution")
            rect(0, brk$lower, tbl, brk$upper, col = TRANSPARENT)

            abline(h = last, col = RED, lwd = 2)
            legend(max(tbl, na.rm = TRUE) / 2, max(error, na.rm = TRUE),
                   sprintf("p = %.3f", ecdf(error)(last)),
                   xjust = 0.5, col = RED, lwd = 2,
                   box.col = NA, bg = NA)

            par(op)
        })
    })


    # LOG-LOG


    f_df_l <- reactive({
        input$sym_l %>%
            parse_symbol() %>%
            handle_get(input$date_l0, input$date_l1, input$intr_l)
    })

    f_m <- reactive({
        df <- f_df_l()
        origin <- paste(input$date_lo)

        if (input$log_lm == "xy") {
            fit <- lm(log(close) ~ log(as_unix_day(date, origin)), df)
        } else if (input$log_lm == "y") {
            fit <- lm(log(close) ~ as_unix_day(date, origin), df)
        } else if (input$log_lm == "x") {
            fit <- lm(close ~ log(as_unix_day(date, origin)), df)
        } else {
            fit <- lm(close ~ as_unix_day(date, origin), df)
        }

        if (grepl("y", input$log_lm)) {
            close_fit <- exp(predict(fit, df))
        } else {
            close_fit <- predict(fit, df)
        }

        list(fit = fit, close_fit = close_fit)
    })

    output$plot_l0 <- renderPlot(res = 90, {
        df <- f_df_l()
        m <- f_m()
        fit <- m$fit
        close_fit <- m$close_fit

        with(df, {
            op <- par(mar = c(3, 4, 3.5, 0.5))

            lv <- ifelse(input$log_lv == "none", "", input$log_lv)


            m <- coef(fit)[2]
            b <- coef(fit)[1]
            r <- summary(fit)$r.squared

            p <- coef(summary(fit))[2, 4]
            sig <- case_when(p < 0.001 ~ "***",
                             p < 0.01 ~ "**",
                             p < 0.05 ~ "*",
                             p < 0.1 ~ ".",
                             TRUE ~ "")

            eq <- sprintf("%.3fx + %.3f", m, b)
            rsq <- sprintf("(Rsq %.4f) %s", r, sig)

            title <- "Log-Log Plot\n(%s, %s, %s, %s, %s, %s, %s)" %>%
                sprintf(trimws(input$sym_l),
                        input$log_lv,
                        input$log_lm,
                        input$intr_l,
                        input$date_l0,
                        input$date_l1,
                        input$date_lo)

            plot(date, close, type = "n", log = lv,
                 xaxt = "n", las = 1,
                 xlab = "", ylab = "",
                 main = title)

            abline(h = PRICES_0, v = YEARS_0, col = GREY90)
            abline(h = PRICES_1, v = YEARS_5, col = GREY70)
            axis(1, YEARS_5, YEARS_5_FMT)

            legend(min(date, na.rm = TRUE), max(close, na.rm = TRUE),
                   c(eq, rsq), lwd = c(2, 0), col = RED)

            lines(date, close)
            lines(date, close_fit, col = RED, lwd = 2)

            par(op)
        })
    })

    output$plot_l1 <- renderPlot(res = 90, {
        df <- f_df_l()
        m <- f_m()
        fit <- m$fit
        close_fit <- m$close_fit

        with(df, {
            op <- par(mar = c(3, 2, 3.5, 0.5))

            error <- (close - close_fit) / close_fit
            last <- tail(error, 1)

            tbl <- error %>%
                cut(20) %>%
                table()

            brk <- breakpoints(names(tbl))

            plot(c(0, max(tbl)), c(min(brk$lower), max(brk$upper)),
                 type = "n", las = 3, main = "Distribution")
            rect(0, brk$lower, tbl, brk$upper, col = TRANSPARENT)

            abline(h = last, col = RED, lwd = 2)
            legend(max(tbl, na.rm = TRUE) / 2, max(error, na.rm = TRUE),
                   sprintf("p = %.3f", ecdf(error)(last)),
                   xjust = 0.5, col = RED, lwd = 2,
                   box.col = NA, bg = NA)

            par(op)
        })
    })


    # SENTIMENT


    output$plot_s <- renderPlot(res = 90, {
        df <- input$sym_s %>%
            parse_symbol() %>%
            handle_get(ymd("1900-01-01"), today(), input$intr_s)

        naaim <- get_naaim()

        # plot
        with(df, {
            op <- par(mar = c(5.5, 5, 3.5, 5))

            title <- "Sentiment Plot\n(%s, %s, %s, %s, %s, %s)" %>%
                    sprintf(trimws(input$sym_s),
                            input$ma_s0,
                            input$ma_s1,
                            input$intr_s,
                            input$date_s0,
                            input$date_s1)

            # simple
            xlim <- c(input$date_s0, input$date_s1)
            ylim <-df %>%
                filter(xlim[1] <= date,
                       date <= xlim[2]) %>%
                with(c(min(low, na.rm = TRUE), max(high, na.rm = TRUE)))

            # sentiment
            vec <- naaim$mean_average
            lo <- min(vec, na.rm = TRUE)
            hi <- max(vec, na.rm = TRUE)

            # zero-one norm, scaled
            one <- (vec - lo) / (hi - lo)
            scl <- (ylim[2] - ylim[1]) * one + ylim[1]

            # axis math
            yaxt0 <- seq(min(ylim), max(ylim), length.out = 6)
            yaxt1 <- seq(min(scl), max(scl), length.out = 6)
            yaxt1_fmt <- seq(min(vec), max(vec), length.out = 6)

            # series
            x <- (open + close) / 2

            # plot init
            plot(xlim, ylim, type = "n",
                 xaxt = "n", yaxt = "n",
                 xlab = "", ylab = trimws(input$sym_s),
                 main = title)

            # plot averages
            if (input$ma_s1 != 0)
                lines(naaim$date, sma(scl, input$ma_s1),
                      col = rgb(1, 0, 0, 0.33),
                      lty = 3, lwd = 2)
            if (input$ma_s0 != 0)
                lines(date, sma(x, input$ma_s0),
                      col = rgb(0, 0, 0, 0.33),
                      lty = 3, lwd = 2)

            # plot series
            lines(naaim$date, scl, lwd = 2, col = RED)
            lines(date, x, lwd = 2, col = rgb(0, 0, 0))

            # plot axes
            mtext("NAAIM", side = 4, line = 3)
            axis(1, MONTHS, MONTHS_FMT, las = 2)
            axis(2, yaxt0, as.numeric(sprintf("%.2e", yaxt0)))
            axis(4, yaxt1, as.numeric(sprintf("%.2e", yaxt1_fmt)))

            par(op)
        })
    })
}


# RUN APP


shinyApp(ui, server)
