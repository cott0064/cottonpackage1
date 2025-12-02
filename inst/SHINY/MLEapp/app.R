# app.R
# Shiny app: Maximum Likelihood Estimation Explorer
# Distributions included: Normal, Poisson, Exponential, Binomial, Gamma

library(shiny)
library(MASS)   # for fitdistr
library(ggplot2)

# --- helper log-likelihood functions ---
loglik_normal <- function(par, x){
  mu <- par[1]; sigma <- par[2]
  if(sigma <= 0) return(-Inf)
  n <- length(x)
  ll <- -n/2 * log(2*pi) - n*log(sigma) - sum((x - mu)^2)/(2*sigma^2)
  return(ll)
}

loglik_poisson <- function(lambda, x){
  if(lambda <= 0) return(-Inf)
  sum(dpois(x, lambda, log=TRUE))
}

loglik_exp <- function(rate, x){
  if(rate <= 0) return(-Inf)
  sum(dexp(x, rate=rate, log=TRUE))
}

loglik_binom <- function(p, x, trials){
  if(p <= 0 || p >= 1) return(-Inf)
  sum(dbinom(x, size=trials, prob=p, log=TRUE))
}

loglik_gamma <- function(par, x){
  shape <- par[1]; rate <- par[2]
  if(shape <=0 || rate <=0) return(-Inf)
  sum(dgamma(x, shape=shape, rate=rate, log=TRUE))
}

# UI
ui <- fluidPage(
  titlePanel("Shiny MLE Explorer — 5+ Univariate Distributions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribution:", choices = c("Normal","Poisson","Exponential","Binomial","Gamma")),
      numericInput("n", "Sample size:", value = 100, min = 1, step = 1),
      hr(),
      conditionalPanel(
        condition = "input.dist == 'Normal'",
        numericInput("norm_mu", "True mu:", value = 0),
        numericInput("norm_sigma", "True sigma:", value = 1, min = 1e-6)
      ),
      conditionalPanel(
        condition = "input.dist == 'Poisson'",
        numericInput("pois_lambda", "True lambda:", value = 3, min = 0.0001)
      ),
      conditionalPanel(
        condition = "input.dist == 'Exponential'",
        numericInput("exp_rate", "True rate:", value = 1, min = 1e-6)
      ),
      conditionalPanel(
        condition = "input.dist == 'Binomial'",
        numericInput("binom_p", "True p:", value = 0.3, min = 0, max = 1, step = 0.01),
        numericInput("binom_trials", "Trials per observation:", value = 1, min = 1, step = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'Gamma'",
        numericInput("gamma_shape", "True shape:", value = 2, min = 1e-6),
        numericInput("gamma_rate", "True rate:", value = 1, min = 1e-6)
      ),
      hr(),
      actionButton("gen", "Generate data / Recompute MLE"),
      checkboxInput("show_hist", "Show histogram / data plot", value = TRUE),
      checkboxInput("show_loglik", "Show log-likelihood plot", value = TRUE),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data & Estimates",
                 verbatimTextOutput("mle_out"),
                 plotOutput("dataPlot", height = "350px")
        ),
        tabPanel("Log-likelihood",
                 conditionalPanel(condition = "input.show_loglik == true",
                                  plotOutput("loglikPlot", height = "450px")
                 ),
                 conditionalPanel(condition = "input.show_loglik == false",
                                  tags$p("Enable 'Show log-likelihood' in the sidebar to see plots.")
                 )
        ),
        tabPanel("Notes & Formulas",
                 tags$h4("What this app demonstrates"),
                 tags$ul(
                   tags$li("Generate data from a chosen univariate distribution."),
                   tags$li("Compute closed-form MLEs (where available) and compare to numerical MLEs."),
                   tags$li("Visualize the log-likelihood (profile or contour) to see where the MLE lies."),
                   tags$li("Distributions: Normal (mu, sigma), Poisson (lambda), Exponential (rate), Binomial (p), Gamma (shape, rate).")
                 ),
                 tags$h4("Quick formulas"),
                 tags$ul(
                   tags$li("Normal: mu_hat = mean(x); sigma_hat_MLE = sqrt(sum((x - mean(x))^2)/n)"),
                   tags$li("Poisson: lambda_hat = mean(x)"),
                   tags$li("Exponential: rate_hat = 1 / mean(x)") ,
                   tags$li("Binomial (same trials each obs): p_hat = sum(successes) / (n * trials)") ,
                   tags$li("Gamma: no closed-form for shape — estimate via numeric optimization or use fitdistr")
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session){
  # generate data when button pressed
  data_reactive <- eventReactive(input$gen, {
    n <- input$n
    switch(input$dist,
           'Normal' = rnorm(n, mean = input$norm_mu, sd = input$norm_sigma),
           'Poisson' = rpois(n, lambda = input$pois_lambda),
           'Exponential' = rexp(n, rate = input$exp_rate),
           'Binomial' = rbinom(n, size = input$binom_trials, prob = input$binom_p),
           'Gamma' = rgamma(n, shape = input$gamma_shape, rate = input$gamma_rate)
    )
  }, ignoreNULL = FALSE)

  # compute MLEs and log-likelihood info
  results <- reactive({
    x <- data_reactive()
    n <- length(x)
    dist <- input$dist
    res <- list()

    if(dist == 'Normal'){
      mu_hat <- mean(x)
      sigma_hat_mle <- sqrt(sum((x - mu_hat)^2)/n) # MLE uses n
      # numerical check via optim (negative log-likelihood)
      negll <- function(p) -loglik_normal(p, x)
      opt <- optim(par = c(mu_hat, sigma_hat_mle), fn = negll, method = "L-BFGS-B", lower = c(-Inf, 1e-8))
      res$closed_form <- list(mu = mu_hat, sigma = sigma_hat_mle)
      res$optim <- list(par = opt$par, value = opt$value, convergence = opt$convergence)
      # prepare grid for profile of mu (fix sigma at mle) and profile for sigma
      mu_grid <- seq(mu_hat - 2*sd(x), mu_hat + 2*sd(x), length.out = 201)
      ll_mu <- sapply(mu_grid, function(m) loglik_normal(c(m, sigma_hat_mle), x))
      sigma_grid <- seq(max(sigma_hat_mle/5, 1e-3), sigma_hat_mle*3, length.out = 201)
      ll_sigma <- sapply(sigma_grid, function(s) loglik_normal(c(mu_hat, s), x))
      res$profile <- list(mu_grid = mu_grid, ll_mu = ll_mu, sigma_grid = sigma_grid, ll_sigma = ll_sigma)
    }

    if(dist == 'Poisson'){
      lambda_hat <- mean(x)
      # numeric check via optimize on log-likelihood (one-dim)
      negll1 <- function(l) -loglik_poisson(l, x)
      opt <- optimize(negll1, interval = c(1e-8, max(10, lambda_hat*3)))
      res$closed_form <- list(lambda = lambda_hat)
      res$optim <- list(par = opt$minimum, value = opt$objective)
      lam_grid <- seq(max(1e-6, lambda_hat/4), lambda_hat*4 + 1e-6, length.out = 201)
      ll_lam <- sapply(lam_grid, function(l) loglik_poisson(l, x))
      res$profile <- list(lam_grid = lam_grid, ll_lam = ll_lam)
    }

    if(dist == 'Exponential'){
      rate_hat <- 1/mean(x)
      negll1 <- function(r) -loglik_exp(r, x)
      opt <- optimize(negll1, interval = c(1e-8, max(10, rate_hat*4)))
      res$closed_form <- list(rate = rate_hat)
      res$optim <- list(par = opt$minimum, value = opt$objective)
      r_grid <- seq(max(1e-6, rate_hat/5), rate_hat*5 + 1e-6, length.out = 201)
      ll_r <- sapply(r_grid, function(r) loglik_exp(r, x))
      res$profile <- list(r_grid = r_grid, ll_r = ll_r)
    }

    if(dist == 'Binomial'){
      trials <- input$binom_trials
      p_hat <- sum(x) / (n * trials)
      # numeric check via optimize
      negll1 <- function(p) -loglik_binom(p, x, trials)
      opt <- optimize(negll1, interval = c(1e-8, 1-1e-8))
      res$closed_form <- list(p = p_hat)
      res$optim <- list(par = opt$minimum, value = opt$objective)
      p_grid <- seq(max(1e-6, p_hat/5), min(1-1e-6, p_hat*1.5 + 0.1), length.out = 201)
      ll_p <- sapply(p_grid, function(p) loglik_binom(p, x, trials))
      res$profile <- list(p_grid = p_grid, ll_p = ll_p)
    }

    if(dist == 'Gamma'){
      # Use fitdistr for starting values / MLE
      fit <- tryCatch({fitdistr(x, "gamma")}, error = function(e) NULL)
      # numeric optimization using negative log-likelihood
      negll <- function(par) -loglik_gamma(par, x)
      start <- if(!is.null(fit)) c(fit$estimate["shape"], fit$estimate["rate"]) else c(1, 1)
      opt <- optim(par = start, fn = negll, method = "L-BFGS-B", lower = c(1e-8, 1e-8))
      res$fitdistr <- fit
      res$optim <- list(par = opt$par, value = opt$value, convergence = opt$convergence)
      # construct a grid for contour plotting
      shape_hat <- opt$par[1]; rate_hat <- opt$par[2]
      shape_grid <- seq(max(shape_hat/3, 1e-3), shape_hat*3 + 1e-3, length.out = 80)
      rate_grid <- seq(max(rate_hat/3, 1e-3), rate_hat*3 + 1e-3, length.out = 80)
      ll_mat <- outer(shape_grid, rate_grid, Vectorize(function(s,r) loglik_gamma(c(s,r), x)))
      res$profile <- list(shape_grid = shape_grid, rate_grid = rate_grid, ll_mat = ll_mat)
    }

    res
  })

  output$mle_out <- renderPrint({
    x <- data_reactive()
    res <- results()
    dist <- input$dist
    cat("Distribution:", dist, "\n")
    cat("Sample size:", length(x), "\n")
    cat("Data summary:\n")
    print(summary(x))
    cat("\nMLE results:\n")
    if(dist == 'Normal'){
      cat("Closed-form:\n")
      print(res$closed_form)
      cat("Optim check (mu, sigma):\n")
      print(res$optim)
    } else if(dist == 'Poisson'){
      cat("Closed-form lambda_hat = mean(x):\n")
      print(res$closed_form)
      cat("Optim check (lambda):\n")
      print(res$optim)
    } else if(dist == 'Exponential'){
      cat("Closed-form rate_hat = 1/mean(x):\n")
      print(res$closed_form)
      cat("Optim check (rate):\n")
      print(res$optim)
    } else if(dist == 'Binomial'){
      cat(sprintf("Closed-form p_hat = total_successes / (n * trials)\nTrials per obs = %d\n", input$binom_trials))
      print(res$closed_form)
      cat("Optim check (p):\n")
      print(res$optim)
    } else if(dist == 'Gamma'){
      cat("fitdistr() result (if available):\n")
      print(res$fitdistr)
      cat("Optim MLE (shape, rate):\n")
      print(res$optim)
    }
  })

  output$dataPlot <- renderPlot({
    x <- data_reactive()
    dist <- input$dist
    df <- data.frame(x = x)

    if(input$show_hist){
      if(dist %in% c('Normal','Exponential','Gamma')){
        ggplot(df, aes(x = x)) +
          geom_histogram(aes(y = ..density..), bins = 30) +
          geom_density() +
          ggtitle(paste("Histogram and density —", dist))
      } else if(dist == 'Poisson'){
        ggplot(df, aes(x = factor(x))) +
          geom_bar() +
          ggtitle("Counts (Poisson)") + xlab("Value")
      } else if(dist == 'Binomial'){
        ggplot(df, aes(x = factor(x))) +
          geom_bar() +
          ggtitle(paste("Counts (Binomial, trials=", input$binom_trials, ")")) + xlab("Successes")
      }
    } else {
      plot.new(); text(0.5,0.5, "Histogram/Density hidden — check the box to show it.")
    }
  })

  output$loglikPlot <- renderPlot({
    res <- results()
    dist <- input$dist
    x <- data_reactive()
    n <- length(x)

    if(dist == 'Normal'){
      df1 <- data.frame(mu = res$profile$mu_grid, ll = res$profile$ll_mu)
      p1 <- ggplot(df1, aes(x = mu, y = ll)) + geom_line() +
        geom_vline(xintercept = mean(x), linetype = "dashed") +
        ggtitle("Profile log-likelihood for mu (sigma fixed at MLE)")
      df2 <- data.frame(sigma = res$profile$sigma_grid, ll = res$profile$ll_sigma)
      p2 <- ggplot(df2, aes(x = sigma, y = ll)) + geom_line() +
        geom_vline(xintercept = res$closed_form$sigma, linetype = "dashed") +
        ggtitle("Profile log-likelihood for sigma (mu fixed at MLE)")
      gridExtra::grid.arrange(p1, p2, ncol = 1)
    }

    if(dist == 'Poisson'){
      df <- data.frame(lambda = res$profile$lam_grid, ll = res$profile$ll_lam)
      ggplot(df, aes(x = lambda, y = ll)) + geom_line() +
        geom_vline(xintercept = res$closed_form$lambda, linetype = "dashed") +
        ggtitle("Log-likelihood for lambda (Poisson)")
    }

    if(dist == 'Exponential'){
      df <- data.frame(rate = res$profile$r_grid, ll = res$profile$ll_r)
      ggplot(df, aes(x = rate, y = ll)) + geom_line() +
        geom_vline(xintercept = res$closed_form$rate, linetype = "dashed") +
        ggtitle("Log-likelihood for rate (Exponential)")
    }

    if(dist == 'Binomial'){
      df <- data.frame(p = res$profile$p_grid, ll = res$profile$ll_p)
      ggplot(df, aes(x = p, y = ll)) + geom_line() +
        geom_vline(xintercept = res$closed_form$p, linetype = "dashed") +
        ggtitle("Log-likelihood for p (Binomial)")
    }

    if(dist == 'Gamma'){
      # contour plot of ll_mat
      sg <- res$profile$shape_grid
      rg <- res$profile$rate_grid
      llm <- res$profile$ll_mat
      df <- expand.grid(shape = sg, rate = rg)
      df$ll <- as.vector(llm)
      ggplot(df, aes(x = shape, y = rate, z = ll)) +
        geom_contour_filled() +
        geom_point(aes(x = res$optim$par[1], y = res$optim$par[2]), color = "black", size = 3) +
        ggtitle("Log-likelihood contours (Gamma) — dot = MLE")
    }
  })
}

shinyApp(ui, server)
