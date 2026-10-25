# =============================================================================
# app.R – DeliveryPulse: Interactive Dispatch & Operational Risk Dashboard
# =============================================================================

# Set user library path
userlib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6")
.libPaths(c(userlib, .libPaths()))

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)

# Set working directory relative to project root if executed directly
if (basename(getwd()) == "app") {
  setwd("..")
}

# -----------------------------------------------------------------------------
# 1. Load Pre-Trained Models and Test Artifacts
# -----------------------------------------------------------------------------
logit_model <- readRDS("models/logit_model.rds")
nb_model    <- readRDS("models/nb_model.rds")
bundle      <- readRDS("models/test_predictions.rds")

test_df          <- bundle$test_data
pred_prob_logit  <- bundle$pred_prob_logit
pred_class_logit <- bundle$pred_class_logit
pred_prob_nb     <- bundle$pred_prob_nb

# Add predictions into dispatch table
dispatch_data <- data.frame(
  OrderID         = if ("ID" %in% names(test_df)) test_df$ID else seq_len(nrow(test_df)),
  RiderID         = if ("Delivery_person_ID" %in% names(test_df)) test_df$Delivery_person_ID else paste0("RIDER_", seq_len(nrow(test_df))),
  Distance_km     = round(test_df$distance_km, 2),
  Traffic         = test_df$Road_traffic_density,
  Weather         = test_df$Weatherconditions,
  Rider_Rating    = round(test_df$Delivery_person_Ratings, 2),
  Rider_Age       = test_df$Delivery_person_Age,
  Vehicle         = test_df$Type_of_vehicle,
  Festival        = test_df$Festival,
  Actual_Outcome  = test_df$LateF,
  Pred_Prob_Late  = round(pred_prob_logit, 4),
  Risk_Tier       = ifelse(pred_prob_logit >= 0.70, "Critical",
                    ifelse(pred_prob_logit >= 0.40, "Moderate", "Low")),
  stringsAsFactors = FALSE
)

# Sort dispatch queue descending by risk probability
dispatch_data <- dispatch_data[order(-dispatch_data$Pred_Prob_Late), ]

# Extract coefficients for Command Center
coef_df <- as.data.frame(summary(logit_model)$coefficients)
coef_df$Feature <- rownames(coef_df)
coef_df <- coef_df[coef_df$Feature != "(Intercept)", ]
coef_df$Odds_Ratio <- exp(coef_df$Estimate)
coef_df$Abs_z <- abs(coef_df$`z value`)
coef_df <- coef_df[order(-coef_df$Abs_z), ]
top_features <- head(coef_df, 10)

# -----------------------------------------------------------------------------
# 2. Shiny Dashboard UI
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "DeliveryPulse Live"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Command Center", tabName = "command", icon = icon("tachometer-alt")),
      menuItem("Dispatch Queue",  tabName = "queue",   icon = icon("list-ol")),
      menuItem("Order Explorer", tabName = "explorer",icon = icon("search")),
      menuItem("Model & Reliability", tabName = "reliability", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f6f9; }
        .box { border-top: 3px solid #1F4E78; }
        .small-box { border-radius: 6px; }
      "))
    ),
    
    tabItems(
      # -----------------------------------------------------------------------
      # TAB 1: Command Center
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "command",
        fluidRow(
          valueBoxOutput("vbox_total", width = 3),
          valueBoxOutput("vbox_late_rate", width = 3),
          valueBoxOutput("vbox_high_risk", width = 3),
          valueBoxOutput("vbox_accuracy", width = 3)
        ),
        fluidRow(
          box(
            title = "Test-Set Predicted Delay Probability Distribution",
            status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("plot_hist", height = "360px")
          ),
          box(
            title = "Top 10 Risk Predictors (Ranked by |z-score| in Logistic GLM)",
            status = "primary", solidHeader = TRUE, width = 6,
            plotlyOutput("plot_predictors", height = "360px")
          )
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 2: Dispatch Queue
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "queue",
        fluidRow(
          box(
            title = "Real-Time In-Flight Dispatch Priority Queue (Ranked by P(Late))",
            status = "primary", solidHeader = TRUE, width = 12,
            p("High-risk deliveries requiring immediate dispatch intervention (rider re-routing or customer notifications) are positioned at the top:"),
            DTOutput("table_queue")
          )
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 3: Order Explorer
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(
            title = "Scenario Simulator: Order & Route Profile",
            status = "primary", solidHeader = TRUE, width = 4,
            sliderInput("in_dist", "Transit Distance (km):", min = 1, max = 25, value = 10, step = 0.5),
            sliderInput("in_rating", "Rider Historical Rating:", min = 2.0, max = 5.0, value = 4.6, step = 0.1),
            sliderInput("in_age", "Delivery Partner Age:", min = 18, max = 50, value = 28, step = 1),
            selectInput("in_traffic", "Road Traffic Density:",
                        choices = levels(test_df$Road_traffic_density), selected = "Jam"),
            selectInput("in_weather", "Atmospheric Weather:",
                        choices = levels(test_df$Weatherconditions), selected = "Stormy"),
            selectInput("in_vehicle", "Vehicle Type:",
                        choices = levels(test_df$Type_of_vehicle), selected = "motorcycle"),
            selectInput("in_multiple", "Multiple Deliveries Assigned:",
                        choices = levels(test_df$multiple_deliveries), selected = "1"),
            selectInput("in_festival", "Festival Day:",
                        choices = levels(test_df$Festival), selected = "No"),
            selectInput("in_city", "City Hub Type:",
                        choices = levels(test_df$City), selected = "Metropolitian")
          ),
          box(
            title = "Real-Time Risk Assessment & Attribution",
            status = "primary", solidHeader = TRUE, width = 8,
            fluidRow(
              valueBoxOutput("vbox_sim_prob", width = 6),
              valueBoxOutput("vbox_sim_tier", width = 6)
            ),
            h4("Feature Impact Breakdown vs. Average Baseline:"),
            plotlyOutput("plot_attribution", height = "320px"),
            hr(),
            p(strong("Operational Recommendation: "), textOutput("text_recommendation", inline = TRUE))
          )
        )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 4: Model & Reliability
      # -----------------------------------------------------------------------
      tabItem(
        tabName = "reliability",
        fluidRow(
          box(
            title = "Hold-Out Test Performance Metrics",
            status = "primary", solidHeader = TRUE, width = 6,
            DTOutput("table_model_eval"),
            hr(),
            h4("Operational Dispatch Triage Metric (Top 20% Capacity):"),
            p("By auditing only the top 20% highest-risk orders (2,474 orders), the platform captures ",
              strong("2,342 out of 3,787 (61.84%) of all delayed orders"),
              "— achieving a ", strong("3.09x operational lift"), " over random auditing.")
          ),
          box(
            title = "ROC Discrimination Curves",
            status = "primary", solidHeader = TRUE, width = 6,
            imageOutput("img_roc", height = "380px")
          )
        ),
        fluidRow(
          box(
            title = "Hypothesis Testing & Cramer's V Association (BH-Adjusted)",
            status = "primary", solidHeader = TRUE, width = 12,
            DTOutput("table_chi")
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# 3. Shiny Server Logic
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Value Boxes (Command Center) ---
  output$vbox_total <- renderValueBox({
    valueBox(format(nrow(test_df), big.mark = ","), "Held-Out Test Orders", icon = icon("shopping-bag"), color = "blue")
  })
  
  output$vbox_late_rate <- renderValueBox({
    rate <- mean(test_df$LateF == "Late") * 100
    valueBox(sprintf("%.2f%%", rate), "Baseline Delay Rate", icon = icon("clock"), color = "red")
  })
  
  output$vbox_high_risk <- renderValueBox({
    high_n <- sum(pred_prob_logit >= 0.50)
    valueBox(format(high_n, big.mark = ","), "High-Risk Orders (P >= 0.5)", icon = icon("exclamation-triangle"), color = "yellow")
  })
  
  output$vbox_accuracy <- renderValueBox({
    valueBox("88.42%", "Logistic GLM Test Accuracy", icon = icon("check-circle"), color = "green")
  })
  
  # --- Plots (Command Center) ---
  output$plot_hist <- renderPlotly({
    p <- ggplot(data.frame(Prob = pred_prob_logit), aes(x = Prob)) +
      geom_histogram(bins = 30, fill = "#2166ac", color = "white", alpha = 0.85) +
      geom_vline(xintercept = 0.50, color = "#b2182b", linetype = "dashed", size = 1) +
      annotate("text", x = 0.52, y = 1500, label = "Cutoff = 0.50", color = "#b2182b", hjust = 0, fontface = "bold") +
      theme_minimal() +
      labs(x = "Predicted Probability P(Late)", y = "Frequency",
           title = "Bimodal Separation of Late Probability")
    ggplotly(p)
  })
  
  output$plot_predictors <- renderPlotly({
    top_clean <- top_features
    top_clean$CleanName <- gsub("^Road_traffic_density", "Traffic: ", top_clean$Feature)
    top_clean$CleanName <- gsub("^Weatherconditions", "Weather: ", top_clean$CleanName)
    top_clean$CleanName <- gsub("^multiple_deliveries", "Stacked Deliveries: ", top_clean$CleanName)
    top_clean$CleanName <- gsub("^Type_of_vehicle", "Vehicle: ", top_clean$CleanName)
    top_clean$CleanName <- gsub("^City", "City: ", top_clean$CleanName)
    
    p <- ggplot(top_clean, aes(x = reorder(CleanName, Abs_z), y = Abs_z, fill = Abs_z)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_gradient(low = "#9ecae1", high = "#08519c") +
      theme_minimal() +
      labs(x = "", y = "Absolute z-statistic (|z|)", title = "Relative Predictor Importance")
    ggplotly(p)
  })
  
  # --- Dispatch Queue Table ---
  output$table_queue <- renderDT({
    datatable(
      head(dispatch_data, 500),
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Risk_Tier",
        backgroundColor = styleEqual(c("Critical", "Moderate", "Low"), c("#ffcdd2", "#fff9c4", "#c8e6c9")),
        fontWeight = "bold"
      ) %>%
      formatPercentage("Pred_Prob_Late", 1)
  })
  
  # --- Scenario Simulation (Order Explorer) ---
  sim_pred <- reactive({
    new_obs <- data.frame(
      distance_km             = input$in_dist,
      Delivery_person_Age     = input$in_age,
      Delivery_person_Ratings = input$in_rating,
      Road_traffic_density    = factor(input$in_traffic, levels = levels(test_df$Road_traffic_density)),
      Weatherconditions       = factor(input$in_weather, levels = levels(test_df$Weatherconditions)),
      Type_of_vehicle         = factor(input$in_vehicle, levels = levels(test_df$Type_of_vehicle)),
      Festival                = factor(input$in_festival, levels = levels(test_df$Festival)),
      multiple_deliveries     = factor(input$in_multiple, levels = levels(test_df$multiple_deliveries)),
      City                    = factor(input$in_city, levels = levels(test_df$City))
    )
    prob <- predict(logit_model, newdata = new_obs, type = "response")
    prob
  })
  
  output$vbox_sim_prob <- renderValueBox({
    p_val <- sim_pred()
    valueBox(sprintf("%.1f%%", p_val * 100), "Predicted Late Probability",
             icon = icon("tachometer-alt"),
             color = ifelse(p_val >= 0.70, "red", ifelse(p_val >= 0.40, "yellow", "green")))
  })
  
  output$vbox_sim_tier <- renderValueBox({
    p_val <- sim_pred()
    tier <- ifelse(p_val >= 0.70, "Critical Risk (Intervene)",
            ifelse(p_val >= 0.40, "Moderate Risk (Monitor)", "Normal Flow (On-Time)"))
    valueBox(tier, "Action Tier",
             icon = icon("bell"),
             color = ifelse(p_val >= 0.70, "red", ifelse(p_val >= 0.40, "yellow", "green")))
  })
  
  output$plot_attribution <- renderPlotly({
    # Compute relative component log-odds shift
    betas <- coef(logit_model)
    dist_shift <- (input$in_dist - mean(test_df$distance_km)) * betas["distance_km"]
    rating_shift <- (input$in_rating - mean(test_df$Delivery_person_Ratings)) * betas["Delivery_person_Ratings"]
    age_shift <- (input$in_age - mean(test_df$Delivery_person_Age)) * betas["Delivery_person_Age"]
    
    attr_df <- data.frame(
      Factor = c("Distance", "Rider Rating", "Rider Age", "Traffic State"),
      LogOdds_Impact = c(dist_shift, rating_shift, age_shift,
                         ifelse(input$in_traffic == "Jam", 0.13, ifelse(input$in_traffic == "Low", -3.63, -0.92))),
      Direction = c(ifelse(dist_shift > 0, "Increases Delay", "Reduces Delay"),
                    ifelse(rating_shift > 0, "Increases Delay", "Reduces Delay"),
                    ifelse(age_shift > 0, "Increases Delay", "Reduces Delay"),
                    "Environmental")
    )
    
    p <- ggplot(attr_df, aes(x = Factor, y = LogOdds_Impact, fill = LogOdds_Impact > 0)) +
      geom_col(width = 0.5, show.legend = FALSE) +
      scale_fill_manual(values = c("TRUE" = "#e41a1c", "FALSE" = "#2b8cbe")) +
      coord_flip() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme_minimal() +
      labs(x = "", y = "Log-Odds Contribution (+ pushes Late, - pushes On-Time)",
           title = "Simulated Order Feature Attribution")
    ggplotly(p)
  })
  
  output$text_recommendation <- renderText({
    p_val <- sim_pred()
    if (p_val >= 0.70) {
      "CRITICAL RISK: Proactively notify customer with a 15-minute ETA extension; reassign order if nearby higher-rated rider is idle."
    } else if (p_val >= 0.40) {
      "MODERATE RISK: Prioritize kitchen pickup and monitor transit speed; avoid adding stacked deliveries."
    } else {
      "OPTIMAL: Order within standard SLA; no dispatch intervention required."
    }
  })
  
  # --- Model & Reliability Tab ---
  output$table_model_eval <- renderDT({
    eval_df <- data.frame(
      Model = c("Logistic Regression", "Naive Bayes Classifier"),
      Accuracy = c("88.42%", "88.61%"),
      Sensitivity_Recall = c("75.94%", "69.82%"),
      Specificity = c("93.93%", "96.90%"),
      ROC_AUC = c("0.9386", "0.9397")
    )
    datatable(eval_df, options = list(dom = 't'), rownames = FALSE)
  })
  
  output$img_roc <- renderImage({
    list(src = "plots/08_roc_curves.png", contentType = "image/png", width = "95%", height = "auto")
  }, deleteFile = FALSE)
  
  output$table_chi <- renderDT({
    chi_summary <- data.frame(
      Feature = c("Road_traffic_density", "multiple_deliveries", "Weatherconditions",
                  "Festival", "Vehicle_condition", "City", "Type_of_vehicle", "Type_of_order"),
      Chi_Square = c("6031.71", "5886.09", "2438.82", "1926.41", "1341.86", "1075.67", "458.18", "0.88"),
      df = c(3, 3, 5, 1, 2, 2, 2, 3),
      Cramers_V = c("0.3825", "0.3779", "0.2432", "0.2162", "0.1804", "0.1615", "0.1054", "0.0046"),
      p_value_BH = c("< 0.0001", "< 0.0001", "< 0.0001", "< 0.0001", "< 0.0001", "< 0.0001", "< 0.0001", "0.8300"),
      Significance = c("Significant", "Significant", "Significant", "Significant", "Significant", "Significant", "Significant", "Not Significant")
    )
    datatable(chi_summary, options = list(pageLength = 8, dom = 't'), rownames = FALSE)
  })
}

# -----------------------------------------------------------------------------
# 4. Launch Application
# -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
