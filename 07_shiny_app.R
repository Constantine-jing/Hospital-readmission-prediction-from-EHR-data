# ==============================================================================
# Step 7: R Shiny Dashboard — Patient Risk Profiling
# Run with: shiny::runApp("07_shiny_app.R")
# ==============================================================================

library(shiny)
library(data.table)
library(ggplot2)
library(DT)
library(plotly)
# install.packages(c("shiny", "DT", "plotly"))

# ======================================================================
# Load data
# ======================================================================
processed_dir <- "G:/PHD_courses_start_star_star/Hospital-readmission-prediction-from-EHR-data/data/processed"
shap_data <- fread(file.path(processed_dir, "shap_data_step6.csv"))

# Feature display names (cleaner labels for plots)
feature_labels <- c(
  prior_admits_12m = "Prior Admissions (12mo)",
  lab_hemoglobin = "Hemoglobin",
  n_medications = "# Medications",
  approx_age = "Age",
  on_statin = "On Statin",
  los_days = "Length of Stay (days)",
  race_Other = "Race: Other",
  female = "Female",
  lab_glucose = "Glucose",
  admit_hour = "Admission Hour",
  lab_albumin = "Albumin",
  n_neuro_dx = "# Neuro Diagnoses",
  lab_platelet_count = "Platelet Count",
  n_diagnoses = "# Total Diagnoses",
  lab_white_blood_cells = "WBC",
  comorbidity_count = "Comorbidity Count",
  on_opioid = "On Opioid",
  lab_creatinine = "Creatinine",
  has_hypertension = "Hypertension",
  has_depression = "Depression",
  emergency_admit = "Emergency Admission",
  on_anticoagulant = "On Anticoagulant",
  lab_potassium = "Potassium",
  has_ckd = "CKD",
  has_heart_failure = "Heart Failure",
  lab_sodium = "Sodium",
  on_antiepileptic = "On Antiepileptic",
  has_diabetes = "Diabetes",
  on_antidepressant = "On Antidepressant",
  has_copd = "COPD",
  married = "Married",
  public_insurance = "Public Insurance",
  has_epilepsy = "Epilepsy",
  on_insulin = "On Insulin",
  weekend_admit = "Weekend Admission",
  has_sleep_disorder = "Sleep Disorder",
  no_labs_flag = "No Labs Taken",
  has_parkinsons = "Parkinson's",
  race_Black = "Race: Black",
  race_Hispanic = "Race: Hispanic",
  race_Asian = "Race: Asian"
)

# Identify SHAP and feature columns
shap_cols <- grep("^shap_", names(shap_data), value = TRUE)
feat_names <- gsub("^shap_", "", shap_cols)

# ======================================================================
# UI
# ======================================================================
ui <- fluidPage(
  
  titlePanel("Hospital Readmission Risk Profiling Dashboard"),
  
  tabsetPanel(
    
    # ------ Tab 1: Population Overview ------
    tabPanel("Population Overview",
      fluidRow(
        column(6,
          h4("Predicted Risk Distribution"),
          plotlyOutput("risk_dist_plot", height = "350px")
        ),
        column(6,
          h4("Readmission Rate by Risk Group"),
          plotlyOutput("risk_group_plot", height = "350px")
        )
      ),
      fluidRow(
        column(12,
          h4("Global Feature Importance (Mean |SHAP|)"),
          plotlyOutput("global_importance_plot", height = "500px")
        )
      )
    ),
    
    # ------ Tab 2: Individual Patient ------
    tabPanel("Individual Patient",
      fluidRow(
        column(4,
          h4("Select a Patient"),
          selectInput("patient_id", "Admission ID (hadm_id):",
                      choices = NULL),  # populated server-side
          actionButton("random_btn", "Random Patient"),
          hr(),
          h4("Patient Summary"),
          tableOutput("patient_info")
        ),
        column(8,
          h4("SHAP Waterfall — What Drives This Patient's Risk?"),
          plotlyOutput("waterfall_plot", height = "500px")
        )
      )
    ),
    
    # ------ Tab 3: Subgroup Analysis ------
    tabPanel("Subgroup Analysis",
      fluidRow(
        column(4,
          selectInput("subgroup_var", "Group by:",
                      choices = c("Epilepsy" = "has_epilepsy",
                                  "Sleep Disorder" = "has_sleep_disorder",
                                  "Parkinson's" = "has_parkinsons",
                                  "Heart Failure" = "has_heart_failure",
                                  "Depression" = "has_depression",
                                  "CKD" = "has_ckd",
                                  "Emergency Admission" = "emergency_admit",
                                  "No Labs Taken" = "no_labs_flag",
                                  "Female" = "female"))
        ),
        column(8,
          h4("Risk Distribution by Subgroup"),
          plotlyOutput("subgroup_risk_plot", height = "350px")
        )
      ),
      fluidRow(
        column(12,
          h4("Readmission Statistics by Subgroup"),
          tableOutput("subgroup_table")
        )
      )
    ),
    
    # ------ Tab 4: Data Explorer ------
    tabPanel("Data Explorer",
      fluidRow(
        column(12,
          h4("Patient-Level Data (sample)"),
          DTOutput("data_table")
        )
      )
    )
  )
)

# ======================================================================
# Server
# ======================================================================
server <- function(input, output, session) {
  
  # --- Populate patient selector ---
  observe({
    choices <- sort(shap_data$hadm_id)
    updateSelectInput(session, "patient_id", choices = choices, selected = choices[1])
  })
  
  # --- Random patient button ---
  observeEvent(input$random_btn, {
    rand_id <- sample(shap_data$hadm_id, 1)
    updateSelectInput(session, "patient_id", selected = rand_id)
  })
  
  # ===== Tab 1: Population Overview =====
  
  output$risk_dist_plot <- renderPlotly({
    p <- ggplot(shap_data, aes(x = pred, fill = factor(y_true))) +
      geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("0" = "#4DAF4A", "1" = "#E41A1C"),
                        labels = c("No Readmission", "Readmitted")) +
      labs(x = "Predicted Probability", y = "Count", fill = "Outcome") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$risk_group_plot <- renderPlotly({
    dt <- copy(shap_data)
    dt[, risk_group := cut(pred, breaks = c(0, 0.1, 0.2, 0.3, 0.5, 1.0),
                           labels = c("<10%", "10-20%", "20-30%", "30-50%", ">50%"),
                           include.lowest = TRUE)]
    summary_dt <- dt[, .(
      n = .N,
      readmit_rate = mean(y_true) * 100
    ), by = risk_group][order(risk_group)]
    
    p <- ggplot(summary_dt, aes(x = risk_group, y = readmit_rate, fill = risk_group)) +
      geom_col() +
      geom_text(aes(label = paste0(round(readmit_rate, 1), "%\n(n=", n, ")")),
                vjust = -0.3, size = 3) +
      scale_fill_brewer(palette = "YlOrRd") +
      labs(x = "Risk Group", y = "Actual Readmission Rate (%)") +
      theme_minimal() +
      theme(legend.position = "none") +
      ylim(0, max(summary_dt$readmit_rate) * 1.2)
    ggplotly(p)
  })
  
  output$global_importance_plot <- renderPlotly({
    mean_shap <- sapply(shap_cols, function(col) mean(abs(shap_data[[col]])))
    names(mean_shap) <- feat_names
    mean_shap <- sort(mean_shap, decreasing = FALSE)
    top20 <- tail(mean_shap, 20)
    
    dt <- data.table(
      feature = factor(names(top20), levels = names(top20)),
      importance = top20,
      label = ifelse(names(top20) %in% names(feature_labels),
                     feature_labels[names(top20)], names(top20))
    )
    
    p <- ggplot(dt, aes(x = importance, y = feature)) +
      geom_col(fill = "#377EB8") +
      scale_y_discrete(labels = dt$label) +
      labs(x = "Mean |SHAP value|", y = NULL) +
      theme_minimal()
    ggplotly(p)
  })
  
  # ===== Tab 2: Individual Patient =====
  
  selected_patient <- reactive({
    req(input$patient_id)
    shap_data[hadm_id == as.integer(input$patient_id)]
  })
  
  output$patient_info <- renderTable({
    pt <- selected_patient()
    if (nrow(pt) == 0) return(NULL)
    
    info <- data.frame(
      Field = c("Admission ID", "Predicted Risk", "Actual Outcome",
                "Age", "Sex", "LOS (days)", "# Medications",
                "Prior Admits (12mo)", "# Diagnoses", "Emergency"),
      Value = c(
        pt$hadm_id,
        paste0(round(pt$pred * 100, 1), "%"),
        ifelse(pt$y_true == 1, "Readmitted", "Not Readmitted"),
        round(pt$approx_age),
        ifelse(pt$female == 1, "Female", "Male"),
        round(pt$los_days, 1),
        pt$n_medications,
        pt$prior_admits_12m,
        pt$n_diagnoses,
        ifelse(pt$emergency_admit == 1, "Yes", "No")
      )
    )
    info
  }, striped = TRUE, hover = TRUE)
  
  output$waterfall_plot <- renderPlotly({
    pt <- selected_patient()
    if (nrow(pt) == 0) return(NULL)
    
    # Get SHAP values for this patient
    shap_vals <- unlist(pt[, ..shap_cols])
    names(shap_vals) <- feat_names
    
    # Sort by absolute value, take top 15
    top_idx <- order(abs(shap_vals), decreasing = TRUE)[1:15]
    top_shap <- shap_vals[top_idx]
    
    # Create labels
    labels <- ifelse(names(top_shap) %in% names(feature_labels),
                     feature_labels[names(top_shap)], names(top_shap))
    
    # Add feature values to labels
    feat_vals <- unlist(pt[, names(top_shap), with = FALSE])
    labels <- paste0(labels, " = ", round(feat_vals, 2))
    
    dt <- data.table(
      feature = factor(labels, levels = rev(labels)),
      shap = top_shap,
      direction = ifelse(top_shap > 0, "Increases Risk", "Decreases Risk")
    )
    
    p <- ggplot(dt, aes(x = shap, y = feature, fill = direction)) +
      geom_col() +
      scale_fill_manual(values = c("Increases Risk" = "#E41A1C", "Decreases Risk" = "#4DAF4A")) +
      labs(x = "SHAP Value (impact on prediction)", y = NULL, fill = NULL,
           title = paste0("Predicted Risk: ", round(pt$pred * 100, 1), "%")) +
      theme_minimal() +
      theme(legend.position = "bottom")
    ggplotly(p)
  })
  
  # ===== Tab 3: Subgroup Analysis =====
  
  output$subgroup_risk_plot <- renderPlotly({
    var <- input$subgroup_var
    dt <- copy(shap_data)
    dt[, group := factor(dt[[var]], levels = c(0, 1), labels = c("No", "Yes"))]
    
    p <- ggplot(dt, aes(x = pred, fill = group)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = c("No" = "#4DAF4A", "Yes" = "#E41A1C")) +
      labs(x = "Predicted Readmission Probability", y = "Density",
           fill = names(which(c("Epilepsy" = "has_epilepsy",
                                "Sleep Disorder" = "has_sleep_disorder",
                                "Parkinson's" = "has_parkinsons",
                                "Heart Failure" = "has_heart_failure",
                                "Depression" = "has_depression",
                                "CKD" = "has_ckd",
                                "Emergency Admission" = "emergency_admit",
                                "No Labs Taken" = "no_labs_flag",
                                "Female" = "female") == var))) +
      theme_minimal()
    ggplotly(p)
  })
  
  output$subgroup_table <- renderTable({
    var <- input$subgroup_var
    dt <- copy(shap_data)
    dt[, group := dt[[var]]]
    
    summary_dt <- dt[, .(
      N = .N,
      `Mean Risk (%)` = round(mean(pred) * 100, 1),
      `Actual Readmit (%)` = round(mean(y_true) * 100, 1),
      `Mean Age` = round(mean(approx_age), 1),
      `Mean LOS` = round(mean(los_days), 1),
      `Mean # Meds` = round(mean(n_medications), 1)
    ), by = group]
    
    summary_dt[, group := ifelse(group == 1, "Yes", "No")]
    setnames(summary_dt, "group", "Has Condition")
    summary_dt
  }, striped = TRUE, hover = TRUE)
  
  # ===== Tab 4: Data Explorer =====
  
  output$data_table <- renderDT({
    display_cols <- c("hadm_id", "pred", "y_true", "approx_age", "female",
                      "los_days", "n_medications", "prior_admits_12m",
                      "n_diagnoses", "emergency_admit",
                      "has_epilepsy", "has_sleep_disorder", "has_parkinsons")
    dt <- shap_data[, ..display_cols]
    dt[, pred := round(pred, 3)]
    setnames(dt, c("ID", "Pred Risk", "Readmitted", "Age", "Female",
                   "LOS", "# Meds", "Prior Admits", "# Dx", "Emergency",
                   "Epilepsy", "Sleep Disorder", "Parkinson's"))
    datatable(dt, options = list(pageLength = 20, scrollX = TRUE),
              filter = "top")
  })
}

# ======================================================================
# Run
# ======================================================================
shinyApp(ui = ui, server = server)
