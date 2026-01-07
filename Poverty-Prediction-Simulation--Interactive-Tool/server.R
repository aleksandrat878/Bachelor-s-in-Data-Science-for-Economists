# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(xgboost)
library(tidyr)
library(tidyverse)
library(DT)

# Load global objects
final_model    <- readRDS("final_xgb_model.rds")
train_cols     <- readRDS("train_cols.rds")
poverty1_train <- readRDS("poverty1_train.rds")
phone_freq <- poverty %>% count(phone_technology) %>% mutate(phone_technology = as.factor(phone_technology))

# Immediately after loading your global objects:
poverty1_train$country <- factor(as.character(poverty1_train$country), 
                                 levels = c("A", "C", "D", "F", "G", "I", "J"))

poverty1_train$employment_type_last_year <- factor(as.character(poverty1_train$employment_type_last_year), 
                                                   levels = c("not_working", "irregular_seasonal", "self_employed", "salaried", "other"))

poverty1_train$relationship_to_hh_head <- factor(as.character(poverty1_train$relationship_to_hh_head), 
                                                 levels = c("Other", "Head", "Spouse", "Son/Daughter", "Father/Mother", "Sister/Brother", "Unknown"))


predict_function <- function(model, newdata) {
  # Ensure 'poverty_probability' exists; if not, add a dummy column.
  if(!"poverty_probability" %in% colnames(newdata)) {
    newdata$poverty_probability <- 0
  }
  
  # Force the full set of factor levels for each categorical predictor.
  newdata$country <- factor(as.character(newdata$country),
                            levels = c("A", "C", "D", "F", "G", "I", "J"),
                            exclude = NULL)
  newdata$employment_type_last_year <- factor(as.character(newdata$employment_type_last_year),
                                              levels = c("not_working", "irregular_seasonal", "self_employed", "salaried", "other"),
                                              exclude = NULL)
  newdata$relationship_to_hh_head <- factor(as.character(newdata$relationship_to_hh_head),
                                            levels = c("Other", "Head", "Spouse", "Son/Daughter", "Father/Mother", "Sister/Brother", "Unknown"),
                                            exclude = NULL)
  
  # Convert newdata to a model matrix using the same formula as training.
  new_matrix <- model.matrix(poverty_probability ~ . - 1, data = newdata)
  
  # Identify missing columns relative to train_cols.
  missing_cols <- setdiff(train_cols, colnames(new_matrix))
  if (length(missing_cols) > 0) {
    missing_mat <- matrix(0, nrow = nrow(new_matrix), ncol = length(missing_cols))
    colnames(missing_mat) <- missing_cols
    new_matrix <- cbind(new_matrix, missing_mat)
  }
  
  # Reorder the columns to match train_cols exactly.
  new_matrix <- new_matrix[, train_cols, drop = FALSE]
  
  # Generate predictions.
  predict(model, newdata = new_matrix)
}




# Define the Shiny server function
server <- function(input, output, session) {
  
  ## SECTION 1: PREDICTOR TAB FUNCTIONS
  
  # Create a reactive data frame from individual user inputs
  input_data <- reactive({
    req(input$start > 0)
    data.frame(
      country = factor(input$country, levels = unique(poverty1_train$country)),
      is_urban = factor(ifelse(input$is_urban == 1, "Yes", "No"), levels = c("Yes", "No")),
      literacy = factor(ifelse(input$literacy == 1, "Yes", "No"), levels = c("Yes", "No")),
      female = factor(ifelse(input$female == 1, "Yes", "No"), levels = c("Yes", "No")),
      married = factor(ifelse(input$married == 1, "Yes", "No"), levels = c("Yes", "No")),
      num_financial_activities_last_year = as.numeric(input$num_financial_activities_last_year),
      employment_type_last_year = factor(input$employment_type_last_year, 
                                         levels = unique(poverty1_train$employment_type_last_year)),
      relationship_to_hh_head = factor(input$relationship_to_hh_head, 
                                       levels = unique(poverty1_train$relationship_to_hh_head)),
      education_level = factor(input$education_level, 
                               levels = unique(poverty1_train$education_level)),
      can_text = factor(ifelse(input$can_text == 1, "Yes", "No"), levels = c("Yes", "No")),
      phone_technology = factor(ifelse(input$phone_technology == 1, "Yes", "No"), levels = c("Yes", "No"))
    )
  })
  
  
  
  ## SECTION 2: GROUP ANALYSIS & DATA FILTERING
  
  # Reactive filtering of poverty1_train based on user inputs
  filtered_data <- reactive({
    df <- poverty1_train
    if (input$use_country)      df <- filter(df, country == input$country)
    if (input$use_urban)        df <- filter(df, is_urban == as.numeric(input$is_urban))
    if (input$use_literacy)     df <- filter(df, literacy == as.numeric(input$literacy))
    if (input$use_female)       df <- filter(df, female == as.numeric(input$female))
    if (input$use_married)      df <- filter(df, married == as.numeric(input$married))
    if (input$use_financial)    df <- filter(df, num_financial_activities_last_year == input$num_financial_activities_last_year)
    if (input$use_employment)   df <- filter(df, employment_type_last_year == input$employment_type_last_year)
    if (input$use_relationship) df <- filter(df, relationship_to_hh_head == input$relationship_to_hh_head)
    if (input$use_education)    df <- filter(df, education_level == input$education_level)
    if (input$use_can_text)     df <- filter(df, can_text == as.numeric(input$can_text))
    if (input$use_phone)        df <- filter(df, phone_technology == as.numeric(input$phone_technology))
    df
  })
  
  
  # Compute group-level risk from filtered poverty1_train data
  group_risk <- reactive({
    df <- filtered_data()
    if (nrow(df) == 0) return(NA_real_)
    mean(predict_function(final_model, df), na.rm = TRUE)
  })
  
  # Compute the share of the total poverty1_train population in the filtered group
  group_share <- reactive({
    df <- filtered_data()
    nrow(df) / nrow(poverty1_train) * 100
  })
  
  # Categorize the group risk
  risk_category <- reactive({
    r <- group_risk()
    if (is.na(r)) return("No data")
    if (r < 0.25) return("Low Risk")
    if (r < 0.60) return("Moderate Risk")
    "High Risk"
  })
  
  # Render a summary text of the group-level risk analysis
  output$group_summary <- renderUI({
    risk_val <- predicted_risk()  # already computed overall predicted risk
    # Use HTML so that the risk value is rendered in bold
    HTML(sprintf("The average predicted poverty risk (across all age groups) is <strong>%.3f</strong>.", risk_val))
  })
  
  
  # 1) Use a normal reactive instead of eventReactive
  predicted_risk <- reactive({
    df <- all_age_predictions()
    if (nrow(df) == 0) return(NA_real_)
    
    # The mean of average_risk across the 3 groups
    mean(df$average_risk, na.rm = TRUE)
  })
  
  # NEW: Compute predictions for each of the 3 age groups
  all_age_predictions <- reactive({
    req(input$start > 0)
    
    # Start with the full training data
    df <- poverty1_train
    
    # Apply the same user filters except for age_group
    if (input$use_country) {
      df <- df %>% filter(country == input$country)
    }
    if (input$use_urban) {
      df <- df %>% filter(is_urban == as.numeric(input$is_urban))
    }
    
    if (input$use_literacy) {
      df <- df %>% filter(literacy == as.numeric(input$literacy))
    }
    if (input$use_female) {
      df <- df %>% filter(female == as.numeric(input$female))
    }
    if (input$use_married) {
      df <- df %>% filter(married == as.numeric(input$married))
    }
    if (input$use_financial) {
      df <- df %>% filter(num_financial_activities_last_year == input$num_financial_activities_last_year)
    }
    if (input$use_employment) {
      df <- df %>% filter(employment_type_last_year == input$employment_type_last_year)
    }
    if (input$use_relationship) {
      df <- df %>% filter(relationship_to_hh_head == input$relationship_to_hh_head)
    }
    if (input$use_education) {
      df <- df %>% filter(education_level == input$education_level)
    }
    if (input$use_can_text) {
      df <- df %>% filter(can_text == as.numeric(input$can_text))
    }
    if (input$use_phone) {
      df <- df %>% filter(phone_technology == as.numeric(input$phone_technology))
    }
    
    # If filtering results in no rows, return an empty data frame
    if (nrow(df) == 0) {
      return(data.frame(age_group = numeric(0), average_risk = numeric(0), n = numeric(0)))
    }
    
    # Force the full set of factor levels on the problematic variables
    df$country <- factor(as.character(df$country), levels = c("A", "C", "D", "F", "G", "I", "J"))
    df$employment_type_last_year <- factor(as.character(df$employment_type_last_year),
                                           levels = c("not_working", "irregular_seasonal", "self_employed", "salaried", "other"))
    df$relationship_to_hh_head <- factor(as.character(df$relationship_to_hh_head),
                                         levels = c("Other", "Head", "Spouse", "Son/Daughter", "Father/Mother", "Sister/Brother", "Unknown"))
    
    # Now predict using your predict_function
    df <- df %>% mutate(predicted_risk = predict_function(final_model, df))
    
    # Group by age_group and compute average predicted risk
    df <- df %>%
      group_by(age_group) %>%
      summarise(
        average_risk = mean(predicted_risk, na.rm = TRUE),
        n = n()
      ) %>%
      ungroup()
    
    df
  })
  
  
  # NEW: Render textual interpretation for each age group prediction with dependency check
  output$age_group_interpretation <- renderUI({
    req(input$start > 0)
    
    # 1) Retrieve grouped predictions (averages per age group)
    preds <- all_age_predictions()
    if (nrow(preds) == 0) {
      return(HTML("No data available after applying the selected filters."))
    }
    
    # 2) Determine if any filter is active
    any_filter_selected <- (
      input$use_country     ||
        input$use_urban       ||
        input$use_literacy    ||
        input$use_female      ||
        input$use_married     ||
        input$use_financial   ||
        input$use_employment  ||
        input$use_relationship||
        input$use_education   ||
        input$use_can_text    ||
        input$use_phone
    )
    
    # 3) Build the introductory text
    explanatory_text <- if (any_filter_selected) {
      "Based on the chosen filters, the distribution above shows predicted risks across each age group."
    } else {
      "Based on the three age categories, the distribution shows predicted risks across each group."
    }
    
    # 4) Convert numeric age_group to descriptive labels
    labeled <- preds %>%
      mutate(
        label = case_when(
          age_group == 1 ~ "Young (15–25)",
          age_group == 2 ~ "Middle‑aged (26–45)",
          age_group == 3 ~ "Older (46+)",
          TRUE ~ as.character(age_group)
        )
      )
    
    # 5) Construct risk statements for each group
    interpretations <- sapply(seq_len(nrow(labeled)), function(i) {
      risk_val <- labeled$average_risk[i]
      risk_cat <- if (risk_val < 0.25) {
        '<span style="background-color: green; color: white; padding: 2px 4px; border-radius: 3px;">Low Risk</span>'
      } else if (risk_val < 0.60) {
        '<span style="background-color: yellow; color: black; padding: 2px 4px; border-radius: 3px;">Moderate Risk</span>'
      } else {
        '<span style="background-color: red; color: white; padding: 2px 4px; border-radius: 3px;">High Risk</span>'
      }
      sprintf("For the %s group, the predicted poverty risk is <strong>%.3f</strong>, which is %s.",
              labeled$label[i], risk_val, risk_cat)
    })
    
    interpretations <- unname(interpretations)
    HTML(paste(explanatory_text, paste(interpretations, collapse = "<br>"), sep = "<br><br>"))
  })
  
  
  output$risk_distribution <- renderPlot({
    # Define age group labels.
    age_levels <- c("Young (15–25)", "Middle‑aged (26–45)", "Older (46+)")
    
    df <- poverty1_train %>%
      mutate(poverty_probability = predict_function(final_model, .)) %>%
      filter(!is.na(age_group))
    
    # Recode age_group if stored as numeric.
    df$age_group <- ifelse(as.character(df$age_group) %in% c("1", "1.0"),
                           "Young (15–25)",
                           ifelse(as.character(df$age_group) %in% c("2", "2.0"),
                                  "Middle‑aged (26–45)", "Older (46+)"))
    df$age_group <- factor(df$age_group, levels = age_levels)
    
    # Ensure poverty_probability is numeric.
    df$poverty_probability <- as.numeric(as.character(df$poverty_probability))
    
    # Apply country filtering if toggled on.
    if (input$use_country) {
      df <- df %>% filter(country == input$country)
    }
    
    # Apply urban filtering if toggled on.
    if (input$use_urban) {
      urban_val <- as.numeric(input$is_urban)
      df <- df %>% filter(is_urban == urban_val)
    }
    
    # Apply literacy filtering if toggled on.
    if (input$use_literacy) {
      literacy_val <- as.numeric(input$literacy)
      df <- df %>% filter(literacy == literacy_val)
    }
    
    # Apply gender filtering if toggled on.
    if (input$use_female) {
      female_val <- as.numeric(input$female)
      df <- df %>% filter(female == female_val)
    }
    
    # Apply marital status filtering if toggled on.
    if (input$use_married) {
      married_val <- as.numeric(input$married)
      df <- df %>% filter(married == married_val)
    }
    
    # Apply financial activities filtering if toggled on.
    if (input$use_financial) {
      fin_act_val <- as.numeric(input$num_financial_activities_last_year)
      df <- df %>% filter(num_financial_activities_last_year == fin_act_val)
    }
    
    # Apply employment type filtering if toggled on.
    if (input$use_employment) {
      df <- df %>% filter(employment_type_last_year == input$employment_type_last_year)
    }
    
    # If filtering results in empty data, display a message.
    if(nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for selected filters.", cex = 1.5)
      return()
    }
    
    # Apply education filtering if toggled on.
    if (input$use_education) {
      df <- df %>% filter(education_level == input$education_level)
    }
    
    # Apply can text filtering if toggled on.
    if (input$use_can_text) {
      can_text_val <- as.numeric(input$can_text)
      df <- df %>% filter(can_text == can_text_val)
    }
    
    # Apply phone technology filtering if toggled on.
    if (input$use_phone) {
      phone_val <- as.numeric(input$phone_technology)
      df <- df %>% filter(phone_technology == phone_val)
    }
    
    # Create dummy data to maintain legend.
    dummy <- data.frame(poverty_probability = NA_real_, 
                        age_group = factor(age_levels, levels = age_levels))
    
    base <- output$risk_distribution <- renderPlot({
      # Define age group labels.
      age_levels <- c("Young (15–25)", "Middle‑aged (26–45)", "Older (46+)")
      
      df <- poverty1_train %>%
        mutate(poverty_probability = predict_function(final_model, .)) %>%
        filter(!is.na(age_group))
      
      # Recode age_group if stored as numeric.
      df$age_group <- ifelse(as.character(df$age_group) %in% c("1", "1.0"),
                             "Young (15–25)",
                             ifelse(as.character(df$age_group) %in% c("2", "2.0"),
                                    "Middle‑aged (26–45)", "Older (46+)"))
      df$age_group <- factor(df$age_group, levels = age_levels)
      df$poverty_probability <- as.numeric(as.character(df$poverty_probability))
      
      # Apply additional filters if toggled on.
      if (input$use_country) {
        df <- df %>% filter(country == input$country)
      }
      if (input$use_urban) {
        urban_val <- as.numeric(input$is_urban)
        df <- df %>% filter(is_urban == urban_val)
      }
      if (input$use_literacy) {
        literacy_val <- as.numeric(input$literacy)
        df <- df %>% filter(literacy == literacy_val)
      }
      if (input$use_female) {
        female_val <- as.numeric(input$female)
        df <- df %>% filter(female == female_val)
      }
      if (input$use_married) {
        married_val <- as.numeric(input$married)
        df <- df %>% filter(married == married_val)
      }
      if (input$use_financial) {
        fin_act_val <- as.numeric(input$num_financial_activities_last_year)
        df <- df %>% filter(num_financial_activities_last_year == fin_act_val)
      }
      if (input$use_employment) {
        df <- df %>% filter(employment_type_last_year == input$employment_type_last_year)
      }
      if (input$use_education) {
        df <- df %>% filter(education_level == input$education_level)
      }
      if (input$use_can_text) {
        can_text_val <- as.numeric(input$can_text)
        df <- df %>% filter(can_text == can_text_val)
      }
      if (input$use_phone) {
        phone_val <- as.numeric(input$phone_technology)
        df <- df %>% filter(phone_technology == phone_val)
      }
      
      if(nrow(df) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for selected filters.", cex = 1.5)
        return()
      }
      
      ggplot() +
        annotate("rect", xmin = 0, xmax = 0.25, ymin = -Inf, ymax = Inf,
                 fill = "#66c2a5", alpha = 0.1) +   # Green for Low Risk
        annotate("rect", xmin = 0.25, xmax = 0.60, ymin = -Inf, ymax = Inf,
                 fill = "#fc8d62", alpha = 0.1) +   # Yellow for Moderate Risk
        annotate("rect", xmin = 0.60, xmax = 1.0, ymin = -Inf, ymax = Inf,
                 fill = "#cc0033", alpha = 0.1) +   # Red for High Risk
        geom_density(data = df, 
                     aes(x = poverty_probability, fill = age_group, color = age_group, 
                         y = ..count.. / sum(..count..) * 100),
                     alpha = 0.4, na.rm = TRUE) +
        scale_fill_manual(
          name   = "Age Group",
          values = c("Young (15–25)" = "#21F0FB",    # Steel blue
                     "Middle‑aged (26–45)" = "#EF50F5",  # Hot pink
                     "Older (46+)" = "#350469")          # Blue violet
        ) +
        scale_color_manual(
          name   = "Age Group",
          values = c("Young (15–25)" = "#21F0FB",    # Dodger blue
                     "Middle‑aged (26–45)" = "#EF50F5",  # Deep pink
                     "Older (46+)" = "#350469")          # Purple
        ) +
        labs(title = "Poverty Risk Distribution by Age Group",
             x = "Poverty Risk Probability",
             y = "Percent of Population (%)") +
        scale_x_continuous(limits = c(0, 1)) +
        theme_minimal(base_size = 15) +
        theme(legend.position = "bottom")
    })
    
    base
  })
  
  ## SECTION 3: DATA TABLE & CORRELATION MATRIX
  
  # Render the full selected_vars dataset as an interactive table in the Dataset tab
  output$data_table <- renderDT({
    datatable(
      selected_vars, 
      options = list(
        pageLength = 5, 
        lengthMenu = c(5, 15, 25, 50, 100),
        scrollX = TRUE,
        searching = FALSE))
  })
  
  # Compute the correlation matrix from selected_vars.
  # Note: Remove "age" because selected_vars does not have it.
  top_vars <- selected_vars %>% 
    select(is_urban, literacy, num_financial_activities_last_year, can_text, female, married, phone_technology) %>%
    mutate(
      is_urban = as.numeric(is_urban), 
      literacy = as.numeric(literacy),
      can_text = as.numeric(can_text),
      female = as.numeric(female),
      married = as.numeric(married),
      phone_technology = as.numeric(phone_technology)
    ) %>%
    rename(
      "Urban Area" = is_urban,
      "Literacy" = literacy,
      "Financial Activities (Last Year)" = num_financial_activities_last_year,
      "Can Text" = can_text,
      "Female" = female,
      "Married" = married,
      "Phone Technology" = phone_technology
    )
  
  cor_matrix <- round(cor(top_vars, use = "complete.obs"), 2)
  cor_matrix_df <- as.data.frame(cor_matrix)
  
  output$correlation_matrix <- renderDT({
    datatable(cor_matrix_df, options = list(
      dom = 't',
      paging = FALSE,
      ordering = FALSE
    )) %>% 
      formatStyle(columns = colnames(cor_matrix_df), color = "black")
  })
  
  ## SECTION 4: ADDITIONAL VISUALIZATIONS
  
  output$selected_plot <- renderPlot({
    if (input$plot_select == "Employment Type Frequency") {
      ggplot(employment_freq, aes(x = reorder(employment_type_last_year, -n), y = n, fill = employment_type_last_year)) +
        geom_bar(stat = "identity", width = 0.7, color = "black") +
        labs(title = "Employment Type Frequency",
             x = "Employment Type", y = "Count") +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold"))
      
    } else if (input$plot_select == "Education Level Frequency") {
      ggplot(education_freq_clean, aes(x = reorder(education_level, -n), y = n, fill = education_level)) +
        geom_bar(stat = "identity", width = 0.7, color = "black") +
        geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 4) +
        labs(title = "Education Level Frequency",
             x = "Education Level", y = "Count") +
        scale_fill_brewer(palette = "Pastel1") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold"))
      
    } else if (input$plot_select == "Relationship to Household Head") {
      ggplot(head_freq, aes(x = reorder(relationship_to_hh_head, -n), y = n, fill = relationship_to_hh_head)) +
        geom_bar(stat = "identity", width = 0.7, color = "black") +
        geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 4) +
        labs(title = "Relationship to Household Head",
             x = "Relationship", y = "Count") +
        scale_fill_brewer(palette = "Set3") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold"))
      
    } else if (input$plot_select == "Phone Technology Frequency") {
      ggplot(phone_freq, aes(x = reorder(phone_technology, -n), y = n, fill = phone_technology)) +
        geom_bar(stat = "identity", width = 0.7, color = "black") +
        labs(title = "Phone Technology Frequency",
             x = "Phone Technology", y = "Count") +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
  
  output$dummy_plot <- renderPlot({
    # Calculate mean values of selected dummy variables from selected_vars
    dummy_means <- selected_vars %>%
      select(female, married, is_urban, literacy, can_text) %>%
      summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
      filter(!is.na(Mean))
    
    ggplot(dummy_means, aes(x = reorder(Variable, Mean), y = Mean, fill = Variable)) +
      geom_bar(stat = "identity", width = 0.7, color = "black") +
      geom_text(aes(label = paste0(round(Mean * 100, 1), "%")), vjust = -0.5, size = 4, na.rm = TRUE) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Mean Values of Dummy Variables",
           x = "Variable",
           y = "Mean (Proportion)") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  ## SECTION 5: DOWNLOAD HANDLER
  
  output$downloadPaper <- downloadHandler(
    filename = function() {
      # This is the name the user will see when downloading
      "Research_paper.pdf"
    },
    content = function(file) {
      # This copies your local "Research_paper.pdf" into the temp location for download
      file.copy("Research_paper.pdf", file)
    }
  )
}