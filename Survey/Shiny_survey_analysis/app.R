library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)
library(plotly)
library(ggalluvial)
library(forcats)

# Load and prepare data once at app start
survey_data <- read_csv("survey_data_shiny.csv")

# Prepare long-format data for plotting
correctness_long <- survey_data %>%
  select(pre_km_code,pre_mrl_code,post_km_code,post_mrl_code,knowledge_rate) %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = -c(id,knowledge_rate),
    names_to = "plot_full",
    values_to = "correctness"
  ) %>%
  mutate(
    phase = factor(
      case_when(
        str_detect(plot_full, "^pre_") ~ "Pre_Learning",
        str_detect(plot_full, "^post_") ~ "Post_Learning"
      ),levels = c("Pre_Learning", "Post_Learning")  
    ),
    plot = case_when(
      str_detect(plot_full, "km") ~ "Kaplan Meier(KM)",
      str_detect(plot_full, "mrl") ~ "Mean Residual Life(MRL)"
    ),
    correctness = factor(correctness, levels = c("INCORRECT", "CORRECT")))%>%
      mutate(
        knowledge_group = if_else(knowledge_rate < 5, "<5 (Low)", "≥5 (High)")
      )

# Calculation of  accuracy per plot  and phase
correct_prop <- correctness_long %>%
  group_by( plot, phase) %>%
  summarise(prop_correct = mean(correctness == "CORRECT")) %>%
  ungroup()

## Ranking data processing 
ranking_df <- survey_data %>%
  select(id,knowledge_rate,post_score,
         ranking1_km, ranking2_km,
         ranking1_survdiff, ranking2_survdiff,
         ranking1_mrl, ranking2_mrl,
         ranking1_mr_ldiff, ranking2_mr_ldiff) %>%
  pivot_longer(
    cols = -c(id,knowledge_rate,post_score),
    names_to = c("Preference", "plot"),
    names_pattern = "ranking(\\d)_(.*)",
    values_to = "rank"
  ) %>%
  mutate(
    Preference = recode(Preference, `1` = "Clinicians_Understanding", `2` = "Use_with_patient"),
    plot = case_when(
      plot == "km" ~ "Kaplan Meier(KM)",
      plot == "survdiff" ~ "Difference in Survival",
      plot == "mrl" ~ "Mean Residual Life(MRL)",
      plot == "mr_ldiff" ~ "Difference in MRL",
      TRUE ~ plot
    ))
    
    
  ranking_agreement <- ranking_df %>% 
  pivot_wider(names_from = Preference, values_from = rank) %>%
  drop_na() %>%
  mutate(
    Clinicians_Understanding = factor(Clinicians_Understanding, levels = c("1", "2", "3", "4")),
    Use_with_patient = factor(Use_with_patient, levels = c("1", "2", "3", "4"))
  )

# user interface 
ui <- dashboardPage(
  
  dashboardHeader(
    title = tags$div(
      style = "white-space: normal; font-size: 16px; line-height: 20px; padding-top: 15px;",
      "Clinicians' knowledge and preference on Survival Data Visualisation"
    )
  )
  
  
  ,
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Self-rated Knowledge", tabName = "knowledge", icon = icon("chart-bar")),
      menuItem("Pre vs Post Learning Accuracy", tabName = "accuracy", icon = icon("chart-line")),
      menuItem("Ranking Distribution", tabName = "preferences", icon = icon("thumbs-up")),
      menuItem("Ranking vs Score", tabName = "Rankingvsscore", icon = icon("thumbs-up")),
      menuItem("Ranking Agreement", tabName = "agreement", icon = icon("balance-scale"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "knowledge",plotOutput("knowledgePlot"),br(),tableOutput("knowledgeSummary"),br(),plotOutput("prelearningPlot")),  # summary table here,
      tabItem(tabName = "accuracy",
              fluidRow(
                box(width = 12, title = "Filters", status = "primary", solidHeader = TRUE,
                    checkboxGroupInput(
                      inputId = "selected_knowledge",
                      label = "Select Knowledge Score (1–9):",
                      choices = sort(unique(correctness_long$knowledge_rate)),
                      selected = sort(unique(correctness_long$knowledge_rate)),
                      inline = TRUE
                    )
                )
              ),
              fluidRow(
                box(width = 6, plotlyOutput("correctnessPlot")),
                box(width = 6, plotOutput("alluvialPlot"))
              )
      )
      ,
      tabItem(tabName = "preferences",
              fluidRow(
                box(width = 12, title = "Filter by Self-rated Knowledge", status = "primary", solidHeader = TRUE,
                    checkboxGroupInput(
                      inputId = "selected_ranking_knowledge",
                      label = "Select Knowledge Score (1–9):",
                      choices = 1:9,
                      selected = 1:9,
                      inline = TRUE
                    )
                )
              ),
              fluidRow(
                box(width = 10, plotlyOutput("preferencePlot"))
                
              )
      ),
      tabItem(tabName = "Rankingvsscore", plotlyOutput("rankingpostscore")), 
      tabItem(tabName = "agreement", plotlyOutput("rankingAgreementPlot"))
      
      
    )
  )
)
# server 
server <- function(input, output) {
  
  output$knowledgePlot <- renderPlot({
    score_rate_distribution <- survey_data %>%  group_by(knowledge_rate) %>% summarise( count=n())
    ggplot(data = score_rate_distribution, aes(x = as.factor(knowledge_rate), y = count)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "black") +
      geom_text(aes(label = count), vjust = -0.3, size = 4.5) +
      labs(title = "Distribution of Self-rated Knowledge Scores",
           x = "Self-rated Knowledge Score",
           y = NULL) +
      theme_bw(base_size = 12, base_family = "sans") +
      theme(
        panel.border = element_rect(colour = "black", size = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),         
        axis.title.x = element_text(size = 12, face = "bold"),       
        axis.text.x = element_text(size = 10),
        plot.caption = element_text(size = 9, hjust = 0)
      )
  })
  
  output$prelearningPlot <- renderPlot({
    knowledge_plot_data <- survey_data %>% mutate(
      pre_correct_KM_MRL = rowMeans(across(ends_with("pre_km_code") | 
                                      
                                      ends_with("pre_mrl_code"), 
                                    ~ . == "CORRECT", 
                                    .names = "correct_{col}"), 
                             na.rm = TRUE)
      
    ) %>% 
      group_by(knowledge_rate) %>%
      summarise(avg_pre_correct = mean(pre_correct_KM_MRL, na.rm = TRUE))
    
    # Plot
    ggplot(knowledge_plot_data, aes(x = factor(knowledge_rate), y = avg_pre_correct)) +
      geom_col(fill = "steelblue") +
      labs(
        title = "Proportion of Baseline Correct Interpretation per Self-rated Knowledge",
        x = "Self-rated Knowledge Score(1 = Low, 10 = High)",
        y = "Proportion(Accuracy)"
      ) +
      theme_bw(base_size = 12, base_family = "sans")+
      theme(
        panel.border = element_rect(colour = "black", size = 0.5),
        plot.title = element_text(face = "bold", size = 12,hjust = 0.5),         
        axis.title.x = element_text(size = 10, face = "bold"),       
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10, face = "bold"),       
        axis.text.y = element_text(size = 8),
        plot.caption = element_text(size = 9, hjust = 0)
      )
    
  })
  
  
  
  
  output$knowledgeSummary <- renderTable({
    survey_data %>%
      summarise(
        Count = n(),
        Mean = mean(knowledge_rate, na.rm = TRUE),
        Median = median(knowledge_rate, na.rm = TRUE),
        SD = sd(knowledge_rate, na.rm = TRUE),
        Min = min(knowledge_rate, na.rm = TRUE),
        Max = max(knowledge_rate, na.rm = TRUE)
      ) %>%
      round(2)
  })
  # Accuracy plot Output
  output$correctnessPlot <- renderPlotly({
    
    # Filter based on selected knowledge rates
    filtered_data <- correctness_long %>%
      filter(knowledge_rate %in% input$selected_knowledge)
    
    # Recompute proportion of correct answers
    correct_prop <- filtered_data %>%
      group_by(plot, phase) %>%
      summarise(prop_correct = mean(correctness == "CORRECT"), .groups = "drop")
    
    accuracy_plot <- ggplot(correct_prop, aes(x = plot, y = prop_correct, fill = phase)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      scale_fill_manual(values = c("Pre_Learning" = "gray30", "Post_Learning" = "cyan")) +
      labs(
        title = "Interpretation Accuracy (Pre vs Post Learning) ",
        x = "Plot",
        y = "Accuracy(%)",
        fill = "Phase"
      ) +
      theme_bw(base_size = 12, base_family = "sans") +
      theme(
        panel.border = element_rect(colour = "black", size = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 11, face = "bold", angle = 20, hjust = 1),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        plot.title = element_text(face = "bold", size = 14),
        plot.caption = element_text(size = 9, hjust = 0),
        legend.position = "top"
      )
    ggplotly(accuracy_plot)
  })
  # Alluvial plot Output 
  output$alluvialPlot <- renderPlot({
    filtered <- correctness_long %>%
      filter(
        knowledge_rate %in% input$selected_knowledge
      )
    
    alluvial_data <- filtered %>%
      select(id, plot, phase, correctness) %>%
      pivot_wider(names_from = phase, values_from = correctness) %>%
      filter(!is.na(Pre_Learning), !is.na(Post_Learning))
    
    ggplot(alluvial_data,
           aes(axis1 = Pre_Learning, axis2 = Post_Learning, y = 1)) +
      geom_alluvium(aes(fill = Pre_Learning), width = 0.3) +
      geom_stratum(width = 0.3, fill = "gray80", color = "black") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.6) +
      scale_x_discrete(limits = c("Pre_Learning", "Post_Learning"), expand = c(0.1, 0.1)) +
      facet_wrap(~plot) +
      labs(title = "Improvement in Accuracy (Pre vs Post)", x = "Phase", y = "Number of Respondents") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(face = "bold", size = 16),
        strip.text = element_text(size = 12, face = "bold")
      )
    
  })
  # Ranking agreement plot output
  output$rankingAgreementPlot <- renderPlotly({
   
    ranking_agreement <- ranking_df %>% 
      pivot_wider(names_from = Preference, values_from = rank) %>%
      drop_na() %>%
      mutate(
        Clinicians_Understanding = factor(Clinicians_Understanding, levels = c("1", "2", "3", "4")),
        Use_with_patient = factor(Use_with_patient, levels = c("1", "2", "3", "4"))
      )
    
     ranking_agreement_plot <-ggplot(ranking_agreement, aes(x = Clinicians_Understanding, y = Use_with_patient)) +
      geom_jitter(width = 0.2, height = 0.2, alpha = 0.4, color = "#0072B2", size = 2.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
      facet_wrap(~plot) +
      scale_x_discrete(name = "Easy to understand by clinician") +
      scale_y_discrete(name = "Easy to use in consultation") +
      labs(
        title = "Agreement Between Rankings"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 12),
        strip.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),       
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14, face = "bold"),       
        axis.text.y = element_text(size = 12),
      )
    
    ggplotly(ranking_agreement_plot)
  })
  
  output$preferencePlot <- renderPlotly({    
    ranking_bar <- survey_data %>%
      filter(knowledge_rate %in% input$selected_ranking_knowledge) %>%
      pivot_longer(
        cols = starts_with("ranking"),
        names_to = "ranking_type",
        values_to = "rank"
      ) %>%
      mutate(
        question = case_when(
          str_detect(ranking_type, "km") ~ "Kaplan Meier(KM)",
          str_detect(ranking_type, "survdiff") ~ "Survival difference",
          str_detect(ranking_type, "mrl$") ~ "Mean Residual Life(MRL)",
          str_detect(ranking_type, "mr_ldiff") ~ "MRL difference"
        ),
        preference = case_when(
          str_detect(ranking_type, "ranking1") ~ "Easy to understand by Clinician",
          str_detect(ranking_type, "ranking2") ~ "Easy to use with patient"
        ),
        rank = factor(rank, levels = c("1", "2", "3", "4"))
      ) %>%
      drop_na(rank)%>%
      count(question, rank, preference)
    
    
    
    ranking_plot <- ggplot(ranking_bar, aes(x = question, y = rank, fill = n)) +
      geom_tile(color = "grey70") +
      scale_fill_gradient(low = "white", high = "blue") +
      facet_wrap(~preference) +
      labs(
        x = "Plot Type",
        y = "Rank (1 = Best)",
        fill = "Count",
        title = "Heatmap : Distribution of Ranks by Plot"
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 45,face = "bold", hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")
      )
  })
    
      
      if (FALSE) { ggplot(ranking_bar, aes(x = rank, fill = preference)) +
      geom_bar(position = "dodge") +
      facet_wrap(~question) +
      scale_fill_manual(values = c("Easy to understand by Clinician" = "purple", "Easy to use with patient" = "orange")) +
      labs(
        title = "Distribution of Ranks by Plot ",
        x = "Rank(1 = Best)",
        y = "Number of Participants",
        fill = "Preference"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12, face = "bold")
      )
    
    #ggplotly(ranking_plot) })
  }
 
  output$rankingselfratedscore <- renderPlotly({ 
    ranking_self_rated <- ranking_df %>%
      filter(knowledge_rate %in% input$selected_ranking_knowledge) %>%
      mutate(
        plot = factor(plot, levels = c("Kaplan Meier(KM)", "Difference in Survival", "Mean Residual Life(MRL)", "Difference in MRL")),
        rank = factor(rank, levels = c("1", "2", "3", "4")),
        knowledge_group = case_when(
          knowledge_rate < 5 ~ "Low(<5)",
          knowledge_rate >= 5 ~ "High(>=5)"
        ),
        knowledge_group = factor(knowledge_group, levels = c("Low(<5)", "High(>=5)"))
      ) %>%
      drop_na()
    
  
    
      if (FALSE) {ggplot(ranking_self_rated, aes(x = knowledge_group, y = rank, color = Preference)) +
      geom_jitter(width = 0.2, height = 0.2, alpha = 0.6, size = 2.5) +
      facet_wrap(~plot, ncol = 2) +
      scale_x_discrete(name = "Self-rated Knowledge Group") +
      scale_y_discrete(name = "Rank (1 = Best)") +
      scale_color_brewer(palette = "Dark2") +
      labs(title = "Rank by self-rated Knowledge Group and Plot Type") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_line(color = "gray85"),
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 13),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 13, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
      )}
    
    ggplotly(ranking_selfrated_plot)
  })
   
 output$rankingpostscore <- renderPlotly({ 
  ranking_post_learning <-  ranking_df %>% mutate(plot =  factor(plot, levels = c("Kaplan Meier(KM)", "Difference in Survival", "Mean Residual Life(MRL)", "Difference in MRL")),
   rank = factor(rank, levels = c("1", "2", "3", "4")),
   score_group = case_when(
     post_score < 2 ~ "<2",
     post_score >= 2 ~ "≥2"
   ),
   score_group = factor(score_group, levels = c("<2", "≥2"),labels = c("below 50%","50% and Above"))
 ) %>%
   drop_na()
 
 # Plot
 ranking_post_learning_plot <- ggplot(ranking_post_learning, aes(x = score_group, y = rank, color = Preference)) +
   geom_jitter(width = 0.2, height = 0.2, alpha = 0.6, size = 2.5) +
   
   geom_point(position = position_jitter(width = 0.2, height = 0.1), alpha = 0.6)+
 
   facet_wrap(~plot, ncol = 2) +
   scale_x_discrete(name = "Post-learning Score") +
   scale_y_discrete(name = "Rank (1 = Best)") +
   scale_color_brewer(palette = "Dark2") +
   labs(title = "Rank by Post-learning Score category(High vs Low) and Plot Type") +
   theme_minimal() +
   theme(
     panel.grid.major.x = element_line(color = "gray85"),
     plot.title = element_text(face = "bold", size = 16),
     axis.title = element_text(face = "bold", size = 13),
     axis.text = element_text(size = 11),
     strip.text = element_text(size = 13, face = "bold"),
     legend.position = "bottom",
     legend.title = element_text(face = "bold")
   )
 
 
 
 # Interactive 
 ggplotly(ranking_post_learning_plot)})   
}

shinyApp(ui = ui, server = server)

