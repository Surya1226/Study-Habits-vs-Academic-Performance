library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)

ALPHA <- 0.05          
PASS_MARK <- 10     

# Loads the student dataset files from the application folder for analysis

load_student_data <- function() { 
  project_folder <- getwd()

  math_file <- file.path(project_folder, "student-mat.csv")
  portuguese_file <- file.path(project_folder, "student-por.csv")

  if (!file.exists(math_file)) {
    stop("Cannot find student-mat.csv. Current working directory: ", project_folder)
  }
  if (!file.exists(portuguese_file)) {
    stop("Cannot find student-por.csv. Current working directory: ", project_folder)
  }

  math_data <- read.csv(math_file, sep = ";", stringsAsFactors = FALSE)
  portuguese_data <- read.csv(portuguese_file, sep = ";", stringsAsFactors = FALSE)

  required_columns <- c("school", "sex", "studytime", "absences", "failures", "G3")

  if (!all(required_columns %in% names(math_data))) {
    stop("student-mat.csv is missing one or more required columns.")
  }
  if (!all(required_columns %in% names(portuguese_data))) {
    stop("student-por.csv is missing one or more required columns.")
  }

  # Cleans and prepares the dataset by selecting and processing the required variables
  
  prepare_data <- function(data) {
    data$studytime_code <- data$studytime

    data$studytime <- factor(
      data$studytime,
      levels = 1:4,
      labels = c(
        "Less than 2 hours",
        "2 to 5 hours",
        "5 to 10 hours",
        "More than 10 hours"
      ),
      ordered = TRUE
    )

    data$failure_group <- factor(
      ifelse(data$failures == 0, "No past failures", "One or more past failures"),
      levels = c("No past failures", "One or more past failures")
    )

    data
  }

  list(
    Mathematics = prepare_data(math_data),
    Portuguese = prepare_data(portuguese_data)
  )
}

dataset_error <- NULL

student_data <- tryCatch(
  load_student_data(),
  error = function(e) {
    dataset_error <<- conditionMessage(e)
    message("Dataset loading failed: ", dataset_error)
    NULL
  }
)

ACCENT_STUDY <- "#3498DB"
ACCENT_ABSENCE <- "#E67E22"
ACCENT_FAILURE <- "#E74C3C"

page_css <- "
  .stat-card { background:#f4f6f8; border-radius:10px; padding:14px 16px;
               text-align:center; margin-bottom:12px; }
  .stat-card .stat-value { font-size:1.8rem; font-weight:700; line-height:1.2; }
  .stat-card .stat-label { font-size:0.85rem; color:#6c757d; }
  .finding-card { background:#ffffff; border:1px solid #e3e6ea; border-left:6px solid #999;
                  border-radius:8px; padding:14px 18px; margin-bottom:14px; }
  .finding-card h5 { margin:0 0 2px 0; color:#6c757d; font-size:0.85rem;
                     text-transform:uppercase; letter-spacing:0.04em; }
  .finding-card h4 { margin:0 0 8px 0; font-weight:700; }
  .finding-card p  { margin:0; font-size:1.02rem; }
  .try-card { background:#f4f6f8; border-radius:10px; padding:16px; margin-bottom:12px; }
  .try-card .try-value { font-size:2rem; font-weight:700; }
  .try-card .try-note { color:#6c757d; font-size:0.9rem; }
"

# Creates a small dashboard card to display a statistical value

stat_card <- function(label, value) {
  div(
    class = "stat-card",
    div(class = "stat-value", value),
    div(class = "stat-label", label)
  )
}

# Displays an important finding from the statistical analysis

finding_card <- function(title, headline, text, accent) {
  div(
    class = "finding-card",
    style = paste0("border-left-color:", accent, ";"),
    h5(title),
    h4(headline),
    p(text)
  )
}

# Generates a short, understandable sentence summarizing a statistical result

share_sentence <- function(label, share) {
  size <- if (share < 0.01) {
    "almost none"
  } else if (share < 0.10) {
    "only a small part"
  } else if (share < 0.30) {
    "a moderate part"
  } else {
    "a large part"
  }
  paste0(
    label, " accounts for ", size,
    " of the differences in final grades (about ",
    round(100 * share, 1), "%)."
  )
}

# Creates the main responsive layout of the application

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(tags$style(HTML(page_css))),

  titlePanel("How Study Habits Relate to Student Grades"),
  p(
    class = "text-muted",
    "Explore how weekly study time, school absences and past class failures ",
    "relate to students' final grades (scored out of 20)."
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons(
        "subject",
        "Subject",
        choices = c("Mathematics", "Portuguese"),
        selected = "Mathematics"
      ),
      checkboxGroupInput(
        "schools",
        "School",
        choices = c("Gabriel Pereira" = "GP", "Mousinho da Silveira" = "MS"),
        selected = c("GP", "MS")
      ),
      checkboxGroupInput(
        "sexes",
        "Students",
        choices = c("Female" = "F", "Male" = "M"),
        selected = c("F", "M")
      ),
      hr(),
      helpText("Every result on the right updates to match your selections."),
      downloadButton("download_findings", "Download Findings")
    ),

    mainPanel(
      width = 9,
      uiOutput("load_status"),

      tabsetPanel(
        tabPanel(
          "At a Glance",
          br(),
          uiOutput("glance_stats"),
          uiOutput("glance_findings"),
          h4("Which factor tells us the most about grades?"),
          plotOutput("glance_chart", height = "260px"),
          helpText(
            "A longer bar means that factor tells you more about a student's final grade. ",
            "None of the three explains everything, and these links do not prove cause and effect."
          )
        ),

        tabPanel(
          "Study Time",
          br(),
          h4("Average final grade by weekly study time"),
          plotOutput("studytime_plot"),
          uiOutput("study_finding")
        ),

        tabPanel(
          "Absences",
          br(),
          h4("Absences and final grades"),
          plotOutput("absence_plot"),
          uiOutput("absence_finding")
        ),

        tabPanel(
          "Past Failures",
          br(),
          h4("Average final grade by past failure record"),
          plotOutput("failures_plot"),
          uiOutput("failure_finding")
        ),

        tabPanel(
          "Try It Yourself",
          br(),
          p("Pick a study habit, a number of absences and a failure record to see ",
            "the typical final grade for students like that."),
          fluidRow(
            column(
              4,
              selectInput(
                "try_study",
                "Weekly study time",
                choices = c(
                  "Less than 2 hours", "2 to 5 hours",
                  "5 to 10 hours", "More than 10 hours"
                ),
                selected = "2 to 5 hours"
              )
            ),
            column(
              4,
              sliderInput("try_absences", "Number of absences",
                          min = 0, max = 30, value = 5, step = 1)
            ),
            column(
              4,
              radioButtons(
                "try_failures",
                "Past class failures",
                choices = c("No past failures", "One or more past failures"),
                selected = "No past failures"
              )
            )
          ),
          fluidRow(
            column(4, uiOutput("try_study_card")),
            column(4, uiOutput("try_absence_card")),
            column(4, uiOutput("try_failure_card"))
          ),
          fluidRow(
            column(12, uiOutput("try_combined_card"))
          ),
          helpText(
            "The three cards above look at one factor at a time. The final prediction ",
            "combines all three factors, but it is still an estimate based on past students, ",
            "not a guarantee for any individual."
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  selected_data <- reactive({
    req(!is.null(student_data))

    data <- student_data[[input$subject]]
    data <- data[data$school %in% input$schools & data$sex %in% input$sexes, ]

    validate(need(
      nrow(data) >= 20,
      "Too few students match your selections. Please widen the School / Students filters."
    ))

    data
  })

  output$load_status <- renderUI({
    if (is.null(student_data)) {
      div(
        class = "alert alert-danger",
        h4("Dataset loading failed"),
        p(dataset_error),
        p("Make sure the app file, student-mat.csv and student-por.csv are in the same folder, ",
          "and that R's working directory is set to that folder.")
      )
    }
  })

# uses descriptive statistics and one-way ANOVA
  
  study_result <- reactive({
    data <- selected_data() %>%
      filter(!is.na(studytime), !is.na(G3)) %>%
      mutate(studytime = droplevels(studytime))

    validate(need(
      nlevels(data$studytime) >= 2,
      "At least two study-time groups are needed for this comparison."
    ))

    groups <- data %>%
      group_by(studytime) %>%
      summarise(n = n(), mean_grade = mean(G3), .groups = "drop")

    anova_table <- summary(aov(G3 ~ studytime, data = data))[[1]]
    p_value <- anova_table[["Pr(>F)"]][1]
    sum_sq <- anova_table[["Sum Sq"]]
    share <- sum_sq[1] / sum(sum_sq)
    if (is.na(share)) share <- 0
    clear <- isTRUE(p_value < ALPHA)

    candidates <- groups %>% filter(n >= 5)
    if (nrow(candidates) < 2) candidates <- groups
    best <- candidates[which.max(candidates$mean_grade), ]
    worst <- candidates[which.min(candidates$mean_grade), ]

    if (clear) {
      headline <- "Study time makes a difference"
      text <- paste0(
        "Students who study ", tolower(as.character(best$studytime)),
        " per week score highest on average (", round(best$mean_grade, 1),
        " out of 20), while those who study ",
        tolower(as.character(worst$studytime)), " score lowest (",
        round(worst$mean_grade, 1), "). ",
        share_sentence("Study time", share)
      )
    } else {
      headline <- "No clear link with study time"
      text <- paste0(
        "Average grades are similar across the study-time groups, so study time alone does not ",
        "show a reliable effect on final grades for these students. ",
        share_sentence("Study time", share)
      )
    }

    list(groups = groups, share = share, clear = clear,
         headline = headline, text = text)
  })

  absence_result <- reactive({
    data <- selected_data() %>%
      filter(!is.na(absences), !is.na(G3))

    validate(need(
      length(unique(data$absences)) >= 2,
      "The selected students do not have enough variety in absences to analyse."
    ))

    x <- data$absences
    y <- data$G3
    n <- length(x)

    sum_x <- sum(x)
    sum_y <- sum(y)
    sum_xy <- sum(x * y)
    sum_x2 <- sum(x^2)

    D <- n * sum_x2 - sum_x^2
    D0 <- sum_y * sum_x2 - sum_x * sum_xy
    D1 <- n * sum_xy - sum_x * sum_y

    b0 <- D0 / D
    b1 <- D1 / D

    fitted_values <- b0 + b1 * x
    sse <- sum((y - fitted_values)^2)
    sst <- sum((y - mean(y))^2)
    share <- if (sst > 0) 1 - sse / sst else 0

    df <- n - 2
    se_slope <- sqrt((sse / df) * n / D)
    t_stat <- b1 / se_slope
    p_value <- 2 * pt(-abs(t_stat), df = df)
    clear <- isTRUE(p_value < ALPHA)

    change_10 <- b1 * 10

    if (clear && b1 < 0) {
      headline <- "More absences go with lower grades"
      text <- paste0(
        "Every 10 extra absences is linked to a final grade about ",
        round(abs(change_10), 2), " points lower on average. ",
        share_sentence("Absence count", share)
      )
    } else if (clear && b1 > 0) {
      headline <- "More absences go with slightly higher grades"
      text <- paste0(
        "Every 10 extra absences is linked to a final grade about ",
        round(change_10, 2), " points higher on average. This likely reflects other ",
        "factors rather than absences helping grades. ",
        share_sentence("Absence count", share)
      )
    } else {
      headline <- "No clear link with absences"
      text <- paste0(
        "Students with more absences do not score noticeably differently from those with fewer, ",
        "so absences alone do not reliably predict the final grade here. ",
        share_sentence("Absence count", share)
      )
    }

    list(data = data, b0 = b0, b1 = b1, share = share, clear = clear,
         headline = headline, text = text)
  })

  failure_result <- reactive({
    data <- selected_data() %>%
      filter(!is.na(failure_group), !is.na(G3))

    validate(need(
      n_distinct(data$failure_group) == 2 && min(table(data$failure_group)) >= 2,
      "There are not enough students in both failure groups for the selected filters."
    ))

    groups <- data %>%
      group_by(failure_group) %>%
      summarise(
        n = n(),
        mean_grade = mean(G3),
        pass_rate = mean(G3 >= PASS_MARK),
        .groups = "drop"
      )

    p_value <- tryCatch(
      t.test(G3 ~ failure_group, data = data)$p.value,
      error = function(e) NA_real_
    )
    clear <- isTRUE(p_value < ALPHA)

    share <- suppressWarnings(
      cor(as.numeric(data$failure_group), data$G3)^2
    )
    if (is.na(share)) share <- 0

    row_no <- groups[groups$failure_group == "No past failures", ]
    row_yes <- groups[groups$failure_group == "One or more past failures", ]
    difference <- row_no$mean_grade - row_yes$mean_grade
    percent_gap <- 100 * abs(difference) / max(row_no$mean_grade, row_yes$mean_grade)

    pass_text <- paste0(
      "The pass rate is ", round(100 * row_no$pass_rate, 1), "% for students with no past failures ",
      "versus ", round(100 * row_yes$pass_rate, 1), "% for those with past failures."
    )

    if (clear && difference > 0) {
      headline <- "Past failures go with lower grades"
      text <- paste0(
        "Students with past failures score about ", round(abs(difference), 1),
        " points lower on average (", round(percent_gap, 1), "% lower). ",
        pass_text, " ", share_sentence("Past failure record", share)
      )
    } else if (clear && difference < 0) {
      headline <- "Past failures go with higher grades"
      text <- paste0(
        "Surprisingly, students with past failures score about ", round(abs(difference), 1),
        " points higher on average. ", pass_text, " ",
        share_sentence("Past failure record", share)
      )
    } else {
      headline <- "No clear link with past failures"
      text <- paste0(
        "The average grades of the two groups are close (",
        round(row_no$mean_grade, 1), " vs ", round(row_yes$mean_grade, 1),
        "), so past failures do not show a reliable difference here. ",
        pass_text, " ", share_sentence("Past failure record", share)
      )
    }

    list(groups = groups, share = share, clear = clear,
         headline = headline, text = text)
  })

  output$glance_stats <- renderUI({
    data <- selected_data()
    fluidRow(
      column(3, stat_card("Students", nrow(data))),
      column(3, stat_card("Average final grade (out of 20)",
                          round(mean(data$G3, na.rm = TRUE), 1))),
      column(3, stat_card("Pass rate",
                          paste0(round(100 * mean(data$G3 >= PASS_MARK, na.rm = TRUE)), "%"))),
      column(3, stat_card("Average absences",
                          round(mean(data$absences, na.rm = TRUE), 1)))
    )
  })

  output$glance_findings <- renderUI({
    study <- study_result()
    absence <- absence_result()
    failure <- failure_result()

    tagList(
      finding_card("Study time", study$headline, study$text, ACCENT_STUDY),
      finding_card("Absences", absence$headline, absence$text, ACCENT_ABSENCE),
      finding_card("Past failures", failure$headline, failure$text, ACCENT_FAILURE)
    )
  })

  output$glance_chart <- renderPlot({
    chart_data <- data.frame(
      Factor = c("Study time", "Absences", "Past failures"),
      Share = 100 * c(
        study_result()$share,
        absence_result()$share,
        failure_result()$share
      )
    )
    chart_data$Factor <- factor(chart_data$Factor, levels = chart_data$Factor[order(chart_data$Share)])

    ggplot(chart_data, aes(x = Factor, y = Share, fill = Factor)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = paste0(round(Share, 1), "%")), hjust = -0.15, size = 5) +
      coord_flip() +
      scale_fill_manual(values = c(
        "Study time" = ACCENT_STUDY,
        "Absences" = ACCENT_ABSENCE,
        "Past failures" = ACCENT_FAILURE
      )) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = "Share of grade differences accounted for (%)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$studytime_plot <- renderPlot({
    groups <- study_result()$groups %>%
      mutate(label = paste0(studytime, "\n(", n, " students)"))
    overall <- mean(selected_data()$G3, na.rm = TRUE)

    ggplot(groups, aes(x = studytime, y = mean_grade, fill = studytime)) +
      geom_col(width = 0.65) +
      geom_hline(yintercept = overall, linetype = "dashed", color = "grey30") +
      geom_text(aes(label = round(mean_grade, 1)), vjust = -0.5, size = 5) +
      scale_x_discrete(labels = groups$label) +
      scale_y_continuous(limits = c(0, 20), expand = expansion(mult = c(0, 0.05))) +
      scale_fill_brewer(palette = "Blues") +
      labs(
        subtitle = "Dashed line = average grade of all selected students",
        x = "Weekly study time", y = "Average final grade (out of 20)"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$study_finding <- renderUI({
    r <- study_result()
    finding_card("Study time", r$headline, r$text, ACCENT_STUDY)
  })

  output$absence_plot <- renderPlot({
    r <- absence_result()

    ggplot(r$data, aes(x = absences, y = G3)) +
      geom_point(color = ACCENT_ABSENCE, alpha = 0.55, size = 2) +
      geom_abline(intercept = r$b0, slope = r$b1, color = "#2C3E50", linewidth = 1.1) +
      labs(
        subtitle = "Each dot is a student. The dark line shows the overall trend.",
        x = "Number of absences", y = "Final grade (out of 20)"
      ) +
      theme_minimal(base_size = 13)
  })

  output$absence_finding <- renderUI({
    r <- absence_result()
    finding_card("Absences", r$headline, r$text, ACCENT_ABSENCE)
  })

  output$failures_plot <- renderPlot({
    groups <- failure_result()$groups %>%
      mutate(label = paste0(failure_group, "\n(", n, " students)"))

    ggplot(groups, aes(x = failure_group, y = mean_grade, fill = failure_group)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = round(mean_grade, 1)), vjust = -0.5, size = 5) +
      scale_x_discrete(labels = groups$label) +
      scale_y_continuous(limits = c(0, 20), expand = expansion(mult = c(0, 0.05))) +
      scale_fill_manual(values = c(
        "No past failures" = "#27AE60",
        "One or more past failures" = ACCENT_FAILURE
      )) +
      labs(x = NULL, y = "Average final grade (out of 20)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })

  output$failure_finding <- renderUI({
    r <- failure_result()
    finding_card("Past failures", r$headline, r$text, ACCENT_FAILURE)
  })

  overall_average <- reactive({
    mean(selected_data()$G3, na.rm = TRUE)
  })

  compare_text <- function(value) {
    gap <- value - overall_average()
    if (abs(gap) < 0.05) {
      "About the same as the overall average."
    } else if (gap > 0) {
      paste0(round(gap, 1), " points above the overall average.")
    } else {
      paste0(round(abs(gap), 1), " points below the overall average.")
    }
  }

  try_card <- function(title, value_text, note, accent) {
    div(
      class = "try-card",
      style = paste0("border-top:5px solid ", accent, ";"),
      h5(title),
      div(class = "try-value", value_text),
      div(class = "try-note", note)
    )
  }

  output$try_study_card <- renderUI({
    groups <- study_result()$groups
    row <- groups[as.character(groups$studytime) == input$try_study, ]

    if (nrow(row) == 0) {
      try_card("By study time", "N/A",
               "No students in this group for the current filters.", ACCENT_STUDY)
    } else {
      try_card(
        "By study time",
        paste0(round(row$mean_grade, 1), " / 20"),
        paste0("Based on ", row$n, " students. ", compare_text(row$mean_grade)),
        ACCENT_STUDY
      )
    }
  })

  output$try_absence_card <- renderUI({
    r <- absence_result()
    value <- min(20, max(0, r$b0 + r$b1 * input$try_absences))

    try_card(
      "By absences",
      paste0(round(value, 1), " / 20"),
      paste0("Trend-line estimate for ", input$try_absences, " absences. ", compare_text(value)),
      ACCENT_ABSENCE
    )
  })

  output$try_failure_card <- renderUI({
    groups <- failure_result()$groups
    row <- groups[as.character(groups$failure_group) == input$try_failures, ]

    try_card(
      "By past failures",
      paste0(round(row$mean_grade, 1), " / 20"),
      paste0("Based on ", row$n, " students. ", compare_text(row$mean_grade)),
      ACCENT_FAILURE
    )
  })

  study_labels <- c("Less than 2 hours", "2 to 5 hours", "5 to 10 hours", "More than 10 hours")

  combined_result <- reactive({
    data <- selected_data() %>%
      filter(!is.na(studytime_code), !is.na(absences), !is.na(failures), !is.na(G3)) %>%
      mutate(has_failures = as.integer(failures > 0))

    validate(need(
      length(unique(data$studytime_code)) >= 2 &&
        length(unique(data$absences)) >= 2 &&
        length(unique(data$has_failures)) == 2,
      "There is not enough variety in the selected students to make a combined prediction."
    ))

    model <- lm(G3 ~ studytime_code + absences + has_failures, data = data)

    list(model = model, share = summary(model)$r.squared)
  })

  output$try_combined_card <- renderUI({
    r <- combined_result()

    new_student <- data.frame(
      studytime_code = match(input$try_study, study_labels),
      absences = input$try_absences,
      has_failures = as.integer(input$try_failures == "One or more past failures")
    )

    pred <- suppressWarnings(
      predict(r$model, newdata = new_student, interval = "prediction", level = 0.8)
    )

    clamp <- function(v) min(20, max(0, v))
    estimate <- clamp(pred[1, "fit"])
    low <- clamp(pred[1, "lwr"])
    high <- clamp(pred[1, "upr"])

    outcome <- if (estimate >= PASS_MARK) {
      "This is above the pass mark of 10."
    } else {
      "This is below the pass mark of 10."
    }

    div(
      class = "try-card",
      style = "border-top:5px solid #2C3E50; text-align:center;",
      h5("Final predicted score (all three factors combined)"),
      div(class = "try-value", paste0(round(estimate, 1), " / 20")),
      div(
        class = "try-note",
        paste0(
          "Students like this usually score between ", round(low, 1), " and ",
          round(high, 1), ". ", outcome
        )
      ),
      div(
        class = "try-note",
        paste0(
          "Based on ", input$try_study, " of study per week, ", input$try_absences,
          " absences and ", tolower(input$try_failures), ". ",
          compare_text(estimate)
        )
      ),
      div(
        class = "try-note",
        paste0(
          "Together, these three factors account for about ",
          round(100 * r$share, 1),
          "% of the differences in final grades, so other things matter too."
        )
      )
    )
  })

  output$download_findings <- downloadHandler(
    filename = function() {
      paste0(tolower(input$subject), "-findings.csv")
    },
    content = function(file) {
      study <- study_result()
      absence <- absence_result()
      failure <- failure_result()

      findings <- data.frame(
        Subject = input$subject,
        Factor = c("Study time", "Absences", "Past failures"),
        Finding = c(study$headline, absence$headline, failure$headline),
        Explanation = c(study$text, absence$text, failure$text),
        stringsAsFactors = FALSE
      )

      write.csv(findings, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
