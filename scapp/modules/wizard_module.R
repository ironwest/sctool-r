# 必要ライブラリ (ファイルの先頭にまとめて記述することを推奨)
library(shiny)
library(shinydashboard)
library(DT) # dataTableOutputのため
library(jsonlite) # 設定ファイルのJSON処理のため
library(readr)
library(stringr)
library(purrr)

# `%||%` 演算子 (NULL の場合にデフォルト値を返すヘルパー)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# --- ウィザードモジュール UI (wizard_module_ui) ---
# (前の回答で提供された wizard_module_ui のコードをここに記述)
wizard_module_ui <- function(id) {
  ns <- NS(id) # 名前空間関数を取得
  
  fluidPage(
    id = ns("wizard_page"),
    
    # ウィザードヘッダー/進捗表示
    uiOutput(ns("wizard_step_indicator_ui")),
    hr(),
    
    # ステップ1: CSVファイルのアップロード
    conditionalPanel(
      condition = paste0("output['", ns("wizard_show_step1"), "'] == true"), # 条件式を正しく構成
      div(
        id = ns("step1_ui"),
        h3("ステップ1: CSVファイルのアップロード"),
        p("ストレスチェック結果（CSV形式）をアップロードしてください。"),
        fileInput(ns("csv_file_input"), "CSVファイルを選択",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                  buttonLabel = "ファイルを選択...",
                  placeholder = "ファイルが選択されていません"),
        textOutput(ns("csv_upload_status_text")),
        dataTableOutput(ns("csv_preview_table")),
        br(),
        actionButton(ns("goto_step2_button"), "次へ：列名マッピング", class = "btn-primary", icon = icon("arrow-right"))
      )
    ),
    
    # ステップ2: 列名マッピング
    conditionalPanel(
      condition = paste0("output['", ns("wizard_show_step2"), "'] == true"),
      div(
        id = ns("step2_ui"),
        h3("ステップ2: 列名マッピング"),
        p("アップロードされたCSVの列名を、分析で使用する標準項目名に対応付けてください。"),
        p("過去に保存した設定ファイルを読み込むことも可能です。"),
        fluidRow(
          column(width = 6,
                 fileInput(ns("col_map_config_load_input"), "列マッピング設定を読み込む (.json)",
                           accept = ".json", buttonLabel = "読込...", placeholder = "設定ファイル未選択")
          ),
          column(width = 6,
                 style = "margin-top: 25px;",
                 downloadButton(ns("col_map_config_save_button"), "現在の列マッピング設定を保存")
          )
        ),
        hr(),
        h4("年齢、性別、所属の列設定"),
        wellPanel(
          fluidRow(
            column(width = 3, selectInput(ns("map_age_column"), "年齢に対応する列:", choices = c("未選択" = ""))),
            column(width = 3, selectInput(ns("map_gender_column"), "性別に対応する列:", choices = c("未選択" = ""))),
            column(width = 3, selectInput(ns("map_dept1_column"), "部署(大分類)に対応する列:", choices = c("未選択" = ""))),
            column(width = 3, selectInput(ns("map_dept2_column"), "部署(中分類)に対応する列:", choices = c("未選択" = "")))
          )
        ),
        h4("NBJSQの各質問項目に対応する列設定"),
        wellPanel(
          map(1:20, ~{
            i <- 4*(. - 1)
            fluidRow(
              column(width=3,selectInput(ns(str_c("colmap_nbjsq_",i+1)), str_c("質問:",i+1), choices = c("未選択" = ""))),
              column(width=3,selectInput(ns(str_c("colmap_nbjsq_",i+2)), str_c("質問:",i+2), choices = c("未選択" = ""))),
              column(width=3,selectInput(ns(str_c("colmap_nbjsq_",i+3)), str_c("質問:",i+3), choices = c("未選択" = ""))),
              column(width=3,selectInput(ns(str_c("colmap_nbjsq_",i+4)), str_c("質問:",i+4), choices = c("未選択" = "")))
            )
          })
          #uiOutput(ns("nbjsq_col_map_main_ui")) # 動的生成UI
        ),
        hr(),
        actionButton(ns("back_to_step1_button"), "戻る：CSVアップロード", icon = icon("arrow-left")),
        actionButton(ns("goto_step3_button"), "次へ：値マッピング", class = "btn-primary", icon = icon("arrow-right"))
      )
    ),
    
    # ステップ3: 値マッピング
    conditionalPanel(
      condition = paste0("output['", ns("wizard_show_step3"), "'] == true"),
      div(
        id = ns("step3_ui"),
        h3("ステップ3: 値マッピング"),
        p("特定の列について、CSVファイル内の実際の値を分析で使用する標準値に対応付けてください。"),
        p("過去に保存した設定ファイルを読み込むことも可能です。"),
        fluidRow(
          column(width = 6,
                 fileInput(ns("val_map_config_load_input"), "値マッピング設定を読み込む (.json)",
                           accept = ".json", buttonLabel = "読込...", placeholder = "設定ファイル未選択")
          ),
          column(width = 6,
                 style = "margin-top: 25px;",
                 downloadButton(ns("val_map_config_save_button"), "現在の値マッピング設定を保存")
          )
        ),
        hr(),
        h4("性別の値マッピング"),
        wellPanel(
          uiOutput(ns("gender_val_map_main_ui")) # 動的生成UI
        ),
        h4("NBJSQの値マッピング"),
        wellPanel(
          tabsetPanel(
            id = ns("nbjsq_val_map_tabs"), # tabsetPanel自体にもIDが必要な場合がある
            tabPanel("一括設定", value = "bulk_settings", uiOutput(ns("nbjsq_val_map_bulk_ui"))), # value属性を追加
            tabPanel("質問群A (Q1-17)", value = "group_a", uiOutput(ns("nbjsq_val_map_group_a_ui"))),
            tabPanel("質問群B (Q18-46)", value = "group_b", uiOutput(ns("nbjsq_val_map_group_b_ui"))),
            tabPanel("質問群C (Q47-55)", value = "group_c", uiOutput(ns("nbjsq_val_map_group_c_ui"))),
            tabPanel("質問群D (Q56-57)", value = "group_d", uiOutput(ns("nbjsq_val_map_group_d_ui"))),
            tabPanel("質問群E-H (Q58-80)", value = "group_e_h", uiOutput(ns("nbjsq_val_map_group_e_h_ui")))
          )
        ),
        hr(),
        actionButton(ns("back_to_step2_button"), "戻る：列名マッピング", icon = icon("arrow-left")),
        actionButton(ns("finish_setup_button"), "設定完了", class = "btn-success", icon = icon("check"))
      )
    )
  )
}


# --- ウィザードモジュール サーバー (wizard_module_server) ---
# (前の回答で提供された wizard_module_server のコードをここに記述)
wizard_module_server <- function(id, year_label) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # 名前空間関数をサーバー内で取得
    
    # --- リアクティブ値の初期化 ---
    rv <- reactiveValues(
      wizard_step = 1,
      csv_data = NULL,
      csv_headers = NULL,
      # 列マッピング (標準名 = CSV列名)
      column_map_age = "", # 初期値は空文字やNULL
      column_map_gender = "",
      column_map_dept1 = "",
      column_map_dept2 = "",
      column_map_nbjsq = stats::setNames(vector("list", 80), paste0("q", 1:80)), # 名前付きリスト
      # 値マッピング
      value_map_gender = list(male = "", female = ""),
      value_map_nbjsq_bulk = list(
        group_aefgh = rep(list(""), 4),
        group_b = rep(list(""), 4),
        group_c = rep(list(""), 4),
        group_d = rep(list(""), 4)
      ),
      # 各質問(1-80)に対し、4つのマッピング値をリストとして保持
      value_map_nbjsq_individual = stats::setNames(lapply(1:80, function(x) rep(list(""), 4)), paste0("q", 1:80))
    )
    
    # --- ウィザードステップ表示制御 ---
    output$wizard_show_step1 <- reactive({ rv$wizard_step == 1 })
    output$wizard_show_step2 <- reactive({ rv$wizard_step == 2 })
    output$wizard_show_step3 <- reactive({ rv$wizard_step == 3 })
    outputOptions(output, "wizard_show_step1", suspendWhenHidden = FALSE)
    outputOptions(output, "wizard_show_step2", suspendWhenHidden = FALSE)
    outputOptions(output, "wizard_show_step3", suspendWhenHidden = FALSE)
    
    # --- ウィザード進捗インジケーター ---
    output$wizard_step_indicator_ui <- renderUI({
      step_names <- c("1. CSVアップロード", "2. 列名マッピング", "3. 値マッピング")
      current_step_name <- step_names[rv$wizard_step]
      tagList(
        h4(paste0(year_label, " 設定 - 現在のステップ: ", current_step_name)),
        div(style = "background-color: #eee; border-radius: 5px; height: 20px;",
            div(style = paste0("background-color: #337ab7; width:", (rv$wizard_step/3)*100, "%; height: 100%; border-radius: 5px; text-align: center; color: white; line-height: 20px;"),
                paste0(round((rv$wizard_step/3)*100), "%")
            )
        )
      )
    })
    
    # --- ステップ1: CSVアップロード ---
    observeEvent(input$csv_file_input, {
      req(input$csv_file_input)
      tryCatch({
        df <- readr::read_csv(file=input$csv_file_input$datapath)
        rv$csv_data <- df
        rv$csv_headers <- colnames(df)
        # 列マッピング用selectInputの選択肢を更新
        header_choices <- c("未選択" = "", stats::setNames(rv$csv_headers, rv$csv_headers))
        updateSelectInput(session, "map_age_column", choices = header_choices, selected = rv$column_map_age)
        updateSelectInput(session, "map_gender_column", choices = header_choices, selected = rv$column_map_gender)
        updateSelectInput(session, "map_dept1_column", choices = header_choices, selected = rv$column_map_dept1)
        updateSelectInput(session, "map_dept2_column", choices = header_choices, selected = rv$column_map_dept2)
        
        output$csv_upload_status_text <- renderText({
          paste(year_label, "ファイル読み込み成功:", nrow(df), "行、", ncol(df), "列検出。")
        })
        output$csv_preview_table <- renderDataTable({
          head(df, 5)
        }, options = list(scrollX = TRUE, pageLength = 5, searching = FALSE, lengthChange = FALSE))
      }, error = function(e) {
        rv$csv_data <- NULL
        rv$csv_headers <- NULL
        output$csv_upload_status_text <- renderText({
          paste(year_label, "ファイル読み込みエラー:", e$message)
        })
        output$csv_preview_table <- renderDataTable(NULL)
      })
    })
    
    observeEvent(input$goto_step2_button, {
      if (!is.null(rv$csv_data)) {
        rv$wizard_step <- 2
      } else {
        showModal(modalDialog(title = "エラー", paste0(year_label, "のCSVファイルをアップロードしてください。"), easyClose = TRUE))
      }
    })
    
    # --- ステップ2: 列名マッピング ---
    # NBJSQ列マッピングUI
    header_first_numbers <- reactive({as.numeric(str_extract(rv$csv_headers,"\\d+"))})
    observe({
      map(1:80, ~{
        matched_index <- which(header_first_numbers() %in% .)
        #自動的に質問番号を選択するロジック
        matched_col <- rv$csv_headers[matched_index]
        if(length(matched_col) > 1){
          matched_col <- matched_col[1]
        }else if(length(matched_col) == 0){
          matched_col <- ""
        }
        
        updateSelectInput(session,str_c("colmap_nbjsq_",.), choices = rv$csv_headers, selected = matched_col)
      })  
    })
    
    
    # 列マッピング設定の読み込み
    observeEvent(input$col_map_config_load_input, {
      req(input$col_map_config_load_input, rv$csv_headers)
      tryCatch({
        config <- jsonlite::fromJSON(input$col_map_config_load_input$datapath)
        header_choices <- c("未選択" = "", stats::setNames(rv$csv_headers, rv$csv_headers))
        
        rv$column_map_age <- config$basic_attributes$age %||% ""
        rv$column_map_gender <- config$basic_attributes$gender %||% ""
        rv$column_map_dept1 <- config$basic_attributes$dept1 %||% ""
        rv$column_map_dept2 <- config$basic_attributes$dept2 %||% ""
        updateSelectInput(session, "map_age_column", choices = header_choices, selected = rv$column_map_age)
        updateSelectInput(session, "map_gender_column", choices = header_choices, selected = rv$column_map_gender)
        updateSelectInput(session, "map_dept1_column", choices = header_choices, selected = rv$column_map_dept1)
        updateSelectInput(session, "map_dept2_column", choices = header_choices, selected = rv$column_map_dept2)
        
        if (!is.null(config$nbjsq_questions) && length(config$nbjsq_questions) == 80) {
          # config$nbjsq_questions が名前付きリストであることを期待
          loaded_nbjsq_map <- config$nbjsq_questions
          if(is.list(loaded_nbjsq_map) && !is.null(names(loaded_nbjsq_map))){
            rv$column_map_nbjsq <- loaded_nbjsq_map
          } else if (is.vector(loaded_nbjsq_map) && length(loaded_nbjsq_map) == 80) { # 古い形式のベクトルかもしれない
            rv$column_map_nbjsq <- stats::setNames(as.list(loaded_nbjsq_map), paste0("q", 1:80))
          }
          
          lapply(1:80, function(i) {
            q_id <- paste0("q",i)
            updateSelectInput(session, paste0("map_nbjsq_", q_id), selected = rv$column_map_nbjsq[[q_id]] %||% "")
          })
        }
        showNotification(paste0(year_label, "の列マッピング設定を読み込みました。"), type = "message", session = session)
      }, error = function(e) {
        showModal(modalDialog(title = "エラー", paste0(year_label, "の設定ファイル読み込みに失敗: ", e$message), easyClose = TRUE))
      })
    })
    
    # 列マッピング設定の保存
    output$col_map_config_save_button <- downloadHandler(
      filename = function() {
        paste0("column_mapping_config_", year_label, "_", Sys.Date(), ".json")
      },
      content = function(file) {
        rv$column_map_age <- input$map_age_column %||% ""
        rv$column_map_gender <- input$map_gender_column %||% ""
        rv$column_map_dept1 <- input$map_dept1_column %||% ""
        rv$column_map_dept2 <- input$map_dept2_column %||% ""
        
        current_nbjsq_map_list <- list()
        
        for(i in 1:80) {
          q_id <- paste0("q", i)
          current_nbjsq_map_list[[q_id]] <- input[[paste0("colmap_nbjsq_", i)]] %||% ""
          
        }
        rv$column_map_nbjsq <- current_nbjsq_map_list
        
        current_config <- list(
          basic_attributes = list(
            age = rv$column_map_age,
            gender = rv$column_map_gender,
            dept1 = rv$column_map_dept1,
            dept2 = rv$column_map_dept2
          ),
          nbjsq_questions = rv$column_map_nbjsq
        )
        jsonlite::write_json(current_config, file, pretty = TRUE, auto_unbox = FALSE)
      }
    )
    
    observeEvent(input$back_to_step1_button, { rv$wizard_step <- 1 })
    observeEvent(input$goto_step3_button, {
      rv$column_map_age <- input$map_age_column %||% ""
      rv$column_map_gender <- input$map_gender_column %||% ""
      rv$column_map_dept1 <- input$map_dept1_column %||% ""
      rv$column_map_dept2 <- input$map_dept2_column %||% ""
      current_nbjsq_map_list <- list()
      for(i in 1:80) {
        q_id <- paste0("q", i)
        current_nbjsq_map_list[[q_id]] <- input[[paste0("map_nbjsq_", q_id)]] %||% ""
      }
      rv$column_map_nbjsq <- current_nbjsq_map_list
      rv$wizard_step <- 3
    })
    
    # --- ステップ3: 値マッピング ---
    get_unique_values_from_mapped_column <- function(mapped_column_name_in_rv_list_element) {
      # mapped_column_name_in_rv_list_element は rv$column_map_gender や rv$column_map_nbjsq[['q1']] のような実際の列名を指す
      req(rv$csv_data, mapped_column_name_in_rv_list_element)
      actual_column_name <- mapped_column_name_in_rv_list_element
      
      if (is.null(actual_column_name) || actual_column_name == "" || !actual_column_name %in% names(rv$csv_data)) {
        return(c("列未マッピング/データなし" = ""))
      }
      
      unique_vals <- sort(unique(as.character(stats::na.omit(rv$csv_data[[actual_column_name]]))))
      if(length(unique_vals) == 0) return(c("データなし" = ""))
      return(c("未選択" = "", stats::setNames(unique_vals, unique_vals)))
    }
    
    output$gender_val_map_main_ui <- renderUI({
      choices_list <- get_unique_values_from_mapped_column(rv$column_map_gender)
      tagList(
        fluidRow(
          column(width = 6, selectInput("val_map_gender_male", "CSV内の「男性」に対応する値:",
                                        choices = choices_list, selected = rv$value_map_gender$male %||% "")),
          column(width = 6, selectInput("val_map_gender_female", "CSV内の「女性」に対応する値:",
                                        choices = choices_list, selected = rv$value_map_gender$female %||% ""))
        )
      )
    })
    
    output$nbjsq_val_map_bulk_ui <- renderUI({
      req(rv$csv_data, rv$column_map_nbjsq)
      q1_col_name <- rv$column_map_nbjsq[["q1"]]
      sample_choices <- get_unique_values_from_mapped_column(q1_col_name)
      if (length(sample_choices) <= 1 && names(sample_choices)[1] != "未選択" ) { # names() check for empty unique_vals
        sample_choices <- c("未選択"="", "1","2","3","4")
      }
      
      tagList(
        p("各評価尺度に対応するCSV内の値を設定し、「一括設定」ボタンで関連する質問群に適用します。"),
        h5("スケール: そうだ(4) / まあそうだ(3) / ややちがう(2) / ちがう(1)"),
        fluidRow(lapply(1:4, function(i) column(width=3, selectInput(paste0("val_map_nbjsq_bulk_aefgh_",i), c("そうだ","まあそうだ","ややちがう","ちがう")[i], choices=sample_choices, selected=rv$value_map_nbjsq_bulk$group_aefgh[[i]] %||% "")))),
        actionButton("bulk_set_aefgh_button", "グループA,E,F,G,Hに一括設定"),
        hr(),
        h5("スケール: ほとんどなかった(1) / ときどきあった(2) / しばしばあった(3) / ほとんどいつもあった(4)"),
        fluidRow(lapply(1:4, function(i) column(width=3, selectInput(paste0("val_map_nbjsq_bulk_b_",i), c("ほとんどなかった","ときどきあった","しばしばあった","ほとんどいつもあった")[i], choices=sample_choices, selected=rv$value_map_nbjsq_bulk$group_b[[i]] %||% "")))),
        actionButton("bulk_set_b_button", "グループBに一括設定"),
        hr(),
        h5("スケール: 非常に(4) / かなり(3) / 多少(2) / まったくない(1)"),
        fluidRow(lapply(1:4, function(i) column(width=3, selectInput(paste0("val_map_nbjsq_bulk_c_",i), c("非常に","かなり","多少","まったくない")[i], choices=sample_choices, selected=rv$value_map_nbjsq_bulk$group_c[[i]] %||% "")))),
        actionButton("bulk_set_c_button", "グループCに一括設定"),
        hr(),
        h5("スケール: 満足(4) / まあ満足(3) / やや不満足(2) / 不満足(1)"),
        fluidRow(lapply(1:4, function(i) column(width=3, selectInput(paste0("val_map_nbjsq_bulk_d_",i), c("満足","まあ満足","やや不満足","不満足")[i], choices=sample_choices, selected=rv$value_map_nbjsq_bulk$group_d[[i]] %||% "")))),
        actionButton("bulk_set_d_button", "グループDに一括設定")
      )
    })
    
    generate_nbjsq_individual_value_map_ui_server <- function(question_indices, scale_labels, group_id_prefix_for_ui_layout_not_input_id, input_id_prefix_for_select) {
      req(rv$csv_data, rv$column_map_nbjsq)
      first_q_col_name <- rv$column_map_nbjsq[[paste0("q", question_indices[1])]] # 最初の質問の列名を取得
      choices_list <- get_unique_values_from_mapped_column(first_q_col_name)
      if (length(choices_list) <=1 && names(choices_list)[1] != "未選択") {
        choices_list <- c("未選択"="", "1","2","3","4")
      }
      
      question_uis <- lapply(question_indices, function(q_num) {
        q_id_str <- paste0("q", q_num)
        # rvから現在の選択値を取得。rv$value_map_nbjsq_individual[[q_id_str]] は4要素のリストを期待
        selected_vals_for_q <- rv$value_map_nbjsq_individual[[q_id_str]]
        if(is.null(selected_vals_for_q) || length(selected_vals_for_q) != 4) {
          selected_vals_for_q <- rep(list(""), 4) # フォールバック
        }
        
        fluidRow(
          column(width = 2, strong(paste0("Q", q_num))),
          lapply(1:4, function(i) {
            column(width = 2,
                   selectInput(paste0(input_id_prefix_for_select, "_q", q_num, "_val", i), # このIDでinputから値を取得
                               label = NULL,
                               choices = choices_list,
                               selected = selected_vals_for_q[[i]] %||% ""
                   ))
          })
        )
      })
      tagList(
        fluidRow(column(width = 2, ""), lapply(scale_labels, function(lbl) column(width = 2, strong(lbl)))),
        hr(),
        do.call(tagList, question_uis) # question_uisはリストのリストなので、適切に展開
      )
    }
    
    output$nbjsq_val_map_group_a_ui <- renderUI({ generate_nbjsq_individual_value_map_ui_server(1:17, c("そうだ","まあそうだ","ややちがう","ちがう"), "group_a", "val_map_nbjsq_ind_a") })
    output$nbjsq_val_map_group_b_ui <- renderUI({ generate_nbjsq_individual_value_map_ui_server(18:46, c("ほとんどなかった","ときどきあった","しばしばあった","ほとんどいつもあった"), "group_b", "val_map_nbjsq_ind_b") })
    output$nbjsq_val_map_group_c_ui <- renderUI({ generate_nbjsq_individual_value_map_ui_server(47:55, c("非常に","かなり","多少","まったくない"), "group_c", "val_map_nbjsq_ind_c") })
    output$nbjsq_val_map_group_d_ui <- renderUI({ generate_nbjsq_individual_value_map_ui_server(56:57, c("満足","まあ満足","やや不満足","不満足"), "group_d", "val_map_nbjsq_ind_d") })
    output$nbjsq_val_map_group_e_h_ui <- renderUI({ generate_nbjsq_individual_value_map_ui_server(58:80, c("そうだ","まあそうだ","ややちがう","ちがう"), "group_e_h", "val_map_nbjsq_ind_e_h") })
    
    observe_bulk_set_button_server <- function(button_id, bulk_input_prefix, question_indices) {
      observeEvent(input[[button_id]], {
        bulk_values <- sapply(1:4, function(i) input[[paste0(bulk_input_prefix, "_", i)]] %||% "")
        if (any(sapply(bulk_values, function(x) x == ""))) {
          showModal(modalDialog(title="注意", "一括設定する4つの値をすべて選択してください。", easyClose = TRUE))
          return()
        }
        for (q_idx in question_indices) {
          q_id_str <- paste0("q", q_idx)
          rv$value_map_nbjsq_individual[[q_id_str]] <- as.list(bulk_values)
        }
        showNotification(paste0(year_label, " の ", gsub("_button", "", button_id), " グループに一括設定を適用しました。"), type = "message", session=session)
      })
    }
    observe_bulk_set_button_server("bulk_set_aefgh_button", "val_map_nbjsq_bulk_aefgh", c(1:17, 58:80))
    observe_bulk_set_button_server("bulk_set_b_button", "val_map_nbjsq_bulk_b", 18:46)
    observe_bulk_set_button_server("bulk_set_c_button", "val_map_nbjsq_bulk_c", 47:55)
    observe_bulk_set_button_server("bulk_set_d_button", "val_map_nbjsq_bulk_d", 56:57)
    
    observeEvent(input$val_map_config_load_input, {
      req(input$val_map_config_load_input)
      tryCatch({
        config <- jsonlite::fromJSON(input$val_map_config_load_input$datapath)
        rv$value_map_gender <- config$gender %||% list(male="", female="")
        rv$value_map_nbjsq_bulk$group_aefgh <- config$nbjsq_bulk$group_aefgh %||% rep(list(""),4)
        rv$value_map_nbjsq_bulk$group_b     <- config$nbjsq_bulk$group_b %||% rep(list(""),4)
        rv$value_map_nbjsq_bulk$group_c     <- config$nbjsq_bulk$group_c %||% rep(list(""),4)
        rv$value_map_nbjsq_bulk$group_d     <- config$nbjsq_bulk$group_d %||% rep(list(""),4)
        
        loaded_individual_map <- config$nbjsq_individual
        if(!is.null(loaded_individual_map) && is.list(loaded_individual_map) && length(loaded_individual_map) == 80 && !is.null(names(loaded_individual_map))){
          rv$value_map_nbjsq_individual <- loaded_individual_map
        } # else keep default
        
        showNotification(paste0(year_label, "の値マッピング設定を読み込みました。"), type = "message", session=session)
      }, error = function(e) {
        showModal(modalDialog(title = "エラー", paste0(year_label, "の値マッピング設定ファイル読み込み失敗: ", e$message), easyClose = TRUE))
      })
    })
    
    output$val_map_config_save_button <- downloadHandler(
      filename = function() {
        paste0("value_mapping_config_", year_label, "_", Sys.Date(), ".json")
      },
      content = function(file) {
        rv$value_map_gender$male <- input$val_map_gender_male %||% ""
        rv$value_map_gender$female <- input$val_map_gender_female %||% ""
        
        rv$value_map_nbjsq_bulk$group_aefgh <- sapply(1:4, function(i) input[[paste0("val_map_nbjsq_bulk_aefgh_",i)]] %||% "")
        rv$value_map_nbjsq_bulk$group_b     <- sapply(1:4, function(i) input[[paste0("val_map_nbjsq_bulk_b_",i)]] %||% "")
        rv$value_map_nbjsq_bulk$group_c     <- sapply(1:4, function(i) input[[paste0("val_map_nbjsq_bulk_c_",i)]] %||% "")
        rv$value_map_nbjsq_bulk$group_d     <- sapply(1:4, function(i) input[[paste0("val_map_nbjsq_bulk_d_",i)]] %||% "")
        
        group_prefixes_map <- list(
          val_map_nbjsq_ind_a = 1:17, val_map_nbjsq_ind_b = 18:46,
          val_map_nbjsq_ind_c = 47:55, val_map_nbjsq_ind_d = 56:57,
          val_map_nbjsq_ind_e_h = 58:80
        )
        current_individual_map <- rv$value_map_nbjsq_individual # Start with existing or default
        for(prefix in names(group_prefixes_map)){
          indices <- group_prefixes_map[[prefix]]
          for(q_num in indices){
            q_id_str <- paste0("q", q_num)
            # Collect values for this question if its UI is currently rendered and inputs exist
            vals_for_q <- sapply(1:4, function(i) input[[paste0(prefix, "_q", q_num, "_val", i)]])
            if(!all(sapply(vals_for_q, is.null))){ # if any input was found
              current_individual_map[[q_id_str]] <- as.list(sapply(vals_for_q, `%||%`, ""))
            }
          }
        }
        rv$value_map_nbjsq_individual <- current_individual_map
        
        current_config <- list(
          gender = rv$value_map_gender,
          nbjsq_bulk = rv$value_map_nbjsq_bulk,
          nbjsq_individual = rv$value_map_nbjsq_individual
        )
        jsonlite::write_json(current_config, file, pretty = TRUE, auto_unbox = FALSE)
      }
    )
    
    observeEvent(input$back_to_step2_button, { rv$wizard_step <- 2 })
    observeEvent(input$finish_setup_button, {
      # 最終保存処理 (downloadHandler内で行っているため、ここではrvの値を最終確認する程度でも良い)
      # (downloadHandlerはボタンではないので、ここでinputからrvへの最終的な移し替えが必要な場合もある)
      showModal(modalDialog(
        title = paste0(year_label, " 設定完了"),
        paste0(year_label, "のデータ設定が完了しました。分析に進む準備ができました。"),
        footer = modalButton("閉じる")
      ))
      # rv$wizard_step <- 1 # or a specific "completed" step like 4
    })
    
    return(
      list(
        get_csv_data = reactive({ rv$csv_data }),
        get_column_map = reactive({ 
          list(
            age = rv$column_map_age, gender = rv$column_map_gender,
            dept1 = rv$column_map_dept1, dept2 = rv$column_map_dept2,
            nbjsq = rv$column_map_nbjsq
          )
        }),
        get_value_map = reactive({
          list(
            gender = rv$value_map_gender,
            nbjsq_bulk = rv$value_map_nbjsq_bulk,
            nbjsq_individual = rv$value_map_nbjsq_individual
          )
        }),
        is_setup_complete = reactive({ rv$wizard_step > 3 }) 
      )
    )
  })
}


# --- アプリケーション実行のためのUIとサーバー ---
ui <- dashboardPage(
  dashboardHeader(title = "ウィザードテストアプリ"),
  dashboardSidebar(
    sidebarMenu(
      id = "main_tabs", # sidebarMenuにはIDを振ることが推奨される
      menuItem("今年度設定", tabName = "current_year_tab", icon = icon("calendar-plus")),
      menuItem("昨年度設定", tabName = "previous_year_tab", icon = icon("calendar-minus"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "current_year_tab",
              h2("今年度データ設定ウィザード"),
              wizard_module_ui("current_year_wizard") # モジュールUI呼び出し (ID指定)
      ),
      tabItem(tabName = "previous_year_tab",
              h2("昨年度データ設定ウィザード"),
              wizard_module_ui("previous_year_wizard") # 別のIDでモジュールUI呼び出し
      )
    )
  )
)

server <- function(input, output, session) {
  # 今年度ウィザードモジュールのサーバーロジック呼び出し
  current_year_results <- wizard_module_server("current_year_wizard", year_label = "今年度")
  
  # 昨年度ウィザードモジュールのサーバーロジック呼び出し
  previous_year_results <- wizard_module_server("previous_year_wizard", year_label = "昨年度")
  
  # モジュールの返り値を利用する例 (デバッグ用)
  observe({
    req(current_year_results$is_setup_complete()) # setupが完了したら
    if (current_year_results$is_setup_complete()) {
      cat("今年度の設定が完了しました。\n")
      print(str(current_year_results$get_column_map()))
      # print(str(current_year_results$get_value_map()))
    }
  })
  observe({
    req(previous_year_results$is_setup_complete())
    if (previous_year_results$is_setup_complete()) {
      cat("昨年度の設定が完了しました。\n")
    }
  })
}

# アプリケーションの実行
shinyApp(ui, server)