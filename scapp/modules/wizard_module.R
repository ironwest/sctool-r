# 必要ライブラリ (ファイルの先頭にまとめて記述することを推奨)
library(shiny)
library(shinydashboard)
library(DT) # dataTableOutputのため
library(jsonlite) # 設定ファイルのJSON処理のため
library(readr)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)

# `%||%` 演算子 (NULL の場合にデフォルト値を返すヘルパー)
`%||%` <- function(a, b) if (!is.null(a)) a else b


#値マッピング用で利用する質問番号の順番に質問の文章が含まれるベクトル
qtext <- read_csv("nbjsq_question_text.csv") |> dplyr::pull(qtext)




# --- ウィザードモジュール UI (wizard_module_ui) ---
# (前の回答で提供された wizard_module_ui のコードをここに記述)
wizard_module_ui <- function(id) {
  ns <- NS(id) # 名前空間関数を取得
  
  # 値マッピングのタブUI(可読性アップのため)-----------------
  overall_value_mapping_tabpanel <- 
    tabPanel(
      title = "一括設定", 
      value = "bulk_settings",
      style = paste(
        "margin-top:20px;",
        "height: 600px;",
        "overflow-y: auto;",
        "border: 1px solid #eeeeee;",
        "padding: 10px;"
      ),
      div(
        pmap(list(
          list("A","B","C","D","EH"), #セクションアルファベット
          list(
            "A)あなたの仕事についてうかがいます",
            "B)最近1か月間のあなたの状態についてうかがいます",
            "C)あなたの周りの方々についてうかがいます",
            "D)満足度についてうかがいます",
            "E-H)仕事・職場・会社について"
          ), #設問の名前
          list(
            c("そうだ","まあそうだ","ややちがう","ちがう"),
            c("ほとんどなかった","ときどきあった","しばしばあった","ほとんどいつもあった"),
            c("非常に","かなり","多少","まったくない"),
            c("満足","まあ満足","やや不満足","不満足"),
            c("そうだ","まあそうだ","ややちがう","ちがう")
          ) #選択のベクトル
        ), ~{
          div(
            fluidRow(
              style = "margin-top:20pt;",
              column(width = 2, ..2),
              column(width = 2, selectInput(inputId = ns(paste0("oavmap_",..1,"1")), label = ..3[1], choices = c())),
              column(width = 2, selectInput(inputId = ns(paste0("oavmap_",..1,"2")), label = ..3[2], choices = c())),
              column(width = 2, selectInput(inputId = ns(paste0("oavmap_",..1,"3")), label = ..3[3], choices = c())),
              column(width = 2, selectInput(inputId = ns(paste0("oavmap_",..1,"4")), label = ..3[4], choices = c())),
              column(width = 2, actionButton(inputId = ns(paste0("oavmap_",..1,"_admit")), label = "上書き設定"))
            ),
            hr()  
          )
        })
      )
    )
  
  value_mapping_tabpanels <- pmap(
    list(
      list("A","B","C","D","EH"), #panel title
      list(c(1:17),c(18:46),c(47:55),c(56:57),c(58:80)), #qnumbers
      list(
        c("そうだ","まあそうだ","ややちがう","ちがう"),
        c("ほとんどなかった","ときどきあった","しばしばあった","ほとんどいつもあった"),
        c("非常に","かなり","多少","まったくない"),
        c("満足","まあ満足","やや不満足","不満足"),
        c("そうだ","まあそうだ","ややちがう","ちがう")
      ) #選択のベクトル
    ), 
    ~{
      choice_of_qs <- ..3
      tabPanel(
        title = ..1, 
        div(
          style = paste(
            "margin-top:20px;",
            "height: 600px;",
            "overflow-y: auto;",
            "border: 1px solid #eeeeee;",
            "padding: 10px;"
          ),
          map(..2, ~{
            fluidRow(
              column(width=3,paste0(.,")",qtext[.])),
              column(width=2,selectInput(label=choice_of_qs[1], inputId = ns(paste0("vmap_q",.,"_1")), choices = c())),
              column(width=2,selectInput(label=choice_of_qs[2], inputId = ns(paste0("vmap_q",.,"_2")), choices = c())),
              column(width=2,selectInput(label=choice_of_qs[3], inputId = ns(paste0("vmap_q",.,"_3")), choices = c())),
              column(width=2,selectInput(label=choice_of_qs[4], inputId = ns(paste0("vmap_q",.,"_4")), choices = c()))
            )  
          })
        )
      ) 
    }
  )
  
 #ui本体---------------------
  fluidPage(
    id = ns("wizard_page"),

    ## ウィザードヘッダー/進捗表示 ------
    uiOutput(ns("wizard_step_indicator_ui")),
    hr(),

    ## ステップ1: CSVファイルのアップロード ------
    conditionalPanel(
      condition = paste0("output['", ns("wizard_show_step1"), "'] == true"),
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
    
    ## ステップ2: 列名マッピング --------
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
          style = paste(
            "margin-top:20px;",
            "height: 600px;",
            "overflow-y: auto;",
            "border: 1px solid #eeeeee;",
            "padding: 10px;"
          ),
          map(1:20, ~{
            i <- 4*(. - 1)
            fluidRow(
              column(width=3,selectInput(ns(str_c("colmap_nbjsq_",i+1)), str_c("質問:",i+1), choices = c("未選択" = ""))),
              column(width=3,selectInput(ns(str_c("colmap_nbjsq_",i+2)), str_c("質問:",i+2), choices = c("未選択" = ""))),
              column(width=3,selectInput(ns(str_c("colmap_nbjsq_",i+3)), str_c("質問:",i+3), choices = c("未選択" = ""))),
              column(width=3,selectInput(ns(str_c("colmap_nbjsq_",i+4)), str_c("質問:",i+4), choices = c("未選択" = "")))
            )
          })
        ),
        hr(),
        actionButton(ns("back_to_step1_button"), "戻る：CSVアップロード", icon = icon("arrow-left")),
        actionButton(ns("goto_step3_button"), "次へ：値マッピング", class = "btn-primary", icon = icon("arrow-right"))
      )
    ),
    
    ## ステップ3: 値マッピング ----------------------
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
          fluidRow(
            column(width = 6, selectInput(ns("val_map_gender_male"), "CSV内の「男性」に対応する値:",
                                          choices = c("男性","女性"), selected = NULL)),
            column(width = 6, selectInput(ns("val_map_gender_female"), "CSV内の「女性」に対応する値:",
                                          choices = c("男性","女性"), selected = NULL))
          )
        ),
        h4("NBJSQの値マッピング"),
        wellPanel(
          tabsetPanel(
            id = ns("nbjsq_val_map_tabs"),
            overall_value_mapping_tabpanel,
            value_mapping_tabpanels[[1]],
            value_mapping_tabpanels[[2]],
            value_mapping_tabpanels[[3]],
            value_mapping_tabpanels[[4]],
            value_mapping_tabpanels[[5]]
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
    
    # --- ステップ1: CSVアップロード ------------
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
    
    # --- ステップ2: 列名マッピング ---------------------
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
    
    # --- ステップ3: 値マッピング -----------------------------
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
    
    # --- 値マッピングUIのchoicesとselectedを更新する observe ブロック -------
    observe({
      req(rv$wizard_step == 3, rv$csv_data, rv$column_map_gender, rv$column_map_nbjsq) # ステップ3かつ必要なデータがある場合
      
      # 1. 性別の値マッピングUIの更新
      gender_choices <- get_unique_values_from_mapped_column(rv$column_map_gender) 
      updateSelectInput(session, "val_map_gender_male", choices = gender_choices, selected = rv$value_map_gender$male %||% "")
      updateSelectInput(session, "val_map_gender_female", choices = gender_choices, selected = rv$value_map_gender$female %||% "")
      
      # 2. 各選択肢の作成
      walk(1:80, ~{
        uniquevals <- sort(unique(rv$csv_data[[input[[paste0("colmap_nbjsq_",.)]]]])) #列マッピングされた列のユニークな数字
        updateSelectInput(session, inputId = paste0("vmap_q",.,"_1"), choices = uniquevals, selected = uniquevals[1])
        updateSelectInput(session, inputId = paste0("vmap_q",.,"_2"), choices = uniquevals, selected = uniquevals[2])
        updateSelectInput(session, inputId = paste0("vmap_q",.,"_3"), choices = uniquevals, selected = uniquevals[3])
        updateSelectInput(session, inputId = paste0("vmap_q",.,"_4"), choices = uniquevals, selected = uniquevals[4])
      })
      
      # 3. NBJSQ 一括設定タブのUI更新
      target_cols <- map_chr(1:80, ~input[[paste0("colmap_nbjsq_",.)]])
      possible_choices <- rv$csv_data |> 
        dplyr::select(!!!rlang::syms(target_cols)) |> 
        pivot_longer(cols = everything()) |> 
        pull(value) |> 
        unique() |> 
        sort() %>%
        {c("未選択",.)}
        
      walk(c("A","B","C","D","EH"), ~{
        updateSelectInput(session, inputId = paste0("oavmap_",.,"1"), choices = possible_choices)
        updateSelectInput(session, inputId = paste0("oavmap_",.,"2"), choices = possible_choices)
        updateSelectInput(session, inputId = paste0("oavmap_",.,"3"), choices = possible_choices)
        updateSelectInput(session, inputId = paste0("oavmap_",.,"4"), choices = possible_choices)
      })
    
    }) # observeの終わり
    
    ## --- 一括設定ボタンのサーバーロジック ------
    purrr::pmap(
      list(
        list("A","B","C","D","EH"),
        list(c(1:17),c(18:46),c(47:55),c(56:57),c(58:80))
      ),
      ~{
        #ns(paste0("oavmap_",..1,"_admit"))
        observeEvent(input[[paste0("oavmap_",..1,"_admit")]], {
          setval1 <- input[[paste0("oavmap_",..1,"1")]] %||% ""
          setval2 <- input[[paste0("oavmap_",..1,"2")]] %||% ""
          setval3 <- input[[paste0("oavmap_",..1,"3")]] %||% ""
          setval4 <- input[[paste0("oavmap_",..1,"4")]] %||% ""
          setvals <- c(setval1,setval2,setval3,setval4)
          
          if(any(setvals %in% "")){
            showModal(modalDialog(title="注意", "一括設定する4つの値をすべて選択してください。", easyClose = TRUE))
            return()
          }
          
          map(..2, ~{
            updateSelectInput(session, inputId = paste0("vmap_q",.,"_1"), selected = setval1)
            updateSelectInput(session, inputId = paste0("vmap_q",.,"_2"), selected = setval2)
            updateSelectInput(session, inputId = paste0("vmap_q",.,"_3"), selected = setval3)
            updateSelectInput(session, inputId = paste0("vmap_q",.,"_4"), selected = setval4)  
          })
          
          showNotification(
            paste0(year_label, " のセクション",.x,"の質問群に一括設定を適用しました。"), 
            type = "message", 
            session=session
          )
          
        })
        
      }
    )
    
    
    # --- 値マッピング設定の読み込み ---
    observeEvent(input$val_map_config_load_input, {
      req(input$val_map_config_load_input)
      tryCatch({
        config <- jsonlite::fromJSON(input$val_map_config_load_input$datapath)
        
        # 性別
        rv$value_map_gender <- config$gender %||% list(male="", female="")
        
        # 一括設定用
        rv$value_map_nbjsq_bulk$group_aefgh <- config$nbjsq_bulk$group_aefgh %||% rep(list(""),4)
        rv$value_map_nbjsq_bulk$group_b     <- config$nbjsq_bulk$group_b %||% rep(list(""),4)
        rv$value_map_nbjsq_bulk$group_c     <- config$nbjsq_bulk$group_c %||% rep(list(""),4)
        rv$value_map_nbjsq_bulk$group_d     <- config$nbjsq_bulk$group_d %||% rep(list(""),4)
        # for (section_alpha in c("A","B","C","D")) { # E-HはAと同じrvを使っているので不要
        #   for (j in 1:4) { # updateSelectInput(session, paste0("oavmap_",section_alpha,j), selected = ...)}
        # }
        
        # 個別設定用
        loaded_individual_map <- config$nbjsq_individual
        if(!is.null(loaded_individual_map) && is.list(loaded_individual_map) && length(loaded_individual_map) == 80 && !is.null(names(loaded_individual_map))){
          rv$value_map_nbjsq_individual <- loaded_individual_map
        }
        # for (q_num in 1:80) { for (val_idx in 1:4) { updateSelectInput(...) } }
        
        showNotification(paste0(year_label, "の値マッピング設定を読み込みました。UIが更新されます。"), type = "message", session=session)
        # rvが更新されたので、上記のobserveブロックが走り、UIのselected値が更新されるはず
      }, error = function(e) {
        showModal(modalDialog(title = "エラー", paste0(year_label, "の値マッピング設定ファイル読み込み失敗: ", e$message), easyClose = TRUE))
      })
    })
    
    # --- 値マッピング設定の保存 ---
    output$val_map_config_save_button <- downloadHandler(
      filename = function() {
        paste0("value_mapping_config_", year_label, "_", Sys.Date(), ".json")
      },
      content = function(file) {
        # 現在のUIの入力値をrvに保存する
        # 1. 性別
        rv$value_map_gender$male <- input$val_map_gender_male %||% ""
        rv$value_map_gender$female <- input$val_map_gender_female %||% ""
        
        # 2. 一括設定用 (UIからrvへ)
        sections_map_to_rv_bulk <- list(
          A = "group_aefgh", B = "group_b", C = "group_c", D = "group_d", `EH` = "group_aefgh"
        )
        for(section_alpha in names(sections_map_to_rv_bulk)){
          rv_key <- sections_map_to_rv_bulk[[section_alpha]]
          rv$value_map_nbjsq_bulk[[rv_key]] <- sapply(1:4, function(i) input[[paste0("oavmap_", section_alpha, i)]] %||% "")
        }
        
        # 3. 個別設定用 (UIからrvへ)
        # UI側のinputId: ns(paste0("vmap_q",.,"_1"))
        # (nsはサーバー側では自動的に解決されるのでinput$vmap_qX_Yでアクセス)
        current_individual_map <- stats::setNames(lapply(1:80, function(x) rep(list(""), 4)), paste0("q", 1:80))
        for(q_num in 1:80){
          q_id_str <- paste0("q", q_num)
          vals_for_q <- sapply(1:4, function(val_idx) {
            # UIのinputIdは ns(paste0("vmap_q", q_num, "_", val_idx)) なので、
            # サーバー側では input[[paste0("vmap_q", q_num, "_", val_idx)]] で取得
            input[[paste0("vmap_q", q_num, "_", val_idx)]] %||% ""
          })
          current_individual_map[[q_id_str]] <- as.list(vals_for_q)
        }
        rv$value_map_nbjsq_individual <- current_individual_map
        
        # rvの内容をJSONとして書き出し
        current_config_to_save <- list(
          gender = rv$value_map_gender,
          nbjsq_bulk = rv$value_map_nbjsq_bulk,
          nbjsq_individual = rv$value_map_nbjsq_individual
        )
        jsonlite::write_json(current_config_to_save, file, pretty = TRUE, auto_unbox = FALSE)
      }
    )
    
    # ステップ3のナビゲーションボタン
    observeEvent(input$back_to_step2_button, { rv$wizard_step <- 2 })
    observeEvent(input$finish_setup_button, {
      # 最終保存処理はdownloadHandlerに任せているので、ここでは完了メッセージのみ
      # 必要なら、ここでもUIからrvへの最終的な値の移し替えを行う
      showModal(modalDialog(
        title = paste0(year_label, " 設定完了"),
        paste0(year_label, "のデータ設定が完了しました。分析に進む準備ができました。"),
        footer = modalButton("閉じる")
      ))
      rv$wizard_step <- 4 # or a specific "completed" step like 4
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
    browser()
    if (current_year_results$is_setup_complete()) {
      cat("今年度の設定が完了しました。\n")
      print(str(current_year_results$get_column_map()))
      write_rds(current_year_results, "temp.rds")
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