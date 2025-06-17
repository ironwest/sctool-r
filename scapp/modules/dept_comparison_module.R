# --- 1. 必要ライブラリ ---
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(reactable)
library(shinycssloaders)
library(ggplot2)     # グラフ描画に使用
library(rmarkdown)   # PDF生成用


# --- 2. 外部ファイルとグローバル変数の読み込み ---
source("calculate_hensati.R")
source("calculate_sougoukrisk.R")
source("calculate_hensati_hyou.R")
source("setting_hensati_hyou.R")
source("setting_gh_analysis.R")

hensati_data <- read_csv("table11.csv")
nbjsq <- read_csv("nbjsq_question_text.csv")
nbjsqlabs <- read_csv("nbjsq_label_hensati.csv")
risk_calc_setting <- read_csv("../modules/risk_coefficients.csv")

skrisk_gyousyu <- c(
  "全産業",
  "医療・福祉",
  "運輸・郵便業",
  "卸売・小売業",
  "教育・学習支援業",
  "金融・保険業",
  "建設業",
  "公務",
  "サービス業",
  "情報通信業",
  "製造業"
)


# --- 3. モジュールUI --------

analysis_target_choices <- list(
  "基本" = list("基本" = "basic"),
  "BJSQ" = list("高ストレス者" = "hs","総合健康リスク" = "skrisk","ストレス判定図:負担" = "hantei_hutan","ストレス判定図:支援" = "hantei_sien"),
  "アウトカム" = list("ワークエンゲイジメント" = "we", "職場の一体感" = "sc", "ハラスメント" = "harass", "心理的ストレス反応" = "stress_reaction", "仕事の負担" = "work_hutan"),
  "資源" = list("作業レベル" = "sigen_sagyou","部署レベル" = "sigen_busyo","事業場レベル" = "sigen_jigyouba"),
  "その他" = list("その他" = "else")
)

analysis_target_tibble <- enframe(analysis_target_choices) |> 
  rename(bunrui = name) |> 
  mutate(dfr = map(value, ~{
    enframe(.) |> unnest(value)
  })) |> 
  select(bunrui,dfr) |> 
  unnest(dfr)

dept_comparison_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "比較設定", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        fluidRow(
          column(width = 4, 
                 h4("対象部門の選択"),
                 selectInput(ns("level_select"), "比較レベル選択", choices = c("部署（大分類）" = "dept1", "部署（大分類 - 中分類）" = "dept1_dept2")),
                 selectInput(ns("target_dept1"), "部署選択(大分類)", choices = NULL),
                 selectInput(ns("target_dept2"), "部署選択(中分類)", choices = NULL)
          ),
          column(width = 4, 
                 h4("総合健康リスク計算の設定"),
                 selectInput(ns("gyousyu"), "総合健康リスク計算の業種の選択",
                             choices = skrisk_gyousyu),
                 selectInput(ns("long_or_cross"), "総合健康リスク計算の種類",
                             choices = c("縦断(推奨)" = "long","横断" = "cross"))
          ),
          column(width = 4, actionButton(ns("run_comparison"), "比較を実行", icon = icon("search-plus"), class = "btn-success", style="margin-top: 25px;"))
        ),
      )
    ),
    fluidRow(
      box(
        title = "分析結果", width = 12, status = "info", solidHeader = TRUE,
        selectInput(ns("analysis_target"),"分析結果の選択", choices = analysis_target_choices),
        fluidRow(
          column(width = 6,
                 h4("グラフ"),
                 plotOutput(ns("graph")) |> withSpinner()
          ),
          column(width = 6,
                 h4("詳細"),
                 reactableOutput(ns("hyou")) |> withSpinner()
          )
        ),
        hr(),
        p("以下のボタンから、分析結果をまとめてPDFレポートとしてダウンロードできます。"),
        downloadButton(ns("download_report_button"), "PDFレポートをダウンロード", icon = icon("file-pdf"))
      )
    )
  )
}

# --- 4. モジュールサーバー (変更あり) ---
dept_comparison_module_server <- function(id, processed_data_now, processed_data_past = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- UIの動的更新 ---
    observe({
      req(processed_data_now())
      data <- processed_data_now()
      level <- input$level_select
      
      dept1_choice <- unique(data$dept1) |> sort()
      updateSelectInput(session, "target_dept1", choices = dept1_choice, selected = input$target_dept1)
      
      if(level == "dept1_dept2"){
        dept2_choice <- data |> 
          filter(dept1 == input$target_dept1) |> 
          pull(dept2) |> 
          unique() |> 
          sort()
        
        updateSelectInput(session, "target_dept2", choices = dept2_choice, selected = input$target_detp2)  
      }else{
        updateSelectInput(session, "target_dept2", choices = "---", selected = "---")  
      }
      
    })
    
    # --- 分析実行 (変更なし) ---
    analysis_results <- eventReactive(input$run_comparison, {
      req(processed_data_now(), input$target_dept1)
      
      #dept1かdept2かでロジックを分けるのは手間なので、grpという変数を作成する
      level <- input$level_select
      data_now <- processed_data_now()  
      data_past <- processed_data_past()  
      
      if(level == "dept1"){
        data_now <- data_now |> mutate(grp = dept1)
        data_past <- data_past |> mutate(grp = dept1)
        target_grp_name <- input$target_dept1
      }else if(level == "dept1_dept2"){
        data_now <- data_now |> mutate(grp = str_c(dept1,"-",dept2))
        data_past <- data_past |> mutate(grp = str_c(dept1,"-",dept2))
        target_grp_name <- str_c(input$target_dept1,"-",input$target_dept2)
      }
      
      #データの加工
      hyou_now <- calculate_hensati_hyou(
        current_data = data_now, 
        hensati_data = hensati_data, 
        target_sheet = "全体", #全体以外は現時点ではなし 
        group_vars = "grp", 
        nbjsq = nbjsq, 
        nbjsqlabs = nbjsqlabs,
        target_gyousyu = input$gyousyu,
        target_longorcross = input$long_or_cross,
        precise=TRUE
      )
      
      hyou_oa_now <- data_now |> 
        mutate(grp = "全体") |> 
        calculate_hensati_hyou(
          hensati_data = hensati_data, 
          target_sheet = "全体", #全体以外は現時点ではなし 
          group_vars = "grp", 
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = input$gyousyu,
          target_longorcross = input$long_or_cross,
          precise=TRUE
        )
      
      hyou_past <- calculate_hensati_hyou(
        current_data = data_past, 
        hensati_data = hensati_data, 
        target_sheet = "全体", #全体以外は現時点ではなし 
        group_vars = "grp", 
        nbjsq = nbjsq, 
        nbjsqlabs = nbjsqlabs,
        target_gyousyu = input$gyousyu,
        target_longorcross = input$long_or_cross,
        precise=TRUE
      )
      
      hyou_oa_past <- data_past |> 
        mutate(grp = "全体") |> 
        calculate_hensati_hyou(
          hensati_data = hensati_data, 
          target_sheet = "全体", #全体以外は現時点ではなし 
          group_vars = "grp", 
          nbjsq = nbjsq, 
          nbjsqlabs = nbjsqlabs,
          target_gyousyu = input$gyousyu,
          target_longorcross = input$long_or_cross,
          precise=TRUE
        )
      
      hyou_oa_now  <<- hyou_oa_now
      hyou_oa_past <<- hyou_oa_past
      hyou_now     <<- hyou_now
      hyou_past    <<- hyou_past
      
      hyou_oa_now  <- hyou_oa_now  |> 
        mutate(`時期` = "今回") |> 
        mutate(`対象` = grp) |> 
        mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1))
        
      hyou_oa_past <- hyou_oa_past |> 
        mutate(`時期` = "前回") |> 
        mutate(`対象` = grp) |> 
        mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1))
      
      hyou_now     <- hyou_now     |> 
        mutate(`時期` = "今回") |> 
        mutate(`対象` = grp) |> 
        mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1)) |> 
        mutate(color_this = grp == target_grp_name)
      
      hyou_past    <- hyou_past    |> 
        mutate(`時期` = "前回") |> 
        mutate(`対象` = grp) |> 
        mutate(`高ストレス者割合(%)` = scales::percent(`高ストレス者割合`, accuracy=0.1)) |> 
        mutate(color_this = grp == target_grp_name)
      
      analysis_target_tibble #基本とBJSQは独自、アウトカム、資源、その他は同じ処理でOK

      #基本‐基本
      select_these <- c("受検人数","不完全回答人数","高ストレス者人数","高ストレス者割合")
      percent_this <- c("高ストレス者割合")
      plot_this <- c("高ストレス者割合")
      
      
      make_gh_result <- function(hyou_oa_now, hyou_now, target_grp_name, select_these, plot_this, percent_this = NULL){
        
        if(!is.null(percent_this)){
          hyou_oa_now <- hyou_oa_now |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
          hyou_oa_past <- hyou_oa_past |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
          hyou_now <- hyou_now |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
          hyou_past <- hyou_past |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
        }
        
        
        hyou <- bind_rows(
          hyou_oa_now  |>  select(all_of(c("時期", "対象", select_these))),
          hyou_oa_past |>  select(all_of(c("時期", "対象", select_these))),
          hyou_now     |>  filter(`対象`==target_grp_name) |> select(all_of(c("時期", "対象", select_these))),
          hyou_past    |>  filter(`対象`==target_grp_name) |> select(all_of(c("時期", "対象", select_these)))
        )
        
        hyou_order <- c("対象", list_c(map(select_these, ~{ c(str_c(.,"_今回"), str_c(.,"_前回")) })))
        
        col_group_list <- list(
          colGroup(name = "全体"         ,align = "left", columns = c(str_c("全体"         , c("_今回","_前回")))),
          colGroup(name = target_grp_name,align = "left", columns = c(str_c(target_grp_name, c("_今回","_前回"))))
          colGroup(name = "全体"         , columns = c(str_c("全体"         , c("_今回","_前回"))), align = "left"),
          colGroup(name = target_grp_name, columns = c(str_c(target_grp_name, c("_今回","_前回"))), align = "left")
        )
        
        col_order <- c(str_c("全体", c("_今回","_前回")),str_c(target_grp_name, c("_今回","_前回")))
        
        col_setting_list <- map(set_names(col_order), ~{
          val <- str_extract(., "今回|前回")
          colDef(name = val, align = "left")
        })
          
          
        
        col_list <- list()
        col_list[["全体_今回"]] <- colDef(name = "今回", align="left")
        col_list[["全体_前回"]] <- colDef(name = "前回", align="left")
        col_list[[str_c(target_grp_name,"_今回")]] <- colDef(name = "今回", align="left")
        col_list[[str_c(target_grp_name,"_前回")]] <- colDef(name = "前回", align="left")
        col_list[["name"]] <- colDef(name = "項目", align="left" )
        
        
        hyou2 <- hyou |> 
          pivot_longer(cols = !c(`対象`,`時期`)) |> 
          pivot_wider(id_cols = `name`, names_from = c(`対象`,`時期`), values_from = value) 
        
        rhyou <-reactable(
          hyou2, 
          columnGroups = col_group_list, 
          columns = col_setting_list
        )
        hh <- reactable(hyou2, columnGroups = col_group_list, columns = col_list)
        
        oa_value <- hyou_oa_now[[plot_this]]
        browser()
        gg <- ggplot(hyou_now) +
          geom_vline(xintercept = oa_value, color = "grey50", linetype="dashed") +
          geom_col(aes(y = reorder(`対象`,`高ストレス者割合`), x = `高ストレス者割合`, fill=color_this), width=0.5) +
          labs(title = "高ストレス者(%)", y = NULL) +
          theme_bw(base_size = 16) +
          theme(legend.position = "none")
        
        ghres <- tribble(
          ~bunrui, ~name, ~value, ~h, ~g,
          "基本", "基本", "basic", rhyou, gg
        )
        
        return(ghres)
      }
      
          "基本", "基本", "basic", hh, gg
        )
        
      }
      
      ghres <- tibble()
      for(i in 1:length(ghsetting)){
        asetting <- enframe(ghsetting)$value[[i]]
        set_type <- asetting$type
        
        if(set_type == "gh"){
          set_select_these <- asetting$select_these
          set_plot_this <- asetting$plot_this
          set_percent_this <- asetting$percent_this
          
          ares <- make_gh_result(hyou_oa_now, hyou_now, target_grp_name, set_select_these, set_plot_this, set_percent_this)   
        }else if(set_type == "hanteizu"){
          
          tgtgyousyu <- input$gyousyu
          set_select_these <- asetting$select_these
          
          ares <- make_hanteizu_result()
          name_mapper <- c("仕事の負担" = "demand", "仕事のコントロール" = "control")  #ggplot のx軸、y軸にこの順番でラベルとして入る
          make_hanteizu_result <- function(hyou_oa_now, hyou_now, set_select_these,target_grp_name,risk_calc_setting, tgtsyokusyu,name_mapper){
            gdat_now <- bind_rows(
              hyou_oa_now |> select(grp, all_of(set_select_these)) |> mutate(type = "会社平均"),
              hyou_now    |> select(grp, all_of(set_select_these)) |> 
                mutate(type = case_when(
                  grp == target_grp_name ~ target_grp_name,
                  TRUE ~ "他"
                ))
            )
            
            gdat_past <- bind_rows(
              hyou_oa_past |> select(grp, all_of(set_select_these)) |> mutate(type = "会社平均"),
              hyou_past    |> select(grp, all_of(set_select_these)) |> 
                mutate(type = case_when(
                  grp == target_grp_name ~ target_grp_name,
                  TRUE ~ "他"
                ))
            )
            
            benchdata_long <- risk_calc_setting |> 
              filter(type=="long",gyousyu == tgtsyokusyu, coefname %in% c(set_select_these)) |> 
              select(grp = gyousyu, coefname, avg)
            
            benchdata <- benchdata_long |> 
              pivot_wider(id_cols = grp, names_from = coefname, values_from = avg) |> 
              mutate(type = str_c(tgtsyokusyu,"(ベンチマーク)"))
            
            gdat_fin <- gdat_now |> bind_rows(benchdata)  
            
            gg <- ggplot(gdat_fin) +
              geom_point(aes(x = demand, y = control, color = type, shape=type), size=2) +
              labs(color = NULL, shape=NULL, x = names(name_mapper[1]), y = names(name_mapper[2])) +
              theme_bw(base_size=14)
            
            col_order <- c("name",str_c("全体",c("_今回","_前回")),str_c(target_grp_name,c("_今回","_前回")))
            
            hdat <- bind_rows(
              gdat_now  |> filter(grp %in% c("全体",target_grp_name)) |> mutate(`時期` = "今回") |> select(`時期`,`grp`,all_of(set_select_these)),
              gdat_past |> filter(grp %in% c("全体",target_grp_name)) |> mutate(`時期` = "前回") |> select(`時期`,`grp`,all_of(set_select_these))
            ) |>
              mutate(across(all_of(set_select_these), ~{round(.,digits=2)})) |> 
              rename(name_mapper) |> 
              pivot_longer(cols = !c("時期","grp")) |> 
              pivot_wider(id_cols = name, values_from = value, names_from = c(grp,`時期`)) |> 
              relocate(all_of(col_order))
            
            benchdata_hyou <- benchdata_long |> 
              mutate(coefname = case_when(
                coefname == as.character(name_mapper[1]) ~ names(name_mapper[1]),
                coefname == as.character(name_mapper[2]) ~ names(name_mapper[2])
              )) |> 
              select(!!rlang::sym(str_c(tgtsyokusyu,"(ベンチマーク)")) := avg, name = coefname)
            
            hdat <- hdat |> 
              left_join(benchdata_hyou,by="name")
            
            col_group_list <- list(
              colGroup(name = "全体"         , columns = c(str_c("全体"         , c("_今回","_前回"))), align = "left"),
              colGroup(name = target_grp_name, columns = c(str_c(target_grp_name, c("_今回","_前回"))), align = "left")
            )
            
            col_list <- list()
            col_list[["全体_今回"]] <- colDef(name = "今回", align="left")
            col_list[["全体_前回"]] <- colDef(name = "前回", align="left")
            col_list[[str_c(target_grp_name,"_今回")]] <- colDef(name = "今回", align="left")
            col_list[[str_c(target_grp_name,"_前回")]] <- colDef(name = "前回", align="left")
            col_list[["name"]] <- colDef(name = "項目", align="left" )
            
            
            hh <- reactable(hdat, columns = col_list, columnGroups = col_group_list)  
            
            
          }
          
        }
        
        
        ghres <- bind_rows(ghres, ares)
      }
      
      gh_result <- make_gh_result(hyou_oa_now, hyou_now, target_grp_name, select_these, plot_this, percent_this = percent_this)
      return(gh_result)
    })
    
    #--- 基本テーブルのレンダリング------------------------
    
    # --- ★ 棒グラフのレンダリング (renderPlotly -> renderPlot) ---
    output$graph <- renderPlot({
      req(analysis_results())
      
      gg <- analysis_results() |> filter(value == input$analysis_target) |> pull(g)
      
      return(gg)
      
    })
    
    # 詳細テーブルのレンダリング (データをワイド形式に再変形)
    output$hyou <- renderReactable({
      req(analysis_results())
      
      hh <- analysis_results() |> filter(value == input$analysis_target) |> pull(h)
      
      return(hh[[1]])
    })
    
    # --- ★ PDFダウンロード (ggplot対応版) ---
    output$download_report_button <- downloadHandler(
      filename = function() {
        paste0("department_report_", input$target_dept, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(analysis_results())
        
        # 1. ggplot オブジェクトを生成
        plot_data_long <- analysis_results()
        comparison_plot <- ggplot(plot_data_long, aes(x = 平均値, y = reorder(尺度, 平均値), fill = group)) +
          geom_col(position = "dodge") +
          geom_vline(xintercept = 50, linetype = "dashed", color = "gray50") +
          labs(
            title = paste(isolate(input$target_dept), "の分析結果"),
            x = "平均スコア (T得点)", y = NULL, fill = "比較グループ"
          ) +
          theme_minimal(base_size = 12) + # PDF用に少しフォントサイズを調整
          theme(legend.position = "bottom")
        
        # 2. R Markdownに渡すパラメータを準備
        report_params <- list(
          selected_dept = input$target_dept,
          comparison_group_name = isolate(input$comparison_group),
          main_plot = comparison_plot, # ★ ggplotオブジェクトを直接渡す
          table_data = analysis_results() # テーブル用データ
        )
        
        # (Rmdファイルのコピーとrenderの呼び出しは変更なし)
        temp_report_path <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", temp_report_path, overwrite = TRUE)
        
        showNotification("PDFレポートを生成中です...", duration = 5, type = "message")
        
        rmarkdown::render(
          input = temp_report_path,
          output_file = file,
          params = report_params,
          envir = new.env(parent = globalenv())
        )
      }
    )
    
  })
}


# --- 4. スタンドアロンアプリ ---
# ダミーデータ
dummy_current_data <- read_csv("../demodata/processed_nbjsq_dummy_data1_alpha.csv")
dummy_previous_data <- read_csv("../demodata/processed_nbjsq_dummy_data2_alpha.csv")



# ---- アプリUI ----
ui <- dashboardPage(
  dashboardHeader(title = "部署比較モジュール テスト"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    dept_comparison_module_ui("dept_comparison")
  )
)

# ---- アプリサーバー ----
server <- function(input, output, session) {
  data_reactive_now <- reactive({ dummy_current_data })
  data_reactive_past <- reactive({ dummy_previous_data })
  
  dept_comparison_module_server(
    id = "dept_comparison",
    processed_data_now = data_reactive_now,
    processed_data_past = data_reactive_past
  )
}

# --- 6. R Markdown テンプレート ("report_template.Rmd") の内容 (変更あり) ---
# ★ グラフ描画部分を、渡されたggplotオブジェクトをprintするだけに変更
# --- report_template.Rmd の内容 ここから ---
# ```yaml
# ---
# title: "部署別 健康度・ストレス状況レポート"
# author: "ストレスチェック分析ツール"
# date: "`r format(Sys.time(), '%Y年%m月%d日')`"
# output: 
#   pdf_document:
#     latex_engine: xelatex
#     mainfont: "Yu Gothic" # Windowsの場合。Macなら "HiraginoSans-W3" など
# params:
#   selected_dept: "部署名"
#   comparison_group_name: "組織全体"
#   main_plot: NULL
#   table_data: NULL
# ---
# 
# ```{r setup, include=FALSE}
# library(knitr)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
# ```
# 
# ## 分析サマリー
# 
# **分析対象部署:** `r params$selected_dept`
# 
# **比較基準:** `r params$comparison_group_name`
# 
# ### 1. 棒グラフによる全体像
# 
# 以下のグラフは、選択された部署と`r params$comparison_group_name`の各尺度の平均値を比較したものです。
# 
# ```{r main-plot, fig.height=5, fig.width=7, fig.align='center'}
# # ★ ggplotオブジェクトを直接プリントするだけ
# if (!is.null(params$main_plot)) {
#   print(params$main_plot)
# }
# ```
# 
# ### 2. 詳細データ
# 
# ```{r detail-table}
# if (!is.null(params$table_data)) {
#   table_for_pdf <- params$table_data |>
#     pivot_wider(names_from = group, values_from = 平均値) |>
#     mutate(across(where(is.numeric), ~round(.x, 2)))
#   
#   knitr::kable(table_for_pdf, caption = "尺度別平均値")
# }
# ```
# --- report_template.Rmd の内容 ここまで ---


# --- 6. アプリケーションの実行 ---
# 注意: 実行する前に、上記の内容で "report_template.Rmd" ファイルを作成してください。
shinyApp(ui, server)