#' @description generate reactable setting object for hensati hyou.
#' @param type Either "single" or "diff"
#' @return column_order 表の列の表示順番
#' @return default_col_def 基本的な列の表示設定
#' @return column_setting_list 個別の列の設定
#' @return col_group_list 列のヘッダーのグループ化定義
#' 
setting_hensati_hyou <- function(type = "single", group_vars){
  
  # 表のスタイル定義------------------
  color_pal <- list(
    light_green_bg = "#e6f5e6", # 背景の淡い緑
    dark_green_text = "#006400", # 文字の濃い緑
    light_red_bg = "#fde8e8",   # 背景の淡い赤
    dark_red_text = "#990000"    # 文字の濃い赤
  )
  
  # 縦書きヘッダーのためのスタイル定義
  vertical_header_style <- list(
    writingMode = "vertical-rl",
    transformOrigin = "bottom left",
    whiteSpace = "nowrap",
    padding = "3px 1px", # 上下左右のパディングを調整
    height = "auto",    # 列名の長さに応じて調整
    #width="auto",
    fontSize = "0.9em"   # フォントサイズを少し小さくする例
  )
  
  
  # 全ての列に適用するデフォルトの列定義
  if(type == "single"){
    style_function <- function(value) {
      # 値が数値でない、またはNAの場合はスタイルを適用しない
      if (!is.numeric(value) || is.na(value)) {
        return()
      }
      
      # スタイル変数を初期化
      bgcolor <- NULL
      fontcolor <- NULL
      fontweight <- NULL
      
      # 条件分岐（条件の厳しい順に並べ替え）
      if (value >= 60) {          # 60以上: 背景淡い緑、文字濃い緑、Bold
        bgcolor <- color_pal$light_green_bg
        fontcolor <- color_pal$dark_green_text
        fontweight <- "bold"
      } else if (value >= 55) {   # 55以上60未満: 背景淡い緑
        bgcolor <- color_pal$light_green_bg
      } else if (value <= 40) {   # 40以下: 背景淡い赤、文字濃い赤、Bold
        bgcolor <- color_pal$light_red_bg
        fontcolor <- color_pal$dark_red_text
        fontweight <- "bold"
      } else if (value <= 45) {   # 40より大きく45以下: 背景淡い赤
        bgcolor <- color_pal$light_red_bg
      }
      
      # 適用するスタイルをリストで返す
      list(background = bgcolor, color = fontcolor, fontWeight = fontweight)
    }  
  }else if(type == "diff"){
    style_function <- function(value) {
      # 値が数値でない、またはNAの場合はスタイルを適用しない
      if (!is.numeric(value) || is.na(value)) {
        return()
      }
      
      # スタイル変数を初期化
      bgcolor <- NULL
      fontcolor <- NULL
      fontweight <- NULL
      
      # 条件分岐（条件の厳しい順に並べ替え）
      if (value >= 5) {          # 60以上: 背景淡い緑、文字濃い緑、Bold
        bgcolor <- color_pal$light_green_bg
        fontcolor <- color_pal$dark_green_text
        fontweight <- "bold"
      } else if (value >= 3) {   # 55以上60未満: 背景淡い緑
        bgcolor <- color_pal$light_green_bg
      } else if (value <= -5) {   # 40以下: 背景淡い赤、文字濃い赤、Bold
        bgcolor <- color_pal$light_red_bg
        fontcolor <- color_pal$dark_red_text
        fontweight <- "bold"
      } else if (value <= -3) {   # 40より大きく45以下: 背景淡い赤
        bgcolor <- color_pal$light_red_bg
      }
      
      # 適用するスタイルをリストで返す
      list(background = bgcolor, color = fontcolor, fontWeight = fontweight)
    } 
  }
  
  
  
  default_col_def <- colDef(
    headerStyle = vertical_header_style,
    minWidth = 40, # 回転後のヘッダー幅（元の高さ）とデータ内容を考慮して調整
    align = "center", # データセルの中央揃え
    format = colFormat(digits=0),
    style = function(value) style_function(value),
  )
  
  default_col_def_nostyle <- colDef(
    headerStyle = vertical_header_style,
    minWidth = 40, # 回転後のヘッダー幅（元の高さ）とデータ内容を考慮して調整
    align = "center", # データセルの中央揃え
    format = colFormat(digits=0),
    style = function(value) {NULL}
  )
  
  # 横書き列の列定義
  make_horizontal_col_def <- function(name){
    #横書きヘッダーのためのスタイル定義
    horizontal_header_style <- list(
      writingMode = "horizontal-tb",
      transformOrigin = "bottom left",
      whiteSpace = "nowrap",
      padding = "5px 2px", # 上下左右のパディングを調整
      #width = "auto",    
      fontSize = "0.9em",   # フォントサイズを少し小さくする例
      backgroundColor = "#FFFFFF"
    )
    
    colDef(
      name = name,
      sticky = "left",
      headerStyle = horizontal_header_style,
      align = "left",
      minWidth = 100,
      vAlign = "bottom"
    )
  }
  
  
  col_setting <- list(
    group = group_vars,
    basic = c("受検人数", "不完全回答人数","高ストレス者人数","高ストレス者割合","総合健康リスク"),
    outcome = c("ソーシャル・キャピタル", "ワークエンゲイジメント","職場のハラスメント","心理的ストレス反応合計","仕事の負担合計"),
    sigen = c("作業レベル資源合計", "部署レベル資源合計","事業場レベル資源"),
    stress = c("活気","イライラ感","疲労感","不安感","抑うつ感"),
    hutan = c("仕事の量的負担","仕事の質的負担","身体的負担度","職場での対人関係","職場環境","情緒的負担","役割葛藤","WSB（－）"),
    sagyou = c("仕事のコントロール","技能の活用","仕事の適正","仕事の意義","役割明確さ","成長の機会"),
    busyo = c("上司の支援","同僚の支援","経済・地位報酬","尊重報酬","安定報酬","上司のリーダーシップ","上司の公正な態度","ほめてもらえる職場","失敗を認める職場"),
    jigyouba = c("経営層との信頼関係","変化への対応","個人の尊重","公正な人事評価","多様な労働者への対応","キャリア形成","WSB（＋）")
  )
  
  group_colors <- list(
    outcome  = "#dbeafe",
    sigen    = "#cffafe",
    stress   = "#ede9fe",
    hutan    = "#ffedd5",
    sagyou   = "#dcfce7",
    busyo    = "#fce7f3",  
    jigyouba = "#e0f2fe"
  )
  
  #コラムグループの設定
  col_group_list <- list(
    colGroup(name = "アウトカム"            , columns = col_setting$outcome , headerStyle = list(backgroundColor = group_colors$outcome , fontWeight = "bold"), sticky="left"),
    colGroup(name = "資源"                  , columns = col_setting$sigen   , headerStyle = list(backgroundColor = group_colors$sigen   , fontWeight = "bold"), sticky="left"),
    colGroup(name = "心理的ストレス反応合計", columns = col_setting$stress  , headerStyle = list(backgroundColor = group_colors$stress  , fontWeight = "bold")),
    colGroup(name = "仕事の負担合計"        , columns = col_setting$hutan   , headerStyle = list(backgroundColor = group_colors$hutan   , fontWeight = "bold")),
    colGroup(name = "作業レベル資源"        , columns = col_setting$sagyou  , headerStyle = list(backgroundColor = group_colors$sagyou  , fontWeight = "bold")),
    colGroup(name = "部署レベル資源"        , columns = col_setting$busyo   , headerStyle = list(backgroundColor = group_colors$busyo   , fontWeight = "bold")),
    colGroup(name = "事業場レベル資源"      , columns = col_setting$jigyouba, headerStyle = list(backgroundColor = group_colors$jigyouba, fontWeight = "bold"))
  )
  
  column_order <- c(col_setting$group,
                    col_setting$basic,
                    col_setting$outcome,
                    col_setting$sigen,
                    col_setting$stress,
                    col_setting$hutan,
                    col_setting$sagyou,
                    col_setting$busyo,
                    col_setting$jigyouba)
  
  column_setting_list <- map(set_names(column_order), ~ default_col_def)
  column_setting_list$dept1 <- make_horizontal_col_def("部署1")
  column_setting_list$dept2 <- make_horizontal_col_def("部署2")
  column_setting_list$age_kubun <- make_horizontal_col_def("年齢区分")
  column_setting_list$gender <- make_horizontal_col_def("性別")
  column_setting_list$`受検人数` <- default_col_def_nostyle
  column_setting_list$`不完全回答人数` <- default_col_def_nostyle
  column_setting_list$`高ストレス者人数` <- default_col_def_nostyle
  column_setting_list$`高ストレス者割合` <- default_col_def_nostyle
  column_setting_list$`総合健康リスク` <- default_col_def_nostyle

  
  
  #列の色を設定する
  column_setting_list[["心理的ストレス反応合計"]]$headerStyle$backgroundColor <- group_colors$stress
  column_setting_list[["仕事の負担合計"]]$headerStyle$backgroundColor <- group_colors$hutan
  column_setting_list[["作業レベル資源合計"]]$headerStyle$backgroundColor <- group_colors$sagyou
  column_setting_list[["部署レベル資源合計"]]$headerStyle$backgroundColor <- group_colors$busyo
  column_setting_list[["事業場レベル資源"]]$headerStyle$backgroundColor <- group_colors$jigyouba
  
  for(tgtgrp in c("stress","hutan","sagyou","busyo","jigyouba")){
    for(tgtcol in col_setting[[tgtgrp]]){
      column_setting_list[[tgtcol]]$headerStyle$backgroundColor <- group_colors[[tgtgrp]]  
    } 
  }
  
  return(
    list(
      column_order = column_order,
      default_col_def = default_col_def,
      column_setting_list = column_setting_list,
      col_group_list = col_group_list
    )
  )
  
}
