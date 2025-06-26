make_ghbase_result <- function(hyou_oa_now, 
                               hyou_now, 
                               hyou_oa_past, 
                               hyou_past, 
                               target_grp_name, 
                               asetting){
  
  
  select_these <- asetting$select_these
  plot_this <- asetting$plot_this
  percent_this <- asetting$percent_this
  set_bunrui <- asetting$bunrui
  set_name <- asetting$name
  name_mapper <- asetting$name_mapper
  
  if(nrow(hyou_oa_past)==0){
    hyou_oa_past <- hyou_oa_past |> add_row(`時期` = "前回", `対象` = "全体")
  }
  if(nrow(hyou_past)==0){
    hyou_past    <- hyou_past |> add_row(`時期` = "前回", `対象` = target_grp_name)
  }

  if(!is.null(percent_this)){
    hyou_oa_now <- hyou_oa_now |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
    hyou_now <- hyou_now |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
    hyou_oa_past <- hyou_oa_past |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))  
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
    colGroup(name = target_grp_name,align = "left", columns = c(str_c(target_grp_name, c("_今回","_前回")))),
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
  
  if(all(is.na(hyou2$`全体_前回`))){
    hyou2 <- hyou2 |> 
      mutate(across(matches("_前回"), ~{"-"}))
  }
  
  if(set_name == "総合健康リスク"){
    hyou2 <- hyou2 |> 
      filter(name != "総合健康リスク") |> 
      mutate(name = factor(name, levels = name_mapper, labels = names(name_mapper)))
  }
  
  hh <- reactable(hyou2, columnGroups = col_group_list, columns = col_list)
  
  hexcel <- tibble::lst(
    hyou2, col_group_list, col_list
  )
  
  gdat <- hyou_now |> 
    select(busyo = `対象`, color_this, plotthis = all_of(plot_this)) |>
    arrange(color_this, plotthis) |> 
    mutate(busyo = if_else(color_this,busyo,str_c("他",1:n())))
  
  avg_company <- hyou_oa_now |> 
    pull(all_of(plot_this))
  
  gg <- ggplot(gdat) +
    geom_col(aes(y = reorder(busyo, plotthis), x = plotthis, fill = color_this), width = 0.5) +
    geom_vline(aes(xintercept = avg_company, color = "企業平均", linetype = "企業平均")) +
    labs(title = str_c(set_bunrui, ":", set_name),y = NULL,x = plot_this,color = NULL,linetype = NULL) +
    scale_color_manual(values = c("企業平均" = "grey50")) +
    scale_linetype_manual(values = c("企業平均" = "dashed")) +
    theme_classic(base_size = 16) +
    theme(legend.position = "bottom") +
    guides(fill="none")  
  
  
  hexcel <- tibble::lst(
    hyou2, col_group_list, col_list
  )
  
  gexcel <- list(
    avg_company = avg_company,
    gdat = gdat
  )
  
  
  ghres <- tribble(
    ~bunrui   , ~name   , ~h   , ~g,   ~q, ~hexcel, ~gexcel, ~qexcel, ~asetting,
    set_bunrui, set_name, hh   , gg, NULL,  hexcel,  gexcel, NULL   ,  asetting
  )
  
  return(ghres)
}

make_hanteizu_result <- function(hyou_oa_now, 
                                 hyou_now, 
                                 hyou_oa_past, 
                                 hyou_past, 
                                 asetting ,
                                 target_grp_name,
                                 risk_calc_setting, 
                                 tgtsyokusyu){
  
  set_bunrui <- asetting$bunrui
  set_name <- asetting$name
  set_select_these <- asetting$select_these
  name_mapper <- asetting$name_mapper
  graph_labels <- asetting$graph_labels
  
  
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
    geom_point(aes(x = !!rlang::sym(set_select_these[1]), y = !!rlang::sym(set_select_these[2]), color = type, shape=type), size=2) +
    labs(color = NULL, shape=NULL, x = names(graph_labels[1]), y = names(graph_labels[2])) +
    theme_bw(base_size=14)
  
  col_order <- c("name",str_c("全体",c("_今回","_前回")),str_c(target_grp_name,c("_今回","_前回")))
  
  if(nrow(gdat_past)==0){
    gdat_past <- gdat_past |> 
      add_row(grp = c("全体", target_grp_name))
  }
  
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
  
  if(all(is.na(hdat$`全体_前回`))){
    hdat <- hdat |> 
      mutate(across(matches("_前回"), ~"-"))
  }
  
  browser()
  
  hh <- reactable(hdat, columns = col_list, columnGroups = col_group_list)  
  
  hexcel <- tibble::lst(
    hdat, col_group_list, col_list
  )
  
  gexcel <- list(
    gdat = gdat_fin
  )
  
  
  ghres <- tribble(
    ~bunrui   , ~name   , ~h   , ~g,   ~q, ~hexcel, ~gexcel, ~qexcel, ~asetting,
    set_bunrui, set_name, hh   , gg, NULL,  hexcel,  gexcel, NULL   ,  asetting
  )
  
  return(ghres)
  
}

make_gh_result <- function(hyou_oa_now, 
                           hyou_now, 
                           hyou_oa_past, 
                           hyou_past, 
                           hensati_data, 
                           tgtnbjsq_gyousyu, 
                           target_grp_name, 
                           asetting, 
                           mode="gh", 
                           display_type = "average"){
  
  select_these <- asetting$select_these
  plot_this <- asetting$plot_this
  percent_this <- asetting$percent_this
  set_bunrui <- asetting$bunrui
  set_name <- asetting$name
  
  # ベンチマークの設定#---------------------------------------------------------------
  bench_data <- hensati_data |> 
    filter(qtype == "NBJSQ") |> 
    filter(sheet == tgtnbjsq_gyousyu) |> 
    filter(`尺度名bench` %in% select_these) |> 
    select(`尺度名bench`, `平均値`)  
  
  if(display_type == "average"){
    #do nothing
  }else if(display_type == "hensati"){
    bench_data <- bench_data |> 
      mutate(`平均値` = 50)
  }
  
  
  if(!is.null(percent_this)){
    hyou_oa_now <- hyou_oa_now |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
    hyou_now <- hyou_now |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
    hyou_oa_past <- hyou_oa_past |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
    hyou_past <- hyou_past |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
  }
  
  if(display_type == "hensati"){
    prefix <- ""
  }else if(display_type == "average"){
    prefix <- "average_"
  }
  
  if(nrow(hyou_oa_past)==0){
    hyou_oa_past <- hyou_oa_past |> 
      add_row(`時期` = "前回", `対象` = "全体")
  }
  
  if(nrow(hyou_past)==0){
    hyou_past <- hyou_past |> 
      add_row(`時期` = "前回", `対象` = target_grp_name)
  }
  
  hyou <- bind_rows(
    hyou_oa_now  |>  select(all_of(c("時期", "対象", str_c(prefix,select_these)))),
    hyou_oa_past |>  select(all_of(c("時期", "対象", str_c(prefix,select_these)))),
    hyou_now     |>  filter(`対象`==target_grp_name) |> select(all_of(c("時期", "対象", str_c(prefix,select_these)))),
    hyou_past    |>  filter(`対象`==target_grp_name) |> select(all_of(c("時期", "対象", str_c(prefix,select_these))))
  )
  
  if(display_type == "average"){
    hyou <- hyou |> setNames(str_remove(colnames(hyou),"^average_"))  
  }
  
  
  hyou_order <- c("対象", list_c(map(select_these, ~{ c(str_c(.,"_今回"), str_c(.,"_前回")) })))
  
  col_group_list <- list(
    colGroup(name = "全体"         ,align = "left", columns = c(str_c("全体"         , c("_今回","_前回")))),
    colGroup(name = target_grp_name,align = "left", columns = c(str_c(target_grp_name, c("_今回","_前回")))),
    colGroup(name = "全体"         , columns = c(str_c("全体"         , c("_今回","_前回"))), align = "left"),
    colGroup(name = target_grp_name, columns = c(str_c(target_grp_name, c("_今回","_前回"))), align = "left")
  )
  
  col_order <- c(str_c("全体", c("_今回","_前回")),str_c(target_grp_name, c("_今回","_前回")))
  
  col_setting_list <- map(set_names(col_order), ~{
    val <- str_extract(., "今回|前回")
    colDef(name = val, align = "left")
  })
  
  col_list <- list()
  col_list[["全体_今回"]] <- colDef(name = "今回", align="left", format = colFormat(digits=2))
  col_list[["全体_前回"]] <- colDef(name = "前回", align="left", format = colFormat(digits=2))
  col_list[[str_c(target_grp_name,"_今回")]] <- colDef(name = "今回", align="left", format = colFormat(digits=2))
  col_list[[str_c(target_grp_name,"_前回")]] <- colDef(name = "前回", align="left", format = colFormat(digits=2))
  col_list[["name"]] <- colDef(name = "項目", align="left" , format = colFormat(digits=2))
  col_list[["ベンチマーク"]] <- colDef(name = "ベンチマーク", align="left" , format = colFormat(digits=2))
  
  hyou2 <- hyou |> 
    pivot_longer(cols = !c(`対象`,`時期`)) |> 
    pivot_wider(id_cols = `name`, names_from = c(`対象`,`時期`), values_from = value) |> 
    left_join(bench_data |> rename(`ベンチマーク` = `平均値`), by=c("name"="尺度名bench"))
  
  if(all(is.na(hyou2$全体_前回))){
    hyou2 <- hyou2 |> 
      mutate(across(matches("前回"),~{"-"}))
  }
  
  browser()
  
  hh <- reactable(hyou2, columnGroups = col_group_list, columns = col_list)

  if(mode == "gh"){
    avg_company <- hyou_oa_now |> pull(all_of(str_c(prefix,plot_this)))
    if(display_type == "average"){
      avg_bench <- bench_data |> filter(`尺度名bench` == plot_this) |> pull(`平均値`)
    }else if(display_type == "hensati"){
      avg_bench <- 50
    }
    
    browser()
    
    gdat <- hyou_now |> 
      select(busyo = `対象`, color_this, plotthis = all_of(str_c(prefix,plot_this))) |>
      arrange(color_this, plotthis) |> 
      mutate(busyo = if_else(color_this,busyo,str_c("他",1:n())))
    
    gg <- ggplot(gdat) +
      geom_col(aes(y = reorder(busyo, plotthis), x = plotthis, fill = color_this), width = 0.5) +
      geom_vline(aes(xintercept = avg_company, color = "企業平均", linetype = "企業平均")) +
      geom_vline(aes(xintercept = avg_bench, color = "ベンチマーク", linetype = "ベンチマーク")) +
      labs(title = str_c(set_bunrui, ":", set_name),y = NULL,x = plot_this,color = NULL,linetype = NULL) +
      scale_color_manual(values = c("企業平均" = "grey50", "ベンチマーク" = "blue")) +
      scale_linetype_manual(values = c("企業平均" = "dashed", "ベンチマーク" = "dotted")) +
      theme_classic(base_size = 16) +
      theme(legend.position = "bottom") +
      guides(fill="none")  
    
    
    
  }else if(mode == "h"){
    
    gg <- NULL
    gdat <- NULL
    avg_company <- NULL
    avg_bench <- NULL
  }
  
  hexcel <- tibble::lst(
    hyou2, col_group_list, col_list
  )
  
  gexcel <- list(
    avg_company = avg_company,
    avg_bench = avg_bench,
    gdat = gdat
  )
  
  
  ghres <- tribble(
    ~bunrui   , ~name   , ~h   , ~g, ~hexcel, ~gexcel,  ~asetting,
    set_bunrui, set_name, hh   , gg,  hexcel,  gexcel,  asetting
  )
  
  return(ghres)
}

make_qq_result <- function(data_now, 
                           data_past, 
                           target_grp_name, 
                           asetting, nbjsq, 
                           nbjsq_answerlabs){
  
  set_bunrui <- asetting$bunrui
  set_name <- asetting$name
  set_questions <- asetting$questions

  #回答状況をfilter
  ansdata <- data_now |> 
    filter(grp == target_grp_name) |> 
    select( all_of(str_c("q",set_questions)) ) |> 
    pivot_longer(cols = everything()) |> 
    count(name, value) |>
    pivot_wider(id_cols = name, names_from = value, values_from = n) |> 
    mutate(qnum = str_extract(name,"\\d+") |> as.numeric())
  
  #設問内容をfilter
  textdata <- nbjsq |> 
    filter(qnum %in% set_questions) |> 
    select(syakudo_minor,  qnum, qtext) |> 
    mutate(qnum = as.numeric(qnum))
  
  #回答内容をfilter
  labeldata <- nbjsq_answerlabs |> 
    filter(q %in% set_questions) |> 
    rename(qnum = q) |> 
    mutate(qnum = as.numeric(qnum))
  
  hdat <- textdata |> 
    left_join(ansdata, by="qnum") |> 
    left_join(labeldata, by="qnum")
  
  bar_style <- function(width = 1, fill = "#e6e6e6", height = "65%",
                        align = c("left", "right"), color = NULL) {
    align <- match.arg(align)
    if (align == "left") {
      position <- paste0(width * 100, "%")
      image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
    } else {
      position <- paste0(100 - width * 100, "%")
      image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
    }
    list(
      backgroundImage = image,
      backgroundSize = paste("100%", height),
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center",
      color = color
    )
  }
  
  hdat <- hdat |> 
    group_nest(label1,label2,label3,label4) |> 
    mutate(qtemp  = pmap(list(label1,label2,label3,label4,data), ~{
      
      adat <- ..5
      
      adat <- adat |> 
        mutate(good_percent = if_else(good_value == 1, (`1`+`2`)/(`1`+`2`+`3`+`4`), (`3`+`4`)/(`1`+`2`+`3`+`4`)))
    
      
      GOOD_COLOR <- "lightcyan"  # 良いイメージの色
      BAD_COLOR <- "mistyrose"   # 悪いイメージの色
      
      col_list <- list(
        syakudo_minor = colDef(name = "尺度", width=250, vAlign = "center", align = "left"),
        qnum          = colDef(name = "Q", width=40, vAlign = "center", align = "left"),
        qtext         = colDef(name = "設問", width=600, vAlign = "center", align="left"),
        name          = colDef(show = FALSE),
        `1`           = colDef(name = ..1, width=70, vAlign = "center", align="center",
                               style = function(value, index) {
                                 good_value_row <- adat$good_value[index]
                                 if (good_value_row == 1) {
                                   list(backgroundColor = GOOD_COLOR) # good_valueが1なら「良い」色
                                 } else if (good_value_row == 4) {
                                   list(backgroundColor = BAD_COLOR) # good_valueが4なら「悪い」色
                                 } else {
                                   list(backgroundColor = NEUTRAL_COLOR) # その他ならデフォルト色
                                 }
                               }),
        `2`           = colDef(name = ..2, width=70, vAlign = "center",align="center"),
        `3`           = colDef(name = ..3, width=70, vAlign = "center",align="center"),
        `4`           = colDef(name = ..4, width=70, vAlign = "center",align="center",
                               style = function(value, index) {
                                 good_value_row <- adat$good_value[index] # その行のgood_valueを取得
                                 if (good_value_row == 1) {
                                   list(backgroundColor = BAD_COLOR) # good_valueが1なら「悪い」色
                                 } else if (good_value_row == 4) {
                                   list(backgroundColor = GOOD_COLOR) # good_valueが4なら「良い」色
                                 } else {
                                   list(backgroundColor = NEUTRAL_COLOR) # その他ならデフォルト色
                                 }
                               }),
        good_value    = colDef(show=FALSE),
        good_percent  = colDef(name = "良好な回答の割合", format = colFormat(digits=1, percent=TRUE), vAlign = "center",align="center",
                               style = function(value) {
                                 bar_style(width = value / 1, fill = "#3CB371", color = "#000")
                               },
                                width = 300)
        
      )
      
      browser()
      
      return(
        list(
          reactable_table = reactable(adat,columns = col_list,bordered = TRUE),
          qexcel = list(adat = adat, col_list = col_list, labels = c(..1,..2,..3,..4))  
        )
      )

    })) |> 
    mutate(qhyou  = map(qtemp, ~.[["reactable_table"]])) |> 
    mutate(qexcel = map(qtemp, ~.[["qexcel"]]))
  
  qq     <- hdat |> select(qhyou)
  qexcel <- hdat |> select(qexcel)
  
  ghres <- tribble(
    ~bunrui   , ~name   , ~q,  ~qexcel,
    set_bunrui, set_name, qq,  qexcel
  )
  
  return(ghres)
}
