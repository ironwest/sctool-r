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
  
  if(set_name == "総合健康リスク"){
    hyou2 <- hyou2 |> 
      filter(name != "総合健康リスク") |> 
      mutate(name = factor(name, levels = name_mapper, labels = names(name_mapper)))
  }
  
  
  hh <- reactable(hyou2, columnGroups = col_group_list, columns = col_list)
  
  
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
  
  
  
  
  ghres <- tribble(
    ~bunrui   , ~name   , ~h   , ~g,~q,
    set_bunrui, set_name, hh   , gg, NULL
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
  
  ghres <- tribble(
    ~bunrui, ~name, ~h, ~g, ~q,
    set_bunrui,set_name, hh, gg, NULL
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
    hyou_oa_past <- hyou_oa_past |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
    hyou_now <- hyou_now |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
    hyou_past <- hyou_past |> mutate(across(all_of(percent_this), ~{round(100*., digits = 1)}))
  }
  
  if(display_type == "hensati"){
    prefix <- ""
  }else if(display_type == "average"){
    prefix <- "average_"
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
  
  hh <- reactable(hyou2, columnGroups = col_group_list, columns = col_list)

  if(mode == "gh"){
    avg_company <- hyou_oa_now |> pull(all_of(str_c(prefix,plot_this)))
    if(display_type == "average"){
      avg_bench <- bench_data |> filter(`尺度名bench` == plot_this) |> pull(`平均値`)
    }else if(display_type == "hensati"){
      avg_bench <- 50
    }
    
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
    
    ghres <- tribble(
      ~bunrui   , ~name   , ~h   , ~g, 
      set_bunrui, set_name, hh   , gg
    )
    
  }else if(mode == "h"){
    
    ghres <- tribble(
      ~bunrui   , ~name   , ~h   , ~g  ,
      set_bunrui, set_name, hh   , NULL
    )
    
  }
  
  return(ghres)
}

make_qq_result <- function(data_now, data_past, target_grp_name, asetting, nbjsq, nbjsq_answerlabs){
  browser()
  data_now <<- data_now
  data_past <<- data_past
  target_grp_name <<- target_grp_name
  
  set_bunrui <<- asetting$bunrui
  set_name <<- asetting$name
  set_questions <<- asetting$questions
  
  nbjsq
  nbjsq_answerlabs
  
  colnames(data_now)
  data_now |> 
    filter(grp == target_grp_name) |> 
    select( q79, q80) |> 
    pivot_longer(cols = everything()) |> 
    count(name, value) |> 
    pivot_wider(id_cols = name, names_from = value, values_from = n)
  
  
  
  
  
  ghres <- tribble(
    ~bunrui   , ~name   , ~q,
    set_bunrui, set_name, qq
  )
}
