library(openxlsx2)
library(reactable)
library(showtext)

font_add_google("Noto Sans JP", family = "noto-sans-jp")

report_params <- read_rds("repparam.rds")


make_region <- function(data, initdim){
  initcol <- str_extract(initdim,"^[A-Z]+")
  initrow <- str_extract(initdim,"\\d+$")
  
  endcol <- col2int(initcol) + ncol(data) - 1
  endrow <- as.numeric(initrow) + nrow(data)
  
  return(str_c(initdim,":",int2col(endcol),endrow))
}

make_excel_report <- function(report_params, filepath){
  rm(wb)
  wb <- openxlsx2::wb_workbook(creator = "FactoryHealthCo",title = "SC分析レポート")  
  wb$set_base_font(font_name="BIZ UDP ゴシック")

  #タイトルページの作成------------------
  
  ## シートの作成
  sht1 <- "表紙"
  wb$add_worksheet(sheet=sht1)
  
  ## 内容の作成--------------------------
  wb$set_col_widths(sht1,cols=1,widths=25)
  wb$set_row_heights(sheet=sht1,rows=1,heights=32)
  wb$add_data(sht1,"SC集団分析レポート", dims = "A1")
  wb$add_font(sheet=sht1, dims="A1", size=18)
  
  wb$add_data(sht1,"分析対象：","A3")
  wb$add_data(sht1,report_params$dept1,"B3")
  wb$add_data(sht1,report_params$dept2,"B4")
  
  wb$add_data(sht1,"総合健康リスク計算基準集団:","A6")
  wb$add_data(sht1,report_params$skr_gyousyu,"B6")
  
  wb$add_data(sht1,"ベンチマーク業種:","A8")
  wb$add_data(sht1,report_params$skr_gyousyu,"B8")
  
  #以下、グラフとプロットのレンダリング START LOOP-----------------------
  showtext_auto()
  aghq <- report_params$rendering_data |> slice(13)
  
  asetting <- aghq$asetting[[1]]
  
  abunrui <- aghq$bunrui
  aname <- aghq$name
  hexcel <- aghq$hexcel[[1]]
  gexcel <- aghq$gexcel[[1]]
  qexcel <- aghq$qexcel[[1]]
  
  #シート名を作成--------
  shtname <- aname
  wb$add_worksheet(shtname)
  
  #タイトル行を作成----------
  wb$set_row_heights(sheet=shtname,rows=1,heights=32)
  wb$add_data(shtname,str_c(abunrui, ":",aname), dims = "A1")
  wb$add_font(sheet=shtname, dims="A1", size=18)
  
  #グラフ描画用のデータの表示-----------------
  graph_titletext <- str_c(asetting$plot_this)
  excel_data_graph <- gexcel$gdat |> 
    add_row(tibble(busyo = "事業場平均", plotthis = gexcel$avg_company)) |> 
    rename("部署" = busyo, "平均値" = plotthis) |> 
    arrange(desc(平均値)) |> 
    mutate(color_this = case_when(
      str_detect(`部署`,"^他\\d+$") ~ "他の部署",
      部署 == "事業場平均" ~ "事業場平均",
      TRUE ~ "対象"
    ))
  wb$add_data(shtname,graph_titletext, dims="A3")
  wb$add_font(shtname,dims = "A3",bold = TRUE, size=14)
  wb$add_data(shtname,excel_data_graph, dims="A5")
  
  graphheight <- nrow(excel_data_graph)
  ytextlength <- excel_data_graph$部署 |> nchar() |> max()
  
  if(ytextlength >= 25){
    excel_data_graph <- excel_data_graph |> 
      mutate(charlen = nchar(`部署`)) |> 
      mutate(`部署` = if_else(charlen >= 25, str_c(str_sub(`部署`,1,20),"・・・"),`部署`))
  }
  
  gg <- ggplot(excel_data_graph) +
    geom_col(aes(x = `平均値`, y = reorder(`部署`, `平均値`, decreasing=TRUE), fill = color_this), position="dodge", width=0.5) +
    labs(y = NULL, fill = NULL, title = asetting$plot_this) +
    theme_bw(base_size= 20, base_family = "noto-sans-jp") +
    theme(legend.position = "none", axis.text.y = element_text(margin = margin(t=0, r=2,b=0,l=-5*ytextlength, unit="pt")))
  
  print(gg)
  wb$add_plot(shtname, dims = "E5", width = 18, height = graphheight*0.80, file_type = "png", units = "cm", dpi=150)
  
  #尺度描画データの表示-----------------------
  syakudo_titletext <- asetting$select_these |> str_c(collapse="/")
  length_select_these <- length(asetting$select_these)
  
  wb$add_data(shtname,syakudo_titletext, dims="P3")
  wb$add_font(shtname,dims="P3", bold=TRUE, size=14)
  
  hdat <- hexcel$hyou2
  header1 <- colnames(hdat) |> str_remove_all("name|_今回|_前回") |> t() |>  as_tibble()
  header2 <- c("","今回","今回","前回","前回") |> t() |> as_tibble()
  wb$add_data(shtname, header1   , start_col = col2int("P"), start_row = 5, col_names = FALSE)
  wb$add_data(shtname, header2   , start_col = col2int("P"), start_row = 6, col_names = FALSE)
  wb$add_data(shtname, hdat      , start_col = col2int("P"), start_row = 7, col_names = FALSE)
  wb$merge_cells(shtname, dims="Q5:R5")
  wb$merge_cells(shtname, dims="S5:T5")
  wb$add_border(
    shtname, 
    dims=str_c("P5:T",7+length_select_these-1),
    inner_hgrid = "thin",
    inner_hcolor = wb_color(hex = "FF000000"),
    inner_vgrid = "thin",
    inner_vcolor = wb_color(hex = "FF000000")
  )
  wb$set_col_widths(shtname,cols = col2int("P"), widths = 30)
  
  #設問描画データの表示-------------------
  questions <- asetting$questions
  length_questions <- length(questions)
  
  aqex <- qexcel$qexcel[[1]]
  
  aqdat <- aqex$adat
  aqlab <- aqex$labels
  
  aqdat |> 
    
  
  
  wb$save("temp.xlsx")
  
  
  showtext_auto(FALSE)
}