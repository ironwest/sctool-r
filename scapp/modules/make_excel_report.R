library(openxlsx2)
library(reactable)
library(mschart)


report_params <- read_rds("repparam.rds")
ardat <- read_rds("temp.rds")
report_params$rendering_data <- ardat

make_region <- function(data, initdim){
  initcol <- str_extract(initdim,"^[A-Z]+")
  initrow <- str_extract(initdim,"\\d+$")
  
  endcol <- col2int(initcol) + ncol(data) - 1
  endrow <- as.numeric(initrow) + nrow(data)
  
  return(str_c(initdim,":",int2col(endcol),endrow))
}

make_excel_report <- function(report_params, filepath){
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
  aghq <- report_params$rendering_data |> slice(1)
  
  asetting <- aghq$asetting[[1]]
  
  abunrui <- aghq$bunrui
  aname <- aghq$name
  hexcel <- aghq$hexcel[[1]]
  gexcel <- aghq$gexcel[[1]]
  qexcel <- aghq$qexcel[[1]]
  
  #シート名を作成
  shtname <- aname
  wb$add_worksheet(shtname)
  
  #タイトル行を作成
  wb$set_row_heights(sheet=shtname,rows=1,heights=32)
  wb$add_data(shtname,str_c(abunrui, ":",aname), dims = "A1")
  wb$add_font(sheet=shtname, dims="A1", size=18)
  
  #グラフ描画用のデータの表示
  graph_titletext <- str_c("「",asetting$plot_this,"」の部署間比較：")
  excel_data_graph <- gexcel$gdat |> 
    add_row(tibble(busyo = "事業場平均", plotthis = gexcel$avg_company)) |> 
    rename("部署" = busyo, "平均値" = plotthis) |> 
    arrange(desc(平均値)) |> 
    mutate(color_this = case_when(
      str_detect(`部署`,"^他\\d+$") ~ "他の部署",
      部署 == "事業場平均" ~ "事業場平均",
      TRUE ~ "対象"
    ))
  wb$add_data(shtname,graph_titletext, dims="A1")
  wb$add_data(shtname,excel_data_graph, dims="A3")
  
  gg <- ggplot(excel_data_graph) +
    geom_col(aes(x = `平均値`, y = reorder(`部署`, `平均値`, decreasing=TRUE), fill = color_this), position="dodge", width=0.5) +
    labs(y = "部署", fill = NULL, title = asetting$plot_this) +
    theme_bw(base_size= 14)
  print(gg)
  wb$add_plot(shtname, dims = "E3", width = 20, height = 10, file_type = "png", units = "cm", dpi=150)
  
  #尺度描画データの表示
  wb$save("temp.xlsx")
  #設問描画データの表示
  
  
  
  #まずタイプ別に分ける
  set_type <- asetting$type
  
  ahyou <- aghq$h[[1]]
  ahyou$x$tag$attribs$data
}