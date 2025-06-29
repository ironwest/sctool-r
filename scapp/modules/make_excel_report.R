library(openxlsx2)
library(reactable)
library(showtext)

font_add_google("Noto Sans JP", family = "noto-sans-jp")

# report_params <- read_rds("repparam.rds")

rc2dim <- function(r,c){
  str_c(int2col(c),r)
}

make_region <- function(data, initdim){
  initcol <- str_extract(initdim,"^[A-Z]+")
  initrow <- str_extract(initdim,"\\d+$")
  
  endcol <- col2int(initcol) + ncol(data) - 1
  endrow <- as.numeric(initrow) + nrow(data)
  
  return(str_c(initdim,":",int2col(endcol),endrow))
}

make_excel_report <- function(report_params, filepath, progress){

  GOOD_COLOR <- "lightcyan" 
  BAD_COLOR <- "mistyrose"
  
  wb <- openxlsx2::wb_workbook(creator = "FactoryHealthCo",title = "SC分析レポート")  
  wb$set_base_font(font_name="BIZ UDP ゴシック")

  #タイトルページの作成------------------
  progress$inc(amount = 0.1, detail = "タイトルページを作成中")
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
  
  #全体集計:尺度----------------------------
  progress$inc(amount = 0.1, detail = "部署の尺度全体集計を作成中")
  shtoa1 <- "部署の結果(尺度)"
  wb$add_worksheet(sheet=shtoa1)
  wb$add_data(shtoa1,"部署の結果(尺度)", dims="A1")
  wb$add_data(shtoa1,"尺度別の結果を事業場平均と比較したものを掲載いたします。",dims="A2")
  wb$add_font(shtoa1,dims="A1",size=18)
  
  syakudodata <- report_params$rendering_data |> 
    select(bunrui, name, hexcel) |> 
    mutate(hyou2 = map(hexcel, ~{
     retthis <-  .$hyou2
     if(is.null(retthis)) retthis <- .$hdat
     return(retthis)
  })) 
  
  write_this <- syakudodata |>
    rename(type = name) |>
    unnest(hyou2) |> 
    select(!hexcel)
  
  colnames(write_this)[8:9] <- c("bench1","bench2")

  write_this <- write_this |> 
    mutate(`ベンチマーク` = case_when(
       is.na(bench1) &  is.na(bench2) ~ NA_real_,
      !is.na(bench1) &  is.na(bench2) ~ bench1,
       is.na(bench1) & !is.na(bench2) ~ bench2
    )) |> 
    select(!c(bench1,bench2))
  
  header1 <- colnames(write_this) |> str_remove_all("type|bunrui|name|_今回|_前回") |>  as_tibble_row(.name_repair="minimal")
  header2 <- c("","","","今回","今回","前回","前回") |> as_tibble_row(.name_repair="minimal")
  
  wb$add_data(shtoa1, header1, start_col = 1, start_row = 4, col_names = FALSE)
  wb$add_data(shtoa1, header2, start_col = 1, start_row = 5, col_names = FALSE)
  wb$merge_cells(shtoa1, dims="D4:E4")
  wb$merge_cells(shtoa1, dims="F4:G4")
  wb$add_fill(shtoa1, dims="A4:H5", color = wb_color("grey90"))
  wb$add_data(shtoa1, write_this, start_col=1,start_row = 6, col_names=FALSE)
  wb$add_border(shtoa1, dims=str_c("A4:H",nrow(write_this)+5), 
                bottom_color = wb_color(hex = "FF000000"),
                left_color = wb_color(hex = "FF000000"),
                right_color = wb_color(hex = "FF000000"),
                top_color = wb_color(hex = "FF000000"),
                bottom_border = "thin",
                left_border = "thin",
                right_border = "thin",
                top_border = "thin",
                inner_hgrid = "thin",
                inner_hcolor = wb_color(hex = "FF000000"),
                inner_vgrid = "thin",
                inner_vcolor = wb_color(hex = "FF000000"))
  
  wb$set_col_widths(shtoa1,cols=1,widths=12)
  wb$set_col_widths(shtoa1, cols=2,widths=39)
  wb$set_col_widths(shtoa1, cols=3,widths=22)
  wb$set_col_widths(shtoa1, cols=4,widths=6)
  wb$set_col_widths(shtoa1, cols=5,widths=6)
  wb$set_col_widths(shtoa1, cols=6,widths=15)
  wb$set_col_widths(shtoa1, cols=7,widths=15)
  
  
  #全体集計:設問---------------------------
  progress$inc(amount = 0.1, detail = "部署の尺度個別質問を作成中")
  shtoa2 <- "部署の結果(設問)"
  wb$add_worksheet(sheet=shtoa2)
  wb$add_data(shtoa2,"部署の結果(設問)", dims="A1")
  wb$add_data(shtoa2,"設問別の結果を事業場平均と比較したものを掲載いたします。",dims="A2")
  wb$add_font(shtoa2,dims="A1",size=18)
  r_oa2 <- 4
  
  
  qex <- report_params$rendering_data |> 
    select(bunrui, name, qexcel) |> 
    unnest(qexcel) |> 
    mutate(adat = map(qexcel, ~.$adat)) |> 
    mutate(labels = map(qexcel, ~.$labels)) |> 
    select(!qexcel)
  
  for(qexrow in 1:nrow(qex)){
    abunrui <- qex$bunrui[qexrow]
    aname   <- qex$name[qexrow]
    adat    <- qex$adat[[qexrow]]
    alab    <- qex$labels[[qexrow]]
    disp    <- adat |> select(!c(name,good_value)) |> 
      mutate(bunrui1 = abunrui, bunrui2 = aname, .before=1)
    header <- c("分類1","分類2","尺度","番号","設問",alab,"良好な回答割合") |> as_tibble_row(.name_repair = "minimal")
    
    wb$add_data(shtoa2, header, start_col=1, start_row = r_oa2, col_names = FALSE)
    r_oa2 <- r_oa2 + 1
    wb$add_data(shtoa2, disp  , start_col=1, start_row = r_oa2, col_names = FALSE)
    initrow <- r_oa2
    end_row <- initrow + nrow(disp) - 1
    
    for(disprow in 1:nrow(disp)){
      if(adat$good_value[disprow] == 1){
        good_dim <- str_c("F",initrow + disprow - 1)
        bad_dim  <- str_c("I",initrow + disprow - 1)
      }else{
        good_dim <- str_c("I",initrow + disprow - 1)
        bad_dim  <- str_c("F",initrow + disprow - 1)
        
      }
      wb$add_fill(shtoa2, dims = good_dim, color = wb_color(GOOD_COLOR))
      wb$add_fill(shtoa2, dims = bad_dim , color = wb_color(BAD_COLOR))
    }
    
    wb$add_fill(shtoa2  , dims = str_c("A",initrow-1,":J",initrow-1), color = wb_color("grey90"))
    
    wb$add_border(shtoa2, dims = str_c("A",initrow-1,":J",end_row),
                  inner_hgrid = "thin",
                  inner_hcolor = wb_color(hex = "FF000000"),
                  inner_vgrid = "thin",
                  inner_vcolor = wb_color(hex = "FF000000"))
    
    r_oa2 <- end_row + 2
  }
  
  wb$set_col_widths(shtoa2,cols=1,widths=1)
  wb$set_col_widths(shtoa2,cols=1,widths=11)
  wb$set_col_widths(shtoa2,cols=2,widths=21)
  wb$set_col_widths(shtoa2,cols=3,widths=22)
  wb$set_col_widths(shtoa2,cols=4,widths=4.25)
  wb$set_col_widths(shtoa2,cols=5,widths=43)
  
  
  
  #以下、グラフとプロットのレンダリング START LOOP-----------------------
  progress$inc(amount = 0.1, detail = "個別尺度のページを作成中")
  showtext_auto()
  
  total_repeat <- nrow(report_params$rendering_data)
  for(apagenum in 1:total_repeat){
    print(apagenum)
    
    #1ページずつここから作成----------------
    aghq <- report_params$rendering_data |> slice(apagenum)
    
    asetting <- aghq$asetting[[1]]
    abunrui <- aghq$bunrui
    aname <- aghq$name
    
    
    progress$inc(amount = 0.6/total_repeat, detail = str_c(abunrui,":",aname))
    
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
    
    #グラフ部分------------------
    is_hanteizu <- asetting$type == "hanteizu"
    if(!is.null(asetting$plot_this) | is_hanteizu){
      #グラフ描画用のデータの表示-----------------
      if(is_hanteizu){
        graph_titletext <- shtname
        asetting
        excel_data_graph <- gexcel$gdat |> 
          select(!grp) |> 
          arrange(desc(type)) |> 
          select(`部署` = type, asetting$name_mapper)
          
        
      }else{
        graph_titletext <- asetting$plot_this
        
        excel_data_graph <- gexcel$gdat |> 
          add_row(tibble(busyo = "事業場平均", plotthis = gexcel$avg_company)) |> 
          rename("部署" = busyo, "平均値" = plotthis) |> 
          arrange(desc(平均値)) |> 
          mutate(color_this = case_when(
            str_detect(`部署`,"^他\\d+$") ~ "他の部署",
            部署 == "事業場平均" ~ "事業場平均",
            TRUE ~ "対象"
          ))
      }
      
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
      
      if(is_hanteizu){
        gg <- ggplot(excel_data_graph) +
          geom_point(aes(x = !!rlang::sym(names(asetting$name_mapper)[1]), y = !!rlang::sym(names(asetting$name_mapper)[2]), color = `部署`, shape=`部署`), size=4) +
          labs(x = names(asetting$graph_labels)[1], y = names(asetting$graph_labels)[2]) +
          theme_bw(base_size=20, base_family="noto-sans-jp")
        graphheight <- 12.5
      }else{
        gg <- ggplot(excel_data_graph) +
          geom_col(aes(x = `平均値`, y = reorder(`部署`, `平均値`, decreasing=TRUE), fill = color_this), position="dodge", width=0.5) +
          labs(y = NULL, fill = NULL, title = asetting$plot_this) +
          theme_bw(base_size= 20, base_family = "noto-sans-jp") +
          theme(legend.position = "none", axis.text.y = element_text(margin = margin(t=0, r=2,b=0,l=-5*ytextlength, unit="pt")))  
      }
      
      
      # print(gg)
      # wb$add_plot(shtname, dims = "E5", width = 18, height = graphheight*0.80, file_type = "png", units = "cm", dpi=150)
    }else{
      
      wb$add_data(shtname,"グラフなし", dims="A3")
      wb$add_font(shtname,dims = "A3",bold = TRUE, size=14)
    }
  
    #尺度描画部分-----------------------
    wb$add_data(shtname,"尺度", dims="P3")
    wb$add_font(shtname,dims="P3", bold=TRUE, size=14)
    
    if(is_hanteizu){
      hdat <- hexcel$hdat
    }else{
      hdat <- hexcel$hyou2
    }
      
    length_select_these <- length(asetting$select_these)
    header1 <- colnames(hdat) |> str_remove_all("name|_今回|_前回") |>  as_tibble_row(.name_repair="minimal")
    header2 <- c("","今回","今回","前回","前回") |> as_tibble_row(.name_repair="minimal")
    
    #個別ページの書き込み：
    wb$add_data(shtname, header1   , start_col = col2int("P"), start_row = 5, col_names = FALSE)
    wb$add_data(shtname, header2   , start_col = col2int("P"), start_row = 6, col_names = FALSE)
    wb$add_data(shtname, hdat      , start_col = col2int("P"), start_row = 7, col_names = FALSE)
    wb$merge_cells(shtname, dims="Q5:R5")
    wb$merge_cells(shtname, dims="S5:T5")
    wb$merge_cells(shtname, dims="U5:U6")
    wb$merge_cells(shtname, dims="P5:P6")
    wb$add_data(shtname,"ベンチマーク", dims="U5:U6")
    wb$add_border(
      shtname, 
      dims=str_c("P5:U",7+length_select_these-1),
      inner_hgrid = "thin",
      inner_hcolor = wb_color(hex = "FF000000"),
      inner_vgrid = "thin",
      inner_vcolor = wb_color(hex = "FF000000")
    )
    wb$add_numfmt(shtname,dims=str_c("Q5:T",7+length_select_these-1), numfmt = "0.0")
    wb_add_fill(wb,shtname,dims="P5:U6",color = wb_color("grey90"))

    
   
    #設問描画データの表示-------------------
    wb$add_data(shtname, "尺度に関連する設問",dims="W3")
    wb$add_font(shtname,dims="W3", bold=TRUE, size=14)
    
    questions <- asetting$questions
    length_questions <- length(questions)
    
    
    
    r <- 6
    if(!is.null(qexcel)){
      for(i in 1:nrow(qexcel)){
        aqex <- qexcel$qexcel[[i]]
        
        aqdat <- aqex$adat
        aqlab <- c("尺度","番号","設問",aqex$labels,"良好な回答の割合")
        
        
        qheader <- aqlab |> as_tibble_row(.name_repair="minimal")
        qwrite_this <- aqdat |> select(syakudo_minor, qnum, qtext, `1`, `2`, `3`, `4`, good_percent)
        
        rinit <- r
        wb$add_data(shtname,qheader,start_col = col2int("W"), start_row = r, col_names = FALSE)
        wb_add_fill(wb,shtname,dims=str_c("W",r,":AD",r),color = wb_color("grey90"))
        r <- r + 1
        wb$add_data(shtname,qwrite_this,start_col = col2int("W"), start_row = r, col_names = FALSE)
        rend <- rinit + nrow(qwrite_this )
        wb$add_border(
          shtname, 
          dims=str_c("W",rinit,":AD",rend),
          inner_hgrid = "thin",
          inner_hcolor = wb_color(hex = "FF000000"),
          inner_vgrid = "thin",
          inner_vcolor = wb_color(hex = "FF000000")
        )
        
        for(j in 1:nrow(aqdat)){
          if(aqdat$good_value[j]==1){
            wb$add_fill(shtname, rc2dim(r,26),color = wb_color(GOOD_COLOR))
            wb$add_fill(shtname, rc2dim(r,29),color = wb_color(BAD_COLOR))
            
          }else{
            wb$add_fill(shtname, rc2dim(r,26),color = wb_color(BAD_COLOR))
            wb$add_fill(shtname, rc2dim(r,29),color = wb_color(GOOD_COLOR))
          }  
          
          r <- r + 1
        }
        
        r <- r + 1
      }
      
    }else{
      #qexcel がない場合は尺度に関連する設問の下になしという文言を表示
      wb$add_data(shtname,"なし",dims="W5")
    }
    
    wb$set_col_widths(shtname,cols = col2int("P"), widths = 30)
    wb$set_col_widths(shtname,cols = col2int("Q"), widths = 10)
    wb$set_col_widths(shtname,cols = col2int("R"), widths = 10)
    wb$set_col_widths(shtname,cols = col2int("S"), widths = 10)
    wb$set_col_widths(shtname,cols = col2int("T"), widths = 10)
    wb$set_col_widths(shtname,cols = col2int("U"), widths = 11.75)
    wb$set_col_widths(shtname,cols = col2int("W") , widths = 23.4)
    wb$set_col_widths(shtname,cols = col2int("X") , widths = 4.25)
    wb$set_col_widths(shtname,cols = col2int("Y") , widths = 43)
    wb$set_col_widths(shtname,cols = col2int("Z") , widths = 10)
    wb$set_col_widths(shtname,cols = col2int("AA"), widths = 10)
    wb$set_col_widths(shtname,cols = col2int("AB"), widths = 10)
    wb$set_col_widths(shtname,cols = col2int("AC"), widths = 10)
    wb$set_col_widths(shtname,cols = col2int("AD"), widths = 15)
    
    
    
    #LOOP DONE-------------
  }
    
  
  wb$save(filepath)
  showtext_auto(FALSE)
}
