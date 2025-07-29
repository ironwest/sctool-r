# ストレスチェック集団分析ツール

ストレスチェック集団分析をR言語で実施するためのアプリケーションの公開レポジトリです。

## 使い方の全体像

本ツールを利用するための手順としては、

1. R言語のインストール、
2. RStudio Desktopのインストール
3. プログラムのダウンロードと解凍
4. 必要なパッケージのインストール
5. アプリケーションの実行

となっています。なお、`git clone https...`というコマンドで手順4.ができる方については、通常のShinyアプリケーションをRStudioから実行いただければアプリが起動します。

## デモの確認

なお、本ツールはShinyapps.io上でも実際に動作する[アプリケーションとして公開](https://factory-health.shinyapps.io/scapp/)しています。

ただし、外部ツールに個人情報を含むデータを公開することは一般的には推奨されていないため、

処理前の生データのダミーデータ：
`scapp/demodata/nbjsq_dummy_data1_alpha.csv`
`scapp/demodata/nbjsq_dummy_data2_alpha.csv`

や、ツールで処理した後のダミーデータ

`scapp/demodata/processed_nbjsq_dummy_data1_alpha.csv`
`scapp/demodata/processed_nbjsq_dummy_data2_alpha.csv`

などで、動作を確認する目的での利用を推奨いたします。（特に流出させるような意図のコードは含まれていませんが、利用される場合は自己判断でお願いいたします。）

## ローカル環境での実行手順

### R言語のインストール

* [https://ftp.yz.yamagata-u.ac.jp/pub/cran/](https://ftp.yz.yamagata-u.ac.jp/pub/cran/)からR言語をインストールしてください。


### RStudio Desktopのインストール

* [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)からRStudio Desktopをインストールしてください


R言語とRStudioDesktopのインストール方法がわからないという方は、Google検索でインストール方法をお調べいただくか、少し古いですが、[https://www.udemy.com/course/data-analysis-by-r-for-medical-profession/]の最初のセクションにインストール方法が動画で解説されている（購入しなくてもプレビューできます）ため、参考にしてください。

### プログラムのダウンロードと解凍

こちらのページの右上のCODEから、Download ZIPを選択してZIPファイルをダウンロードしてください。
![image](https://github.com/user-attachments/assets/74713e0d-ff5b-47da-bffe-fb89ee7956f3)

ダウンロードしたZIPファイルを解凍して好きな場所においてください。

開いたファイルの中にある、`scrool-r.Rproj`をダブルクリックすると、RStudioが立ち上がります。

### 必要なパッケージのインストール

はじめて利用する場合に、ツールが動作するうえで必要な追加のライブラリ（機能）をインストールする必要があります。
RStudioを起動した場合に、以下のような画面が起動するはずです。

![image](https://github.com/user-attachments/assets/2e2ae81e-3557-4eab-8a49-44ddb9d11ff6)

このとき、「Console」となっている画面を探してください。（画像では、画面左上にConsoleという窓が表示されています。設定によっては右側に来ている場合もあるので注意ください）

このConsoleと表示されている窓に、以下のコマンドを入力（コピーして）Enterキーを入力してください。

```r
install.packages("shiny","shinydashboard","shinyjs","jsonlite","readr","stringr","purrr","dplyr","tidyr","reactable","shinycssloaders","ggplot2","broom","openxlsx2","showtext")
```

![image](https://github.com/user-attachments/assets/00ac830c-98c2-43cd-bcff-16e3f30dc669)


必要な機能がインストールされるためしばらく待ちます。

![image](https://github.com/user-attachments/assets/4cc1fadd-e251-4964-92f2-d55211c017d3)

`＞`という記号で入力ができるようになったらインストール完了です。

### アプリケーションの実行

必要なパッケージのインストールが終われば、scappフォルダの中にあるapp.RをRStudio上で開きます。
右下のFilesと書いてある窓からapp.Rをクリックすると以下のような画面になるはずです。

![image](https://github.com/user-attachments/assets/caaa6425-08d2-418c-89aa-bb54273d8bac)

このとき、`Run App`という緑の▶がついているボタンをクリックしてください。

![image](https://github.com/user-attachments/assets/3a54fb54-8a63-4b63-95c4-807b140d6901)

パッケージのインストールなどが無事に終了しているとアプリケーションは動作するはずです。
