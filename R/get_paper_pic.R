library(dplyr)
library(stringr)
library(purrr)
library(httr)
library(jsonlite)
library(rvest)
library(downloader)


my_cookie = '179B9993F044A125F4FDDA6BFD038E13'

## 获取论文对应的唯一编码
get_paper_pic = function(paper_piclink, paper_name, paper_index, cookie = '179B9993F044A125F4FDDA6BFD038E13', sleep_time = 0.5){
  Sys.sleep(runif(1, max = sleep_time))

  paper_code_select = "div.loadingBg > img"
  my_header = c(
    'Accept'= 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8',
    'Accept-Encoding'= 'gzip, deflate, br',
    'Accept-Language'= 'zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2',
    'Cache-Control'= 'max-age=0',
    'Connection'= 'keep-alive',
    'Host'= '10.12.162.84',
    'Origin'= 'http://10.12.162.84',
    'Referer'= 'http://10.12.162.84/simpsearch.action',
    'Sec-Fetch-Dest'= 'document',
    'Sec-Fetch-Mode'= 'navigate',
    'Sec-Fetch-Site'= 'cross-site',
    'Upgrade-Insecure-Requests'= '1',
    'User-Agent'= 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36',
    'cookie'= paste0("JSESSIONID=", cookie)
  )

  tmp_path = paste0("./p_", paper_name)
  if (!file.exists(tmp_path)) {
    dir.create(file.path(tmp_path))
  }

  i = 0
  while (i < 300) {
    Sys.sleep(runif(1, max = sleep_time))
    i = i + 1
    pic_link_tmp = paste0(paper_piclink, "P01_", sprintf("%05d", i), ".jpg")

    tryCatch({
      download.file(pic_link_tmp, paste0(tmp_path, sprintf("/picture_%05d", i),".jpg"), quiet = TRUE, headers = my_header)
    }, error = function(e) {
      # break()
    })
  }

}











