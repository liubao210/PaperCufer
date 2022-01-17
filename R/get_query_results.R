# 中财校内学生论文整理助手 ====
library(dplyr)
library(stringr)
library(purrr)
library(httr)
library(jsonlite)
library(rvest)


## 根据搜索词获取列表 ====
url_base = 'http://10.12.162.84/simpsearch.action'
query_url_base = 'http://10.12.162.84/resultlist.jsp'
page_next_url_base = 'http://10.12.162.84/pagedown.action?pager.offset=${offset}'

max_page_num_select = "body > div > div.page > span"
paper_secend_url_select = "body > div > table > tbody > tr > td > a"
paper_pdf_url_link = "div.look > a"

my_cookie = '179B9993F044A125F4FDDA6BFD038E13'


get_query_results = function(keyword = "REITs", cookie = '179B9993F044A125F4FDDA6BFD038E13', dbid = '72', max_page_num_select = "body > div > div.page > span", sleep_time = 0.5) {
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

  res_init = POST(url_base, add_headers(.headers = my_header), body = list(keyword = keyword, dbid = as.character(dbid)), encode = "form")
  res = GET(query_url_base, add_headers(.headers = my_header))
  content = res %>% content()
  # query_results = content %>%
  #   html_element("table") %>%
  #   html_table()

  # 最大页数
  tmp_max = html_nodes(content, max_page_num_select) %>% html_text %>% strsplit(., split = "/")
  max_page_num = max(tmp_max[[1]] %>% as.numeric())

  # 所有查询结果
  next_page = function(page_index = 1, page_nest_url_base = page_nest_url_base, sleep_time = 0.5){
    Sys.sleep(runif(1, max = sleep_time))

    print(paste0("querying page: ", page_index))
    offset = (page_index - 1) * 20
    page_nest_url = str_interp(page_nest_url_base)
    res = GET(page_nest_url, add_headers(.headers = my_header))
    content = res %>% content()
    query_results = content %>%
      html_element("table") %>%
      html_table()

    tmp = query_results %>% as.matrix() %>% as.data.frame()
    colnames(tmp) = paste0("col_", 1:dim(tmp)[2])
    paper_link_list = paste0("http://10.12.162.84/", html_nodes(content, paper_secend_url_select) %>% html_attr('href'))

    ## 找出全文链接
    get_pdflink = function(paper_link, sleep_time = 0.5){
      Sys.sleep(runif(1, max = sleep_time))
      res = GET(paper_link, add_headers(.headers = my_header))
      content = res %>% content()
      tmp_pdflink = html_nodes(content, paper_pdf_url_link) %>% html_attr('href')
      paper_pdflink = if_else(nchar(tmp_pdflink) > 0, paste0("http://10.12.162.84/", tmp_pdflink), "")

      paper_pdflink
    }

    paper_pdflink_list = paper_link_list %>%
      map(get_pdflink)


    get_paper_code = function(paper_link, sleep_time = 0.5) {
      Sys.sleep(runif(1, max = sleep_time))
      # print(length(paper_link) == 0)
      if (length(paper_link) > 0) {
        res = GET(paper_link, add_headers(.headers = my_header))
        content = res %>% content()
        piclink_tmp = html_nodes(content, paper_code_select) %>% html_attr('src')
        piclink = if_else(nchar(piclink_tmp[1]) > 0, (strsplit(piclink_tmp[1], "P01_0") |> as.list())[[1]][1], "")

        piclink
      } else {
        return("")
      }
    }

    paper_code_list = paper_pdflink_list %>%
      map(get_paper_code)

    tmp %>%
      mutate(
        paper_link = paper_link_list,
        paper_pdflink = paper_pdflink_list,
        paper_piclink = paper_code_list
      )
  }

  query_results = c(1:max_page_num) %>%
    map_dfr(~ next_page(., page_nest_url_base))

  query_results
}

