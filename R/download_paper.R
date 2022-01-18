
download_paper = function(keyword = 'REITs', cookies = '6A1564CC547414D601A06D4EE5539E61', sleep_time = 0.5){
  query_results = get_query_results(keyword = keyword, cookie = cookies, sleep_time = sleep_time)
  # write.csv(query_results, "./query_result.csv")

  query_results %>%
    dplyr::select(paper_piclink, paper_name = col_2, paper_index = col_1) %>%
    # slice(1:3) %>%
    pmap(~ get_paper_pic(.x, .y, .z, cookies, sleep_time))

}
