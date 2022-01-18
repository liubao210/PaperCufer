
download_paper = function(keyword = 'REITs', cookies = 'DBA24B22726FA8ED9BE1A1353F601FAD', sleep_time = 0.5){
  query_results = get_query_results(keyword = keyword, cookie = cookies, sleep_time = sleep_time)
  write.csv(query_results, "./query_result.csv")

  query_results %>%
    dplyr::select(paper_piclink, paper_name = col_2, paper_index = col_1) %>%
    # slice(1:3) %>%
    pmap(~ get_paper_pic(.x, .y, .z, cookie, sleep_time))

}
