#' @importFrom stringr str_split
para2lines <- function(para) {
    para %>%
        stringr::str_split("\n") %>%
        .[[1]]
}

#' @importFrom clipr read_clip write_clip
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom stringr str_trim str_length str_sub str_squish
extract_p <-
    function(url = NULL,
             content_path = NULL,
             pattern = "p , section span") {
        if (is.null(url)) {
            url <- clipr::read_clip()
        }
        if (is.null(content_path)) {
            content = add2md:::extract_html_text(url, pattern)
        } else {
            content = readr::read_lines(content_path)
        }
        output <-
            content %>%
            stringr::str_flatten("\n")
        clipr::write_clip(output)
        output %>% stringr::str_sub(1, 20) %>% cat
        invisible(output)

    }

#' Extract relavant contents from a WeChat article with BibTex.
#'
#' @import stringr
#' @importFrom xml2 read_html
#' @importFrom add2bibtex get_wechat_title
#' @importFrom add2md extract_firstline extract_highlight
#' @importFrom usethis ui_warn
#' @importFrom glue glue
#' @importFrom clipr write_clip
#' @export
add_wechat_portfolio <-
    function(path, pattern = "p , section span") {
        bibtex_text <- add2bibtex::add_wechat(path)
        firstline_text <- add2md::extract_firstline(path, pattern = pattern) %>% stringr::str_trim() %>% stringr::str_squish()
        highlight_text <- add2md::extract_highlight(path, pattern = pattern) %>% stringr::str_trim() %>% stringr::str_squish()

        text <- extract_p(path, pattern = pattern) %>%
            stringr::str_flatten("\n") %>%
            stringr::str_remove_all("<U\\+00A0>")

        # pip install snownlp
        snownlp <- reticulate::import("snownlp")
        # https://blog.rstudio.com/2018/03/26/reticulate-r-interface-to-python/

        snownlp_text <- snownlp$SnowNLP(text %>% stringr::str_flatten("\n"))$summary() %>%
            stringr::str_trim() %>%
            stringr::str_flatten("\n")

        if (length(highlight_text) > 0 & length(firstline_text) > 0) {
            highlight_text <-
                setdiff(
                    highlight_text %>% para2lines(),
                    firstline_text %>% para2lines()
                ) %>%
                stringr::str_flatten("\n")
        }

        title <- path %>%
            xml2::read_html() %>%
            add2bibtex::get_wechat_title()
        if (length(firstline_text) == 0) {
            firstline_text = ""
            usethis::ui_warn("\n\nfirstline_text is empty.")
        }
        if (length(highlight_text) == 0) {
            highlight_text = ""
            usethis::ui_warn("\n\nhighlight_text is empty.")
        }
        output <-
            glue::glue(
                "{title}\n\n\n{path}\n\n\n**主题句**\n\n\n{firstline_text}\n\n\n**高亮**\n\n\n{highlight_text}\n\n\n**SnowNLP**\n\n\n{snownlp_text}\n\n\n```bibtex\n{bibtex_text}\n```"
            )
        clipr::write_clip(output)
        invisible(output)
    }
