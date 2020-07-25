#' @rdname queue
#'
#' @title wanted HTML file (**absolute** url) queue
#'
#' @details the url can contain http:// or not.
#'
#'   But it must be HTML files, and actually linked by other files (? and # removed). That is to
#'   say,
#'
#'   - if some file links to www.foo.com/bar, you shouldn't add
#'   www.foo.com/bar/index.html to the queue
#'
#'   - if some file links to /bar.html, you shouldn't add www.foo.com/bar.html
#'   to the queue
#'
#'   Thus ensure \code{setdiff(queue$current, queue$succeed)} makes sense
#'
#' @format A list of three character
#'
#'   1. current files to be downloaded
#'
#'   1. succeed files successfully downloaded
#'
#'   1. failed files failed to be downloaded
#'
.queue <- function() {}





#' process a html file a time
#' @param queue [queue][.queue]
#' @param ... other arguments passed on to [download_file()]
#'
#' @return queue
rget_html <- function(queue, ...) {
    queue$current %<>% setdiff(queue$succeed) %>% setdiff(queue$failed);
    if (queue$current %>% length == 0) return(queue);

    url <- queue$current[1] %>% shorten_url %>% as_url2file;
    dest_file <- url %>% remove_protocol %>% {ifelse(str_detect(., '/$'), paste0(., 'index.html'),.)};
    ok <- download_file(url, dest_file, ...);

    if (ok) {
        queue$succeed %<>% append(queue$current[1]);
        
        html <- tryCatch(xml2::read_html(dest_file), error = function(e) {e});
        if (is(html, 'xml_document')) {
            href <- xml2::read_html(dest_file) %>% html_nodes('a') %>% html_attr('href');
            #" url contain tailing '/' if it's a directory, but no query or fragment
            #" in theory url should contain protocol, but I use some track so it also works even without
            href %<>% as_absolute(url) %>% {.[belong_to(., base_name(url))]};
        
            #" to do: get_base(), but www.cookbook-r.com doesn't contains <base> tag
            queue$current %<>% append(href);
        }
    } else {
        queue$failed %<>% append(queue$current[1]);
    }

    return(queue);
}

#' @title download dependencies for a website
#'
#' @description download dependencies for a website after all HTML files are downloaded
#' 
#' @param website [rget()]
#'
#' @return NULL
download_dependencies <- function(website, ...) {
    #" website <- 'www.cookbook-r.com'; 
    index.html <- dir(website, pattern = '\\.html$', recursive = T, full.names = T);
    
    get_dependencies <- function(path) {
        html <- xml2::read_html(path);
        src <- html %>% html_nodes('script, img, audio, video, iframe, source') %>% html_attr('src')
        srcset <- html %>% html_nodes('picture > source') %>% html_attr('srcset')
        link <- html %>% html_nodes('link') %>% html_attr('href')
        c(src, srcset, link) %>% na.omit %>% as_absolute(path) %>% {.[belong_to(., base_name(path))]};
    }
    
    resoruce <- plyr::llply(index.html, get_dependencies) %>% unlist %>% unique
    resoruce %>% {.[!str_detect(., '/$')]} %>% plyr::l_ply(. %>% {download_file(., remove_protocol(.), ...)});
}

#' @title download a static website
#'
#' @param website string. url of the website to be cloned, it should be the domain name (sub directory not supported yet). you can't append ?...#..., for
#'   example, 'www.foobar.com?haha#xixi' is forbidden,
#' @param dest_dir string. No tailling '/'
#' @param force [download_file()]
#' @param verbose [download_file()]
#'
#' @details we assume:
#'
#'   1. static http(s) website, no query (?foo=bar), no account authentication
#'
#'   1. HTML file is always linked by <a> tag 
#'
#'   1. HTML file name all end with '.html', and default file name is
#'   'index.html'
#'
#'   rget('www.cookbook-r.com', 'data-raw'); rget('www.cookbook-r.com',
#'   '~/Git/mirrors')
#'   
#' @section motivation: [as_url2file()]
#'
#' @export
rget <- function(website, dest_dir, force = FALSE, verbose = TRUE) {
    old_wd <- getwd();    
    on.exit({setwd(old_wd)});

    # prepare folder for the website and set working directory
    setwd(dest_dir);
    website %>% remove_protocol %>% dir_create;
    # download html files
        #" website won't contain ?...#..., so we don't short it here
    queue <- list(current = website, succeed = '', failed = '');
    while (queue$current %>% length > 0) {
        queue = rget_html(queue, force, verbose);
    }

    download_dependencies(website, force, verbose);

}
#rget('www.cookbook-r.com', 'data-raw');
