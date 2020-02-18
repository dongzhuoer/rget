#' @note https://tools.ietf.org/html/rfc3986#section-2.2
#' @note see [https://tools.ietf.org/html/rfc3986#appendix-B] for URL specify
 



#' @title remove 'protocol://' in url
#' 
#' @param url character.
#' 
#' @return character.
remove_protocol <- function(url) {
    str_replace(url, '^[^:/?#]+://', '');
}


#' @title remove '?...#...' from url
#' 
#' @description  remove query (?...) and fragment (#...) from url 
#' 
#' @param url character.
#' 
#' @return character.
shorten_url <- function(url) {
    url %<>% str_replace('#[\\w\\W]*$', '');
    url %<>% str_replace('\\?[\\w\\W]*$', '');
    
    url;
}


#' @title get base directory name of url
#'
#' @description `base_name()` get base directory name of url. In other words,
#'   extracts protocal (http or https) and domain name from url it It works
#'   similar to but a little different from [basename()][base::basename].
#'   
#' @details Although www.foo.com/bar must actually be www.foo.com/bar/index.html, but it's also possible that `bar` is a file under domain root directory. So `base_name()` has to return www.foo.com
#'
#' @param url character.
#'
#' @return character. with tailing '/'
#'
base_name <- function(url, tailing = T) {
    str_extract(url, '^([^:/?#]+://)?[^/]+(?=/?)') %>% paste0('/');
}


#' @title get directory name of url or path
#'
#' @description `dir_name()` get directory name of url or path. It works similar to but a little different from [dirname()][base::dirname].
#'
#' @details It doesn't works on single file name, i.e. `dir_name('foo/bar')` is okay but `dir_name('foobar')` doesn't return what you want. Since it is used to transform relative url to absolute url, `dir_name('www.github.com')` must return `'www.github.com'`.
#'
#' @param path character.
#'
#' @return character. with tailing '/'
#'
dir_name <- function(url) {
    ifelse(
        base_name(url) == url | base_name(url) == paste0(url, '/'),
        base_name(url),
        #" there must be at least one '/' after domain name to use following regexp
        str_extract(url, '^([^:/?#]+://)?[\\w\\W]+(?=/)') %>% paste0('/')
    )
}


#' @title transform url to absolute
#'
#' @param url character. must be valid, preferably href or src, see `relative
#'   <-` in function body
#' @param base string. base url for all relative url, must contain protocol
#'
#' @return character. Invalid values are removed and the original order isn't
#'   granted
as_absolute <- function(url, base) {
    #" https://www.w3schools.com/tags/att_a_href.asp
    
    #" remove 'javascript:...;'
    url %<>% {.[!str_detect(., '^javascript:')]};
    #" remove #id
    url %<>% shorten_url;
    
    #" you have to return here. since `character() %>% ifelse(. == '', base, .)` returns `logical(0)`
    if (length(url) == 0) return(character()); 
    
    domain   <- str_subset(url, '^/');
    absoulte <- str_subset(url, '^[^:/?#]+://');
    empty    <- str_subset(url, '^$');
    relative <- url %>% setdiff(domain) %>% setdiff(absoulte) %>% setdiff(empty);
    
    if (length(domain) > 0) {
        domain %<>% str_replace('^/', '') %>% {paste0(base_name(base), .)};
    }
    if (length(relative) > 0) relative %<>% {paste0(dir_name(base), .)};
    #" empty is treat as base, https://www.w3schools.com/tags/tryit.asp?filename=tryhtml_base_test
    if (length(empty) > 0) empty <- base;
    
    c(domain, absoulte, empty, relative) %>% tidy_url;
}


#' @title whether the url belong to a domain
#'
#' @description whether the url belong to a domain (including protocol)
#' 
#' @param url character.
#' @param domain string. must include protocol if url does, with tailing '/'. 
#'
#' @return logical
#' 
belong_to <- function(url, domain) {
    base_name(url) == domain;
}



#' @title remove . and .. in url
#' 
#' @param url character.
#' 
#' @return character.
tidy_url <- function(url) {
    #" if end with . or .., it must be a directory
    url %<>% {ifelse(str_detect(., '\\.$'), paste0(., '/'), .)};
    
    while (str_detect(url, '/\\.\\.') %>% any) {
        url %<>% str_replace('/[^/]+/\\.\\.', '');
    }

    while (str_detect(url, '/\\.') %>% any) {
        url %<>% str_replace('/\\.', '');
    }

    url;
}
