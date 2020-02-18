#' @title recursive create directory it not exist
#'
#' @param dir string.
#'
#' @return logical scalar. invisible
#'
#' @section example:
#'
#'   dir_create('data-raw/test');
dir_create <- function(dir) {
    if (dir.exists(dir)) return(NULL);

    dir.create(dir, recursive = T);
}



#' @title test whether a url can be accessed
#' 
#' @param url string.
#' 
#' @return logical scalar.
#' 
#' @section example:
#' 
#'   test_url('www.cookbook-r.com')
#'   test_url('www.cookbook-r.com/Graphs2')  # not exist
#'   test_url('www.cookbook-r.com/Data_input_and_output/Loading_data_from_a_file/datafile.csv')
#'   test_url('yihui.name')
#'   test_url('www.cosx.org')
#'   test_url('www.baidu.com') 
#'   test_url('www.github.com')  
#'   test_url('www.google.com')  # of course fails and takes several seconds
test_url <- function(url) {
    result <- tryCatch(
        system2('wget', paste0('-S --spider -t 2 -T 2 "', url, '"'), T, T),
        warning = function(w) {w}
    );
    
    !is(result, 'warning');
}


#' @title transfrom a url to link to a file
#'
#' @description This is the **core** function, and why I want to write this
#'   package. wget just download www.foo.com/bar/index.html to ./www.foo.com/bar
#'   , and then overwrite that file to a folder while downloading other HTML
#'   files in www.foo.com/bar/ . I just want to ask whether you want to cry.
#' 
#' @param url string. mustn't contain tailing '/', because in this situation there is no need to distinguish www.foo.com/bar/index.html from www.foo.com/bar (www.foo.com/bar/ must be www.foo.com/bar/index.html)
#' 
#' @return string. `paste0(url, '/')` if it's accessible (there exist a `index.html` file)
#' 
#' @section example:
#'
#'  as_url2file('www.cookbook-r.com/Graphs'); 
#'  as_url2file('www.cookbook-r.com/Graphs2'); # not exist, for testing
as_url2file <- function(url) {
    old_warn <- options(warn = -1);
    on.exit({options(old_warn)});
    
    url2 <- paste0(url, '/');
    
    if (url2 %>% test_url) url2 else url;
}



#' @title using wget to download file
#' 
#' @description using wget to download file only when server version is newer 
#' 
#' @param url string. 
#' @param dest_file string. 
#' @param force logical scalar. Always download file, even when server file
#'   isn't newer.
#' @param verbose logical scalar.  
#'
#' @return logical scalar
#'
download_file <- function(url, dest_file, force, verbose) {
    std <- if (verbose) "" else FALSE;
    
    dest_dir <- dest_file %>% dirname;

    #" to do: use `Content-Length` of `wget -S --spider` and remove force in `rget()`
    #" empty file may remain when an error happends, but its time stamp is the
    #"     same as server one. Then `read_html` would read an empty file and
    #"     cause error.
    if (file.exists(dest_file)) {
        if (file.info(dest_file)$size == 0) file.remove(dest_file);
    } 
    
    dest_dir %>% dir_create;

    bash_args <- paste0('-P "', dest_dir, '" "', url, '"');
    if (!force) bash_args %<>% paste0('-N ', .);

    ok <- tryCatch(system2('wget', bash_args, std, std), error = function(e) {1L});
    ok %>% identical(0L) %>% return();
}
