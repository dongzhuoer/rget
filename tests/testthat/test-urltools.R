testthat::context("Testing utility")

testthat::test_that("Testing remove_protocol", {
    testthat::expect_identical(remove_protocol('http://github.com'), 'github.com');
    testthat::expect_identical(remove_protocol('https://zhuoer.netlify.com'), 'zhuoer.netlify.com');
    testthat::expect_identical(remove_protocol('ftp://foo.bar'), 'foo.bar');
    testthat::expect_identical(remove_protocol('mailto://foo@bar.com'), 'foo@bar.com');
});

testthat::test_that("Testing shorten_url", {
    testthat::expect_identical(shorten_url('2017.igem.org/wiki/index.php?title=Team:NKU_China'), '2017.igem.org/wiki/index.php');
    testthat::expect_identical(shorten_url('cookbook-r.com/Graphs/Lines_(ggplot2)/#solution'), 'cookbook-r.com/Graphs/Lines_(ggplot2)/');
    testthat::expect_identical(shorten_url('zhuoer.com/?title=foo#bar'), 'zhuoer.com/');
})


testthat::test_that("Testing base_name", {
    testthat::expect_identical(base_name('http://foo.com'), 'http://foo.com/');
    testthat::expect_identical(base_name('https://foo.com/bar.html'), 'https://foo.com/');
    testthat::expect_identical(base_name('http://foo.com'), 'http://foo.com/');
    testthat::expect_identical(base_name('https://foo.com/bar'), 'https://foo.com/');
    testthat::expect_identical(base_name('foo.com/bar/'), 'foo.com/');
    testthat::expect_identical(base_name('foo.com/bar/index.html'), 'foo.com/');
})

testthat::test_that("Testing dir_name", {
    testthat::expect_identical(dir_name('http://foo.com'), 'http://foo.com/');
    testthat::expect_identical(dir_name('https://foo.com/bar.html'), 'https://foo.com/');
    testthat::expect_identical(dir_name('http://foo.com'), 'http://foo.com/');
    testthat::expect_identical(dir_name('https://foo.com/bar'), 'https://foo.com/');
    testthat::expect_identical(dir_name('foo.com/bar/'), 'foo.com/bar/');
    testthat::expect_identical(dir_name('foo.com/bar/index.html'), 'foo.com/bar/');
})

testthat::test_that("Testing as_absolute", {
    testthat::expect_identical(as_absolute('javascript:alert("hello");', ''), character());
    testthat::expect_identical(as_absolute('#id', 'http://foo.com/bar.html'), 'http://foo.com/bar.html');
    testthat::expect_identical(as_absolute('', 'http://foo.com/bar.html'), 'http://foo.com/bar.html');
    testthat::expect_identical(as_absolute('/', 'http://foo.com/bar/index.html'), 'http://foo.com/');
    testthat::expect_identical(as_absolute('bar.html', 'http://foo.com/index.html'), 'http://foo.com/bar.html');
    testthat::expect_identical(as_absolute('.', 'http://foo.com/bar/'), 'http://foo.com/bar/');
    testthat::expect_identical(as_absolute('.', 'http://foo.com/bar/index.html'), 'http://foo.com/bar/');
    testthat::expect_identical(as_absolute('..', 'http://foo.com/bar/'), 'http://foo.com/');
    testthat::expect_identical(as_absolute('..', 'http://foo.com/bar/index.html'), 'http://foo.com/');
    testthat::expect_identical(as_absolute('http://foo.com', 'http://bar.com/bar/'), 'http://foo.com');
})

testthat::test_that("Testing as_absolute when base doesn't contains protocol", {
    testthat::expect_identical(as_absolute('#id', 'foo.com/bar.html'), 'foo.com/bar.html');
    testthat::expect_identical(as_absolute('#id', 'foo.com/bar'), 'foo.com/bar');
    testthat::expect_identical(as_absolute('#id', 'foo.com/bar/'), 'foo.com/bar/');
 })


testthat::test_that("Testing belong_to", {
    testthat::expect_true(belong_to('foo.com/bar/index.html', 'foo.com/'));
    testthat::expect_false(belong_to('foo.com2/bar/index.html', 'foo.com/'));
    testthat::expect_true(belong_to('http://foo.com/bar/index.html', 'http://foo.com/'));
    testthat::expect_false(belong_to('https://foo.com/bar/index.html', 'http://foo.com/'));
    testthat::expect_false(belong_to('http://foo.com/bar/index.html', 'foo.com/'));
})


testthat::test_that("Testing tidy_url", {
    testthat::expect_identical(tidy_url('foo.com/bar/.././'), 'foo.com/');
    testthat::expect_identical(tidy_url('foo.com/bar/..'), 'foo.com/');
    testthat::expect_identical(tidy_url('foo.com/bar/.'), 'foo.com/bar/');
})


# test_that("Testing foobar", {
#     testthat::expect_identical(foobar(), '')
# })

