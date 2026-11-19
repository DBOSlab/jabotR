test_that(".arg_check_dir handles valid and invalid input", {
  expect_equal(.arg_check_dir("my/path"), "my/path")
  expect_equal(.arg_check_dir("my/path/"), "my/path")
  expect_error(.arg_check_dir(123), "The argument dir should be a character")
})


test_that(".arg_check_path detects missing or invalid folders", {
  temp <- tempdir()
  empty_dir <- file.path(temp, "empty_folder")
  dir.create(empty_dir, showWarnings = FALSE)

  # 1. Invalid path (not a dir)
  expect_error(.arg_check_path(42, character(), list()), "should be a character")

  # 2. Dir doesn't exist
  expect_error(.arg_check_path("not_exist", character(), list()), "There is no folder")

  # 3. No dwca folders
  expect_error(.arg_check_path(empty_dir, "folder", list()), "no JABOT-downloaded dwca")

  # 4. dwca folder but empty
  fake_dwca <- file.path(temp, "dwca_example")
  dir.create(fake_dwca, showWarnings = FALSE)
  expect_error(.arg_check_path(temp, basename(fake_dwca), list(list())), "fully empty")

  # 5. Missing key files
  expect_error(.arg_check_path(temp, basename(fake_dwca), list(list("random.txt"))),
               "files are missing")
})


test_that(".arg_check_recordYear detects bad input", {
  expect_silent(.arg_check_recordYear(c("2000", "2024")))
  expect_error(.arg_check_recordYear(c("2024", "2000")), "first year must be less")
  expect_error(.arg_check_recordYear(c("199", "2020")), "4-digit")
  expect_error(.arg_check_recordYear(c("2000", "2020", "2024")), "should be either a single year or a range")
})


test_that(".arg_check_state converts and validates state names", {
  expect_equal(.arg_check_state("BA"), "Bahia")
  expect_equal(.arg_check_state("Bahia"), "Bahia")
  expect_equal(.arg_check_state("São Paulo"), "São Paulo")
  expect_equal(.arg_check_state("Sao Paulo"), "São Paulo") # test with diacritic removed
})


test_that(".arg_check_state handles acronyms, full names, and unknowns", {
  input <- c("BA", "Minas Gerais", "Santa Cruz de la Sierra")
  result <- .arg_check_state(input)
  expect_equal(result, c("Bahia", "Minas Gerais", "Santa Cruz de la Sierra"))
})


test_that(".arg_check_state handles diacritics", {
  input <- c("Sao Paulo", "Ceara", "Espirito Santo")
  expected <- c("São Paulo", "Ceará", "Espírito Santo")
  result <- .arg_check_state(input)
  expect_equal(result, expected)
})


test_that(".arg_check_state returns unknown inputs unchanged", {
  input <- c("Unknown State", "ZZ")
  result <- .arg_check_state(input)
  expect_equal(result, c("Unknown State", "ZZ"))
})


test_that(".arg_check_state handles case-insensitive acronyms", {
  input <- c("rj", "mg", "Sp")
  result <- .arg_check_state(toupper(input))
  expect_equal(result, c("Rio de Janeiro", "Minas Gerais", "São Paulo"))
})



test_that(".arg_check_path reports several empty dwca folders in the plural", {
  temp <- withr::local_tempdir()
  expect_error(
    .arg_check_path(temp, c("dwca_a", "dwca_b", "dwca_c"),
                    list(character(), list("occurrence.txt"), character())),
    "The dwca folders 'dwca_a', 'dwca_c' within the directory .* are fully empty"
  )
  expect_error(
    .arg_check_path(temp, c("dwca_a", "dwca_b"), list(character(), character())),
    "download such dwca folders again or exclude them", fixed = TRUE
  )
})


test_that(".arg_check_path passes silently for complete dwca folders", {
  temp <- withr::local_tempdir()
  expect_silent(
    res <- .arg_check_path(temp, c("dwca_a", "dwca_b"),
                           list(c("identification.txt", "occurrence.txt"),
                                c("occurrence.txt", "meta.xml")))
  )
  expect_null(res)
})


test_that(".arg_check_path names only the dwca folders lacking key files", {
  temp <- withr::local_tempdir()
  expect_error(
    .arg_check_path(temp, c("dwca_ok", "dwca_bad"),
                    list("occurrence.txt", c("meta.xml", "eml.xml"))),
    "missing in the dwca folders 'dwca_bad'.", fixed = TRUE
  )
})


.mock_ipt_codes <- function(codes = c("HATM", "PALM", "RB"), env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0
  calls$herbarium <- "unset"
  testthat::local_mocked_bindings(
    .get_ipt_info = function(herbarium) {
      calls$n <- calls$n + 1
      calls$herbarium <- herbarium
      list(as.list(tolower(codes)), tolower(codes), codes)
    },
    .package = "jabotR",
    .env = env
  )
  calls
}


test_that(".arg_check_herbarium returns early for NULL or empty input", {
  calls <- .mock_ipt_codes()
  expect_invisible(.arg_check_herbarium(NULL, verbose = TRUE))
  expect_true(.arg_check_herbarium(character(), verbose = TRUE))
  # no message and no IPT lookup
  expect_silent(.arg_check_herbarium(NULL, verbose = TRUE))
  expect_equal(calls$n, 0)
})


test_that(".arg_check_herbarium accepts valid acronyms using the full IPT list", {
  calls <- .mock_ipt_codes()
  expect_silent(res <- .arg_check_herbarium(c("RB", "PALM"), verbose = FALSE))
  expect_true(res)
  expect_equal(calls$n, 1)
  expect_null(calls$herbarium)
})


test_that(".arg_check_herbarium informs when verbose", {
  .mock_ipt_codes()
  expect_message(.arg_check_herbarium("HATM", verbose = TRUE),
                 "Checking whether the input herbarium code exists in the JABOT...",
                 fixed = TRUE)
})


test_that(".arg_check_herbarium lists every unrecognised acronym", {
  .mock_ipt_codes()
  err <- tryCatch(.arg_check_herbarium(c("RB", "XX", "rb"), verbose = FALSE),
                  error = function(e) e)
  expect_s3_class(err, "error")
  expect_null(err$call)
  msg <- conditionMessage(err)
  # shQuote() quoting differs by platform
  expect_match(msg, "not recognized by JABOT: [\"']XX[\"'], [\"']rb[\"']")
  expect_match(msg, "Use `jabot_summary()` to view available collections.", fixed = TRUE)
})


test_that(".arg_check_level normalises case and whitespace", {
  expect_equal(.arg_check_level(" family "), "FAMILY")
  expect_equal(.arg_check_level(c("genus", "Family")), c("GENUS", "FAMILY"))
})


test_that(".arg_check_level rejects unsupported levels", {
  err <- tryCatch(.arg_check_level(c("species", "genus", "order")),
                  error = function(e) conditionMessage(e))
  # shQuote() quoting differs by platform
  expect_match(err, "invalid: [\"']SPECIES[\"'], [\"']ORDER[\"']")
  expect_match(err, "Accepted values are: [\"']FAMILY[\"'], [\"']GENUS[\"']")
})
