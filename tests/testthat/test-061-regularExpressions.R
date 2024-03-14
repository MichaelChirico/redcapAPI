context("Regular Expressions")

#####################################################################
# REGEX_FIELD_NAME                                               ####

test_that(
  "REGEX_FIELD_NAME correctly identifies acceptable field names", 
  {
    field_name <- c("a", "ab", "a1", "a_1", 
                    "a_", "_a", "a__1", "1", "1a", 
                    "multiple_under_scores")
    
    expect_equal(grepl(REGEX_FIELD_NAME, field_name, perl = TRUE), 
                 c(TRUE, TRUE, TRUE, TRUE, 
                   FALSE, FALSE, FALSE, FALSE, FALSE, 
                   TRUE))
  }
)

#####################################################################
# REGEX_FORM_NAME                                                ####

test_that(
  "REGEX_FORM_NAME correctly identifies acceptable field names", 
  {
    form_name <- c("a", "ab", "a1", "a_1", 
                    "a_", "_a", "a__1", "1", "1a", 
                    "multiple_under_scores")
    
    expect_equal(grepl(REGEX_FORM_NAME, form_name, perl = TRUE), 
                 c(TRUE, TRUE, TRUE, TRUE, 
                   FALSE, FALSE, FALSE, FALSE, FALSE, 
                   TRUE))
  }
)

#####################################################################
# REGEX_MULT_CHOICE                                              ####

test_that(
  "REGEX_MULT_CHOICE correctly matches allowable variations on multiple choice definitions", 
  {
    choice <- c("0,0 - Not at all|1|2|3|4|5|6, 6 - Very", 
                "0,0 - Not at all|1,1|2,2|3,3|4,4|5,5|6, 6 - Very", 
                "0|1|2|3|4|5|6", 
                "a,A | b, b with, a comma | text", 
                "a", 
                "a|A", 
                "|b, B", 
                "a, A|")
    expect_equal(grepl(REGEX_MULT_CHOICE, choice, perl = TRUE), 
                 rep(c(TRUE, FALSE), c(6, 2)))
  }
)