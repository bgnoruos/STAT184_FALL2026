student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)
#grading function
grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}
grade_one(NA)   
grade_one(90)   
grade_one(80)   
grade_one(85)   
grade_one(74)   

#Part B
grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id
grades
seq_along(scores)
#Part C-1
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  c(
    count = length(x),
    missing = sum(is.na(x)),
    mean = round(mean(x, na.rm = na.rm), digits),
    sd = round(sd(x, na.rm = na.rm), digits),
    min = round(min(x, na.rm = na.rm), digits),
    max = round(max(x, na.rm = na.rm), digits)
  )
}
summarize_scores(scores)
summarize_scores(x = scores, na.rm = TRUE, digits = 2)
# Part C-2
plot_scores <- function(x, ...) {
  plot(x, ...)
}

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores")


