student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
section <- factor(section, levels = c("A", "B"))
students <- data.frame(student_id, section, quiz1, quiz2, passed)
students
score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)
score_matrix["S04", "quiz2"]
score_matrix[1:2, , drop = FALSE]
course_record["course"]     # using [
course_record[["course"]]        
course_record$course
students$average <- (students$quiz1 + students$quiz2) / 2
students$excellent <- students$average >= 90
subset_a <- students[students$section == "A" & students$average >= 80, ]
subset_a[, c("student_id", "section", "average")]
avg_vector <- students$average
names(avg_vector) <- students$student_id



