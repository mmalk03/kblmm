save_submission <- function(submission) {
    write.csv(submission, file = 'submission.csv', quote = FALSE, row.names = FALSE)
}
