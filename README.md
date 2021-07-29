# markeR

A web based application for marking student assignments.

Assignments are randomised and presented to markers. Markers may then add general comments from a pre-defined
list, or student-specific comments as appropriate, alongside with recording a mark.

Marks are automatically tallied and at the end of marking, student-specific feedback is generated.

As marks and comments are recorded down to the sub-question level, fine-grained analyses can be done,
such as comparing the distribution of comments and marks to determine if there are any questions that
the cohort has been consistently performing well or poorly in. Individualised feedback gives students
a better idea of where they sit compared with their class mates. And if there are multiple markers,
then consistency of marking can be evaluated (e.g. marks by order of marking should not show trend).

## Setting up for marking

1. Download all assignments from stream.
2. Extract to a folder.
3. Copy the convert_to_html.R, create_marks_database.R and parse_assignment.R scripts across.
4. Create a solutions.Rmd file for the solutions. Square brackets for mark allocation.
5. Edit convert_to_html.R accordingly (paths) and run it.
  - possibly check for weirdness/errors etc
6. Run parse_assignment.R to create questions.csv
7. Download marks sheet from stream and run create_marks_database.R
8. Copy marks.csv, questions.csv, www folder over to app

## After marking marking

1. Copy marks.csv, questions.csv from app
2. Copy create_feedback.R, feedback.Rmd, star.png into folder.
3. Edit create_feedback/feedback.Rmd as needed.
4. Check marks, then run create_feedback script to gen feedback.
5. Zip up student folders (i.e. not high level folder, individual student folders all at once) for upload to Stream.
6. Upload marks to stream, feedback files to stream.
7. Copy summarise_results.Rmd and alter accordingly.
