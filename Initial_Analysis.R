df <- `Resident.MD.Survey_March.24,.2026_13.39`  
df <- df[-c(3,4), ] # skip the header rows
head(df)
demographics <- df [, c("V1", "V5", "V8", "V9", "V10")]
grocery_habits <- df[, c("V11", "V12", "V13", "V14", "V15", "V16")]
cooking        <- df[, c("V17", "V18", "V19")]
med_diet       <- df[, c("V20", "V21", "V22", "V23", "V24")]
open_ended     <- df[, c("V2", "V25", "V26", "V27")]



demographics$V1 <- trimws(demographics$V1)
demographics$V1 <- gsub("High point", "High Point", demographics$V1, ignore.case = TRUE)
demographics$V1 <- gsub("High Point NC", "High Point", demographics$V1, ignore.case = TRUE)
demographics$V1 <- gsub("Winston salem|Winston-Salem|WINSTON-SALEM", "Winston-Salem", demographics$V1, ignore.case = TRUE)
demographics$V1 <- gsub("Greensboro", "Greensboro", demographics$V1, ignore.case = TRUE)
demographics$V1 <- gsub("Lexington", "Lexington", demographics$V1, ignore.case = TRUE)
demographics <- demographics[!demographics$V1 %in% c("Q6", "Which city or town in North Carolina do you currently live in?"), ]

table(demographics$V1)
table(demographics$V5)
table(demographics$V8)
table(demographics$V9)
table(demographics$V10)

table(cooking$V17)
table(cooking$V18)
table(cooking$V19)

table(grocery_habits$V11)
table(grocery_habits$V12)
table(grocery_habits$V13)
table(grocery_habits$V14)
table(grocery_habits$V15)
table(grocery_habits$V16)


table(med_diet$V20)
table(med_diet$V21)
table(med_diet$V22)
table(med_diet$V23)
table(med_diet$V24)
