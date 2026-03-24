df <- `Resident.MD.Survey_March.24,.2026_13.39`  # skip the header rows
df <- df[-c(3,4), ]
head(df)
demographics <- df [, c("V1", "V5", "V8", "V9", "V10")]
grocery_habits <- df[, c("V11", "V12", "V13", "V14", "V15", "V16")]
cooking        <- df[, c("V17", "V18", "V19")]
med_diet       <- df[, c("V20", "V21", "V22", "V23", "V24")]
open_ended     <- df[, c("V2", "V25", "V26", "V27")]

