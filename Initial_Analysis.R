install.packages("ggplot2")  # if you haven't already
library(ggplot2)
awareness <- as.data.frame(table(med_diet$V36))
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
grocery_habits <- grocery_habits[-c(1,2), ]
cooking        <- cooking[-c(1,2), ]
med_diet       <- med_diet[-c(1,2), ]
open_ended     <- open_ended[-c(1,2), ]

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

# Percent aware
prop.table(table(med_diet$V20)) * 100


# Frequency Bar Graphs

awareness <- data.frame(
  Response = names(table(med_diet$V20)),
  Freq = as.numeric(table(med_diet$V20))
)

ggplot(awareness, aes(x=Response, y=Freq)) +
  geom_bar(stat="identity") +
  labs(title="Mediterranean Diet Awareness", x="", y="Count") +
  theme(axis.text.x = element_text(angle=45, hjust=1))



# Cross tabulating


# Male vs. Female awareness
genderAware <- data.frame(
  Gender = demographics$V5,
  Awareness = med_diet$V20
)

ggplot(combined, aes(x=Awareness, fill=Gender)) +
  geom_bar(position="dodge") +
  labs(title="MD Awareness by Gender", x="", y="Count") +
  theme(axis.text.x = element_text(angle=45, hjust=1))
# End of M v F

# Per town awareness

townAware <- data.frame(
  Town = demographics$V1,
  Awareness = med_diet$V20
)


ggplot(townAware, aes(x=Town, fill=Awareness)) +
  geom_bar(position="dodge") +
  labs(title="MD Awareness by Town", x="", y="Count") +
  theme(axis.text.x = element_text(angle=45, hjust=1))
# End of town awareness

# Salary Range awareness

salaryAware <- data.frame(
  Salary = demographics$V8,
  Awareness = med_diet$V20
)

ggplot(salaryAware, aes(x=Salary, fill=Awareness)) +
  geom_bar(position="dodge") +
  labs(title="MD Awareness by Salary", x="", y="Count") +
  theme(axis.text.x = element_text(angle=45, hjust=1))
# End of salary awareness


# Budget compared to awareness

budgetAware <- data.frame(
  Budget = as.numeric(grocery_habits$V14),
  Awareness = med_diet$V20
)
ggplot(budgetAware, aes(x=Awareness, y=Budget)) + 
  geom_point() + 
  labs(title="Grocery Budget by MD Awareness", x="Awareness Level", y="Monthly Budget ($)") +
  theme(axis.text.x = element_text(angle=45, hjust=1))



aggregate(Budget ~ Awareness, data=budgetAware, FUN=mean)
# End of budget analysis


# Times cooking per week vs awareness

cookingTime <- data.frame(
  Cook = cooking$V17,
  Awareness = med_diet$V20
)

ggplot(cookingTime, aes(x=Cook, fill=Awareness)) +
  geom_bar(position="dodge") +
  labs(title="MD Awareness by Cook Time", x="", y="Count") +
  theme(axis.text.x = element_text(angle=45, hjust=1))
