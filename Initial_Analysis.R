install.packages("ggmap")
install.packages("maps")
library(ggplot2)
library(maps)
awareness <- as.data.frame(table(med_diet$V36))
df <- `Resident.MD.Survey_March.24,.2026_13.39`  
df <- df[-c(3,4), ] # skip the header rows
head(df)
demographics <- df [, c("V1", "V5", "V8", "V9", "V10", "V4")]
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
# Age awareness
ageAware <- data.frame(
  Age = demographics$V4,
  Awareness = med_diet$V20
)


ggplot(ageAware, aes(x=Age, fill=Awareness)) +
  geom_bar(position="dodge") +
  labs(title="MD Awareness by Age", x="", y="Count") +
  theme(axis.text.x = element_text(angle=45, hjust=1))
# End age awareness

# Interest vs. Awareness
interestAware <- data.frame(
  Interest = med_diet$V23, 
  Awareness = med_diet$V20
)

ggplot(interestAware, aes(x=Awareness, fill=Interest)) +
  geom_bar(position="dodge") +
  labs(title="MD Interest by Awareness", x="", y="Count") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

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


# NC Map Awareness
town_awareness <- as.data.frame(table(df$V1[med_diet$V20 == "Yes"]))
colnames(town_awareness) <- c("Town", "Count")


coords <- data.frame(
  Town = c("Cary", "Clemmons", "Durham", "Greensboro", "High Point", 
           "Lexington", "Mocksville", "Statesville", "Stokesdale", 
           "Summerfield", "Winston-Salem"),
  Lat  = c(35.7915, 36.0343, 35.9940, 36.0726, 35.9557,
           35.8237, 35.8921, 35.7815, 36.2365,
           36.2090, 36.0999),
  Long = c(-78.7811, -80.3820, -78.8986, -79.7910, -79.9939,
           -80.2534, -80.5629, -80.8498, -79.9736,
           -79.9827, -80.2442)
)


map_data_nc <- merge(town_awareness, coords, by="Town")

nc_map <- map_data("state") |> subset(region == "north carolina")

ggplot() +
  geom_polygon(data=nc_map, aes(x=long, y=lat, group=group), 
               fill="lightgray", color="white") +
  geom_point(data=map_data_nc, aes(x=Long, y=Lat, size=Count, color=Town), 
             alpha=0.8) +
  scale_size_continuous(range=c(3, 12)) +
  coord_fixed(ratio=1.3) +
  labs(title="MD Awareness by Town in NC", size="# Aware", color="Town") +
  theme_void()
