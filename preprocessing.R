# טעינת חבילה
library(dplyr)

# בדיקה
test_data <- read.csv("subj001.csv")
head(test_data)  

files <- dir(pattern = ".csv")
all_data <- list()


for(file in files) {
  temp_data <- read.csv(file, stringsAsFactors = FALSE)  
  all_data[[file]] <- temp_data
}

# איחוד 
df <- do.call(rbind, all_data)
# עיבוד 
df <- df %>%
  mutate(
    #  task ו-congruency
    task = ifelse(grepl("naming", condition, ignore.case = TRUE), "naming", "reading"),
    congruency = ifelse(grepl("congruent", condition, ignore.case = TRUE), "congruent", "incongruent"),
    
    # משתנה דיוק
    accuracy = ifelse(correct_response == participant_response, 1, 0)
  ) %>%
  
  # בחירת המשתנים והמרה
  select(subject, task, congruency, block, trial, accuracy, rt) %>%
  mutate(
    subject = as.factor(subject),
    task = as.factor(task),
    congruency = as.factor(congruency),
    block = as.numeric(block),
    trial = as.numeric(trial)
  )

# בדיקה 
head(df)
summary(df)

# שמירת הנתונים הגולמיים
save(df, file = "raw_data.RData")

# סינון הנתונים
df_filtered <- df %>%
  filter(!is.na(rt)) %>%                     # הוצאת NA
  filter(rt >= 0.3 & rt <= 3)                # הוצאת זמני תגובה חקיגים

# חישוב אחוז הטריילים שנשאר לכל נבדק
trials_remaining <- df_filtered %>%
  group_by(subject) %>%
  summarise(
    trials_before = n(),
    percent_remaining = (n() / 400) * 100    # כל נבדק התחיל עם 400 ניסיונות
  )

#  הצגה
print("אחוז הטריילים שנשאר לכל נבדק:")
print(trials_remaining)

# חישוב ממוצע וסטיית תקן של אחוז ההוצאה
removal_stats <- trials_remaining %>%
  summarise(
    mean_percent_removed = mean(100 - percent_remaining),
    sd_percent_removed = sd(100 - percent_remaining)
  )

print("סטטיסטיקת הסרת הנתונים:")
print(removal_stats)

# שמירת 
save(df_filtered, file = "filtered_data.RData")

