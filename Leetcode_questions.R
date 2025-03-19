##################################################################################################
#Question 1.#Write a solution to find the ids of products that are both low fat and recyclable.
#Return the result table in any order.
library(dplyr)
result <- products %>% filter(low_fats == 'Y' & recyclable == 'Y') %>% select(prodct_id)
##################################################################################################
#Question 2.Find the names of the customer that are not referred by the customer with id = 2.
#Return the result table in any order.
library(dplyr)
result <- Customer %>% filter(is.na(referee_id) | referee_id!=2) %>% select(name)
##################################################################################################
#Question 3. Write a solution to find the name, population, and area of the big countries.
library(dplyr)
result <- World %>% filter(area >= 300000 | population >= 25000000) %>% select(name, population, area)
##################################################################################################
#Question 4.Write a solution to find all the authors that viewed at least one of their own articles.
#Return the result table sorted by id in ascending order.
library(dplyr)
result <- Views %>% filter(author_id == viewer_id) %>% distinct(author_id) %>% arrange(author_id)
##################################################################################################
#Question 5.Write a solution to find the IDs of the invalid tweets. The tweet is invalid if the number of characters used in the content of the tweet is strictly greater than 15.
library(dplyr) 
result <- Tweets %>% filter( nchar(content) > 15) %>% select(tweet_id)
##################################################################################################
library(dplyr)
result <- Employees %>% left_join(EmployeeUNI, by = "id") %>% select(name, unique_id = coalesce(unique_id, NA_integer_))
##################################################################################################
library(dplyr)
result <- Sales %>% left_join(Product, by = "product_id") %>% select(product_name, year, price)
##################################################################################################
library(dplyr)
result <- Visits %>% left_join(Transactions, by = "visit_id") %>% filter(is.na(transaction_id)) %>% group_by(customer_id) %>% summarize(count_no_trans = n())
##################################################################################################
library(dplyr)
result <- Weather %>% arrange(recordDate) %>% filter(temperature > lag(temperature, default = first(temperature)))
##################################################################################################
library(dplyr)
result <- Activity %>% group_by(machine_id, process_id) %>% summarise(total_time = max(timestamp) - min(timestamp)) %>% group_by(machine_id) %>% summarise(average_time = round(mean(total_time), digits =3))
##################################################################################################
library(dplyr)
result <- Employee %>% left_join(Bonus, by = "empId") %>% filter(is.na(bonus) | bonus < 1000) %>% select(name, bonus)
##################################################################################################
#####Practice this again##############################################################################
#Question 12.# Create input datasets
Students <- data.frame(
  student_id = c(1, 2, 13, 6),
  student_name = c("Alice", "Bob", "John", "Alex")
)

Subjects <- data.frame(
  subject_name = c("Math", "Physics", "Programming")
)

Examinations <- data.frame(
  student_id = c(1, 1, 1, 2, 1, 1, 13, 13, 13, 2, 1),
  subject_name = c('Math', 'Physics', 'Programming', 'Programming', 'Physics', 'Math', 'Math', 'Programming', 'Physics', 'Math', 'Math')
)
result <- Students %>%
  # Perform cross join between Students and Subjects
  cross_join(Subjects) %>%
  # Left join with Examinations, adding a dummy column to count occurrences
  left_join(
    Examinations %>%
      mutate(dummy = 1) %>%
    select(student_id, subject_name, dummy),
    by = c("student_id", "subject_name")
  ) %>%
  group_by(student_id, student_name, subject_name) %>%
  summarise(attended_exams = sum(dummy,na.rm = TRUE)) %>%
  arrange(student_id, subject_name)
##################################################################################################
#####Practice this again##############################################################################
# Create the Employee data frame
Employee <- data.frame(
  id = c(101, 102, 103, 104, 105, 106),
  name = c("John", "Dan", "James", "Amy", "Anne", "Ron"),
  department = c("A", "A", "A", "A", "A", "B"),
  managerId = c(NA, 101, 101, 101, 101, 101),
  stringsAsFactors = FALSE
)
#We perform a left join between the Employee table and a summary table of manager reports counts
library(dplyr)
result <- Employee %>% left_join(Employee %>% filter(!is.na(managerId)) %>%
                                group_by(managerId) %>% summarise(number_of_reports = n()),
                                by = c("id" = "managerId")) %>% # Join on manager ID
  filter(number_of_reports >= 5) %>%      # Filter for >= 5 reports
  select(name)                           # Extract names as vector
##################################################################################################
library(dplyr)
result <- Signups %>% left_join(Confirmations, by = "user_id") %>% mutate(confirmed = ifelse(is.na(action) | action == "timeout", 0, 1)) %>%
  group_by(user_id) %>% summarise(confirmation_rate = round(sum(confirmed)/n(), 2))
##################################################################################################
library(dplyr)
result <- Cinema %>% filter(id %% 2 != 0 & description != "boring") %>% arrange(desc(rating))
##################################################################################################
Prices <- data.frame(
  product_id = c(1, 1, 2, 2),
  start_date = as.Date(c("2019-02-17", "2019-03-01", "2019-02-01", "2019-02-21")),
  end_date = as.Date(c("2019-02-28", "2019-03-22", "2019-02-20", "2019-03-31")),
  price = c(5, 20, 15, 30)
)

# Create the UnitsSold table
UnitsSold <- data.frame(
  product_id = c(1, 1, 2, 2),
  purchase_date = as.Date(c("2019-02-25", "2019-03-01", "2019-02-10", "2019-03-22")),
  units = c(100, 15, 200, 30)
)
library(dplyr)
result <- Prices %>% left_join(UnitsSold, by = "product_id") %>%
# Filter the rows based on the condition: purchase_date between start_date and end_date
  filter(purchase_date >= start_date & purchase_date <= end_date) %>% 
  group_by(product_id) %>% summarise(tot_price = sum(price*units),
                                  total_units = sum(units)) %>%
  mutate(average_price = round(tot_price/total_units, digits = 2)) %>%
  select(product_id, average_price)
##################################################################################################
Project <- data.frame(
  project_id = c(1, 1, 1, 2, 2),
  employee_id = c(1, 2, 3, 1, 4)
)

# Create the Employee table
Employee <- data.frame(
  employee_id = c(1, 2, 3, 4),
  name = c("Khaled", "Ali", "John", "Doe"),
  experience_years = c(3, 2, 1, 2)
)
library(dplyr)
result <- Project %>% left_join(Employee, by = "employee_id") %>% 
  group_by(project_id) %>% summarise(avg_exp = round(sum(experience_years)/n(), 2))
##################################################################################################
Users <- data.frame(
  user_id = c(6, 2, 7),
  user_name = c("Alice", "Bob", "Alex")
)

# Create the Register table
Register <- data.frame(
  contest_id = c(215, 209, 208, 210, 208, 209, 209, 215, 208, 210, 207, 210),
  user_id = c(6, 2, 2, 6, 6, 7, 6, 7, 7, 2, 2, 7)
)
# Calculate the total number of users
total_users <- nrow(Users)

library(dplyr)
result <- Register  %>%
  group_by(contest_id) %>% summarise(percentage = round(n()/total_users * 100, 2)) %>% arrange(desc(percentage), contest_id)
##################################################################################################
Queries <- data.frame(
  query_name = c("Dog", "Dog", "Dog", "Cat", "Cat", "Cat"),
  result = c("Golden Retriever", "German Shepherd", "Mule", "Shirazi", "Siamese", "Sphynx"),
  position = c(1, 2, 200, 5, 3, 7),
  rating = c(5, 5, 1, 2, 3, 4)
)

# Calculate the quality and poor_query_percentage
result <- Queries %>%
  group_by(query_name) %>%
  summarise(
    quality = round(mean(rating / position), 2),
    poor_query_percentage = round(mean(rating < 3) * 100, 2)
  )

library(dplyr)
result <- Queries  %>% mutate(poor_query = ifelse(rating < 3, 1, 0)) %>% 
  group_by(query_name) %>% summarise(quality = round(mean(rating/position), 2),
                                    poor_query_perc = round(sum(poor_query)/n() * 100, 2))
##################################################################################################
Transactions <- data.frame(
  id = c(121, 122, 123, 124),
  country = c("US", "US", "US", "DE"),
  state = c("approved", "declined", "approved", "approved"),
  amount = c(1000, 2000, 2000, 2000),
  trans_date = as.Date(c("2018-12-18", "2018-12-19", "2019-01-01", "2019-01-07"))
)
library(dplyr)
result <- Transactions %>% mutate(month_year = format(trans_date, "%Y-%m")) %>%
  group_by(month_year, country) %>% summarise(number_of_trans = n(),
                                              totamt = sum(amount),
                                              num_approved = sum(state == "approved"),
                                              approved_tot_amt = sum(amount[state == "approved"]))
##################################################################################################
Delivery <- data.frame(
  delivery_id = c(1, 2, 3, 4, 5, 6, 7),
  customer_id = c(1, 2, 1, 3, 3, 2, 4),
  order_date = as.Date(c("2019-08-01", "2019-08-02", "2019-08-11", "2019-08-24", "2019-08-21", "2019-08-11", "2019-08-09")),
  customer_pref_delivery_date = as.Date(c("2019-08-02", "2019-08-02", "2019-08-12", "2019-08-24", "2019-08-22", "2019-08-13", "2019-08-09"))
)
library(dplyr)

result <- Delivery %>%
  # Find first order for each customer
  group_by(customer_id) %>%
  mutate(first_order = min(order_date)) %>%
           ungroup() %>% 
  filter(order_date == first_order) %>% 
  mutate(order_name = ifelse(order_date == customer_pref_delivery_date, "immediate", "scheduled")) %>%
  summarise(perc_immediate = round(sum(order_name == "immediate")/n() *100, 2))

##################################################################################################
Activity <- data.frame(
  player_id = c(1, 1, 2, 3, 3),
  device_id = c(2, 2, 3, 1, 4),
  event_date = as.Date(c("2016-03-01", "2016-03-02", "2017-06-25", "2016-03-02", "2018-07-03")),
  games_played = c(5, 6, 1, 0, 5)
)
library(dplyr)
tot_players <- n_distinct(Activity$player_id)
result <- Activity %>% group_by(player_id) %>% arrange(event_date) %>% filter(event_date - lag(event_date) == 1) %>% summarise(fraction = n()/tot_players)
##################################################################################################
Teacher <- data.frame(
  teacher_id = c(1, 1, 1, 2, 2, 2, 2),
  subject_id = c(2, 2, 3, 1, 2, 3, 4),
  dept_id = c(3, 4, 3, 1, 1, 1, 1)
)

library(dplyr)
result <- Teacher %>% group_by(teacher_id) %>% summarise(unique_sub = n_distinct(subject_id))
##################################################################################################
Activity <- data.frame(
  user_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4),
  session_id = c(1, 1, 1, 4, 4, 4, 2, 2, 2, 3, 3),
  activity_date = as.Date(c("2019-07-20", "2019-07-20", "2019-07-20", "2019-07-20", "2019-07-21", "2019-07-21", "2019-07-21", "2019-07-21", "2019-07-21", "2019-06-25", "2019-06-25")),
  activity_type = c('open_session', 'scroll_down', 'end_session', 'open_session', 'send_message', 'end_session', 'open_session', 'send_message', 'end_session', 'open_session', 'end_session')
)
library(dplyr)
result <- Activity %>% filter(activity_date >= "2019-06-28" & activity_date <= "2019-07-27") %>%
  group_by(activity_date) %>%
  summarise(dau = n_distinct(user_id))
##################################################################################################
Sales <- data.frame(
  sale_id = c(1, 2, 7),
  product_id = c(100, 100, 200),
  year = c(2008, 2009, 2011),
  quantity = c(10, 12, 15),
  price = c(5000, 5000, 9000)
)
library(dplyr)
result <- Sales %>% group_by(product_id) %>% mutate(first_year = min(year)) %>% filter(year == first_year) %>%
  select(product_id, year, quantity, price)
##################################################################################################
Courses <- data.frame(
  student = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'),
  class = c('Math', 'English', 'Math', 'Biology', 'Math', 'Computer', 'Math', 'Math', 'Math')
)
library(dplyr)
result <- Courses %>% group_by(class) %>% filter(n() > 5) %>%
  distinct(class)
##################################################################################################
library(dplyr)
result <- Followers %>% group_by(user_id) %>% summarise(number_of_foll = n()) %>% arrange(user_id)
##################################################################################################
MyNumbers <- data.frame(num = c(8, 8, 3, 3, 1, 1, 1, 5))
library(dplyr)
result <- MyNumbers %>% group_by(num) %>% filter(n() ==1) %>%
  mutate(largest_number = ifelse(is.na(num), NA, max(num))) %>% select(num)
##################################################################################################
Customer <- data.frame(customer_id = c(1, 2, 3, 3, 1),
                       product_key = c(5, 6, 5, 6, 6))

Product <- data.frame(product_key = c(5, 6))

library(dplyr)
result <- Customer %>% group_by(customer_id) %>% filter(all(Product$product_key %in% product_key)) %>% distinct(customer_id)
##################################################################################################
Employees <- data.frame(
  employee_id = c(9, 6, 4, 2),
  name = c("Hercy", "Alice", "Bob", "Winston"),
  reports_to = c(NA, 9, 9, NA),
  age = c(43, 41, 36, 37)
)
library(dplyr)
managers <- Employees %>% filter(employee_id %in% reports_to & is.na(reports_to)) 
result <- Employees %>%
  filter(!is.na(reports_to)) %>%
  group_by(reports_to) %>%
  summarise(
    reports_count = n(),
    average_age = round(mean(age))
  ) %>%
  rename(employee_id = reports_to) %>%
  left_join(Employees %>% select(employee_id, name), by = "employee_id") %>%
  select(employee_id, name, reports_count, average_age) %>%
  arrange(employee_id)
##################################################################################################
#Past year sample questions
#Given two tables Friend_request (requester_id, sent_to_id, time) Request_accepted (acceptor_id, requestor_id, time).
#Find the overall acceptance rate of requests (total number of accepted requests/total number of sent requests).
library(dplyr)
total_requests <- nrow(Friend_request)
total_accepts <- nrow(request_accepted)
acceptance_rate <- total_accepted / total_requests
##################################################################################################
#Write a query to find out the overall friend acceptance rate for all dates.
actions_df %>%
  group_by(date) %>%
  summarize(total_sent = sum(Action == "Sent"),
    total_accepted  = sum(Action == "Accepted"),
    acceptance_rate = total_accepted / total_sent)
#Acceptance rate for a SPECIFIC date, e.g. "2023-01-01"
actions_df %>%
  filter(date == date_of_interest) %>%
  summarize(total_sent = sum(Action == "Sent"),
    total_accepted  = sum(Action == "Accepted"),
    acceptance_rate = total_accepted / total_sent)

##################################################################################################
#Given the tables, how would you know who has the most friends?
#Tables:
#REQUESTS(date, sender_id, accepter_id)
#ACCEPTED(accepted_at, accepter_id, sender_id)
# Example data for REQUESTS and ACCEPTED tables
REQUESTS <- data.frame(
  date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
  sender_id = c(1, 2, 3),
  accepter_id = c(2, 3, 4)
)

ACCEPTED <- data.frame(
  accepted_at = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
  accepter_id = c(2, 3, 4),
  sender_id = c(1, 2, 3)
)
library(dplyr)
library(tidyr)
# Step 1: Combine REQUESTS and ACCEPTED to identify all unique friendships
friendships <- bind_rows(
  REQUESTS %>% select(user1 = sender_id, user2 = accepter_id),
  ACCEPTED %>% select(user1 = sender_id, user2 = accepter_id)
) %>%
  distinct() # Remove duplicates to ensure unique friendships
# Step 2: Count the number of friends for each user
friend_counts <- bind_rows(
  friendships %>% select(user = user1, friend = user2),
  friendships %>% select(user = user2, friend = user1)
) %>%
  group_by(user) %>%
  summarise(friend_count = n(), .groups = 'drop')
# Step 3: Identify the user(s) with the most friends
most_friends <- friend_counts %>%
  filter(friend_count == max(friend_count))

# Print the result
print(most_friends)
##################################################################################################
