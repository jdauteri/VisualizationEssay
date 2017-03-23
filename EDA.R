nnv %>% 
  select(id, State, Office, Affiliation, Vote, Name, County) %>% 
  mutate(year = as.integer(str_extract(id, "\\d{4}"))) %>% 
  filter(State == "Louisiana") 

nnv %>% 
  select(id, State, Office, Affiliation, Vote, Name, County) %>% 
  mutate(year = as.integer(str_extract(id, "\\d{4}"))) %>% 
  group_by(year) %>% 
  summarize(total)
  filter(State == "Louisiana",
         year == "1820",
         name == "") %>% 
  ggplot(aes(y = Office)) +
  geom_histogram() 

View(nnv %>% 
  select(id, State, Office, Vote, Name, County) %>% 
  mutate(year = as.integer(str_extract(id, "\\d{4}"))) %>%
  group_by(Name, County, year) %>% 
  filter(State == "Louisiana",
         Office == "Governor")) 

  ggplot(aes(x = Name)) +
  geom_bar() +
  facet_wrap(~ County)
  
#state level  
nnv %>%
  filter(State == "Maryland") %>% 
  mutate(year = as.integer(str_extract(id, "\\d{4}"))) %>%
  group_by(year, `Affiliation ID`, Affiliation) %>%
  summarize(Vote = sum(Vote)) %>%
  group_by(year) %>%
  mutate(total_vote = sum(Vote)) %>% 
  filter(year >= "1802")

#governor in maryland between 1809-1821
nnv %>%
  filter(State == "New Jersey",
         Office == "Governor") %>% 
  mutate(year = as.integer(str_extract(id, "\\d{4}"))) %>%
  group_by(year, Name, Affiliation, County) %>%
  summarize(Vote = sum(Vote)) %>%
  group_by(year) %>%
  mutate(total_vote = sum(Vote)) %>% 
  ggplot(aes(x = year, y = Vote, color = Name)) +
  geom_line() +
  geom_point()

#1805-1810 competition
nnv %>%
  filter(State == "New Jersey",
         Office == "Governor") %>% 
  mutate(year = as.integer(str_extract(id, "\\d{4}"))) %>%
  group_by(year, Name, Affiliation, County) %>%
  summarize(Vote = sum(Vote)) %>%
  group_by(year) %>%
  mutate(total_vote = sum(Vote)) %>% 
  filter(year >= "1800", year <= "1805") %>% 
  ggplot(aes(x = year, y = Vote, color = Name)) +
  geom_line() +
  geom_point()
    