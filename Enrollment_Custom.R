# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

library(here)

source(here("reading_severance.R"))

Enrollment <-
  read_csv("data_to_Clarity/Enrollment.csv",
           col_types =
             "nnnDcnnnlnDnnnDDDnnnncccnnDnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTnTn")


# County Served -----------------------------------------------------------

county_served <- da_answer %>% 
  filter(question == "County in which client is being served") %>%
  left_join(entry_exit_answer_link, by = "answer_id")

# the problem with this is if the Effective Dates don't match exactly, it doesn't
# link the EE ID to the answer.





