module DateFormatter where

import Date

formatRange : Date.Date -> Date.Date -> String
formatRange startDate endDate =
  let startMonth = Date.month startDate |> toString
      startDayOfMonth = Date.day startDate |> toString
      startYear = Date.year startDate |> toString
      endMonth = Date.month endDate |> toString
      endDayOfMonth = Date.day endDate |> toString
      endYear = Date.year endDate |> toString
  in
     if startYear /= endYear then
       startMonth ++ " " ++ startDayOfMonth ++ ", " ++ startYear ++ "-" ++ endMonth ++ " " ++ endDayOfMonth ++ ", " ++ endYear
     else if startMonth == endMonth && startDayOfMonth == endDayOfMonth then
       startMonth ++ " " ++ startDayOfMonth ++ ", " ++ startYear
     else if startMonth == endMonth then
       startMonth ++ " " ++ startDayOfMonth ++ "-" ++ endDayOfMonth ++ ", " ++ startYear
     else
       startMonth ++ " " ++ startDayOfMonth ++ "-" ++ endMonth ++ " " ++ endDayOfMonth ++ ", " ++ startYear
