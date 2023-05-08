module FormatDuration where
import Data.List

toYear :: (Integral i, Show i) => i -> String
toYear n = (show years) ++ "year" ++ (if years > 1 then "s" else "")
  where years = div n 31536000

toDay :: (Integral i, Show i) => i -> String
toDay n = (show days) ++ "day" ++ (if days > 1 then "s" else "")
  where days = div n 86400

toHour :: (Integral i, Show i) => i -> String
toHour n = (show hours) ++ "hour" ++ (if hours > 1 then "s" else "")
  where hours = div n 3600

toMinute :: (Integral i, Show i) => i -> String
toMinute n = (show minutes) ++ "minute" ++ (if minutes > 1 then "s" else "")
  where minutes = div n 60

toSecond :: (Integral i, Show i) => i -> String
toSecond seconds = (show seconds) ++ "second" ++ (if seconds > 1 then "s" else "")


formatDuration' :: (Integral i, Show i) => i -> [String]
formatDuration' n
  | n > 31536000 = [toYear n] ++ formatDuration'(mod n 31536000)
  | n > 864000 = [toDay n] ++ formatDuration'(mod n 86400)
  | n > 3600 = [toHour n] ++ formatDuration'(mod n 3600)
  | n > 60 = [toMinute n] ++ formatDuration'(mod n 60)
  | otherwise  = [toSecond]

formatDuration :: (Integral i, Show i) => i -> String
formatDuration n = intercalate ", " (formatDuration' n)
