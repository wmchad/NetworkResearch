## Pads a number so that it starts with a leading 0
## if only one digit (1 -> 01)
Pad2 <- function(num) { sprintf("%02d", num); };

## Converts a year, month, day set into a date object
Date.ymd <- function(year, month, day) {
  as.Date(paste(year, month, day, sep="-"));
}
