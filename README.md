# File-Format-Conversion
Data Format Conversion is one of the most common problems in Computer Science. Similarly command lines tools like ”unix2dos” and ”dos2unix” are used to convert files between different newline conventions.

We shall mostly follow the RFC4180 conventions.
 Each record is located on a separate line, delimited by a newline (LF).
 The last record is also terminated by a newline (LF).
 Within each record, there may be one or more fields, separated by delimiter (comma, semicolons etc).
The last field in the record must not be followed by a delimiter.
 Each line should contain the same number of fields throughout the file.
 Spaces are considered part of a field and should not be ignored.
 Each field may or may not be enclosed in double quotes.
 Fields containing line breaks (LF), double quotes, and commas should be enclosed in double-quotes.
 If double-quotes are used to enclose fields, then a double-quote appearing inside a field must be escaped
by preceding it with another double quote.