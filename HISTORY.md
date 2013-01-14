0.8.3
-----

*   or statement schemas now only report the first batch of errors, so `(validation-errors [:or nil? String] 5)` would only report that 5 is not nil.  This is to avoid a combinatorial explosion of error messages when validating elaborate nested schemas.

*started on Jan 13, 2013*