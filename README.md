# example-contracts

If you want to support multiple functions for a contract, use an `or` structure for the parameter type. If the function takes no parameters then its type should be unit, otherwise, use the types of the parameters.

For a token standard, you are likely going to need something like `(big_map address mutez)`.
