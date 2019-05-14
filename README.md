# example-contracts

If you want to support multiple functions for a contract, use an `or` structure for the parameter type. If the function takes no parameters then its type should be unit, otherwise, use the types of the parameters.

For a token standard, you are likely going to need something like `(big_map address mutez)`.

## caller and callback contracts

Originate the callback contract

```
~/alphanet.sh client originate contract callback1 for alice \
                                transferring 0 from alice \
                                running container:callback.tz \
                                --init '"Hello World! This is a callback"'
```

This gave me this address: "KT1Roo3UhX6WP8eoaNP3DnnmaUWqPZmbiGGw".

Originate the caller contract with the address of the callback contract

```
~/alphanet.sh client originate contract caller1 for alice \
                                transferring 0 from alice \
                                running container:caller.tz \
                                --init '(Pair "Nothing to say..." "KT1Roo3UhX6WP8eoaNP3DnnmaUWqPZmbiGGw")'
```

This gave me this address: "KT1E1NoYWyYjdVuhHPUCBDZgLiHZiM2SqRcN"


```
~/alphanet.sh client transfer 0 from alice to caller1 --arg '(Left Unit)'
```

```
~/alphanet.sh client transfer 0 from alice to callback1 --arg '(Left "Updated a string from a transaction")'
~/alphanet.sh client transfer 0 from alice to callback1 --arg '(Right "KT1ADK1mE6JhY3bfL5RrhJ7cmY5wkTMf5ZVW")'
```


https://tezos.stackexchange.com/questions/278/get-a-returned-value-when-calling-a-michelson-contract/281#281
https://tezos.stackexchange.com/questions/424/is-there-a-way-to-call-a-function-inside-a-contract-from-a-function-in-another-c/425#425
https://tezos.stackexchange.com/questions/965/method-for-calling-to-entry-point-of-contract-instance-created-by-contract-c/966#966
