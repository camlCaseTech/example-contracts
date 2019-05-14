# example-contracts

If you want to support multiple functions for a contract, use an `or` structure for the parameter type. If the function takes no parameters then its type should be unit, otherwise, use the types of the parameters.

For a token standard, you are likely going to need something like `(big_map address mutez)`.

## caller and callback contracts

Originate the callback contract

```
~/alphanet.sh client originate contract callback for alice \
                                transferring 0 from alice \
                                running container:callback.tz \
                                --init '"Hello World! This is a callback"'
```

This gave me this address: "KT1Hk43CvzG9RJNnNCbidha57wd6MZdC9KMd".

Originate the caller contract with the address of the callback contract

```
~/alphanet.sh client originate contract caller for alice \
                                transferring 0 from alice \
                                running container:caller.tz \
                                --init '(Pair "Nothing to say..." "KT1Hk43CvzG9RJNnNCbidha57wd6MZdC9KMd")'
```

This gave me this address: "KT1ADK1mE6JhY3bfL5RrhJ7cmY5wkTMf5ZVW"


```
~/alphanet.sh client transfer 0 from alice to caller --arg '(Left Unit)'
```

```
~/alphanet.sh client transfer 0 from alice to callback --arg '(Left "Updated a string from a transaction")'
~/alphanet.sh client transfer 0 from alice to callback --arg '(Right "KT1ADK1mE6JhY3bfL5RrhJ7cmY5wkTMf5ZVW")'
```


try something
```
~/alphanet.sh client originate contract caller3 for alice \
                                transferring 1 from alice \
                                running container:caller2.tz \
                                --init 'Unit'
```
KT1PmamTQhrq42ys9aAfHzesCESpkytRwi74

`~/alphanet.sh client transfer 12.345 from alice to caller3 --arg '(Pair "sending a message from home" "KT1ADK1mE6JhY3bfL5RrhJ7cmY5wkTMf5ZVW")'`

`~/alphanet.sh client list known contracts`




https://tezos.stackexchange.com/questions/278/get-a-returned-value-when-calling-a-michelson-contract/281#281
https://tezos.stackexchange.com/questions/424/is-there-a-way-to-call-a-function-inside-a-contract-from-a-function-in-another-c/425#425
https://tezos.stackexchange.com/questions/965/method-for-calling-to-entry-point-of-contract-instance-created-by-contract-c/966#966
