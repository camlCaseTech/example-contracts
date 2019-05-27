module ERC20 where

{-
https://www.michelson-lang.com/contract-a-day.html

https://github.com/ethereum/vyper/blob/master/examples/tokens/ERC20.vy
http://tztoken.teztech.io/
https://gitlab.com/tzip/tzip/blob/master/A/A1.md

https://gitlab.com/tips2/TIPs/blob/master/TIPS/tip-7.md
http://tztoken.teztech.io/
https://github.com/OCamlPro/liquidity/blob/next/tests/others/token.liq

https://gitlab.com/tzip/tzip

https://tezos.gitlab.io/alphanet/tutorials/michelson_anti_patterns.html
https://tezos.gitlab.io/alphanet/whitedoc/michelson.html
http://www.liquidity-lang.org/doc/tutorial/game.html#

https://github.com/Uniswap/contracts-vyper/blob/master/contracts/uniswap_exchange.vy
https://github.com/Uniswap/contracts-vyper/blob/master/contracts/uniswap_factory.vy
https://theethereum.wiki/w/index.php/ERC20_Token_Standard
https://github.com/ethereum/vyper/blob/master/examples/tokens/ERC20.vy

https://vyper.readthedocs.io/en/v0.1.0-beta.9/structure-of-a-contract.html

struct TokenData(
  string name,
  string symbol,
  nat decimals
);

struct PassthroughData(
  address from,
  mutez amount,
  bytes contractData
);

storage map[address=>mutez] balances;

storage TokenData data;

entry transfer(address to, mutez amount, ?bytes contractData){
  assert(storage.balances.in(SENDER));
  let mutez bal = storage.balances.get(SENDER);
  assert(bal >= input.amount);
  storage.balances.push(SENDER, sub(bal, input.amount));
  bal = mutez 0;

  if (storage.balances.in(input.to)){
    bal = storage.balances.get(input.to);
  }
  storage.balances.push(input.to, add(bal, input.amount));

  if (isset(input.contractData)){
    transfer(input.to, mutez 0, new PassthroughData(SENDER, input.amount, to_some(input.contractData)));
  }
}

You are limited to a single big_map per program, which must appear on the left hand side of a pair in the contract’s storage.


parameter bytes;

storage (pair (big_map address mutez) (pair string (pair string nat)));

# big map functions
# get
# mem
# update

# slice, string access

code{ DUP;
      CDR;
      NIL operation;
      PAIR;
      SWAP;
      CAR;
      DUP;
      PUSH nat 4;
      PUSH nat 0;
      SLICE;
      IF_NONE {PUSH nat 100; FAILWITH} {};
      DUP;
      PUSH bytes 0x19308cc0;
      COMPARE;
      EQ;
      IF { DROP;
           DUP;
           SIZE;
           PUSH nat 4;
           SWAP;
           SUB;
           DUP;
           GT;
           IF {} {PUSH nat 102; FAILWITH};
           ABS;
           PUSH nat 4;
           SLICE;
           IF_NONE {PUSH nat 101; FAILWITH} {};
           UNPACK (pair address (pair mutez (option bytes)));
           IF_NONE {PUSH nat 103; FAILWITH} {};
           PAIR;
           NONE mutez;
           PAIR;
           SENDER;
           DIP {DUP; CDDDAR};
           MEM;
           DIP{PUSH bool True};
           COMPARE;
           EQ;
           IF {} {PUSH string "Failed assert";FAILWITH};
           SENDER;
           DIP {DUP; CDDDAR};
           GET;
           IF_NONE {PUSH string "Key not found in map"; FAILWITH} {};
           SWAP;
           SET_CAR;
           DUP;
           CAR;
           DIP {DUP; CDADAR};
           COMPARE;
           GE;
           IF {} {PUSH string "Failed assert";FAILWITH};
           SENDER;
           DIP {DUP; CAR; DIP {DUP; CDADAR}; SUB; SOME};
           DIIP {DUP; CDDDAR};
           UPDATE;
           SWAP;
           SET_CDDDAR;
           PUSH mutez 0;
           SWAP;
           SET_CAR;
           DUP;
           CDAAR;
           DIP {DUP; CDDDAR};
           MEM;
           DIP{PUSH bool True};
           COMPARE;
           EQ;
           IF {DUP; CDAAR; DIP{DUP; CDDDAR}; GET; IF_NONE {PUSH string "Key not found in map";FAILWITH} {}; SWAP; SET_CAR} {};
           DUP;
           CDAAR;
           DIP {DUP; CAR; DIP {DUP; CDADAR}; ADD; SOME};
           DIIP {DUP; CDDDAR};
           UPDATE;
           SWAP;
           SET_CDDDAR;
           DUP;
           CDADDR;
           IF_NONE {PUSH bool False} {DROP; PUSH bool True};
           DIP {PUSH bool True};
           COMPARE;
           EQ;
           IF { DUP;
                CDADDR;
                IF_NONE {PUSH string "Optional value is empty"; FAILWITH} {};
                DIP {DUP; CDADAR};
                SWAP;
                PAIR;
                DIP {SENDER};
                SWAP;
                PAIR;
                DIP {PUSH mutez 0};
                DIIP{ DUP; CDAAR; CONTRACT (pair address (pair mutez bytes)); IF_NONE{ PUSH string "Invalid contract"; FAILWITH} {}};
                TRANSFER_TOKENS;
                DIP {DUP; CDDAR};
                CONS;
                SWAP;
                SET_CDDAR
              }
              {};
           CDDR
           }
           {DROP;PUSH nat 400;FAILWITH}
    } 
-}
