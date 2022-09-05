---
sidebar_position:  10
---

# 2.4  Numeric Literals

There are two kinds of [numeric_literal](./AA-2.4#S0006)s, real literals and integer literals. A real literal is a [numeric_literal](./AA-2.4#S0006) that includes a point; an integer literal is a [numeric_literal](./AA-2.4#S0006) without a point. 


#### Syntax

numeric_literal<a id="S0006"></a> ::= [decimal_literal](./AA-2.4#S0007) | [based_literal](./AA-2.4#S0011)

NOTE 1   The type of an integer literal is universal_integer. The type of a real literal is universal_real. 


## 2.4.1  Decimal Literals

A [decimal_literal](./AA-2.4#S0007) is a [numeric_literal](./AA-2.4#S0006) in the conventional decimal notation (that is, the base is ten). 


#### Syntax

decimal_literal<a id="S0007"></a> ::= [numeral](./AA-2.4#S0008) [.[numeral](./AA-2.4#S0008)] [[exponent](./AA-2.4#S0009)]

numeral<a id="S0008"></a> ::= [digit](./AA-2.4#S0010) {[underline] [digit](./AA-2.4#S0010)}

exponent<a id="S0009"></a> ::= E [+] [numeral](./AA-2.4#S0008) | E  [numeral](./AA-2.4#S0008)

{AI95-00285-01} digit<a id="S0010"></a> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

An [exponent](./AA-2.4#S0009) for an integer literal shall not have a minus sign. 

Ramification: Although this rule is in this subclause, it applies also to the next subclause. 


#### Static Semantics

An underline character in a [numeric_literal](./AA-2.4#S0006) does not affect its meaning. The letter E of an [exponent](./AA-2.4#S0009) can be written either in lower case or in upper case, with the same meaning. 

Ramification: Although these rules are in this subclause, they apply also to the next subclause. 

An [exponent](./AA-2.4#S0009) indicates the power of ten by which the value of the [decimal_literal](./AA-2.4#S0007) without the [exponent](./AA-2.4#S0009) is to be multiplied to obtain the value of the [decimal_literal](./AA-2.4#S0007) with the [exponent](./AA-2.4#S0009). 


#### Examples

Examples of decimal literals: 

```ada
12        0      1E6    123_456    --  integer literals

12.0      0.0    0.456  3.14159_26 --  real literals

```


#### Wording Changes from Ada 83

We have changed the syntactic category name integer to be [numeral](./AA-2.4#S0008). We got this idea from ACID. It avoids the confusion between this and integers. (Other places don't offer similar confusions. For example, a [string_literal](./AA-2.6#S0016) is different from a string.) 


## 2.4.2  Based Literals

[ A [based_literal](./AA-2.4#S0011) is a [numeric_literal](./AA-2.4#S0006) expressed in a form that specifies the base explicitly.] 


#### Syntax

based_literal<a id="S0011"></a> ::= 
   [base](./AA-2.4#S0012) # [based_numeral](./AA-2.4#S0013) [.[based_numeral](./AA-2.4#S0013)] # [[exponent](./AA-2.4#S0009)]

base<a id="S0012"></a> ::= [numeral](./AA-2.4#S0008)

based_numeral<a id="S0013"></a> ::= 
   [extended_digit](./AA-2.4#S0014) {[underline] [extended_digit](./AA-2.4#S0014)}

extended_digit<a id="S0014"></a> ::= [digit](./AA-2.4#S0010) | A | B | C | D | E | F


#### Legality Rules

The base (the numeric value of the decimal [numeral](./AA-2.4#S0008) preceding the first #) shall be at least two and at most sixteen. The [extended_digit](./AA-2.4#S0014)s A through F represent the digits ten through fifteen, respectively. The value of each [extended_digit](./AA-2.4#S0014) of a [based_literal](./AA-2.4#S0011) shall be less than the base. 


#### Static Semantics

The conventional meaning of based notation is assumed. An [exponent](./AA-2.4#S0009) indicates the power of the base by which the value of the [based_literal](./AA-2.4#S0011) without the [exponent](./AA-2.4#S0009) is to be multiplied to obtain the value of the [based_literal](./AA-2.4#S0011) with the [exponent](./AA-2.4#S0009). The [base](./AA-2.4#S0012) and the [exponent](./AA-2.4#S0009), if any, are in decimal notation.

The [extended_digit](./AA-2.4#S0014)s A through F can be written either in lower case or in upper case, with the same meaning. 


#### Examples

Examples of based literals: 

```ada
2#1111_1111#  16#FF#       016#0ff#   --  integer literals of value 255
16#E#E1       2#1110_0000#            --  integer literals of value 224
16#F.FF#E+2   2#1.1111_1111_1110#E11  --  real literals of value 4095.0

```


#### Wording Changes from Ada 83

The rule about which letters are allowed is now encoded in BNF, as suggested by Mike Woodger. This is clearly more readable. 

