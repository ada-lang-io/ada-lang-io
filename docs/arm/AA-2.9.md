---
sidebar_position:  15
---

# 2.9  Reserved Words


#### Syntax

This paragraph was deleted.

{AI95-00284-02} {AI95-00395-01} {AI05-0091-1} The following are the reserved words. Within a program, some or all of the letters of a reserved word may be in upper case. 

Discussion: Reserved words have special meaning in the syntax. In addition, certain reserved words are used as attribute names.

The syntactic category [identifier](./AA-2.3#S0002) no longer allows reserved words. We have added the few reserved words that are legal explicitly to the syntax for [attribute_reference](./AA-4.1#S0100). Allowing identifier to include reserved words has been a source of confusion for some users, and differs from the way they are treated in the C and Pascal language definitions.

{AI12-0005-1} Reserved words are sometimes called keywords  in informal material. 

abort
abs
abstract
accept
access
aliased
all
and
array
at

begin
body

case
constant

declare
delay
delta
digits
do

else
elsif
end
entry
exception
exit

for
function

generic
goto

if
in
interface
is

limited
loop

mod

new
not
null

of
or
others
out
overriding

package
parallel
pragma
private
procedure
protected

raise
range
record
rem
renames
requeue

return
reverse

select
separate
some
subtype
synchronized

tagged
task
terminate
then
type

until
use

when
while
with

xor

NOTE 1   {AI12-0440-1} The reserved words appear in lower case boldface in this document, except when used in the [designator](./AA-6.1#S0199) of an attribute (see 4.1.4). Lower case boldface is also used for a reserved word in a [string_literal](./AA-2.6#S0016) used as an [operator_symbol](./AA-6.1#S0202). This is merely a convention - programs can be written in whatever typeface is desired and available. 


#### Incompatibilities With Ada 83

The following words are not reserved in Ada 83, but are reserved in Ada 95: abstract, aliased, protected, requeue, tagged, until. 


#### Wording Changes from Ada 83

{AI05-0299-1} The subclause entitled "Allowed Replacements of Characters" has been moved to Annex J, "Obsolescent Features". 


#### Incompatibilities With Ada 95

{AI95-00284-02} The following words are not reserved in Ada 95, but are reserved in Ada 2005: interface, overriding, synchronized. A special allowance is made for pragma Interface (see J.12). Uses of these words as identifiers will need to be changed, but we do not expect them to be common. 


#### Wording Changes from Ada 95

{AI95-00395-01} The definition of upper case equivalence has been modified to allow identifiers using all of the characters of ISO 10646. This change has no effect on the character sequences that are reserved words, but does make some unusual sequences of characters illegal. 


#### Incompatibilities With Ada 2005

{AI05-0091-1} Correction: Removed other_format characters from reserved words in order to be compatible with the latest Unicode recommendations. This change can only affect programs written for original Ada 2005, and there is little reason to put other_format characters into reserved words in the first place, so there should be very few such programs.

{AI05-0176-1} The following word is not reserved in Ada 2005, but is reserved in Ada 2012: some. Uses of this word as an identifier will need to be changed, but we do not expect them to be common. 


#### Incompatibilities With Ada 2012

{AI12-0119-1} The following word is not reserved in Ada 2012, but is reserved in Ada 2022: parallel. Uses of this word as an identifier will need to be changed, but we do not expect them to be common. 

