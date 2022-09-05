---
sidebar_position:  195
---

# J.5  ASCII


#### Static Semantics

The following declaration exists in the declaration of package Standard: 

```ada
package ASCII is

```

```ada
  --  Control characters:

```

```ada
  NUL   : constant Character := nul; 	SOH   : constant Character := soh;
  STX   : constant Character := stx; 	ETX   : constant Character := etx;
  EOT   : constant Character := eot; 	ENQ   : constant Character := enq;
  ACK   : constant Character := ack; 	BEL   : constant Character := bel;
  BS    : constant Character := bs; 	HT    : constant Character := ht;
  LF    : constant Character := lf; 	VT    : constant Character := vt;
  FF    : constant Character := ff; 	CR    : constant Character := cr;
  SO    : constant Character := so; 	SI    : constant Character := si;
  DLE   : constant Character := dle; 	DC1   : constant Character := dc1;
  DC2   : constant Character := dc2; 	DC3   : constant Character := dc3;
  DC4   : constant Character := dc4; 	NAK   : constant Character := nak;
  SYN   : constant Character := syn; 	ETB   : constant Character := etb;
  CAN   : constant Character := can; 	EM    : constant Character := em;
  SUB   : constant Character := sub; 	ESC   : constant Character := esc;
  FS    : constant Character := fs; 	GS    : constant Character := gs;
  RS    : constant Character := rs; 	US    : constant Character := us;
  DEL   : constant Character := del;

```

```ada
  -- Other characters:

```

```ada
  Exclam   : constant Character:= '!';	Quotation : constant Character:= '"';
  Sharp    : constant Character:= '#';	Dollar    : constant Character:= '$';
  Percent  : constant Character:= '%';	Ampersand : constant Character:= '&';
  Colon    : constant Character:= ':';	Semicolon : constant Character:= ';';
  Query    : constant Character:= '?';	At_Sign   : constant Character:= '@';
  L_Bracket: constant Character:= '[';	Back_Slash: constant Character:= '\';
  R_Bracket: constant Character:= ']';	Circumflex: constant Character:= '^';
  Underline: constant Character:= '_';	Grave     : constant Character:= '`';
  L_Brace  : constant Character:= '{';	Bar       : constant Character:= '|';
  R_Brace  : constant Character:= '}';	Tilde     : constant Character:= '~';

```

```ada
  -- Lower case letters:

```

```ada
  LC_A: constant Character:= 'a';
  ...
  LC_Z: constant Character:= 'z';

```

```ada
end ASCII;

```

