"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[9069],{3176:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>g,contentTitle:()=>h,default:()=>v,frontMatter:()=>m,metadata:()=>f,toc:()=>k});var a=t(1716),i=t(7556),o=Object.defineProperty,r=Object.defineProperties,s=Object.getOwnPropertyDescriptors,l=Object.getOwnPropertySymbols,d=Object.prototype.hasOwnProperty,u=Object.prototype.propertyIsEnumerable,c=(e,n,t)=>n in e?o(e,n,{enumerable:!0,configurable:!0,writable:!0,value:t}):e[n]=t,p=(e,n)=>{for(var t in n||(n={}))d.call(n,t)&&c(e,t,n[t]);if(l)for(var t of l(n))u.call(n,t)&&c(e,t,n[t]);return e};const m={sidebar_position:139},h="B.2  The Package Interfaces",f={unversionedId:"arm/AA-B.2",id:"arm/AA-B.2",title:"B.2  The Package Interfaces",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-B.2.mdx",sourceDirName:"arm",slug:"/arm/AA-B.2",permalink:"/docs/arm/AA-B.2",draft:!1,tags:[],version:"current",sidebarPosition:139,frontMatter:{sidebar_position:139},sidebar:"tutorialSidebar",previous:{title:"B.1  Interfacing Aspects",permalink:"/docs/arm/AA-B.1"},next:{title:"B.3  Interfacing with C and C++",permalink:"/docs/arm/AA-B.3"}},g={},k=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}],b={toc:k};function v(e){var n,t=e,{components:o}=t,c=((e,n)=>{var t={};for(var a in e)d.call(e,a)&&n.indexOf(a)<0&&(t[a]=e[a]);if(null!=e&&l)for(var a of l(e))n.indexOf(a)<0&&u.call(e,a)&&(t[a]=e[a]);return t})(t,["components"]);return(0,a.kt)("wrapper",(n=p(p({},b),c),r(n,s({components:o,mdxType:"MDXLayout"}))),(0,a.kt)("h1",p({},{id:"b2--the-package-interfaces"}),"B.2  The Package Interfaces"),(0,a.kt)("admonition",p({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,a.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("p",null,"Package Interfaces is the parent of several library packages that declare types and other entities useful for interfacing to foreign languages. It also contains some implementation-defined types that are useful across more than one language (in particular for interfacing to assembly language). "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Implementation defined: "),"The contents of the visible part of package Interfaces and its language-defined descendants."),(0,a.kt)("h4",p({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)("p",null,"The library package Interfaces has the following skeletal declaration: "),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"{","AI12-0414-1","}"," package Interfaces","\n","   with Pure is","\n"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"type Integer_n is range -2**(n-1) .. 2**(n-1) - 1;  --2's complement","\n"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"type Unsigned_n is mod 2**n;","\n"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"function Shift_Left  (Value : Unsigned_n; Amount : Natural)","\n","      return Unsigned_n;","\n","   function Shift_Right (Value : Unsigned_n; Amount : Natural)","\n","      return Unsigned_n;","\n","   function Shift_Right_Arithmetic (Value : Unsigned_n; Amount : Natural)","\n","      return Unsigned_n;","\n","   function Rotate_Left  (Value : Unsigned_n; Amount : Natural)","\n","      return Unsigned_n;","\n","   function Rotate_Right (Value : Unsigned_n; Amount : Natural)","\n","      return Unsigned_n;","\n","   ...","\n","end Interfaces;","\n"),(0,a.kt)("h4",p({},{id:"implementation-requirements"}),"Implementation Requirements"),(0,a.kt)("p",null,"An implementation shall provide the following declarations in the visible part of package Interfaces: "),(0,a.kt)("p",null,"Signed and modular integer types of ",(0,a.kt)("em",null,"n")," bits, if supported by the target architecture, for each ",(0,a.kt)("em",null,"n")," that is at least the size of a storage element and that is a factor of the word size. The names of these types are of the form Integer_",(0,a.kt)("em",null,"n")," for the signed types, and Unsigned_",(0,a.kt)("em",null,"n")," for the modular types; "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"For example, for a typical 32-bit machine the corresponding types might be Integer_8, Unsigned_8, Integer_16, Unsigned_16, Integer_32, and Unsigned_32."),(0,a.kt)("p",null,"The wording above implies, for example, that Integer_16'Size = Unsigned_16'Size = 16. Unchecked conversions between same-Sized types will work as expected. "),(0,a.kt)("p",null,"For each such modular type in Interfaces, shifting and rotating subprograms as specified in the declaration of Interfaces above. These subprograms are Intrinsic. They operate on a bit-by-bit basis, using the binary representation of the value of the operands to yield a binary representation for the result. The Amount parameter gives the number of bits by which to shift or rotate. For shifting, zero bits are shifted in, except in the case of Shift_Right_Arithmetic, where one bits are shifted in if Value is at least half the modulus. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Reason: "),'We considered making shifting and rotating be primitive operations of all modular types. However, it is a design principle of Ada that all predefined operations should be operators (not functions named by identifiers). (Note that an early version of Ada had "',(0,a.kt)("strong",null,"abs"),'" as an identifier, but it was changed to a reserved word operator before standardization of Ada 83.) This is important because the implicit declarations would hide nonoverloadable declarations with the same name, whereas operators are always overloadable. Therefore, we would have had to make shift and rotate into reserved words, which would have been upward incompatible, or else invent new operator symbols, which seemed like too much mechanism. '),(0,a.kt)("p",null,(0,a.kt)("strong",null,"To be honest: "),"{",(0,a.kt)("em",null,"AI12-0264-1"),"}",' "Shifting" and "rotating" have the conventional meaning. Neither of these terms is usefully defined by the usual normative references of the Reference Manual, so we provide pseudo-code here to describe the intended semantics of the above wording (all operations in these examples are using modular semantics).'),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"function Rotate_Left (Value : Unsigned_n; Amount : Natural)","\n","   return Unsigned_n is","\n","   Result : Unsigned_n := Value;","\n","   Bit : Unsigned_n range 0 .. 1;","\n","begin","\n","   for Count in 1 .. Amount loop","\n","      Bit := Result/2**(n-1); -- High-bit of Result","\n","      Result := Result*2 + Bit;","\n","   end loop;","\n","   return Result;","\n","end Rotate_Left;","\n"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"function Rotate_Right (Value : Unsigned_n; Amount : Natural)","\n","   return Unsigned_n is","\n","   Result : Unsigned_n := Value;","\n","   Bit : Unsigned_n range 0 .. 1;","\n","begin","\n","   for Count in 1 .. Amount loop","\n","      Bit := Result mod 2; -- Low-bit of Result","\n","      Result := Result/2 + (Bit * 2**(n-1));","\n","   end loop;","\n","   return Result;","\n","end Rotate_Right;","\n"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"function Shift_Left (Value : Unsigned_n; Amount : Natural)","\n","   return Unsigned_n is","\n","   Result : Unsigned_n := Value;","\n","begin","\n","   for Count in 1 .. Amount loop","\n","      Result := Result * 2;","\n","   end loop;","\n","   return Result;","\n","end Shift_Left;","\n"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"function Shift_Right (Value : Unsigned_n; Amount : Natural)","\n","   return Unsigned_n is","\n","   Result : Unsigned_n := Value;","\n","begin","\n","   for Count in 1 .. Amount loop","\n","      Result := Result / 2;","\n","   end loop;","\n","   return Result;","\n","end Shift_Right;","\n"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"function Shift_Right_Arithmetic (Value : Unsigned_n; Amount : Natural)","\n","   return Unsigned_n is","\n","   Result : Unsigned_n := Value;","\n","   Neg : constant Boolean :=","\n","      Result/2**(n-1) = 1; -- High-bit of Result","\n","begin","\n","   for Count in 1 .. Amount loop","\n","      if Neg then","\n","         Result := Result / 2 + 2**(n-1);","\n","      else","\n","         Result := Result / 2;","\n","      end if;","\n","   end loop;","\n","   return Result;","\n","end Shift_Right_Arithmetic;","\n"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0264-1"),"}"," These generally correspond to machine instructions, although there may not be an exact match in terms of boundary conditions, as Ada requires the correct result to be produced for all values of Amount. "),(0,a.kt)("p",null,"Floating point types corresponding to each floating point format fully supported by the hardware. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Implementation Note: "),"The names for these floating point types are not specified. However, if IEEE arithmetic is supported, then the names should be IEEE_Float_32 and IEEE_Float_64 for single and double precision, respectively."),(0,a.kt)("h4",p({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,a.kt)("p",null,"An implementation may provide implementation-defined library units that are children of Interfaces, and may add declarations to the visible part of Interfaces in addition to the ones defined above. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Implementation defined: "),"Implementation-defined children of package Interfaces."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00204-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0229-1"),"}"," A child package of package Interfaces with the name of a convention may be provided independently of whether the convention is supported by the Convention aspect and vice versa. Such a child package should contain any declarations that would be useful for interfacing to the language (implementation) represented by the convention. Any declarations useful for interfacing to any language on the given hardware architecture should be provided directly in Interfaces. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"For example, package Interfaces.XYZ_Pascal might contain declarations of types that match the data types provided by the XYZ implementation of Pascal, so that it will be more convenient to pass parameters to a subprogram whose convention is XYZ_Pascal. "),(0,a.kt)("h4",p({},{id:"implementation-advice"}),"Implementation Advice"),(0,a.kt)("p",null,(0,a.kt)("em",null,"This paragraph was deleted."),"{",(0,a.kt)("em",null,"AI95-00204-01"),"}"," "),(0,a.kt)("p",null,(0,a.kt)("em",null,"This paragraph was deleted.")),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," An implementation supporting an interface to C, COBOL, or Fortran should provide the corresponding package or packages described in the following subclauses. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Implementation Advice: "),'If an interface to C, COBOL, or Fortran is provided, the corresponding package or packages described in Annex B, "Interface to Other Languages" should also be provided.'),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Implementation Note: "),'The intention is that an implementation might support several implementations of the foreign language: Interfaces.This_Fortran and Interfaces.That_Fortran might both exist. The "default" implementation, overridable by the user, should be declared as a renaming: '),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"package Interfaces.Fortran renames Interfaces.This_Fortran;","\n"),(0,a.kt)("h4",p({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00204-01"),"}"," Clarified that interfacing to foreign languages is optional and has the same restrictions as a Specialized Needs Annex. "),(0,a.kt)("h4",p({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0262-1"),"}"," Move the restrictions on implementations of optional features to the start of this Annex. "))}v.isMDXComponent=!0}}]);