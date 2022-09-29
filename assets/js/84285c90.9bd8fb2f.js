"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8816],{5641:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>T,contentTitle:()=>h,default:()=>A,frontMatter:()=>y,metadata:()=>f,toc:()=>x});var a=n(1716),i=n(3050),r=n(8604),o=n(7318),s=n(4768),l=Object.defineProperty,d=Object.defineProperties,m=Object.getOwnPropertyDescriptors,p=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,g=(e,t,n)=>t in e?l(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,k=(e,t)=>{for(var n in t||(t={}))u.call(t,n)&&g(e,n,t[n]);if(p)for(var n of p(t))c.call(t,n)&&g(e,n,t[n]);return e};const y={sidebar_position:111},h="13.7 The Package System",f={unversionedId:"arm/AA-13/AA-13.7",id:"arm/AA-13/AA-13.7",title:"13.7 The Package System",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-13/AA-13.7.mdx",sourceDirName:"arm/AA-13",slug:"/arm/AA-13/AA-13.7",permalink:"/docs/arm/AA-13/AA-13.7",draft:!1,tags:[],version:"current",sidebarPosition:111,frontMatter:{sidebar_position:111},sidebar:"referenceManualSidebar",previous:{title:"13.6 Change of Representation",permalink:"/docs/arm/AA-13/AA-13.6"},next:{title:"13.8 Machine Code Insertions",permalink:"/docs/arm/AA-13/AA-13.8"}},T={},x=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"13.7.1  The Package System.Storage_Elements",id:"1371--the-package-systemstorage_elements",level:2},{value:"Static Semantics",id:"static-semantics-1",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Implementation Advice",id:"implementation-advice-1",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95-1",level:4},{value:"13.7.2  The Package System.Address_To_Access_Conversions",id:"1372--the-package-systemaddress_to_access_conversions",level:2},{value:"Static Semantics",id:"static-semantics-2",level:4},{value:"Implementation Permissions",id:"implementation-permissions-1",level:4}],b={toc:x};function A(e){var t,n=e,{components:l}=n,g=((e,t)=>{var n={};for(var a in e)u.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&p)for(var a of p(e))t.indexOf(a)<0&&c.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=k(k({},b),g),d(t,m({components:l,mdxType:"MDXLayout"}))),(0,a.kt)("h1",k({},{id:"137-the-package-system"}),"13.7 The Package System"),(0,a.kt)("admonition",k({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",k({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"[For each implementation there is a library package called System which includes the definitions of certain configuration-dependent characteristics.] ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"2"),(0,a.kt)("p",null,"The following language-defined library package exists: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"2.a/2"),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The contents of the visible part of package System.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"3/5"),(0,a.kt)(s.Z,{items:["AI95-00362-01","AI12-0414-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"package System ","\n","   with  Pure is ","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"4"),"type Name is implementation-defined-enumeration-type;","\n","   System_Name : constant Name := implementation-defined;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"5"),"-- System-Dependent Named Numbers:","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"6"),"Min_Int               : constant := root_integer'First;","\n","   Max_Int               : constant := root_integer'Last;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"7"),"Max_Binary_Modulus    : constant := implementation-defined;","\n","   Max_Nonbinary_Modulus : constant := implementation-defined;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"8"),"Max_Base_Digits       : constant := root_real'Digits;","\n","   Max_Digits            : constant := implementation-defined;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"9"),"Max_Mantissa          : constant := implementation-defined;","\n","   Fine_Delta            : constant := implementation-defined;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"10"),"Tick                  : constant := implementation-defined;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"11"),"-- Storage-related Declarations:","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"12"),"type Address is implementation-defined;","\n","   Null_Address : constant Address;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"13"),"Storage_Unit : constant := implementation-defined;","\n","   Word_Size    : constant := implementation-defined * Storage_Unit;","\n","   Memory_Size  : constant := implementation-defined;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"14/3"),(0,a.kt)(s.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),"-- Address Comparison:","\n",'   function "',"<",'" (Left, Right : Address) return Boolean',"\n","      with Convention =",">"," Intrinsic;","\n",'   function "',"<",'="(Left, Right : Address) return Boolean',"\n","      with Convention =",">"," Intrinsic;","\n",'   function "',">",'" (Left, Right : Address) return Boolean',"\n","      with Convention =",">"," Intrinsic;","\n",'   function "',">",'="(Left, Right : Address) return Boolean',"\n","      with Convention =",">"," Intrinsic;","\n",'   function "=" (Left, Right : Address) return Boolean',"\n","      with Convention =",">"," Intrinsic;","\n",'-- function "/=" (Left, Right : Address) return Boolean;',"\n",'   -- "/=" is implicitly defined',"\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"15/2"),(0,a.kt)(s.Z,{items:["AI95-00221-01"],mdxType:"MarginInfo"}),"-- Other System-Dependent Declarations:","\n","   type Bit_Order is (High_Order_First, Low_Order_First);","\n","   Default_Bit_Order : constant Bit_Order := implementation-defined;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"16"),"-- Priority-related declarations (see ",(0,a.kt)("a",{href:"../AA-D/AA-D.1"},"D.1"),"):","\n","   subtype Any_Priority is Integer range implementation-defined;","\n","   subtype Priority is Any_Priority range Any_Priority'First ..","\n","             implementation-defined;","\n","   subtype Interrupt_Priority is Any_Priority range Priority'Last+1 ..","\n","             Any_Priority'Last;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"17"),"Default_Priority : constant Priority :=","\n","             (Priority'First + Priority'Last)/2;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"18"),"private","\n","   ... -- not specified by the language","\n","end System;","\n"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19"),(0,a.kt)("p",null,"Name is an enumeration subtype. Values of type Name are the names of alternative machine configurations handled by the implementation. System_Name represents the current machine configuration.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"20"),(0,a.kt)("p",null,"The named numbers Fine_Delta and Tick are of the type ",(0,a.kt)("em",null,"universal_real"),"; the others are of the type ",(0,a.kt)("em",null,"universal_integer"),".",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"21"),(0,a.kt)("p",null,"The meanings of the named numbers are: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"22"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"[ Min_Int "),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"The smallest (most negative) value allowed for the expressions of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0042"},"signed_integer_type_definition")),".",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"23"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"Max_Int "),(0,a.kt)("dd",null,"The largest (most positive) value allowed for the expressions of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0042"},"signed_integer_type_definition")),".",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"24"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"Max_Binary_Modulus "),(0,a.kt)("dd",null,"A power of two such that it, and all lesser positive powers of two, are allowed as the modulus of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0043"},"modular_type_definition")),".",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"25"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"Max_Nonbinary_Modulus "),(0,a.kt)("dd",null,"A value such that it, and all lesser positive integers, are allowed as the modulus of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0043"},"modular_type_definition")),". ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"25.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"There is no requirement that Max_Nonbinary_Modulus be less than or equal to Max_Binary_Modulus, although that's what makes most sense. On a typical 32-bit machine, for example, Max_Binary_Modulus will be 2**32 and Max_Nonbinary_Modulus will be 2**31, because supporting nonbinary moduli in above 2**31 causes implementation difficulties. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"26"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"Max_Base_Digits "),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"The largest value allowed for the requested decimal precision in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0045"},"floating_point_definition")),".",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"27"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"Max_Digits "),(0,a.kt)("dd",null,"The largest value allowed for the requested decimal precision in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0045"},"floating_point_definition"))," that has no ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0046"},"real_range_specification")),". Max_Digits is less than or equal to Max_Base_Digits.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"28"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"Max_Mantissa "),(0,a.kt)("dd",null,"The largest possible number of binary digits in the mantissa of machine numbers of a user-defined ordinary fixed point type. (The mantissa is defined in ",(0,a.kt)("a",{href:"../AA-G/"},"Annex G"),".)",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"29"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"Fine_Delta "),(0,a.kt)("dd",null,"The smallest delta allowed in an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0048"},"ordinary_fixed_point_definition"))," that has the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0046"},"real_range_specification"))," ",(0,a.kt)("strong",null,"range")," \u20131.0 .. 1.0. ]",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"30"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"Tick "),(0,a.kt)("dd",null,"A period in seconds approximating the real time interval during which the value of Calendar.Clock remains constant. ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"30.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"There is no required relationship between System.Tick and Duration'Small, other than the one described here.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"30.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The inaccuracy of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.6#S0266"},"delay_statement"))," has no relation to Tick. In particular, it is possible that the clock used for the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.6#S0266"},"delay_statement"))," is less accurate than Calendar.Clock.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"30.c"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"We considered making Tick a run-time-determined quantity, to allow for easier configurability. However, this would not be upward compatible, and the desired configurability can be achieved using functionality defined in ",(0,a.kt)("a",{href:"../AA-D/"},"Annex D"),", \u201c",(0,a.kt)("a",{href:"../AA-D/"},"Real-Time Systems"),"\u201d. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"31"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"Storage_Unit "),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"The number of bits per storage element.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"32"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"Word_Size "),(0,a.kt)("dd",null,"The number of bits per word.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"33"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"Memory_Size "),(0,a.kt)("dd",null,"An implementation-defined value [that is intended to reflect the memory size of the configuration in storage elements.] ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"33.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"It is unspecified whether this refers to the size of the address space, the amount of physical memory on the machine, or perhaps some other interpretation of \u201cmemory size\u201d. In any case, the value has to be given by a static expression, even though the amount of memory on many modern machines is a dynamic quantity in several ways. Thus, Memory_Size is not very useful. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"34/2"),(0,a.kt)(s.Z,{items:["AI95-00161-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"Address is a definite, nonlimited type with preelaborable initialization (see ",(0,a.kt)("a",{href:"../AA-10/AA-10.2#Subclause_10.2.1"},"10.2.1"),"). Address represents machine addresses capable of addressing individual storage elements. Null_Address is an address that is distinct from the address of any object or program unit. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"34.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The implementation has to ensure that there is at least one address that nothing will be allocated to; Null_Address will be one such address. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"34.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Address is the type of the result of the attribute Address. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"34.c"),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Address is required to be nonlimited and definite because it is important to be able to assign addresses, and to declare uninitialized address variables. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"34.d/5"),(0,a.kt)(s.Z,{items:["AI95-00161-01","AI12-0417-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"If System.Address is defined as a private type (as suggested below), it might be necessary to add the  Preelaborable_Initialization aspect to the declaration  of Address  in order that it  have preelaborable initialization as required. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"35/2"),(0,a.kt)(s.Z,{items:["AI95-00221-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"Default_Bit_Order shall be a static constant. See ",(0,a.kt)("a",{href:"../AA-13/AA-13.5#Subclause_13.5.3"},"13.5.3")," for an explanation of Bit_Order and Default_Bit_Order. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"36/2"),(0,a.kt)(s.Z,{items:["AI95-00362-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"An implementation may add additional implementation-defined declarations to package System and its children. [However, it is usually better for the implementation to provide additional functionality via implementation-defined children of System.] ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"36.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),'The declarations in package System and its children can be implicit. For example, since Address is not limited, the predefined "=" and "/=" operations are probably sufficient. However, the implementation is not ',(0,a.kt)("em",null,"required"),' to use the predefined "=". ',(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"implementation-advice"}),"Implementation Advice"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"37"),(0,a.kt)("p",null,"Address should be a private type. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"37.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"This promotes uniformity by avoiding having implementation-defined predefined operations for the type. We don't require it, because implementations may want to stick with what they have. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"37.a.1/2"),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-advice",title:"Implementation Advice",mdxType:"Admonition"},(0,a.kt)("strong",null),"Type System.Address should be a private type.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"37.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-note",title:"Implementation Note: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"It is not necessary for Address to be able to point at individual bits within a storage element. Nor is it necessary for it to be able to point at machine registers. It is intended as a memory address that matches the hardware's notion of an address.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"37.c"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The representation of the ",(0,a.kt)("strong",null,"null")," value of a general access type should be the same as that of Null_Address; instantiations of Unchecked_Conversion should work accordingly. If the implementation supports interfaces to other languages, the representation of the ",(0,a.kt)("strong",null,"null")," value of a general access type should be the same as in those other languages, if appropriate.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"37.d"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Note that the children of the Interfaces package will generally provide foreign-language-specific null values where appropriate. See UI-0065 regarding Null_Address. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"38"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 1   There are also some language-defined child packages of System defined elsewhere. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"38.a.1/1"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The declarations Max_Binary_Modulus, Max_Nonbinary_Modulus, Max_Base_Digits, Null_Address, Word_Size, Bit_Order, Default_Bit_Order, Any_Priority, Interrupt_Priority, and Default_Priority are added to System in Ada 95. The presence of ordering operators for type Address is also guaranteed (the existence of these depends on the definition of Address in an Ada 83 implementation). We do not list these as incompatibilities, as the contents of System can vary between implementations anyway; thus a program that depends on the contents of System (by using ",(0,a.kt)("strong",null,"use")," System; for example) is already at risk of being incompatible when moved between Ada implementations. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"38.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Much of the content of System is standardized, to provide more uniformity across implementations. Implementations can still add their own declarations to System, but are encouraged to do so via children of System.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"38.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Some of the named numbers are defined more explicitly in terms of the standard numeric types.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"38.c"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The pragmas System_Name, Storage_Unit, and Memory_Size are no longer defined by the language. However, the corresponding declarations in package System still exist. Existing implementations may continue to support the three pragmas as implementation-defined pragmas, if they so desire.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"38.d"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Priority semantics, including subtype Priority, have been moved to the Real Time Annex. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"38.e/2"),(0,a.kt)(s.Z,{items:["AI95-00161-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-advice",title:"Implementation Advice",mdxType:"Admonition"},(0,a.kt)("strong",null,"Amendment ")," Type Address is defined to have preelaborable initialization, so that it can be used without restriction in preelaborated units. (If Address is defined to be a private type, as suggested by the , in Ada 95 it cannot be used in some contexts in a preelaborated units. This is an unnecessary portability issue.)",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"38.f/2"),(0,a.kt)(s.Z,{items:["AI95-00221-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"correction",title:"Correction:",mdxType:"Admonition"},(0,a.kt)("strong",null,"Amendment ")," Default_Bit_Order is now a static constant.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"38.g/2"),(0,a.kt)(s.Z,{items:["AI95-00362-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Package System is now Pure, so it can be portably used in more places. (Ada 95 allowed it to be Pure, but did not require that.) ",(0,a.kt)("br",null)),(0,a.kt)("a",{id:"Subclause_13.7.1"}),(0,a.kt)("h2",k({},{id:"1371--the-package-systemstorage_elements"}),"13.7.1  The Package System.Storage_Elements"),(0,a.kt)("h4",k({},{id:"static-semantics-1"}),"Static Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"The following language-defined library package exists: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"2/5"),(0,a.kt)(s.Z,{items:["AI95-00362-01","AI12-0399-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"package System.Storage_Elements ","\n","   with Pure is ","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"3"),"type Storage_Offset is range implementation-defined;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"4"),"subtype Storage_Count is Storage_Offset range 0..Storage_Offset'Last;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"5"),"type Storage_Element is mod implementation-defined;","\n","   for Storage_Element'Size use Storage_Unit;","\n","   type Storage_Array is array","\n","     (Storage_Offset range ","<",">",") of aliased Storage_Element;","\n","   for Storage_Array'Component_Size use Storage_Unit;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"6"),"-- Address Arithmetic:","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"7/3"),(0,a.kt)(s.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),'function "+"(Left : Address; Right : Storage_Offset) return Address',"\n","      with Convention =",">"," Intrinsic;","\n",'   function "+"(Left : Storage_Offset; Right : Address) return Address',"\n","      with Convention =",">"," Intrinsic;","\n",'   function "-"(Left : Address; Right : Storage_Offset) return Address',"\n","      with Convention =",">"," Intrinsic;","\n",'   function "-"(Left, Right : Address) return Storage_Offset',"\n","      with Convention =",">"," Intrinsic;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"8/3"),(0,a.kt)(s.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),'function "mod"(Left : Address; Right : Storage_Offset)',"\n","      return Storage_Offset","\n","         with Convention =",">"," Intrinsic;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"9"),"-- Conversion to/from integers:","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"10/3"),(0,a.kt)(s.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),"type Integer_Address is implementation-defined;","\n","   function To_Address(Value : Integer_Address) return Address","\n","      with Convention =",">"," Intrinsic;","\n","   function To_Integer(Value : Address) return Integer_Address","\n","      with Convention =",">"," Intrinsic;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"11/3"),(0,a.kt)(s.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),"end System.Storage_Elements;","\n"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"11.a/3"),(0,a.kt)(s.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The Convention aspects imply that the attribute Access is not allowed for those operations.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"11.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The ",(0,a.kt)("strong",null,"mod")," function is needed so that the definition of Alignment makes sense. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"11.c/2"),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The range of Storage_Elements.Storage_Offset, the modulus of Storage_Elements.Storage_Element, and the declaration of Storage_Elements.Integer_Address.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"12"),(0,a.kt)("p",null,"Storage_Element represents a storage element. Storage_Offset represents an offset in storage elements. Storage_Count represents a number of storage elements. Storage_Array represents a contiguous sequence of storage elements. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"12.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The index subtype of Storage_Array is Storage_Offset because we wish to allow maximum flexibility. Most Storage_Arrays will probably have a lower bound of 0 or 1, but other lower bounds, including negative ones, make sense in some situations.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"12.b/2"),(0,a.kt)(s.Z,{items:["AI95-00114-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("em",null,"This paragraph was deleted."),(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"13"),(0,a.kt)("p",null,"Integer_Address is a [(signed or modular)] integer subtype. To_Address and To_Integer convert back and forth between this type and Address. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"implementation-requirements"}),"Implementation Requirements"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14"),(0,a.kt)("p",null,"Storage_Offset'Last shall be greater than or equal to Integer'Last or the largest possible storage offset, whichever is smaller. Storage_Offset'First shall be ","<","= (\u2013Storage_Offset'Last). ",(0,a.kt)("br",null)),(0,a.kt)("p",null,(0,a.kt)("em",null,"Paragraph 15 was deleted.")," ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"implementation-advice-1"}),"Implementation Advice"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"16"),(0,a.kt)("p",null,"Operations in System and its children should reflect the target environment semantics as closely as is reasonable. For example, on most machines, it makes sense for address arithmetic to \u201cwrap around\u201d. Operations that do not make sense should raise Program_Error. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"16.a.1/2"),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-advice",title:"Implementation Advice",mdxType:"Admonition"},(0,a.kt)("strong",null),"Operations in System and its children should reflect the target environment; operations that do not make sense should raise Program_Error.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"16.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"For example, on a segmented architecture, X ","<"," Y might raise Program_Error if X and Y do not point at the same segment (assuming segments are unordered). Similarly, on a segmented architecture, the conversions between Integer_Address and Address might not make sense for some values, and so might raise Program_Error. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"16.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"We considered making Storage_Element a private type. However, it is better to declare it as a modular type in the visible part, since code that uses it is already low level, and might as well have access to the underlying representation. We also considered allowing Storage_Element to be any integer type, signed integer or modular, but it is better to have uniformity across implementations in this regard, and viewing storage elements as unsigned seemed to make the most sense. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"16.c"),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-note",title:"Implementation Note: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"To_Address is intended for use in Address clauses. Implementations should overload To_Address if appropriate. For example, on a segmented architecture, it might make sense to have a record type representing a segment/offset pair, and have a To_Address conversion that converts from that record type to type Address. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"extensions-to-ada-95-1"}),"Extensions to Ada 95"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"16.d/2"),(0,a.kt)(s.Z,{items:["AI95-00362-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Package System.Storage_Elements is now Pure, so it can be portably used in more places. (Ada 95 allowed it to be Pure, but did not require that.) ",(0,a.kt)("br",null)),(0,a.kt)("a",{id:"Subclause_13.7.2"}),(0,a.kt)("h2",k({},{id:"1372--the-package-systemaddress_to_access_conversions"}),"13.7.2  The Package System.Address_To_Access_Conversions"),(0,a.kt)("h4",k({},{id:"static-semantics-2"}),"Static Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"The following language-defined generic library package exists: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"2/5"),(0,a.kt)(s.Z,{items:["AI12-0241-1","AI12-0302-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"generic","\n","   type Object(","<",">",") is limited private;","\n","package System.Address_To_Access_Conversions  ","\n","   with  Preelaborate, Nonblocking, Global =",">"," in out synchronized is ","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"3/3"),(0,a.kt)(s.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),"type Object_Pointer is access all Object;","\n","   function To_Pointer(Value : Address) return Object_Pointer","\n","      with Convention =",">"," Intrinsic;","\n","   function To_Address(Value : Object_Pointer) return Address","\n","      with Convention =",">"," Intrinsic;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"4/3"),(0,a.kt)(s.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),"end System.Address_To_Access_Conversions;","\n"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5/2"),(0,a.kt)(s.Z,{items:["AI95-00230-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"The To_Pointer and To_Address subprograms convert back and forth between values of types Object_Pointer and Address. To_Pointer(X'Address) is equal to X'Unchecked_Access for any X that allows Unchecked_Access. To_Pointer(Null_Address) returns ",(0,a.kt)("strong",null,"null"),". For other addresses, the behavior is unspecified. To_Address(",(0,a.kt)("strong",null,"null"),") returns Null_Address. To_Address(Y), where Y /= ",(0,a.kt)("strong",null,"null"),", returns Y.",(0,a.kt)("strong",null,"all"),"'Address. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5.a/3"),(0,a.kt)(s.Z,{items:["AI95-00114-01","AI05-0005-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The programmer should ensure that the address passed to To_Pointer is either Null_Address, or the address of an object of type Object. (If Object is not a by-reference type, the object ought to be aliased; recall that the Address attribute is not required to provide a useful result for other objects.) Otherwise, the behavior of the program is unspecified; it might raise an exception or crash, for example. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Unspecified is almost the same thing as erroneous; they both allow arbitrarily bad behavior. We don't say erroneous here, because the implementation might allow the address passed to To_Pointer to point at some memory that just happens to \u201clook like\u201d an object of type Object. That's not necessarily an error; it's just not portable. However, if the actual type passed to Object is (for example) an array type, the programmer would need to be aware of any dope that the implementation expects to exist, when passing an address that did not come from the Address attribute of an object of type Object.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5.c"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"One might wonder why To_Pointer and To_Address are any better than unchecked conversions. The answer is that Address does not necessarily have the same representation as an access type. For example, an access value might point at the bounds of an array when an address would point at the first element. Or an access value might be an offset in words from someplace, whereas an address might be an offset in bytes from the beginning of memory. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"implementation-permissions-1"}),"Implementation Permissions"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"6"),(0,a.kt)("p",null,"An implementation may place restrictions on instantiations of Address_To_Access_Conversions. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"6.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"For example, if the hardware requires aligned loads and stores, then dereferencing an access value that is not properly aligned might raise an exception.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"6.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"For another example, if the implementation has chosen to use negative component offsets (from an access value), it might not be possible to preserve the semantics, since negative offsets from the Address are not allowed. (The Address attribute always points at \u201cthe first of the storage elements...\u201d.) Note that while the implementation knows how to convert an access value into an address, it might not be able to do the reverse. To avoid generic contract model violations, the restriction might have to be detected at run time in some cases. ",(0,a.kt)("br",null)))}A.isMDXComponent=!0}}]);