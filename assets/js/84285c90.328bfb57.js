"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7606],{47311:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>x,contentTitle:()=>m,default:()=>A,frontMatter:()=>h,metadata:()=>t,toc:()=>j});const t=JSON.parse('{"id":"arm/AA-13/AA-13.7","title":"13.7 The Package System","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-13/AA-13.7.mdx","sourceDirName":"arm/AA-13","slug":"/arm/AA-13/AA-13.7","permalink":"/docs/arm/AA-13/AA-13.7","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":111,"frontMatter":{"sidebar_position":111},"sidebar":"referenceManualSidebar","previous":{"title":"13.6 Change of Representation","permalink":"/docs/arm/AA-13/AA-13.6"},"next":{"title":"13.8 Machine Code Insertions","permalink":"/docs/arm/AA-13/AA-13.8"}}');var i=s(74848),r=s(28453),a=s(13842),o=s(91435),d=s(21432),l=s(79162),c=s(34421);const h={sidebar_position:111},m="13.7 The Package System",x={},j=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"13.7.1  The Package System.Storage_Elements",id:"1371--the-package-systemstorage_elements",level:2},{value:"Static Semantics",id:"static-semantics-1",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Implementation Advice",id:"implementation-advice-1",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95-1",level:4},{value:"13.7.2  The Package System.Address_To_Access_Conversions",id:"1372--the-package-systemaddress_to_access_conversions",level:2},{value:"Static Semantics",id:"static-semantics-2",level:4},{value:"Implementation Permissions",id:"implementation-permissions-1",level:4}];function p(e){const n={a:"a",admonition:"admonition",h1:"h1",h2:"h2",h4:"h4",header:"header",p:"p",...(0,r.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.header,{children:(0,i.jsx)(n.h1,{id:"137-the-package-system",children:"13.7 The Package System"})}),"\n",(0,i.jsx)(n.admonition,{type:"danger",children:(0,i.jsxs)(n.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,i.jsx)(n.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,i.jsx)(l.A,{children:"1"}),"\n",(0,i.jsxs)("p",{children:["[For each implementation there is a library package called System which includes the definitions of certain configuration-dependent characteristics.] ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(n.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,i.jsx)(l.A,{children:"2"}),"\n",(0,i.jsxs)("p",{children:["The following language-defined library package exists: ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"2.a/2"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"implementation-defined",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"The contents of the visible part of package System.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(l.A,{children:"3/5"}),"\n",(0,i.jsx)(c.A,{items:["AI95-00362-01","AI12-0414-1"]}),"\n",(0,i.jsxs)(d.A,{language:"ada",children:[(0,i.jsxs)(n.p,{children:["package System","\n","   with Pure is","\n","\n",(0,i.jsx)(l.A,{children:"4"}),"\ntype Name is implementation-defined-enumeration-type;","\n","   System","_","Name : constant Name := implementation-defined;","\n","\n",(0,i.jsx)(l.A,{children:"5"}),"\n-- System-Dependent Named Numbers:","\n","\n",(0,i.jsx)(l.A,{children:"6"}),"\nMin","_","Int               : constant := root","_","integer'First;","\n","   Max","_","Int               : constant := root","_","integer'Last;","\n","\n",(0,i.jsx)(l.A,{children:"7"}),"\nMax","_","Binary","_","Modulus    : constant := implementation-defined;","\n","   Max","_","Nonbinary","_","Modulus : constant := implementation-defined;","\n","\n",(0,i.jsx)(l.A,{children:"8"}),"\nMax","_","Base","_","Digits       : constant := root","_","real'Digits;","\n","   Max","_","Digits            : constant := implementation-defined;","\n","\n",(0,i.jsx)(l.A,{children:"9"}),"\nMax","_","Mantissa          : constant := implementation-defined;","\n","   Fine","_","Delta            : constant := implementation-defined;","\n","\n",(0,i.jsx)(l.A,{children:"10"}),"\nTick                  : constant := implementation-defined;","\n","\n",(0,i.jsx)(l.A,{children:"11"}),"\n-- Storage-related Declarations:","\n","\n",(0,i.jsx)(l.A,{children:"12"}),"\ntype Address is implementation-defined;","\n","   Null","_","Address : constant Address;","\n","\n",(0,i.jsx)(l.A,{children:"13"}),"\nStorage","_","Unit : constant := implementation-defined;","\n","   Word","_","Size    : constant := implementation-defined ","*"," Storage","_","Unit;","\n","   Memory","_","Size  : constant := implementation-defined;","\n","\n",(0,i.jsx)(l.A,{children:"14/3"})]}),(0,i.jsx)(c.A,{items:["AI05-0229-1"]}),(0,i.jsxs)(n.p,{children:["-- Address Comparison:","\n",'   function "',"<",'" (Left, Right : Address) return Boolean',"\n","      with Convention =",">"," Intrinsic;","\n",'   function "',"<",'="(Left, Right : Address) return Boolean',"\n","      with Convention =",">"," Intrinsic;","\n",'   function "',">",'" (Left, Right : Address) return Boolean',"\n","      with Convention =",">"," Intrinsic;","\n",'   function "',">",'="(Left, Right : Address) return Boolean',"\n","      with Convention =",">"," Intrinsic;","\n",'   function "=" (Left, Right : Address) return Boolean',"\n","      with Convention =",">"," Intrinsic;","\n",'-- function "/=" (Left, Right : Address) return Boolean;',"\n",'   -- "/=" is implicitly defined',"\n","\n",(0,i.jsx)(l.A,{children:"15/2"})]}),(0,i.jsx)(c.A,{items:["AI95-00221-01"]}),(0,i.jsxs)(n.p,{children:["-- Other System-Dependent Declarations:","\n","   type Bit","_","Order is (High","_","Order","_","First, Low","_","Order","_","First);","\n","   Default","_","Bit","_","Order : constant Bit","_","Order := implementation-defined;","\n","\n",(0,i.jsx)(l.A,{children:"16"}),"\n-- Priority-related declarations (see ",(0,i.jsx)("a",{href:"/docs/arm/AA-D/AA-D.1",children:"D.1"}),"):","\n","   subtype Any","_","Priority is Integer range implementation-defined;","\n","   subtype Priority is Any","_","Priority range Any","_","Priority'First ..","\n","             implementation-defined;","\n","   subtype Interrupt","_","Priority is Any","_","Priority range Priority'Last+1 ..","\n","             Any","_","Priority'Last;","\n","\n",(0,i.jsx)(l.A,{children:"17"}),"\nDefault","_","Priority : constant Priority :=","\n","             (Priority'First + Priority'Last)/2;","\n","\n",(0,i.jsx)(l.A,{children:"18"}),"\nprivate","\n","   ... -- not specified by the language","\n","end System;","\n"]})]}),"\n",(0,i.jsx)(l.A,{children:"19"}),"\n",(0,i.jsxs)("p",{children:["Name is an enumeration subtype. Values of type Name are the names of alternative machine configurations handled by the implementation. System","_","Name represents the current machine configuration.",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(l.A,{children:"20"}),"\n",(0,i.jsxs)("p",{children:["The named numbers Fine","_","Delta and Tick are of the type ",(0,i.jsxs)("em",{children:["universal","_","real"]}),"; the others are of the type ",(0,i.jsxs)("em",{children:["universal","_","integer"]}),".",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(l.A,{children:"21"}),"\n",(0,i.jsxs)("p",{children:["The meanings of the named numbers are: ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(l.A,{children:"22"}),"\n",(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"[ Min","_","Int "]}),"\n",(0,i.jsxs)("dl",{children:[(0,i.jsxs)("dd",{children:["The smallest (most negative) value allowed for the expressions of a ",(0,i.jsx)("code",{children:(0,i.jsxs)("a",{href:"/docs/arm/AA-3/AA-3.5#S0042",children:["signed","_","integer","_","type","_","definition"]})}),".",(0,i.jsx)("br",{})]}),(0,i.jsx)(l.A,{children:"23"}),(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Max","_","Int "]}),(0,i.jsxs)("dd",{children:["The largest (most positive) value allowed for the expressions of a ",(0,i.jsx)("code",{children:(0,i.jsxs)("a",{href:"/docs/arm/AA-3/AA-3.5#S0042",children:["signed","_","integer","_","type","_","definition"]})}),".",(0,i.jsx)("br",{})]}),(0,i.jsx)(l.A,{children:"24"}),(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Max","_","Binary","_","Modulus "]}),(0,i.jsxs)("dd",{children:["A power of two such that it, and all lesser positive powers of two, are allowed as the modulus of a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0043",children:"modular_type_definition"})}),".",(0,i.jsx)("br",{})]}),(0,i.jsx)(l.A,{children:"25"}),(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Max","_","Nonbinary","_","Modulus "]}),(0,i.jsxs)("dd",{children:["A value such that it, and all lesser positive integers, are allowed as the modulus of a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0043",children:"modular_type_definition"})}),". ",(0,i.jsx)("br",{})]})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"25.a"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"There is no requirement that Max","_","Nonbinary","_","Modulus be less than or equal to Max","_","Binary","_","Modulus, although that's what makes most sense. On a typical 32-bit machine, for example, Max","_","Binary","_","Modulus will be 2","*","*","32 and Max","_","Nonbinary","_","Modulus will be 2","*","*","31, because supporting nonbinary moduli in above 2","*","*","31 causes implementation difficulties. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(l.A,{children:"26"}),"\n",(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Max","_","Base","_","Digits "]}),"\n",(0,i.jsxs)("dl",{children:[(0,i.jsxs)("dd",{children:["The largest value allowed for the requested decimal precision in a ",(0,i.jsx)("code",{children:(0,i.jsxs)("a",{href:"/docs/arm/AA-3/AA-3.5#S0045",children:["floating","_","point","_","definition"]})}),".",(0,i.jsx)("br",{})]}),(0,i.jsx)(l.A,{children:"27"}),(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Max","_","Digits "]}),(0,i.jsxs)("dd",{children:["The largest value allowed for the requested decimal precision in a ",(0,i.jsx)("code",{children:(0,i.jsxs)("a",{href:"/docs/arm/AA-3/AA-3.5#S0045",children:["floating","_","point","_","definition"]})})," that has no ",(0,i.jsx)("code",{children:(0,i.jsxs)("a",{href:"/docs/arm/AA-3/AA-3.5#S0046",children:["real","_","range","_","specification"]})}),". Max","_","Digits is less than or equal to Max","_","Base","_","Digits.",(0,i.jsx)("br",{})]}),(0,i.jsx)(l.A,{children:"28"}),(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Max","_","Mantissa "]}),(0,i.jsxs)("dd",{children:["The largest possible number of binary digits in the mantissa of machine numbers of a user-defined ordinary fixed point type. (The mantissa is defined in ",(0,i.jsx)("a",{href:"/docs/arm/AA-G/",children:"Annex G"}),".)",(0,i.jsx)("br",{})]}),(0,i.jsx)(l.A,{children:"29"}),(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Fine","_","Delta "]}),(0,i.jsxs)("dd",{children:["The smallest delta allowed in an ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0048",children:"ordinary_fixed_point_definition"})})," that has the ",(0,i.jsx)("code",{children:(0,i.jsxs)("a",{href:"/docs/arm/AA-3/AA-3.5#S0046",children:["real","_","range","_","specification"]})})," ",(0,i.jsx)("strong",{children:"range"})," \u20131.0 .. 1.0. ]",(0,i.jsx)("br",{})]}),(0,i.jsx)(l.A,{children:"30"}),(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Tick "]}),(0,i.jsxs)("dd",{children:["A period in seconds approximating the real time interval during which the value of Calendar.Clock remains constant. ",(0,i.jsx)("br",{})]})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"30.a"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"There is no required relationship between System.Tick and Duration'Small, other than the one described here.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"30.b"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["The inaccuracy of the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-9/AA-9.6#S0266",children:"delay_statement"})})," has no relation to Tick. In particular, it is possible that the clock used for the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-9/AA-9.6#S0266",children:"delay_statement"})})," is less accurate than Calendar.Clock.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"30.c"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["We considered making Tick a run-time-determined quantity, to allow for easier configurability. However, this would not be upward compatible, and the desired configurability can be achieved using functionality defined in ",(0,i.jsx)("a",{href:"/docs/arm/AA-D/",children:"Annex D"}),", \u201c",(0,i.jsx)("a",{href:"/docs/arm/AA-D/",children:"Real-Time Systems"}),"\u201d. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(l.A,{children:"31"}),"\n",(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Storage","_","Unit "]}),"\n",(0,i.jsxs)("dl",{children:[(0,i.jsxs)("dd",{children:["The number of bits per storage element.",(0,i.jsx)("br",{})]}),(0,i.jsx)(l.A,{children:"32"}),(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Word","_","Size "]}),(0,i.jsxs)("dd",{children:["The number of bits per word.",(0,i.jsx)("br",{})]}),(0,i.jsx)(l.A,{children:"33"}),(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Memory","_","Size "]}),(0,i.jsxs)("dd",{children:["An implementation-defined value [that is intended to reflect the memory size of the configuration in storage elements.] ",(0,i.jsx)("br",{})]})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"33.a"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"discussion",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"It is unspecified whether this refers to the size of the address space, the amount of physical memory on the machine, or perhaps some other interpretation of \u201cmemory size\u201d. In any case, the value has to be given by a static expression, even though the amount of memory on many modern machines is a dynamic quantity in several ways. Thus, Memory","_","Size is not very useful. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(l.A,{children:"34/2"}),"\n",(0,i.jsx)(c.A,{items:["AI95-00161-01"]}),"\n",(0,i.jsxs)("p",{children:["Address is a definite, nonlimited type with preelaborable initialization (see ",(0,i.jsx)("a",{href:"/docs/arm/AA-10/AA-10.2#Subclause_10.2.1",children:"10.2.1"}),"). Address represents machine addresses capable of addressing individual storage elements. Null","_","Address is an address that is distinct from the address of any object or program unit. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"34.a"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"The implementation has to ensure that there is at least one address that nothing will be allocated to; Null","_","Address will be one such address. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"34.b"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"Address is the type of the result of the attribute Address. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"34.c"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"reason",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"Address is required to be nonlimited and definite because it is important to be able to assign addresses, and to declare uninitialized address variables. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"34.d/5"}),(0,i.jsx)(c.A,{items:["AI95-00161-01","AI12-0417-1"]}),(0,i.jsx)(a.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"If System.Address is defined as a private type (as suggested below), it might be necessary to add the Preelaborable","_","Initialization aspect to the declaration of Address in order that it have preelaborable initialization as required. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(l.A,{children:"35/2"}),"\n",(0,i.jsx)(c.A,{items:["AI95-00221-01"]}),"\n",(0,i.jsxs)("p",{children:["Default","_","Bit","_","Order shall be a static constant. See ",(0,i.jsx)("a",{href:"/docs/arm/AA-13/AA-13.5#Subclause_13.5.3",children:"13.5.3"})," for an explanation of Bit","_","Order and Default","_","Bit","_","Order. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(n.h4,{id:"implementation-permissions",children:"Implementation Permissions"}),"\n",(0,i.jsx)(l.A,{children:"36/2"}),"\n",(0,i.jsx)(c.A,{items:["AI95-00362-01"]}),"\n",(0,i.jsxs)("p",{children:["An implementation may add additional implementation-defined declarations to package System and its children. [However, it is usually better for the implementation to provide additional functionality via implementation-defined children of System.] ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"36.a"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),'The declarations in package System and its children can be implicit. For example, since Address is not limited, the predefined "=" and "/=" operations are probably sufficient. However, the implementation is not ',(0,i.jsx)("em",{children:"required"}),' to use the predefined "=". ',(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(n.h4,{id:"implementation-advice",children:"Implementation Advice"}),"\n",(0,i.jsx)(l.A,{children:"37"}),"\n",(0,i.jsxs)("p",{children:["Address should be a private type. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"37.a"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"reason",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"This promotes uniformity by avoiding having implementation-defined predefined operations for the type. We don't require it, because implementations may want to stick with what they have. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"37.a.1/2"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"implementation-advice",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"Type System.Address should be a private type.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"37.b"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"implementation-note",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"It is not necessary for Address to be able to point at individual bits within a storage element. Nor is it necessary for it to be able to point at machine registers. It is intended as a memory address that matches the hardware's notion of an address.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"37.c"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["The representation of the ",(0,i.jsx)("strong",{children:"null"})," value of a general access type should be the same as that of Null","_","Address; instantiations of Unchecked","_","Conversion should work accordingly. If the implementation supports interfaces to other languages, the representation of the ",(0,i.jsx)("strong",{children:"null"})," value of a general access type should be the same as in those other languages, if appropriate.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"37.d"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Note that the children of the Interfaces package will generally provide foreign-language-specific null values where appropriate. See UI-0065 regarding Null","_","Address. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"38"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["NOTE   There are also some language-defined child packages of System defined elsewhere. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsx)(n.h4,{id:"extensions-to-ada-83",children:"Extensions to Ada 83"})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"38.a.1/1"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["The declarations Max","_","Binary","_","Modulus, Max","_","Nonbinary","_","Modulus, Max","_","Base","_","Digits, Null","_","Address, Word","_","Size, Bit","_","Order, Default","_","Bit","_","Order, Any","_","Priority, Interrupt","_","Priority, and Default","_","Priority are added to System in Ada 95. The presence of ordering operators for type Address is also guaranteed (the existence of these depends on the definition of Address in an Ada 83 implementation). We do not list these as incompatibilities, as the contents of System can vary between implementations anyway; thus a program that depends on the contents of System (by using ",(0,i.jsx)("strong",{children:"use"})," System; for example) is already at risk of being incompatible when moved between Ada implementations. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsx)(n.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"38.a"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Much of the content of System is standardized, to provide more uniformity across implementations. Implementations can still add their own declarations to System, but are encouraged to do so via children of System.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"38.b"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Some of the named numbers are defined more explicitly in terms of the standard numeric types.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"38.c"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["The pragmas System","_","Name, Storage","_","Unit, and Memory","_","Size are no longer defined by the language. However, the corresponding declarations in package System still exist. Existing implementations may continue to support the three pragmas as implementation-defined pragmas, if they so desire.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"38.d"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Priority semantics, including subtype Priority, have been moved to the Real Time Annex. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsx)(n.h4,{id:"extensions-to-ada-95",children:"Extensions to Ada 95"})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"38.e/2"}),(0,i.jsx)(c.A,{items:["AI95-00161-01"]}),(0,i.jsx)(a.A,{type:"aarm",aarm:"implementation-advice",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{children:"Amendment "})," Type Address is defined to have preelaborable initialization, so that it can be used without restriction in preelaborated units. (If Address is defined to be a private type, as suggested by the , in Ada 95 it cannot be used in some contexts in a preelaborated units. This is an unnecessary portability issue.)",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"38.f/2"}),(0,i.jsx)(c.A,{items:["AI95-00221-01"]}),(0,i.jsx)(a.A,{type:"aarm",aarm:"correction",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{children:"Amendment "})," Default","_","Bit","_","Order is now a static constant.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"38.g/2"}),(0,i.jsx)(c.A,{items:["AI95-00362-01"]}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Package System is now Pure, so it can be portably used in more places. (Ada 95 allowed it to be Pure, but did not require that.) ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)("a",{id:"Subclause_13.7.1"}),"\n",(0,i.jsx)(n.h2,{id:"1371--the-package-systemstorage_elements",children:"13.7.1  The Package System.Storage_Elements"}),"\n",(0,i.jsx)(n.h4,{id:"static-semantics-1",children:"Static Semantics"}),"\n",(0,i.jsx)(l.A,{children:"1_13.7.1"}),"\n",(0,i.jsxs)("p",{children:["The following language-defined library package exists: ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(l.A,{children:"2/5_13.7.1"}),"\n",(0,i.jsx)(c.A,{items:["AI95-00362-01","AI12-0399-1"]}),"\n",(0,i.jsxs)(d.A,{language:"ada",children:[(0,i.jsxs)(n.p,{children:["package System.Storage","_","Elements","\n","   with Pure is","\n","\n",(0,i.jsx)(l.A,{children:"3_13.7.1"}),"\ntype Storage","_","Offset is range implementation-defined;","\n","\n",(0,i.jsx)(l.A,{children:"4_13.7.1"}),"\nsubtype Storage","_","Count is Storage","_","Offset range 0..Storage","_","Offset'Last;","\n","\n",(0,i.jsx)(l.A,{children:"5_13.7.1"}),"\ntype Storage","_","Element is mod implementation-defined;","\n","   for Storage","_","Element'Size use Storage","_","Unit;","\n","   type Storage","_","Array is array","\n","     (Storage","_","Offset range ","<",">",") of aliased Storage","_","Element;","\n","   for Storage","_","Array'Component","_","Size use Storage","_","Unit;","\n","\n",(0,i.jsx)(l.A,{children:"6_13.7.1"}),"\n-- Address Arithmetic:","\n","\n",(0,i.jsx)(l.A,{children:"7/3_13.7.1"})]}),(0,i.jsx)(c.A,{items:["AI05-0229-1"]}),(0,i.jsxs)(n.p,{children:['function "+"(Left : Address; Right : Storage',"_","Offset) return Address","\n","      with Convention =",">"," Intrinsic;","\n",'   function "+"(Left : Storage',"_","Offset; Right : Address) return Address","\n","      with Convention =",">"," Intrinsic;","\n",'   function "-"(Left : Address; Right : Storage',"_","Offset) return Address","\n","      with Convention =",">"," Intrinsic;","\n",'   function "-"(Left, Right : Address) return Storage',"_","Offset","\n","      with Convention =",">"," Intrinsic;","\n","\n",(0,i.jsx)(l.A,{children:"8/3_13.7.1"})]}),(0,i.jsx)(c.A,{items:["AI05-0229-1"]}),(0,i.jsxs)(n.p,{children:['function "mod"(Left : Address; Right : Storage',"_","Offset)","\n","      return Storage","_","Offset","\n","         with Convention =",">"," Intrinsic;","\n","\n",(0,i.jsx)(l.A,{children:"9_13.7.1"}),"\n-- Conversion to/from integers:","\n","\n",(0,i.jsx)(l.A,{children:"10/3_13.7.1"})]}),(0,i.jsx)(c.A,{items:["AI05-0229-1"]}),(0,i.jsxs)(n.p,{children:["type Integer","_","Address is implementation-defined;","\n","   function To","_","Address(Value : Integer","_","Address) return Address","\n","      with Convention =",">"," Intrinsic;","\n","   function To","_","Integer(Value : Address) return Integer","_","Address","\n","      with Convention =",">"," Intrinsic;","\n","\n",(0,i.jsx)(l.A,{children:"11/3_13.7.1"})]}),(0,i.jsx)(c.A,{items:["AI05-0229-1"]}),(0,i.jsxs)(n.p,{children:["end System.Storage","_","Elements;","\n"]})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"11.a/3_13.7.1"}),(0,i.jsx)(c.A,{items:["AI05-0229-1"]}),(0,i.jsx)(a.A,{type:"aarm",aarm:"reason",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"The Convention aspects imply that the attribute Access is not allowed for those operations.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"11.b_13.7.1"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["The ",(0,i.jsx)("strong",{children:"mod"})," function is needed so that the definition of Alignment makes sense. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"11.c/2_13.7.1"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"implementation-defined",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"The range of Storage","_","Elements.Storage","_","Offset, the modulus of Storage","_","Elements.Storage","_","Element, and the declaration of Storage","_","Elements.Integer","_","Address.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(l.A,{children:"12_13.7.1"}),"\n",(0,i.jsxs)("p",{children:["Storage","_","Element represents a storage element. Storage","_","Offset represents an offset in storage elements. Storage","_","Count represents a number of storage elements. Storage","_","Array represents a contiguous sequence of storage elements. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"12.a_13.7.1"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"reason",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"The index subtype of Storage","_","Array is Storage","_","Offset because we wish to allow maximum flexibility. Most Storage","_","Arrays will probably have a lower bound of 0 or 1, but other lower bounds, including negative ones, make sense in some situations.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"12.b/2_13.7.1"}),(0,i.jsx)(c.A,{items:["AI95-00114-01"]}),(0,i.jsxs)(a.A,{type:"aarm",aarm:"note",children:[(0,i.jsx)("em",{children:"This paragraph was deleted."}),(0,i.jsx)("br",{})]})]}),"\n",(0,i.jsx)(l.A,{children:"13_13.7.1"}),"\n",(0,i.jsxs)("p",{children:["Integer","_","Address is a [(signed or modular)] integer subtype. To","_","Address and To","_","Integer convert back and forth between this type and Address. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(n.h4,{id:"implementation-requirements",children:"Implementation Requirements"}),"\n",(0,i.jsx)(l.A,{children:"14_13.7.1"}),"\n",(0,i.jsxs)("p",{children:["Storage","_","Offset'Last shall be greater than or equal to Integer'Last or the largest possible storage offset, whichever is smaller. Storage","_","Offset'First shall be ","<","= (\u2013Storage","_","Offset'Last). ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)("p",{children:[(0,i.jsx)("em",{children:"Paragraph 15 was deleted."})," ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(n.h4,{id:"implementation-advice-1",children:"Implementation Advice"}),"\n",(0,i.jsx)(l.A,{children:"16_13.7.1"}),"\n",(0,i.jsxs)("p",{children:["Operations in System and its children should reflect the target environment semantics as closely as is reasonable. For example, on most machines, it makes sense for address arithmetic to \u201cwrap around\u201d. Operations that do not make sense should raise Program","_","Error. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"16.a.1/2_13.7.1"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"implementation-advice",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"Operations in System and its children should reflect the target environment; operations that do not make sense should raise Program","_","Error.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"16.a_13.7.1"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"discussion",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"For example, on a segmented architecture, X ","<"," Y might raise Program","_","Error if X and Y do not point at the same segment (assuming segments are unordered). Similarly, on a segmented architecture, the conversions between Integer","_","Address and Address might not make sense for some values, and so might raise Program","_","Error. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"16.b_13.7.1"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"reason",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"We considered making Storage","_","Element a private type. However, it is better to declare it as a modular type in the visible part, since code that uses it is already low level, and might as well have access to the underlying representation. We also considered allowing Storage","_","Element to be any integer type, signed integer or modular, but it is better to have uniformity across implementations in this regard, and viewing storage elements as unsigned seemed to make the most sense. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"16.c_13.7.1"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"implementation-note",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"To","_","Address is intended for use in Address clauses. Implementations should overload To","_","Address if appropriate. For example, on a segmented architecture, it might make sense to have a record type representing a segment/offset pair, and have a To","_","Address conversion that converts from that record type to type Address. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsx)(n.h4,{id:"extensions-to-ada-95-1",children:"Extensions to Ada 95"})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"16.d/2_13.7.1"}),(0,i.jsx)(c.A,{items:["AI95-00362-01"]}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Package System.Storage","_","Elements is now Pure, so it can be portably used in more places. (Ada 95 allowed it to be Pure, but did not require that.) ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)("a",{id:"Subclause_13.7.2"}),"\n",(0,i.jsx)(n.h2,{id:"1372--the-package-systemaddress_to_access_conversions",children:"13.7.2  The Package System.Address_To_Access_Conversions"}),"\n",(0,i.jsx)(n.h4,{id:"static-semantics-2",children:"Static Semantics"}),"\n",(0,i.jsx)(l.A,{children:"1_13.7.2"}),"\n",(0,i.jsxs)("p",{children:["The following language-defined generic library package exists: ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(l.A,{children:"2/5_13.7.2"}),"\n",(0,i.jsx)(c.A,{items:["AI12-0241-1","AI12-0302-1"]}),"\n",(0,i.jsxs)(d.A,{language:"ada",children:[(0,i.jsxs)(n.p,{children:["generic","\n","   type Object(","<",">",") is limited private;","\n","package System.Address","_","To","_","Access","_","Conversions ","\n","   with Preelaborate, Nonblocking, Global =",">"," in out synchronized is","\n","\n",(0,i.jsx)(l.A,{children:"3/3_13.7.2"})]}),(0,i.jsx)(c.A,{items:["AI05-0229-1"]}),(0,i.jsxs)(n.p,{children:["type Object","_","Pointer is access all Object;","\n","   function To","_","Pointer(Value : Address) return Object","_","Pointer","\n","      with Convention =",">"," Intrinsic;","\n","   function To","_","Address(Value : Object","_","Pointer) return Address","\n","      with Convention =",">"," Intrinsic;","\n","\n",(0,i.jsx)(l.A,{children:"4/3_13.7.2"})]}),(0,i.jsx)(c.A,{items:["AI05-0229-1"]}),(0,i.jsxs)(n.p,{children:["end System.Address","_","To","_","Access","_","Conversions;","\n"]})]}),"\n",(0,i.jsx)(l.A,{children:"5/2_13.7.2"}),"\n",(0,i.jsx)(c.A,{items:["AI95-00230-01"]}),"\n",(0,i.jsxs)("p",{children:["The To","_","Pointer and To","_","Address subprograms convert back and forth between values of types Object","_","Pointer and Address. To","_","Pointer(X'Address) is equal to X'Unchecked","_","Access for any X that allows Unchecked","_","Access. To","_","Pointer(Null","_","Address) returns ",(0,i.jsx)("strong",{children:"null"}),". For other addresses, the behavior is unspecified. To","_","Address(",(0,i.jsx)("strong",{children:"null"}),") returns Null","_","Address. To","_","Address(Y), where Y /= ",(0,i.jsx)("strong",{children:"null"}),", returns Y.",(0,i.jsx)("strong",{children:"all"}),"'Address. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"5.a/3_13.7.2"}),(0,i.jsx)(c.A,{items:["AI95-00114-01","AI05-0005-1"]}),(0,i.jsx)(a.A,{type:"aarm",aarm:"discussion",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"The programmer should ensure that the address passed to To","_","Pointer is either Null","_","Address, or the address of an object of type Object. (If Object is not a by-reference type, the object ought to be aliased; recall that the Address attribute is not required to provide a useful result for other objects.) Otherwise, the behavior of the program is unspecified; it might raise an exception or crash, for example. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"5.b_13.7.2"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"reason",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"Unspecified is almost the same thing as erroneous; they both allow arbitrarily bad behavior. We don't say erroneous here, because the implementation might allow the address passed to To","_","Pointer to point at some memory that just happens to \u201clook like\u201d an object of type Object. That's not necessarily an error; it's just not portable. However, if the actual type passed to Object is (for example) an array type, the programmer would need to be aware of any dope that the implementation expects to exist, when passing an address that did not come from the Address attribute of an object of type Object.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"5.c_13.7.2"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["One might wonder why To","_","Pointer and To","_","Address are any better than unchecked conversions. The answer is that Address does not necessarily have the same representation as an access type. For example, an access value might point at the bounds of an array when an address would point at the first element. Or an access value might be an offset in words from someplace, whereas an address might be an offset in bytes from the beginning of memory. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(n.h4,{id:"implementation-permissions-1",children:"Implementation Permissions"}),"\n",(0,i.jsx)(l.A,{children:"6_13.7.2"}),"\n",(0,i.jsxs)("p",{children:["An implementation may place restrictions on instantiations of Address","_","To","_","Access","_","Conversions. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"6.a_13.7.2"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"For example, if the hardware requires aligned loads and stores, then dereferencing an access value that is not properly aligned might raise an exception.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(l.A,{children:"6.b_13.7.2"}),(0,i.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["For another example, if the implementation has chosen to use negative component offsets (from an access value), it might not be possible to preserve the semantics, since negative offsets from the Address are not allowed. (The Address attribute always points at \u201cthe first of the storage elements...\u201d.) Note that while the implementation knows how to convert an access value into an address, it might not be able to do the reverse. To avoid generic contract model violations, the restriction might have to be detected at run time in some cases. ",(0,i.jsx)("br",{})]})})]})]})}function A(e={}){const{wrapper:n}={...(0,r.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(p,{...e})}):p(e)}}}]);