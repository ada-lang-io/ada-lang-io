"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2133],{21371:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>x,contentTitle:()=>A,default:()=>m,frontMatter:()=>h,metadata:()=>a,toc:()=>j});const a=JSON.parse('{"id":"arm/AA-7/AA-7.4","title":"7.4 Deferred Constants","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-7/AA-7.4.mdx","sourceDirName":"arm/AA-7","slug":"/arm/AA-7/AA-7.4","permalink":"/docs/arm/AA-7/AA-7.4","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":63,"frontMatter":{"sidebar_position":63},"sidebar":"referenceManualSidebar","previous":{"title":"7.3 Private Types and Private Extensions","permalink":"/docs/arm/AA-7/AA-7.3"},"next":{"title":"7.5 Limited Types","permalink":"/docs/arm/AA-7/AA-7.5"}}');var r=s(74848),t=s(28453),i=s(13842),d=s(91435),c=s(21432),o=s(79162),l=s(34421);const h={sidebar_position:63},A="7.4 Deferred Constants",x={},j=[{value:"Legality Rules",id:"legality-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}];function f(e){const n={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,t.R)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(n.header,{children:(0,r.jsx)(n.h1,{id:"74-deferred-constants",children:"7.4 Deferred Constants"})}),"\n",(0,r.jsx)(n.admonition,{type:"danger",children:(0,r.jsxs)(n.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,r.jsx)(n.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,r.jsx)(o.A,{children:"1"}),"\n",(0,r.jsxs)("p",{children:["[Deferred constant declarations may be used to declare constants in the visible part of a package, but with the value of the constant given in the private part. They may also be used to declare constants imported from other languages (see ",(0,r.jsx)("a",{href:"/docs/arm/AA-B/",children:"Annex B"}),").] ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(n.h4,{id:"legality-rules",children:"Legality Rules"}),"\n",(0,r.jsx)(o.A,{children:"2/3"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0229-1","AI05-0269-1"]}),"\n",(0,r.jsxs)("p",{children:["[ A ",(0,r.jsx)("em",{children:"deferred constant declaration"})," is an ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.3#S0032",children:"object_declaration"})})," with the reserved word ",(0,r.jsx)("strong",{children:"constant"})," but no initialization expression.] The constant declared by a deferred constant declaration is called a ",(0,r.jsx)("em",{children:"deferred constant"}),". [Unless the Import aspect (see ",(0,r.jsx)("a",{href:"/docs/arm/AA-B/AA-B.1",children:"B.1"}),") is True for a deferred constant declaration, the] deferred constant declaration requires a completion, which shall be a full constant declaration (called the ",(0,r.jsx)("em",{children:"full declaration"})," of the deferred constant). ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"2.a"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"proof",children:(0,r.jsxs)(n.p,{children:[(0,r.jsx)("strong",{}),"The first sentence is redundant, as it is stated officially in ",(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.3#Subclause_3.3.1",children:"3.3.1"}),".",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"2.b/3"}),(0,r.jsx)(l.A,{items:["AI05-0229-1","AI05-0269-1"]}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["The first part of the last sentence is redundant, as no imported entity may have a completion, as stated in ",(0,r.jsx)("a",{href:"/docs/arm/AA-B/AA-B.1",children:"B.1"}),". ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(o.A,{children:"3"}),"\n",(0,r.jsxs)("p",{children:["A deferred constant declaration that is completed by a full constant declaration shall occur immediately within the visible part of a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})}),". For this case, the following additional rules apply to the corresponding full declaration: ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(o.A,{children:"4"}),"\n",(0,r.jsxs)("ul",{children:[(0,r.jsxs)("li",{children:["The full declaration shall occur immediately within the private part of the same package;",(0,r.jsx)("br",{})]}),(0,r.jsx)(o.A,{children:"5/2"}),(0,r.jsx)(l.A,{items:["AI95-00385-01"]}),(0,r.jsxs)("li",{children:["The deferred and full constants shall have the same type, or shall have statically matching anonymous access subtypes; ",(0,r.jsx)("br",{})]})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"5.a/2"}),(0,r.jsx)(l.A,{items:["AI95-00385-01"]}),(0,r.jsx)(i.A,{type:"aarm",aarm:"ramification",children:(0,r.jsxs)(n.p,{children:[(0,r.jsx)("strong",{}),"This implies that both the deferred declaration and the full declaration have to have a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0027",children:"subtype_indication"})})," or ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.10#S0084",children:"access_definition"})})," rather than an ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.6#S0051",children:"array_type_definition"})}),", because each ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.6#S0051",children:"array_type_definition"})})," would define a new type. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(o.A,{children:"6/3"}),"\n",(0,r.jsx)(l.A,{items:["AI95-00385-01","AI05-0062-1","AI05-0262-1"]}),"\n",(0,r.jsxs)("ul",{children:[(0,r.jsxs)("li",{children:["If the deferred constant declaration includes a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0027",children:"subtype_indication"})})," ",(0,r.jsx)("em",{children:"S"})," that defines a constrained subtype, then the constraint defined by the ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0027",children:"subtype_indication"})})," in the full declaration shall match the constraint defined by ",(0,r.jsx)("em",{children:"S"})," statically.[ On the other hand, if the subtype of the deferred constant is unconstrained, then the full declaration is still allowed to impose a constraint. The constant itself will be constrained, like all constants;]",(0,r.jsx)("br",{})]}),(0,r.jsx)(o.A,{children:"7/2"}),(0,r.jsx)(l.A,{items:["AI95-00231-01"]}),(0,r.jsxs)("li",{children:["If the deferred constant declaration includes the reserved word ",(0,r.jsx)("strong",{children:"aliased"}),", then the full declaration shall also; ",(0,r.jsx)("br",{})]})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"7.a"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"ramification",children:(0,r.jsxs)(n.p,{children:[(0,r.jsx)("strong",{}),"On the other hand, the full constant can be aliased even if the deferred constant is not. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(o.A,{children:"7.1/2"}),"\n",(0,r.jsx)(l.A,{items:["AI95-00231-01"]}),"\n",(0,r.jsx)("ul",{children:(0,r.jsxs)("li",{children:["If the subtype of the deferred constant declaration excludes null, the subtype of the full declaration shall also exclude null. ",(0,r.jsx)("br",{})]})}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"7.a.1/2"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"ramification",children:(0,r.jsxs)(n.p,{children:[(0,r.jsx)("strong",{}),"On the other hand, the full constant can exclude null even if the deferred constant does not. But that can only happen for a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0027",children:"subtype_indication"})}),", as anonymous access types are required to statically match (which includes any ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.10#S0083",children:"null_exclusion"})}),"). ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(o.A,{children:"8/5"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0229-1","AI12-0444-1"]}),"\n",(0,r.jsxs)("p",{children:["[A deferred constant declaration for which the Import aspect is True can appear anywhere that an ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.3#S0032",children:"object_declaration"})})," is allowed, and has no full constant declaration.]",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(o.A,{children:"9/2"}),"\n",(0,r.jsx)(l.A,{items:["AI95-00256-01"]}),"\n",(0,r.jsxs)("p",{children:["The completion of a deferred constant declaration shall occur before the constant is frozen (see ",(0,r.jsx)("a",{href:"/docs/arm/AA-13/AA-13.14",children:"13.14"}),").",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(n.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,r.jsx)(o.A,{children:"10/3"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0004-1"]}),"\n",(0,r.jsxs)("p",{children:["The elaboration of a deferred constant declaration elaborates the ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0027",children:"subtype_indication"})}),", ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.10#S0084",children:"access_definition"})}),", or (only allowed in the case of an imported constant) the ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.6#S0051",children:"array_type_definition"})}),". ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"10.a/3"}),(0,r.jsx)(l.A,{items:["AI05-0004-1"]}),(0,r.jsx)(i.A,{type:"aarm",aarm:"ramification",children:(0,r.jsxs)(n.p,{children:[(0,r.jsx)("strong",{}),"For nonimported constants, these elaborations cannot require any code or checks for a legal program, because the given ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0027",children:"subtype_indication"})})," has to be indefinite or statically match that of the full constant, meaning that either it is a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0028",children:"subtype_mark"})})," or it has static constraints. If the deferred constant instead has an ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.10#S0084",children:"access_definition"})}),", the designated subtype must be a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0028",children:"subtype_mark"})}),". We still say that these are elaborated, however, because part of elaboration is creating the type, which is clearly needed for ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.10#S0084",children:"access_definition"})}),"s. (A deferred constant and its full constant have different types when they are specified by an ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.10#S0084",children:"access_definition"})}),", although there is no visible effect of these types being different as neither can be named.) ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"11"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["NOTE   The full constant declaration for a deferred constant that is of a given private type or private extension is not allowed before the corresponding ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0024",children:"full_type_declaration"})}),". This is a consequence of the freezing rules for types (see ",(0,r.jsx)("a",{href:"/docs/arm/AA-13/AA-13.14",children:"13.14"}),"). ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"11.a"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"ramification",children:(0,r.jsxs)(n.p,{children:[(0,r.jsx)("strong",{}),"Multiple or single declarations are allowed for the deferred and the full declarations, provided that the equivalent single declarations would be allowed.",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"11.b"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["Deferred constant declarations are useful for declaring constants of private views, and types with components of private views. They are also useful for declaring access-to-constant objects that designate variables declared in the private part of a package. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(n.h4,{id:"examples",children:"Examples"}),"\n",(0,r.jsx)(o.A,{children:"12"}),"\n",(0,r.jsxs)("p",{children:[(0,r.jsx)("em",{children:"Examples of deferred constant declarations:"})," ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(o.A,{children:"13"}),"\n",(0,r.jsxs)(c.A,{language:"ada",children:[(0,r.jsxs)(n.p,{children:["Null","_","Key : constant Key;      -- see ",(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.3#Subclause_7.3.1",children:"7.3.1"}),"\n","\n",(0,r.jsx)(o.A,{children:"14/3"})]}),(0,r.jsx)(l.A,{items:["AI05-0229-1"]}),(0,r.jsxs)(n.p,{children:["CPU","_","Identifier : constant String(1..8)","\n","   with Import =",">"," True, Convention =",">"," Assembler, Link","_","Name =",">",' "CPU',"_",'ID";',"\n","                              -- see ",(0,r.jsx)("a",{href:"/docs/arm/AA-B/AA-B.1",children:"B.1"}),"\n"]})]}),"\n",(0,r.jsx)(d.A,{children:(0,r.jsx)(n.h4,{id:"extensions-to-ada-83",children:"Extensions to Ada 83"})}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.a"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["In Ada 83, a deferred constant is required to be of a private type declared in the same visible part. This restriction is removed for Ada 95; deferred constants can be of any type.",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.b"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["In Ada 83, a deferred constant declaration was not permitted to include a constraint, nor the reserved word ",(0,r.jsx)("strong",{children:"aliased"}),".",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.c"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["In Ada 83, the rules required conformance of type marks; here we require static matching of subtypes if the deferred constant is constrained.",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.d"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["A deferred constant declaration can be completed with a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})})," Import. Such a deferred constant declaration need not be within a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})}),".",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.e"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["The rules for too-early uses of deferred constants are modified in Ada 95 to allow more cases, and catch all errors at compile time. This change is necessary in order to allow deferred constants of a tagged type without violating the principle that for a dispatching call, there is always an implementation to dispatch to. It has the beneficial side effect of catching some Ada-83-erroneous programs at compile time. The new rule fits in well with the new freezing-point rules. Furthermore, we are trying to convert undefined-value problems into bounded errors, and we were having trouble for the case of deferred constants. Furthermore, uninitialized deferred constants cause trouble for the shared variable / tasking rules, since they are really variable, even though they purport to be constant. In Ada 95, they cannot be touched until they become constant.",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.f"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["Note that we do not consider this change to be an upward incompatibility, because it merely changes an erroneous execution in Ada 83 into a compile-time error.",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.g"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["The Ada 83 semantics are unclear in the case where the full view turns out to be an access type. It is a goal of the language design to prevent uninitialized access objects. One wonders if the implementation is required to initialize the deferred constant to null, and then initialize it (again!) to its real value. In Ada 95, the problem goes away. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(d.A,{children:(0,r.jsx)(n.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.h/3"}),(0,r.jsx)(l.A,{items:["AI05-0299-1"]}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["Since deferred constants can now be of a nonprivate type, we have made this a stand-alone subclause, rather than a subclause of ",(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.3",children:"7.3"}),", \u201c",(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.3",children:"Private Types and Private Extensions"}),"\u201d.",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.i"}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["Deferred constant declarations used to have their own syntax, but now they are simply a special case of ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.3#S0032",children:"object_declaration"})}),"s. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(d.A,{children:(0,r.jsx)(n.h4,{id:"extensions-to-ada-95",children:"Extensions to Ada 95"})}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.j/2"}),(0,r.jsx)(l.A,{items:["AI95-00385-01"]}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["Deferred constants were enhanced to allow the use of anonymous access types in them. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(d.A,{children:(0,r.jsx)(n.h4,{id:"wording-changes-from-ada-95",children:"Wording Changes from Ada 95"})}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.k/2"}),(0,r.jsx)(l.A,{items:["AI95-00231-01"]}),(0,r.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(n.p,{children:["Added matching rules for subtypes that exclude null. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(d.A,{children:(0,r.jsx)(n.h4,{id:"wording-changes-from-ada-2005",children:"Wording Changes from Ada 2005"})}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"14.l/3"}),(0,r.jsx)(l.A,{items:["AI05-0062-1"]}),(0,r.jsx)(i.A,{type:"aarm",aarm:"correction",children:(0,r.jsxs)(n.p,{children:[(0,r.jsx)("strong",{})," Corrected rules so that the intent that a full constant may have a null exclusion even if the deferred constant does not is actually met. ",(0,r.jsx)("br",{})]})})]})]})}function m(e={}){const{wrapper:n}={...(0,t.R)(),...e.components};return n?(0,r.jsx)(n,{...e,children:(0,r.jsx)(f,{...e})}):f(e)}}}]);