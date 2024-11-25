"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2602],{8345:(e,s,r)=>{r.r(s),r.d(s,{assets:()=>A,contentTitle:()=>x,default:()=>p,frontMatter:()=>l,metadata:()=>n,toc:()=>j});const n=JSON.parse('{"id":"arm/AA-11/AA-11.3","title":"11.3 Raise Statements and Raise Expressions","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-11/AA-11.3.mdx","sourceDirName":"arm/AA-11","slug":"/arm/AA-11/AA-11.3","permalink":"/docs/arm/AA-11/AA-11.3","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":91,"frontMatter":{"sidebar_position":91},"sidebar":"referenceManualSidebar","previous":{"title":"11.2 Exception Handlers","permalink":"/docs/arm/AA-11/AA-11.2"},"next":{"title":"11.4 Exception Handling","permalink":"/docs/arm/AA-11/AA-11.4"}}');var i=r(74848),a=r(28453),d=r(13842),c=r(91435),o=r(21432),t=r(79162),h=r(34421);const l={sidebar_position:91},x="11.3 Raise Statements and Raise Expressions",A={},j=[{value:"Syntax",id:"syntax",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4}];function m(e){const s={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,a.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(s.header,{children:(0,i.jsx)(s.h1,{id:"113-raise-statements-and-raise-expressions",children:"11.3 Raise Statements and Raise Expressions"})}),"\n",(0,i.jsx)(s.admonition,{type:"danger",children:(0,i.jsxs)(s.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,i.jsx)(s.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,i.jsx)(t.A,{children:"1"}),"\n",(0,i.jsxs)("p",{children:["[A ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0308",children:"raise_statement"})})," raises an exception.] ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(s.h4,{id:"syntax",children:"Syntax"}),"\n",(0,i.jsx)(t.A,{children:"2/2"}),"\n",(0,i.jsx)(h.A,{items:["AI95-00361-01"]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsxs)(s.p,{children:[(0,i.jsxs)("code",{children:["raise","_","statement"]}),(0,i.jsx)("a",{id:"S0308"}),(0,i.jsx)("code",{children:" ::= "}),(0,i.jsx)("strong",{children:"raise"}),";",(0,i.jsx)("br",{}),"      | ",(0,i.jsx)("strong",{children:"raise"})," ",(0,i.jsxs)("em",{children:["exception","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," [",(0,i.jsx)("strong",{children:"with"})," ",(0,i.jsxs)("em",{children:["string","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),"];",(0,i.jsx)("br",{})]})}),"\n",(0,i.jsx)(t.A,{children:"2.1/4"}),"\n",(0,i.jsx)(h.A,{items:["AI12-0022-1","AI12-0152-1"]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsxs)(s.p,{children:[(0,i.jsxs)("code",{children:["raise","_","expression"]}),(0,i.jsx)("a",{id:"S0309"}),(0,i.jsx)("code",{children:" ::= "}),(0,i.jsx)("strong",{children:"raise"})," ",(0,i.jsxs)("em",{children:["exception","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," [",(0,i.jsx)("strong",{children:"with"})," ",(0,i.jsxs)("em",{children:["string","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})}),"]",(0,i.jsx)("br",{})]})}),"\n",(0,i.jsx)(t.A,{children:"2.2/4"}),"\n",(0,i.jsx)(h.A,{items:["AI12-0152-1"]}),"\n",(0,i.jsxs)("p",{class:"Indented2",children:["If a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," appears within the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," of one of the following contexts, the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," shall appear within a pair of parentheses within the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),": ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(t.A,{children:"2.3/4"}),"\n",(0,i.jsxs)("ul",{children:[(0,i.jsxs)("li",{children:[(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.3#S0032",children:"object_declaration"})}),";",(0,i.jsx)("br",{})]}),(0,i.jsx)(t.A,{children:"2.4/4"}),(0,i.jsxs)("li",{children:[(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0043",children:"modular_type_definition"})}),";",(0,i.jsx)("br",{})]}),(0,i.jsx)(t.A,{children:"2.5/4"}),(0,i.jsxs)("li",{children:[(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0045",children:"floating_point_definition"})}),";",(0,i.jsx)("br",{})]}),(0,i.jsx)(t.A,{children:"2.6/4"}),(0,i.jsxs)("li",{children:[(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0048",children:"ordinary_fixed_point_definition"})}),";",(0,i.jsx)("br",{})]}),(0,i.jsx)(t.A,{children:"2.7/4"}),(0,i.jsxs)("li",{children:[(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0049",children:"decimal_fixed_point_definition"})}),";",(0,i.jsx)("br",{})]}),(0,i.jsx)(t.A,{children:"2.8/4"}),(0,i.jsxs)("li",{children:[(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.7#S0063",children:"default_expression"})}),";",(0,i.jsx)("br",{})]}),(0,i.jsx)(t.A,{children:"2.9/4"}),(0,i.jsxs)("li",{children:[(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0112",children:"ancestor_part"})}),". ",(0,i.jsx)("br",{})]})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.1/4"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"reason",children:(0,i.jsxs)(s.p,{children:[(0,i.jsx)("strong",{}),'Unlike conditional expressions, this doesn\'t say "immediately surrounded"; the only requirement is that it is somehow within a pair of parentheses that is part of the ',(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),". We need this restriction in order that ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})}),"s cannot be syntactically confused with immediately following constructs (such as ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})}),"s). ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.2/4"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"discussion",children:(0,i.jsxs)(s.p,{children:[(0,i.jsx)("strong",{}),"We only need to require that a right parenthesis appear somewhere between the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," and the surrounding context; that's all we need to specify in order to eliminate the ambiguities. Moreover, we don't care at all where the left parenthesis is (so long as it is legal, of course).",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.3/4"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:["For instance, the following is illegal by this rule: ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.4/4"}),(0,i.jsx)(o.A,{language:"ada",children:(0,i.jsxs)(s.p,{children:["Obj : Boolean := Func","_","Call or else raise TBD","_","Error with Atomic;","\n"]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.5/4"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:['as the "',(0,i.jsx)("strong",{children:"with"}),' Atomic" could be part of the ',(0,i.jsxs)("strong",{children:["raise","_","expression"]})," or part of the object declaration. Both of the following are legal: ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.6/4"}),(0,i.jsx)(o.A,{language:"ada",children:(0,i.jsxs)(s.p,{children:["Obj : Boolean := Func","_","Call or else (raise TBD","_","Error) with Atomic;","\n","Obj : Boolean := (Func","_","Call or else raise TBD","_","Error) with Atomic;","\n"]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.7/4"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:["and if the ",(0,i.jsx)("strong",{children:"with"})," belongs to the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})}),", then both of the following are legal: ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.8/4"}),(0,i.jsx)(o.A,{language:"ada",children:(0,i.jsxs)(s.p,{children:["Obj : Boolean := Func","_","Call or else (raise TBD","_","Error with Atomic);","\n","Obj : Boolean := (Func","_","Call or else raise TBD","_","Error with Atomic);","\n"]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.9/4"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:["This rule only requires parentheses for ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})}),'s that are part of the "top-level" of an ',(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," in one of the named contexts; the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," is either the entire ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),", or part of a chain of logical operations. In practice, the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," will almost always be last in interesting top-level ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),"s; anything that follows it could never be executed, so that should be rare. Other contexts such as conditional expressions, qualified expressions, aggregates, and even function calls, provide the needed parentheses. All of the following are legal, no additional parens are needed: ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.10/4"}),(0,i.jsx)(o.A,{language:"ada",children:(0,i.jsxs)(s.p,{children:["Pre : Boolean  := (if not Is","_","Valid(Param) then raise Not","_","Valid","_","Error);","\n","A : A","_","Tagged   := (Some","_","Tagged'(raise TBD","_","Error) with Comp =",">"," 'A');","\n","B : Some","_","Array := (1, 2, 3, others =",">"," raise Not","_","Valid","_","Error);","\n","C : Natural    := Func (Val =",">"," raise TBD","_","Error);","\n"]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.11/4"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:["Parentheses that are part of the context of the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," don't count. For instance, the parentheses around the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," are required in the following: ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.12/4"}),(0,i.jsx)(o.A,{language:"ada",children:(0,i.jsxs)(s.p,{children:["D : A","_","Tagged   := ((raise TBD","_","Error) with Comp =",">"," 'A');","\n"]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.13/4"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:["as ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0112",children:"ancestor_part"})})," is one of the contexts that triggers the rule.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.14/4"}),(0,i.jsx)(h.A,{items:["AI12-0152-1"]}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:["This English-language rule could have been implemented instead by adding nonterminals ",(0,i.jsxs)("code",{children:["initial","_","expression"]})," and ",(0,i.jsxs)("code",{children:["initial","_","relation"]}),", which are the same as ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0133",children:"choice_expression"})})," and ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})})," except for the inclusion of membership in ",(0,i.jsxs)("code",{children:["initial","_","relation"]}),". Then, ",(0,i.jsxs)("code",{children:["initial","_","expresion"]})," could be used in place of ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," in all of the contexts noted. We did not do that because of the large amount of change required, both to the grammar and to language rules that refer to the grammar. A complete grammar is given in .",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"2.a.15/4"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:["The use of a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," is illegal in each of ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0043",children:"modular_type_definition"})}),", ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0045",children:"floating_point_definition"})}),", ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0048",children:"ordinary_fixed_point_definition"})}),", and ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0049",children:"decimal_fixed_point_definition"})})," as these uses are required to be static and a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," is never static. We include these in this rule so that Ada text has an unambiguous syntax in these cases. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(s.h4,{id:"legality-rules",children:"Legality Rules"}),"\n",(0,i.jsx)(t.A,{children:"3/4"}),"\n",(0,i.jsx)(h.A,{items:["AI12-0022-1","AI12-0159-1"]}),"\n",(0,i.jsxs)("p",{children:["The ",(0,i.jsxs)("em",{children:["exception","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})}),", if any, of a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0308",children:"raise_statement"})})," or ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," shall denote an exception. A ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0308",children:"raise_statement"})})," with no ",(0,i.jsxs)("em",{children:["exception","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," (that is, a ",(0,i.jsx)("em",{children:"re-raise statement"}),") shall be within a handler, but not within a body enclosed by that handler. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(s.h4,{id:"name-resolution-rules",children:"Name Resolution Rules"}),"\n",(0,i.jsx)(t.A,{children:"3.1/4"}),"\n",(0,i.jsx)(h.A,{items:["AI95-00361-01","AI12-0022-1","AI12-0152-1"]}),"\n",(0,i.jsxs)("p",{children:["The ",(0,i.jsxs)("em",{children:["string","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," or ",(0,i.jsxs)("em",{children:["string","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})}),", if any, of a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0308",children:"raise_statement"})})," or ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," is expected to be of type String.",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(t.A,{children:"3.2/4"}),"\n",(0,i.jsx)(h.A,{items:["AI12-0022-1","AI12-0159-1"]}),"\n",(0,i.jsxs)("p",{children:["The expected type for a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," shall be any single type. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(s.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,i.jsx)(t.A,{children:"4/4"}),"\n",(0,i.jsx)(h.A,{items:["AI95-00361-01","AI12-0022-1","AI12-0152-1"]}),"\n",(0,i.jsxs)("p",{children:["To ",(0,i.jsx)("em",{children:"raise an exception"})," is to raise a new occurrence of that exception[, as explained in ",(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.4",children:"11.4"}),"]. For the execution of a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0308",children:"raise_statement"})})," with an ",(0,i.jsxs)("em",{children:["exception","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})}),", the named exception is raised. Similarly, for the evaluation of a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})}),", the named exception is raised. [In both of these cases, if a ",(0,i.jsxs)("em",{children:["string","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," or ",(0,i.jsxs)("em",{children:["string","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})})," is present, the expression is evaluated and its value is associated with the exception occurrence.] For the execution of a re-raise statement, the exception occurrence that caused transfer of control to the innermost enclosing handler is raised [again]. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"4.a.1/2"}),(0,i.jsx)(h.A,{items:["AI95-00361-01"]}),(0,i.jsx)(d.A,{type:"aarm",aarm:"proof",children:(0,i.jsxs)(s.p,{children:[(0,i.jsx)("strong",{}),"The definition of Exceptions.Exception","_","Message includes a statement that the string is returned (see ",(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.4#Subclause_11.4.1",children:"11.4.1"}),"). We describe the use of the string here so that we don't have an unexplained parameter in this subclause. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"4.a"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"implementation-note",children:(0,i.jsxs)(s.p,{children:[(0,i.jsx)("strong",{}),"For a re-raise statement, the implementation does not create a new Exception","_","Occurrence, but instead propagates the same Exception","_","Occurrence value. This allows the original cause of the exception to be determined. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"4.b/5"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:[(0,i.jsx)("strong",{children:"Term entry: "}),(0,i.jsx)("strong",{children:"raise an exception"})," \u2014 abandon normal program execution so as to draw attention to the fact that the corresponding situation has arisen",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"4.1/4"}),(0,i.jsx)(h.A,{items:["AI12-0062-1","AI12-0152-1","AI12-0159-1"]}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:["NOTE   If the evaluation of a ",(0,i.jsxs)("em",{children:["string","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," or ",(0,i.jsxs)("em",{children:["string","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})})," raises an exception, that exception is propagated instead of the one denoted by the ",(0,i.jsxs)("em",{children:["exception","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," of the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0308",children:"raise_statement"})})," or ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})}),". ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(s.h4,{id:"examples",children:"Examples"}),"\n",(0,i.jsx)(t.A,{children:"5"}),"\n",(0,i.jsxs)("p",{children:[(0,i.jsx)("em",{children:"Examples of raise statements:"})," ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(t.A,{children:"6/2"}),"\n",(0,i.jsx)(h.A,{items:["AI95-00433-01"]}),"\n",(0,i.jsxs)(o.A,{language:"ada",children:[(0,i.jsxs)(s.p,{children:["raise Ada.IO","_","Exceptions.Name","_","Error;   -- see ",(0,i.jsx)("a",{href:"/docs/arm/AA-A/AA-A.13",children:"A.13"}),"\n","raise Queue","_",'Error with "Buffer Full"; -- see ',(0,i.jsx)("a",{href:"/docs/arm/AA-9/AA-9.11",children:"9.11"}),"\n","\n",(0,i.jsx)(t.A,{children:"7"}),"\nraise;                                -- re-raise the current exception","\n","\n",(0,i.jsx)(t.A,{children:"8/5"})]}),(0,i.jsx)(h.A,{items:["AI12-0312-1"]}),(0,i.jsxs)(s.p,{children:["-- For an example of a raise expression, see the Streams Subsystem definitions in ",(0,i.jsx)("a",{href:"/docs/arm/AA-13/AA-13.13#Subclause_13.13.1",children:"13.13.1"}),".","\n"]})]}),"\n",(0,i.jsx)(c.A,{children:(0,i.jsx)(s.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"8.a"}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:["The fact that the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," in a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0308",children:"raise_statement"})})," has to denote an exception is not clear from RM83. Clearly that was the intent, since the italicized part of the syntax rules so indicate, but there was no explicit rule. RM83-1.5(11) doesn't seem to give the italicized parts of the syntax any force. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(c.A,{children:(0,i.jsx)(s.h4,{id:"extensions-to-ada-95",children:"Extensions to Ada 95"})}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"8.b/2"}),(0,i.jsx)(h.A,{items:["AI95-00361-01"]}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:["The syntax of a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0308",children:"raise_statement"})})," is extended to include a string message. This is more convenient than calling Exceptions.Exception","_","Message (",(0,i.jsxs)("em",{children:["exception","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})}),"'Identity, ",(0,i.jsxs)("em",{children:["string","_"]}),(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),"), and should encourage the use of message strings when raising exceptions. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(c.A,{children:(0,i.jsx)(s.h4,{id:"extensions-to-ada-2012",children:"Extensions to Ada 2012"})}),"\n",(0,i.jsxs)(c.A,{children:[(0,i.jsx)(t.A,{children:"8.c/4"}),(0,i.jsx)(h.A,{items:["AI12-0022-1","AI12-0152-1","AI12-0159-1"]}),(0,i.jsx)(d.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(s.p,{children:[(0,i.jsx)("strong",{children:"Corrigendum:"})," The ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," is new. This construct is necessary to allow conversion of existing specifications to use preconditions and predicates without changing the exceptions raised. It is considered important enough to be added to Ada 2012 rather than waiting for Ada 2022. ",(0,i.jsx)("br",{})]})})]})]})}function p(e={}){const{wrapper:s}={...(0,a.R)(),...e.components};return s?(0,i.jsx)(s,{...e,children:(0,i.jsx)(m,{...e})}):m(e)}}}]);