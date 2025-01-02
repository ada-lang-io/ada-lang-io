"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2067],{3286:(e,s,n)=>{n.r(s),n.d(s,{assets:()=>A,contentTitle:()=>x,default:()=>f,frontMatter:()=>h,metadata:()=>i,toc:()=>j});const i=JSON.parse('{"id":"arm/AA-6/AA-6.8","title":"6.8 Expression Functions","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-6/AA-6.8.mdx","sourceDirName":"arm/AA-6","slug":"/arm/AA-6/AA-6.8","permalink":"/docs/arm/AA-6/AA-6.8","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":58,"frontMatter":{"sidebar_position":58},"sidebar":"referenceManualSidebar","previous":{"title":"6.7 Null Procedures","permalink":"/docs/arm/AA-6/AA-6.7"},"next":{"title":"7 Packages","permalink":"/docs/arm/AA-7/"}}');var r=n(4848),a=n(8453),c=n(3842),t=n(1435),o=n(1432),d=n(9162),l=n(4421);const h={sidebar_position:58},x="6.8 Expression Functions",A={},j=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}];function p(e){const s={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,a.R)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(s.header,{children:(0,r.jsx)(s.h1,{id:"68-expression-functions",children:"6.8 Expression Functions"})}),"\n",(0,r.jsx)(s.admonition,{type:"danger",children:(0,r.jsxs)(s.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,r.jsx)(s.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,r.jsx)(d.A,{children:"1/3"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0177-1"]}),"\n",(0,r.jsxs)("p",{children:["An ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:"expression_function_declaration"})})," provides a shorthand to declare a function whose body consists of a single return statement. ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(s.h4,{id:"syntax",children:"Syntax"}),"\n",(0,r.jsx)(d.A,{children:"2/4"}),"\n",(0,r.jsx)(l.A,{items:["AI95-0177-1","AI12-0157-1"]}),"\n",(0,r.jsx)(o.A,{children:(0,r.jsxs)(s.p,{children:[(0,r.jsxs)("code",{children:["expression","_","function","_","declaration"]}),(0,r.jsx)("a",{id:"S0228"}),(0,r.jsx)("code",{children:" ::= "}),(0,r.jsx)("br",{}),"   [",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-8/AA-8.3#S0234",children:"overriding_indicator"})}),"]",(0,r.jsx)("br",{}),"   ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0198",children:"function_specification"})})," ",(0,r.jsx)("strong",{children:"is"}),(0,r.jsx)("br",{}),"       (",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),")",(0,r.jsx)("br",{}),"       [",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})}),"];",(0,r.jsx)("br",{})," | [",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-8/AA-8.3#S0234",children:"overriding_indicator"})}),"]",(0,r.jsx)("br",{}),"   ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0198",children:"function_specification"})})," ",(0,r.jsx)("strong",{children:"is"}),(0,r.jsx)("br",{}),"       ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0106",children:"aggregate"})}),(0,r.jsx)("br",{}),"       [",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})}),"];",(0,r.jsx)("br",{})]})}),"\n",(0,r.jsx)(s.h4,{id:"name-resolution-rules",children:"Name Resolution Rules"}),"\n",(0,r.jsx)(d.A,{children:"3/4"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0177-1","AI12-0157-1"]}),"\n",(0,r.jsxs)("p",{children:["The expected type for the ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," or ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0106",children:"aggregate"})})," of an ",(0,r.jsx)("code",{children:(0,r.jsxs)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:["expression","_","function","_","declaration"]})})," is the result type (see ",(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.5",children:"6.5"}),") of the function. ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(s.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,r.jsx)(d.A,{children:"3.1/5"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0177-1","AI05-0264-1","AI12-0075-1","AI12-0157-1","AI12-0408-1"]}),"\n",(0,r.jsxs)("p",{children:["An ",(0,r.jsx)("code",{children:(0,r.jsxs)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:["expression","_","function","_","declaration"]})})," that is not a completion declares an ",(0,r.jsx)("em",{children:"expression function"}),". The ",(0,r.jsx)("em",{children:"return expression of an expression function"})," is the ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," or ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0106",children:"aggregate"})})," of the ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:"expression_function_declaration"})}),". A completion is not allowed for an ",(0,r.jsx)("code",{children:(0,r.jsxs)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:["expression","_","function","_","declaration"]})}),"; however, an ",(0,r.jsx)("code",{children:(0,r.jsxs)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:["expression","_","function","_","declaration"]})})," can complete a previous declaration.",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(d.A,{children:"3.2/5"}),"\n",(0,r.jsx)(l.A,{items:["AI12-0075-1"]}),"\n",(0,r.jsxs)("p",{children:["A ",(0,r.jsx)("em",{children:"potentially static expression"})," is defined in the same way as a static expression except that",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(d.A,{children:"3.3/5"}),"\n",(0,r.jsxs)("ul",{children:[(0,r.jsxs)("li",{children:["a name denoting a formal parameter of an expression function is a potentially static expression; and",(0,r.jsx)("br",{})]}),(0,r.jsx)(d.A,{children:"3.4/5"}),(0,r.jsxs)("li",{children:["each use of \u201cstatic expression\u201d in the definition of \u201cstatic expression\u201d is replaced with a corresponding use of \u201cpotentially static expression\u201d in the definition of \u201cpotentially static expression\u201d. ",(0,r.jsx)("br",{})]})]}),"\n",(0,r.jsxs)(t.A,{children:[(0,r.jsx)(d.A,{children:"3.a/5"}),(0,r.jsx)(c.A,{type:"aarm",aarm:"discussion",children:(0,r.jsxs)(s.p,{children:[(0,r.jsx)("strong",{}),"These uses occur in the definition of \u201cstatic expression\u201d in the cases of function calls, type conversions, qualified expressions, membership tests, short circuit control forms, conditional expressions, and parenthesized expressions. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(d.A,{children:"3.5/5"}),"\n",(0,r.jsx)(l.A,{items:["AI12-0075-1"]}),"\n",(0,r.jsxs)("p",{children:["The following language-defined representation aspect may be specified for an expression function:",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(d.A,{children:"3.6/5"}),"\n",(0,r.jsxs)("dt",{children:[(0,r.jsx)("br",{}),"Static"]}),"\n",(0,r.jsx)("dl",{children:(0,r.jsxs)("dd",{children:["The type of aspect Static is Boolean. When aspect Static is True for an expression function, the function is a ",(0,r.jsx)("em",{children:"static expression function"}),". If directly specified, the ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0348",children:"aspect_definition"})})," shall be a static expression.",(0,r.jsx)("br",{})]})}),"\n",(0,r.jsxs)(t.A,{children:[(0,r.jsx)(d.A,{children:"3.b/5"}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(s.p,{children:[(0,r.jsx)("strong",{children:"Aspect Description for "}),(0,r.jsx)("strong",{children:"Static: "}),"Specifies that an associated expression function can be used in static expressions.",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(d.A,{children:"3.7/5"}),"\n",(0,r.jsx)("dt",{children:(0,r.jsx)("br",{})}),"\n",(0,r.jsx)("dl",{children:(0,r.jsxs)("dd",{children:["The Static value for an inherited function is True if some corresponding primitive function of the parent or progenitor type is a static expression function; otherwise, if not directly specified, the aspect is False. ",(0,r.jsx)("br",{})]})}),"\n",(0,r.jsx)(d.A,{children:"3.8/5"}),"\n",(0,r.jsx)(l.A,{items:["AI12-0075-1"]}),"\n",(0,r.jsxs)("p",{children:["[A static expression function is a static function; see ",(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.9",children:"4.9"}),".] ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(s.h4,{id:"legality-rules",children:"Legality Rules"}),"\n",(0,r.jsx)(d.A,{children:"4/3"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0177-1"]}),"\n",(0,r.jsxs)("p",{children:["If an ",(0,r.jsx)("code",{children:(0,r.jsxs)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:["expression","_","function","_","declaration"]})})," is a completion, it shall be the completion of a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0195",children:"subprogram_declaration"})})," or ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0311",children:"generic_subprogram_declaration"})}),". The profile of an ",(0,r.jsx)("code",{children:(0,r.jsxs)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:["expression","_","function","_","declaration"]})})," that completes a declaration shall conform fully to that of the declaration.",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(d.A,{children:"5/4"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0177-1","AI12-0157-1"]}),"\n",(0,r.jsxs)("p",{children:["If the result subtype has one or more unconstrained access discriminants, the accessibility level of the anonymous access type of each access discriminant, as determined by the ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," or ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0106",children:"aggregate"})})," of the ",(0,r.jsx)("code",{children:(0,r.jsxs)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:["expression","_","function","_","declaration"]})}),", shall not be statically deeper than that of the master that elaborated the ",(0,r.jsx)("code",{children:(0,r.jsxs)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:["expression","_","function","_","declaration"]})}),".",(0,r.jsx)("br",{})]}),"\n",(0,r.jsxs)(t.A,{children:[(0,r.jsx)(d.A,{children:"5.a/3"}),(0,r.jsx)(c.A,{type:"aarm",aarm:"ramification",children:(0,r.jsxs)(s.p,{children:[(0,r.jsx)("strong",{}),"This can only fail if the discriminant is an access to a part of a nonaliased parameter, as there can be no local declarations here. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(t.A,{children:[(0,r.jsx)(d.A,{children:"5.b/4"}),(0,r.jsx)(l.A,{items:["AI12-0005-1"]}),(0,r.jsx)(c.A,{type:"aarm",aarm:"discussion",children:(0,r.jsxs)(s.p,{children:[(0,r.jsx)("strong",{}),"We don't need to repeat any of the other Legality Rules for return statements since none of them can fail here: the implicit return statement has to apply to this function (and isn't nested in something), there clearly is a return statement in this function, and the static class-wide accessibility check cannot fail as a tagged type cannot be declared locally in an expression function. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(d.A,{children:"5.1/5"}),"\n",(0,r.jsx)(l.A,{items:["AI12-0075-1"]}),"\n",(0,r.jsxs)("p",{children:["Aspect Static shall be specified to have the value True only if the associated ",(0,r.jsx)("code",{children:(0,r.jsxs)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:["expression","_","function","_","declaration"]})}),":",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(d.A,{children:"5.2/5"}),"\n",(0,r.jsxs)("ul",{children:[(0,r.jsxs)("li",{children:["is not a completion;",(0,r.jsx)("br",{})]}),(0,r.jsx)(d.A,{children:"5.3/5"}),(0,r.jsxs)("li",{children:["has an ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," that is a potentially static expression;",(0,r.jsx)("br",{})]}),(0,r.jsx)(d.A,{children:"5.4/5"}),(0,r.jsxs)("li",{children:["contains no calls to itself;",(0,r.jsx)("br",{})]}),(0,r.jsx)(d.A,{children:"5.5/5"}),(0,r.jsxs)("li",{children:["each parameter (if any) is of mode ",(0,r.jsx)("strong",{children:"in"})," and is of a static subtype;",(0,r.jsx)("br",{})]}),(0,r.jsx)(d.A,{children:"5.6/5"}),(0,r.jsxs)("li",{children:["has a result subtype that is a static subtype;",(0,r.jsx)("br",{})]}),(0,r.jsx)(d.A,{children:"5.7/5"}),(0,r.jsxs)("li",{children:["has no applicable precondition or postcondition expression; and",(0,r.jsx)("br",{})]}),(0,r.jsx)(d.A,{children:"5.8/5"}),(0,r.jsx)(l.A,{items:["AI12-0075-1","AI12-0191-1"]}),(0,r.jsxs)("li",{children:["for result type ",(0,r.jsx)("em",{children:"R"}),", if the function is a boundary entity for type ",(0,r.jsx)("em",{children:"R"})," (see ",(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.3#Subclause_7.3.2",children:"7.3.2"}),"), no type invariant applies to type ",(0,r.jsx)("em",{children:"R"}),"; if ",(0,r.jsx)("em",{children:"R"})," has a component type ",(0,r.jsx)("em",{children:"C"}),", a similar rule applies to ",(0,r.jsx)("em",{children:"C"}),". ",(0,r.jsx)("br",{})]})]}),"\n",(0,r.jsxs)(t.A,{children:[(0,r.jsx)(d.A,{children:"5.c/5"}),(0,r.jsx)(c.A,{type:"aarm",aarm:"ramification",children:(0,r.jsxs)(s.p,{children:[(0,r.jsx)("strong",{}),"Since a string subtype can be static, this allows an expression function of a string type to be static. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)("p",{children:[(0,r.jsx)("em",{children:"Paragraph 6 was deleted."})," ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(s.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,r.jsx)(d.A,{children:"7/5"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0177-1","AI05-0262-1","AI12-0157-1","AI12-0408-1"]}),"\n",(0,r.jsxs)("p",{children:["The execution of an expression function is invoked by a subprogram call. For the execution of a subprogram call on an expression function, or on a function completed with a ",(0,r.jsx)("code",{children:(0,r.jsxs)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:["expression","_","function","_","declaration"]})}),", the execution of the ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.3#S0216",children:"subprogram_body"})})," executes an implicit function body containing only a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.5#S0222",children:"simple_return_statement"})})," whose ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," is the return expression of the expression function.",(0,r.jsx)("br",{})]}),"\n",(0,r.jsxs)(t.A,{children:[(0,r.jsx)(d.A,{children:"7.a/3"}),(0,r.jsx)(c.A,{type:"aarm",aarm:"discussion",children:(0,r.jsxs)(s.p,{children:[(0,r.jsx)("strong",{}),"The last sentence effectively means that all of the dynamic wording in ",(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.5",children:"6.5"})," applies as needed, and we don't have to repeat it here. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(d.A,{children:"8/3"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0177-1"]}),"\n",(0,r.jsxs)("p",{children:["The elaboration of an ",(0,r.jsx)("code",{children:(0,r.jsxs)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:["expression","_","function","_","declaration"]})})," has no other effect than to establish that the expression function can be called without failing the Elaboration","_","Check. ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(s.h4,{id:"examples",children:"Examples"}),"\n",(0,r.jsx)(d.A,{children:"9/5"}),"\n",(0,r.jsx)(l.A,{items:["AI12-0429-1"]}),"\n",(0,r.jsxs)("p",{children:[(0,r.jsx)("em",{children:"Example of an expression function:"})," ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(d.A,{children:"10/3"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0177-1"]}),"\n",(0,r.jsx)(o.A,{language:"ada",children:(0,r.jsxs)(s.p,{children:["function Is","_","Origin (P : in Point) return Boolean is -- see ",(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.9",children:"3.9"}),"\n","   (P.X = 0.0 and P.Y = 0.0);","\n"]})}),"\n",(0,r.jsx)(t.A,{children:(0,r.jsx)(s.h4,{id:"extensions-to-ada-2005",children:"Extensions to Ada 2005"})}),"\n",(0,r.jsxs)(t.A,{children:[(0,r.jsx)(d.A,{children:"10.a/3"}),(0,r.jsx)(l.A,{items:["AI05-0177-1"]}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(s.p,{children:["Expression functions are new in Ada 2012. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(t.A,{children:(0,r.jsx)(s.h4,{id:"extensions-to-ada-2012",children:"Extensions to Ada 2012"})}),"\n",(0,r.jsxs)(t.A,{children:[(0,r.jsx)(d.A,{children:"10.b/4"}),(0,r.jsx)(l.A,{items:["AI12-0157-1"]}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(s.p,{children:[(0,r.jsx)("strong",{children:"Corrigendum:"})," A ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0106",children:"aggregate"})})," can directly be the return expression of an expression function. This eliminates the double parentheses that otherwise would be necessary.",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(t.A,{children:[(0,r.jsx)(d.A,{children:"10.c/5"}),(0,r.jsx)(l.A,{items:["AI12-0075-1"]}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(s.p,{children:["Aspect Static is new; it allows using suitable expression functions in static expressions. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(t.A,{children:(0,r.jsx)(s.h4,{id:"wording-changes-from-ada-2012",children:"Wording Changes from Ada 2012"})}),"\n",(0,r.jsxs)(t.A,{children:[(0,r.jsx)(d.A,{children:"10.d/5"}),(0,r.jsx)(l.A,{items:["AI12-0408-1"]}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(s.p,{children:["Clarified the term \u201cexpression function\u201d so it matches the meaning expected in ",(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1",children:"6.1"}),". ",(0,r.jsx)("br",{})]})})]})]})}function f(e={}){const{wrapper:s}={...(0,a.R)(),...e.components};return s?(0,r.jsx)(s,{...e,children:(0,r.jsx)(p,{...e})}):p(e)}}}]);