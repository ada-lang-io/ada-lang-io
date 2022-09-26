"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7911],{2849:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>g,contentTitle:()=>x,default:()=>_,frontMatter:()=>h,metadata:()=>f,toc:()=>y});var a=n(1716),i=n(3050),l=n(8604),r=n(7318),o=n(4768),s=Object.defineProperty,A=Object.defineProperties,d=Object.getOwnPropertyDescriptors,k=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,m=Object.prototype.propertyIsEnumerable,p=(e,t,n)=>t in e?s(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,c=(e,t)=>{for(var n in t||(t={}))u.call(t,n)&&p(e,n,t[n]);if(k)for(var n of k(t))m.call(t,n)&&p(e,n,t[n]);return e};const h={sidebar_position:89},x="11.3 Raise Statements and Raise Expressions",f={unversionedId:"arm/AA-11/AA-11.3",id:"arm/AA-11/AA-11.3",title:"11.3 Raise Statements and Raise Expressions",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-11/AA-11.3.mdx",sourceDirName:"arm/AA-11",slug:"/arm/AA-11/AA-11.3",permalink:"/docs/arm/AA-11/AA-11.3",draft:!1,tags:[],version:"current",sidebarPosition:89,frontMatter:{sidebar_position:89},sidebar:"referenceManualSidebar",previous:{title:"11.2 Exception Handlers",permalink:"/docs/arm/AA-11/AA-11.2"},next:{title:"11.4 Exception Handling",permalink:"/docs/arm/AA-11/AA-11.4"}},g={},y=[{value:"Syntax",id:"syntax",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4}],T={toc:y};function _(e){var t,n=e,{components:s}=n,p=((e,t)=>{var n={};for(var a in e)u.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&k)for(var a of k(e))t.indexOf(a)<0&&m.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=c(c({},T),p),A(t,d({components:s,mdxType:"MDXLayout"}))),(0,a.kt)("h1",c({},{id:"113-raise-statements-and-raise-expressions"}),"11.3 Raise Statements and Raise Expressions"),(0,a.kt)("admonition",c({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",c({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(r.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"[A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0308"},"raise_statement"))," raises an exception.] ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"syntax"}),"Syntax"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2/2"),(0,a.kt)(o.Z,{items:["AI95-00361-01"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{mdxType:"CodeBlock"},"{",(0,a.kt)("em",null,"AI95-00361-01"),"}"," ",(0,a.kt)("br",null),(0,a.kt)("code",null,"raise_statement"),(0,a.kt)("a",{id:"S0308"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("strong",null,"raise"),";",(0,a.kt)("br",null),"      | ",(0,a.kt)("strong",null,"raise")," ",(0,a.kt)("em",null,"exception_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," [",(0,a.kt)("strong",null,"with")," ",(0,a.kt)("em",null,"string_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),"];",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.1/4"),(0,a.kt)(o.Z,{items:["AI12-0022-1","AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{mdxType:"CodeBlock"},"{",(0,a.kt)("em",null,"AI12-0022-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," ",(0,a.kt)("br",null),(0,a.kt)("code",null,"raise_expression"),(0,a.kt)("a",{id:"S0309"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("strong",null,"raise")," ",(0,a.kt)("em",null,"exception_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," [",(0,a.kt)("strong",null,"with")," ",(0,a.kt)("em",null,"string_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),"]",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.2/4"),(0,a.kt)(o.Z,{items:["AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," ",(0,a.kt)("br",null),"If a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," appears within the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," of one of the following contexts, the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," shall appear within a pair of parentheses within the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),": ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.3/4"),(0,a.kt)("ul",null,(0,a.kt)("li",null,(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.3#S0032"},"object_declaration")),";",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.4/4"),(0,a.kt)("li",null,(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0043"},"modular_type_definition")),";",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.5/4"),(0,a.kt)("li",null,(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0045"},"floating_point_definition")),";",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.6/4"),(0,a.kt)("li",null,(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0048"},"ordinary_fixed_point_definition")),";",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.7/4"),(0,a.kt)("li",null,(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0049"},"decimal_fixed_point_definition")),";",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.8/4"),(0,a.kt)("li",null,(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.7#S0063"},"default_expression")),";",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.9/4"),(0,a.kt)("li",null,(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.3#S0112"},"ancestor_part")),". ",(0,a.kt)("br",null))),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.1/4"),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),'Unlike conditional expressions, this doesn\'t say "immediately surrounded"; the only requirement is that it is somehow within a pair of parentheses that is part of the ',(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),". We need this restriction in order that ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression")),"s cannot be syntactically confused with immediately following constructs (such as ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0346"},"aspect_specification")),"s). ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.2/4"),(0,a.kt)(i.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"We only need to require that a right parenthesis appear somewhere between the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," and the surrounding context; that's all we need to specify in order to eliminate the ambiguities. Moreover, we don't care at all where the left parenthesis is (so long as it is legal, of course).",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.3/4"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"For instance, the following is illegal by this rule: ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.4/4"),(0,a.kt)(l.Z,{language:"ada",mdxType:"CodeBlock"},"Obj : Boolean := Func_Call or else raise TBD_Error with Atomic;","\n"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.5/4"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},'as the "',(0,a.kt)("strong",null,"with"),' Atomic" could be part of the ',(0,a.kt)("strong",null,"raise_expression")," or part of the object declaration. Both of the following are legal: ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.6/4"),(0,a.kt)(l.Z,{language:"ada",mdxType:"CodeBlock"},"Obj : Boolean := Func_Call or else (raise TBD_Error) with Atomic;","\n","Obj : Boolean := (Func_Call or else raise TBD_Error) with Atomic;","\n"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.7/4"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"and if the ",(0,a.kt)("strong",null,"with")," belongs to the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression")),", then both of the following are legal: ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.8/4"),(0,a.kt)(l.Z,{language:"ada",mdxType:"CodeBlock"},"Obj : Boolean := Func_Call or else (raise TBD_Error with Atomic);","\n","Obj : Boolean := (Func_Call or else raise TBD_Error with Atomic);","\n"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.9/4"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"This rule only requires parentheses for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression")),'s that are part of the "top-level" of an ',(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," in one of the named contexts; the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," is either the entire ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),", or part of a chain of logical operations. In practice, the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," will almost always be last in interesting top-level ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),"s; anything that follows it could never be executed, so that should be rare. Other contexts such as conditional expressions, qualified expressions, aggregates, and even function calls, provide the needed parentheses. All of the following are legal, no additional parens are needed: ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.10/4"),(0,a.kt)(l.Z,{language:"ada",mdxType:"CodeBlock"},"Pre : Boolean  := (if not Is_Valid(Param) then raise Not_Valid_Error);","\n","A : A_Tagged   := (Some_Tagged'(raise TBD_Error) with Comp =",">"," 'A');","\n","B : Some_Array := (1, 2, 3, others =",">"," raise Not_Valid_Error);","\n","C : Natural    := Func (Val =",">"," raise TBD_Error);","\n"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.11/4"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Parentheses that are part of the context of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," don't count. For instance, the parentheses around the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," are required in the following: ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.12/4"),(0,a.kt)(l.Z,{language:"ada",mdxType:"CodeBlock"},"D : A_Tagged   := ((raise TBD_Error) with Comp =",">"," 'A');","\n"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.13/4"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"as ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.3#S0112"},"ancestor_part"))," is one of the contexts that triggers the rule.",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.14/4"),(0,a.kt)(o.Z,{items:["AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"This English-language rule could have been implemented instead by adding nonterminals ",(0,a.kt)("code",null,"initial_expression")," and ",(0,a.kt)("code",null,"initial_relation"),", which are the same as ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0133"},"choice_expression"))," and ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation"))," except for the inclusion of membership in ",(0,a.kt)("code",null,"initial_relation"),". Then, ",(0,a.kt)("code",null,"initial_expresion")," could be used in place of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," in all of the contexts noted. We did not do that because of the large amount of change required, both to the grammar and to language rules that refer to the grammar. A complete grammar is given in ","AI12-0152-1",(0,a.kt)("br",null),".",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2.a.15/4"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The use of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," is illegal in each of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0043"},"modular_type_definition")),", ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0045"},"floating_point_definition")),", ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0048"},"ordinary_fixed_point_definition")),", and ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0049"},"decimal_fixed_point_definition"))," as these uses are required to be static and a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," is never static. We include these in this rule so that Ada text has an unambiguous syntax in these cases. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"legality-rules"}),"Legality Rules"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"3/4"),(0,a.kt)(o.Z,{items:["AI12-0022-1","AI12-0159-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0022-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0159-1"),"}"," ",(0,a.kt)("br",null),"The ",(0,a.kt)("em",null,"exception_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),", if any, of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0308"},"raise_statement"))," or ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," shall denote an exception. A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0308"},"raise_statement"))," with no ",(0,a.kt)("em",null,"exception_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," (that is, a ",(0,a.kt)("em",null,"re-raise statement"),") shall be within a handler, but not within a body enclosed by that handler. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"name-resolution-rules"}),"Name Resolution Rules"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"3.1/4"),(0,a.kt)(o.Z,{items:["AI95-00361-01","AI12-0022-1","AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00361-01"),"}"," ","{",(0,a.kt)("em",null,"AI12-0022-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," ",(0,a.kt)("br",null),"The ",(0,a.kt)("em",null,"string_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," or ",(0,a.kt)("em",null,"string_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),", if any, of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0308"},"raise_statement"))," or ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," is expected to be of type String.",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"3.2/4"),(0,a.kt)(o.Z,{items:["AI12-0022-1","AI12-0159-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0022-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0159-1"),"}"," ",(0,a.kt)("br",null),"The expected type for a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," shall be any single type. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"4/4"),(0,a.kt)(o.Z,{items:["AI95-00361-01","AI12-0022-1","AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00361-01"),"}"," ","{",(0,a.kt)("em",null,"AI12-0022-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," ",(0,a.kt)("br",null),"To ",(0,a.kt)("em",null,"raise an exception")," is to raise a new occurrence of that exception[, as explained in ",(0,a.kt)("a",{href:"../AA-11/AA-11.4"},"11.4"),"]. For the execution of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0308"},"raise_statement"))," with an ",(0,a.kt)("em",null,"exception_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),", the named exception is raised. Similarly, for the evaluation of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression")),", the named exception is raised. [In both of these cases, if a ",(0,a.kt)("em",null,"string_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," or ",(0,a.kt)("em",null,"string_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," is present, the expression is evaluated and its value is associated with the exception occurrence.] For the execution of a re-raise statement, the exception occurrence that caused transfer of control to the innermost enclosing handler is raised [again]. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"4.a.1/2"),(0,a.kt)(o.Z,{items:["AI95-00361-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"proof",title:"Proof: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"{",(0,a.kt)("em",null,"AI95-00361-01"),"}"," ",(0,a.kt)("br",null),"The definition of Exceptions.Exception_Message includes a statement that the string is returned (see ",(0,a.kt)("a",{href:"../AA-11/AA-11.4#Subclause_11.4.1"},"11.4.1"),"). We describe the use of the string here so that we don't have an unexplained parameter in this subclause. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"4.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-note",title:"Implementation Note: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"For a re-raise statement, the implementation does not create a new Exception_Occurrence, but instead propagates the same Exception_Occurrence value. This allows the original cause of the exception to be determined. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"5"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"5.1/4"),(0,a.kt)(o.Z,{items:["AI12-0062-1","AI12-0152-1","AI12-0159-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 1   ","{",(0,a.kt)("em",null,"AI12-0062-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0159-1"),"}"," ",(0,a.kt)("br",null),"If the evaluation of a ",(0,a.kt)("em",null,"string_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," or ",(0,a.kt)("em",null,"string_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," raises an exception, that exception is propagated instead of the one denoted by the ",(0,a.kt)("em",null,"exception_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0308"},"raise_statement"))," or ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression")),". ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"examples"}),"Examples"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"6"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Examples of raise statements:")," ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"7/2"),(0,a.kt)(o.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{language:"ada",mdxType:"CodeBlock"},"--  ","{","AI95-00433-01","}","\n"," raise Ada.IO_Exceptions.Name_Error;   -- see ",(0,a.kt)("a",{href:"../AA-A/AA-A.13"},"A.13"),"\n",'raise Queue_Error with "Buffer Full"; -- see ',(0,a.kt)("a",{href:"../AA-9/AA-9.11"},"9.11"),"\n",(0,a.kt)(r.Z,{mdxType:"MarginText"},"8"),"raise;                                -- re-raise the current exception","\n",(0,a.kt)(r.Z,{mdxType:"MarginText"},"9/5"),(0,a.kt)(o.Z,{items:["AI12-0312-1"],mdxType:"MarginInfo"}),"--  ","{","AI12-0312-1","}","\n"," -- For an example of a raise expression, see the Streams Subsystem definitions in ",(0,a.kt)("a",{href:"../AA-13/AA-13.13#Subclause_13.13.1"},"13.13.1"),".","\n"),(0,a.kt)("h4",c({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"9.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The fact that the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0308"},"raise_statement"))," has to denote an exception is not clear from RM83. Clearly that was the intent, since the italicized part of the syntax rules so indicate, but there was no explicit rule. RM83-1.5(11) doesn't seem to give the italicized parts of the syntax any force. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"9.b/2"),(0,a.kt)(o.Z,{items:["AI95-00361-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00361-01"),"}"," ",(0,a.kt)("br",null),"The syntax of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0308"},"raise_statement"))," is extended to include a string message. This is more convenient than calling Exceptions.Exception_Message (",(0,a.kt)("em",null,"exception_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"'Identity, ",(0,a.kt)("em",null,"string_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),"), and should encourage the use of message strings when raising exceptions. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"extensions-to-ada-2012"}),"Extensions to Ada 2012"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"9.c/4"),(0,a.kt)(o.Z,{items:["AI12-0022-1","AI12-0152-1","AI12-0159-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI12-0022-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0159-1"),"}"," ",(0,a.kt)("strong",null,(0,a.kt)("br",null),"Corrigendum:")," The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," is new. This construct is necessary to allow conversion of existing specifications to use preconditions and predicates without changing the exceptions raised. It is considered important enough to be added to Ada 2012 rather than waiting for Ada 2022. ",(0,a.kt)("br",null)))}_.isMDXComponent=!0}}]);