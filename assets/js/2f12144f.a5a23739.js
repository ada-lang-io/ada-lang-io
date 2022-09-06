"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6023],{2757:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>k,contentTitle:()=>m,default:()=>A,frontMatter:()=>h,metadata:()=>f,toc:()=>g});var a=n(1716),i=n(7556),r=Object.defineProperty,o=Object.defineProperties,l=Object.getOwnPropertyDescriptors,s=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,p=(e,t,n)=>t in e?r(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,u=(e,t)=>{for(var n in t||(t={}))c.call(t,n)&&p(e,n,t[n]);if(s)for(var n of s(t))d.call(t,n)&&p(e,n,t[n]);return e};const h={sidebar_position:87},m="11.1  Exception Declarations",f={unversionedId:"arm/AA-11.1",id:"arm/AA-11.1",title:"11.1  Exception Declarations",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-11.1.mdx",sourceDirName:"arm",slug:"/arm/AA-11.1",permalink:"/docs/arm/AA-11.1",draft:!1,tags:[],version:"current",sidebarPosition:87,frontMatter:{sidebar_position:87},sidebar:"tutorialSidebar",previous:{title:"11 Exceptions",permalink:"/docs/arm/AA-11"},next:{title:"11.2  Exception Handlers",permalink:"/docs/arm/AA-11.2"}},k={},g=[{value:"Syntax",id:"syntax",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Inconsistencies With Ada 83",id:"inconsistencies-with-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4}],x={toc:g};function A(e){var t,n=e,{components:r}=n,p=((e,t)=>{var n={};for(var a in e)c.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&s)for(var a of s(e))t.indexOf(a)<0&&d.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=u(u({},x),p),o(t,l({components:r,mdxType:"MDXLayout"}))),(0,a.kt)("h1",u({},{id:"111--exception-declarations"}),"11.1  Exception Declarations"),(0,a.kt)("admonition",u({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,a.kt)("a",u({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("p",null,"An ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-11.1#S0303"},"exception_declaration"))," declares a name for an exception. "),(0,a.kt)("h4",u({},{id:"syntax"}),"Syntax"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0183-1"),"}"," ",(0,a.kt)("code",null,"exception_declaration"),(0,a.kt)("a",{id:"S0303"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.3#S0033"},"defining_identifier_list"))," : ",(0,a.kt)("strong",null,"exception"),"   [",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-13.1#S0346"},"aspect_specification")),"];"),(0,a.kt)("h4",u({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)("p",null,"Each single ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-11.1#S0303"},"exception_declaration"))," declares a name for a different exception. If a generic unit includes an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-11.1#S0303"},"exception_declaration")),", the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-11.1#S0303"},"exception_declaration")),"s implicitly generated by different instantiations of the generic unit refer to distinct exceptions (but all have the same ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.1#S0022"},"defining_identifier")),"). The particular exception denoted by an exception name is determined at compilation time and is the same regardless of how many times the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-11.1#S0303"},"exception_declaration"))," is elaborated. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Reason: "),"We considered removing this requirement inside generic bodies, because it is an implementation burden for implementations that wish to share code among several instances. In the end, it was decided that it would introduce too much implementation dependence. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"Hence, if an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-11.1#S0303"},"exception_declaration"))," occurs in a recursive subprogram, the exception name denotes the same exception for all invocations of the recursive subprogram. The reason for this rule is that we allow an exception occurrence to propagate out of its declaration's innermost containing master; if exceptions were created by their declarations like other entities, they would presumably be destroyed upon leaving the master; we would have to do something special to prevent them from propagating to places where they no longer exist. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"Exception identities are unique across all partitions of a program. "),(0,a.kt)("p",null,"The ",(0,a.kt)("em",null,"predefined")," exceptions are the ones declared in the declaration of package Standard: Constraint_Error, Program_Error, Storage_Error, and Tasking_Error[; one of them is raised when a language-defined check fails.] "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"The exceptions declared in the language-defined package IO_Exceptions, for example, are not predefined. "),(0,a.kt)("h4",u({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)("p",null,"The elaboration of an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-11.1#S0303"},"exception_declaration"))," has no effect."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0445-1"),"}"," The execution of any construct raises Storage_Error if there is insufficient storage for that execution. The amount of storage necessary for the execution of constructs is unspecified. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"Note that any execution whatsoever can raise Storage_Error. This allows much implementation freedom in storage management. "),(0,a.kt)("h4",u({},{id:"examples"}),"Examples"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Examples of user-defined exception declarations:")," "),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"Singular : exception;","\n","Error    : exception;","\n","Overflow, Underflow : exception;","\n"),(0,a.kt)("h4",u({},{id:"inconsistencies-with-ada-83"}),"Inconsistencies With Ada 83"),(0,a.kt)("p",null,'The exception Numeric_Error is now defined in the Obsolescent features Annex, as a rename of Constraint_Error. All checks that raise Numeric_Error in Ada 83 instead raise Constraint_Error in Ada 95. To increase upward compatibility, we also changed the rules to allow the same exception to be named more than once by a given handler. Thus, "',(0,a.kt)("strong",null,"when")," Constraint_Error | Numeric_Error =",">",'" will remain legal in Ada 95, even though Constraint_Error and Numeric_Error now denote the same exception. However, it will not be legal to have separate handlers for Constraint_Error and Numeric_Error. This change is inconsistent in the rare case that an existing program explicitly raises Numeric_Error at a point where there is a handler for Constraint_Error; the exception will now be caught by that handler. '),(0,a.kt)("h4",u({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)("p",null,"We explicitly define elaboration for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-11.1#S0303"},"exception_declaration")),"s. "),(0,a.kt)("h4",u({},{id:"extensions-to-ada-2005"}),"Extensions to Ada 2005"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0183-1"),"}"," An optional ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-13.1#S0346"},"aspect_specification"))," can be used in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-11.1#S0303"},"exception_declaration")),". This is described in 13.1.1. "))}A.isMDXComponent=!0}}]);