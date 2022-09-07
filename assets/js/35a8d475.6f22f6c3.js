"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[9288],{5554:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>b,contentTitle:()=>v,default:()=>A,frontMatter:()=>h,metadata:()=>k,toc:()=>g});var i=t(1716),a=t(7556),r=t(3183),o=Object.defineProperty,l=Object.defineProperties,s=Object.getOwnPropertyDescriptors,m=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,c=(e,n,t)=>n in e?o(e,n,{enumerable:!0,configurable:!0,writable:!0,value:t}):e[n]=t,p=(e,n)=>{for(var t in n||(n={}))u.call(n,t)&&c(e,t,n[t]);if(m)for(var t of m(n))d.call(n,t)&&c(e,t,n[t]);return e};const h={sidebar_position:134},v="A.17 The Package Environment_Variables",k={unversionedId:"arm/AA-A/AA-A.17",id:"arm/AA-A/AA-A.17",title:"A.17 The Package Environment_Variables",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-A/AA-A.17.mdx",sourceDirName:"arm/AA-A",slug:"/arm/AA-A/AA-A.17",permalink:"/docs/arm/AA-A/AA-A.17",draft:!1,tags:[],version:"current",sidebarPosition:134,frontMatter:{sidebar_position:134},sidebar:"referenceManualSidebar",previous:{title:"A.16 The Package Directories",permalink:"/docs/arm/AA-A/AA-A.16"},next:{title:"A.18 Containers",permalink:"/docs/arm/AA-A/AA-A.18"}},b={},g=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Bounded (Run-Time) Errors",id:"bounded-run-time-errors",level:4},{value:"Erroneous Execution",id:"erroneous-execution",level:4},{value:"Documentation Requirements",id:"documentation-requirements",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Incompatibilities With Ada 2005",id:"incompatibilities-with-ada-2005",level:4},{value:"A.17.1  The Packages Wide_Environment_Variables and Wide_Wide_Environment_Variables",id:"a171--the-packages-wide_environment_variables-and-wide_wide_environment_variables",level:2},{value:"Static Semantics",id:"static-semantics-1",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4}],f={toc:g};function A(e){var n,t=e,{components:o}=t,c=((e,n)=>{var t={};for(var i in e)u.call(e,i)&&n.indexOf(i)<0&&(t[i]=e[i]);if(null!=e&&m)for(var i of m(e))n.indexOf(i)<0&&d.call(e,i)&&(t[i]=e[i]);return t})(t,["components"]);return(0,i.kt)("wrapper",(n=p(p({},f),c),l(n,s({components:o,mdxType:"MDXLayout"}))),(0,i.kt)("h1",p({},{id:"a17-the-package-environment_variables"}),"A.17 The Package Environment_Variables"),(0,i.kt)("admonition",p({},{type:"warning"}),(0,i.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,i.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," The package Environment_Variables allows a program to read or modify environment variables. Environment variables are name-value pairs, where both the name and value are strings. The definition of what constitutes an ",(0,i.kt)("em",null,"environment variable"),", and the meaning of the name and value, are implementation defined. "),(0,i.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,i.kt)("p",null,(0,i.kt)("strong",null,"Implementation defined: "),"The definition and meaning of an environment variable.")),(0,i.kt)("h4",p({},{id:"static-semantics"}),"Static Semantics"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," The library package Environment_Variables has the following declaration: "),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"{","AI12-0241-1","}"," ","{","AI12-0302-1","}"," package Ada.Environment_Variables","\n","   with Preelaborate, Nonblocking, Global =",">"," in out synchronized is","\n"),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"function Value (Name : in String) return String;","\n"),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"{","AI05-0285-1","}","    function Value (Name : in String; Default : in String) return String;","\n"),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"function Exists (Name : in String) return Boolean;","\n"),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"procedure Set (Name : in String; Value : in String);","\n"),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"procedure Clear (Name : in String);","\n","   procedure Clear;","\n"),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"{","AI05-0248-1","}"," ","{","AI12-0286-1","}","    procedure Iterate","\n","      (Process : not null access procedure (Name, Value : in String))","\n","      with Allows_Exit;","\n"),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"end Ada.Environment_Variables;","\n"),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"function Value (Name : in String) return String;","\n"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," If the external execution environment supports environment variables, then Value returns the value of the environment variable with the given name. If no environment variable with the given name exists, then Constraint_Error is propagated. If the execution environment does not support environment variables, then Program_Error is propagated."),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"function Value (Name : in String; Default : in String) return String;","\n"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0285-1"),"}"," If the external execution environment supports environment variables and an environment variable with the given name currently exists, then Value returns its value; otherwise, it returns Default."),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"function Exists (Name : in String) return Boolean;","\n"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," ","{",(0,i.kt)("em",null,"AI05-0264-1"),"}"," If the external execution environment supports environment variables and an environment variable with the given name currently exists, then Exists returns True; otherwise, it returns False."),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"procedure Set (Name : in String; Value : in String);","\n"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," ","{",(0,i.kt)("em",null,"AI05-0264-1"),"}"," If the external execution environment supports environment variables, then Set first clears any existing environment variable with the given name, and then defines a single new environment variable with the given name and value. Otherwise, Program_Error is propagated."),(0,i.kt)("p",null,"If implementation-defined circumstances prohibit the definition of an environment variable with the given name and value, then Constraint_Error is propagated. "),(0,i.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,i.kt)("p",null,(0,i.kt)("strong",null,"Implementation defined: "),"The circumstances where an environment variable cannot be defined.")),(0,i.kt)("p",null,"It is implementation defined whether there exist values for which the call Set(Name, Value) has the same effect as Clear (Name). "),(0,i.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,i.kt)("p",null,(0,i.kt)("strong",null,"Implementation defined: "),"Environment names for which Set has the effect of Clear.")),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"procedure Clear (Name : in String);","\n"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," ","{",(0,i.kt)("em",null,"AI05-0264-1"),"}"," ","{",(0,i.kt)("em",null,"AI05-0269-1"),"}"," If the external execution environment supports environment variables, then Clear deletes all existing environment variables with the given name. Otherwise, Program_Error is propagated."),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"procedure Clear;","\n"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," ","{",(0,i.kt)("em",null,"AI05-0264-1"),"}"," If the external execution environment supports environment variables, then Clear deletes all existing environment variables. Otherwise, Program_Error is propagated."),(0,i.kt)(a.Z,{mdxType:"CodeBlock"},"{","AI05-0248-1","}"," ","{","AI12-0286-1","}"," procedure Iterate","\n","   (Process : not null access procedure (Name, Value : in String))","\n","      with Allows_Exit;","\n"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," ","{",(0,i.kt)("em",null,"AI05-0264-1"),"}"," If the external execution environment supports environment variables, then Iterate calls the subprogram designated by Process for each existing environment variable, passing the name and value of that environment variable. Otherwise, Program_Error is propagated."),(0,i.kt)("p",null,"If several environment variables exist that have the same name, Process is called once for each such variable."),(0,i.kt)("h4",p({},{id:"bounded-run-time-errors"}),"Bounded (Run-Time) Errors"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," It is a bounded error to call Value if more than one environment variable exists with the given name; the possible outcomes are that: "),(0,i.kt)("p",null,"one of the values is returned, and that same value is returned in subsequent calls in the absence of changes to the environment; or"),(0,i.kt)("p",null,"Program_Error is propagated. "),(0,i.kt)("h4",p({},{id:"erroneous-execution"}),"Erroneous Execution"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," Making calls to the procedures Set or Clear concurrently with calls to any subprogram of package Environment_Variables, or to any instantiation of Iterate, results in erroneous execution."),(0,i.kt)("p",null,"Making calls to the procedures Set or Clear in the actual subprogram corresponding to the Process parameter of Iterate results in erroneous execution. "),(0,i.kt)("h4",p({},{id:"documentation-requirements"}),"Documentation Requirements"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," An implementation shall document how the operations of this package behave if environment variables are changed by external mechanisms (for instance, calling operating system services). "),(0,i.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,i.kt)("p",null,(0,i.kt)("strong",null,"Documentation Requirement: "),"The behavior of package Environment_Variables when environment variables are changed by external mechanisms.")),(0,i.kt)("h4",p({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," An implementation running on a system that does not support environment variables is permitted to define the operations of package Environment_Variables with the semantics corresponding to the case where the external execution environment does support environment variables. In this case, it shall provide a mechanism to initialize a nonempty set of environment variables prior to the execution of a partition. "),(0,i.kt)("h4",p({},{id:"implementation-advice"}),"Implementation Advice"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," If the execution environment supports subprocesses, the currently defined environment variables should be used to initialize the environment variables of a subprocess. "),(0,i.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,i.kt)("p",null,(0,i.kt)("strong",null,"Implementation Advice: "),"If the execution environment supports subprocesses, the current environment variables should be used to initialize the environment variables of a subprocess.")),(0,i.kt)("p",null,"Changes to the environment variables made outside the control of this package should be reflected immediately in the effect of the operations of this package. Changes to the environment variables made using this package should be reflected immediately in the external execution environment. This package should not perform any buffering of the environment variables. "),(0,i.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,i.kt)("p",null,(0,i.kt)("strong",null,"Implementation Advice: "),"Changes to the environment variables made outside the control of Environment_Variables should be reflected immediately.")),(0,i.kt)("h4",p({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,i.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00370-01"),"}"," Package Environment_Variables is new. ")),(0,i.kt)("h4",p({},{id:"incompatibilities-with-ada-2005"}),"Incompatibilities With Ada 2005"),(0,i.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0285-1"),"}"," A new overloaded function Value is added to Environment_Variables. If Environment_Variables is referenced in a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"../AA-8/AA-8.4#S0235"},"use_clause")),", and an entity ",(0,i.kt)("em",null,"E")," with the name Value is defined in a package that is also referenced in a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"../AA-8/AA-8.4#S0235"},"use_clause")),", the entity ",(0,i.kt)("em",null,"E")," may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. ")),(0,i.kt)("h2",p({},{id:"a171--the-packages-wide_environment_variables-and-wide_wide_environment_variables"}),"A.17.1  The Packages Wide_Environment_Variables and Wide_Wide_Environment_Variables"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0021-1"),"}"," The packages Wide_Environment_Variables and Wide_Wide_Environment_Variables allow a program to read or modify environment variables. "),(0,i.kt)("h4",p({},{id:"static-semantics-1"}),"Static Semantics"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0021-1"),"}"," The specification of package Wide_Environment_Variables is the same as for Environment_Variables, except that each occurrence of String is replaced by Wide_String."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0021-1"),"}"," The specification of package Wide_Wide_Environment_Variables is the same as for Environment_Variables, except that each occurrence of String is replaced by Wide_Wide_String. "),(0,i.kt)("h4",p({},{id:"extensions-to-ada-2012"}),"Extensions to Ada 2012"),(0,i.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0021-1"),"}"," These packages are new. ")))}A.isMDXComponent=!0}}]);