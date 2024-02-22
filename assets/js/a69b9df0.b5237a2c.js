"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2050],{59481:(e,a,n)=>{n.r(a),n.d(a,{assets:()=>p,contentTitle:()=>m,default:()=>f,frontMatter:()=>s,metadata:()=>c,toc:()=>d});var t=n(58168),l=(n(96540),n(15680)),i=n(20793),r=n(91435),o=n(21432),g=n(79162),y=n(34421);const s={sidebar_position:102},m="12.7 Formal Packages",c={unversionedId:"arm/AA-12/AA-12.7",id:"arm/AA-12/AA-12.7",title:"12.7 Formal Packages",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-12/AA-12.7.mdx",sourceDirName:"arm/AA-12",slug:"/arm/AA-12/AA-12.7",permalink:"/docs/arm/AA-12/AA-12.7",draft:!1,tags:[],version:"current",sidebarPosition:102,frontMatter:{sidebar_position:102},sidebar:"referenceManualSidebar",previous:{title:"12.6 Formal Subprograms",permalink:"/docs/arm/AA-12/AA-12.6"},next:{title:"12.8 Example of a Generic Package",permalink:"/docs/arm/AA-12/AA-12.8"}},p={},d=[{value:"Syntax",id:"syntax",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Incompatibilities With Ada 2005",id:"incompatibilities-with-ada-2005",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4}],A={toc:d},u="wrapper";function f(e){let{components:a,...n}=e;return(0,l.yg)(u,(0,t.A)({},A,n,{components:a,mdxType:"MDXLayout"}),(0,l.yg)("h1",{id:"127-formal-packages"},"12.7 Formal Packages"),(0,l.yg)("admonition",{type:"warning"},(0,l.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,l.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,l.yg)(g.A,{mdxType:"MarginText"},"1"),(0,l.yg)("p",null,"[ Formal packages can be used to pass packages to a generic unit. The ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0340"},"formal_package_declaration"))," declares that the formal package is an instance of a given generic package. Upon instantiation, the actual package has to be an instance of that generic package.] ",(0,l.yg)("br",null)),(0,l.yg)("h4",{id:"syntax"},"Syntax"),(0,l.yg)(g.A,{mdxType:"MarginText"},"2/3"),(0,l.yg)(y.A,{items:["AI05-0183-1"],mdxType:"MarginInfo"}),(0,l.yg)(o.A,{mdxType:"CodeBlock"},(0,l.yg)("code",null,"formal_package_declaration"),(0,l.yg)("a",{id:"S0340"}),(0,l.yg)("code",null," ::= "),(0,l.yg)("br",null),"    ",(0,l.yg)("strong",null,"with")," ",(0,l.yg)("strong",null,"package")," ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier"))," ",(0,l.yg)("strong",null,"is")," ",(0,l.yg)("strong",null,"new")," ",(0,l.yg)("em",null,"generic_package_"),(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0341"},"formal_package_actual_part")),(0,l.yg)("br",null),"        [",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-13/AA-13.1#S0346"},"aspect_specification")),"];",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"3/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)(o.A,{mdxType:"CodeBlock"},(0,l.yg)("code",null,"formal_package_actual_part"),(0,l.yg)("a",{id:"S0341"}),(0,l.yg)("code",null," ::= "),(0,l.yg)("br",null),"    ([",(0,l.yg)("strong",null,"others")," =",">","] ","<",">",")",(0,l.yg)("br",null),"  | [",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.3#S0316"},"generic_actual_part")),"]",(0,l.yg)("br",null),"  | (",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0342"},"formal_package_association"))," ","{",", ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0342"},"formal_package_association")),"}"," [, ",(0,l.yg)("strong",null,"others")," =",">"," ","<",">","])",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"3.1/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)(o.A,{mdxType:"CodeBlock"},(0,l.yg)("code",null,"formal_package_association"),(0,l.yg)("a",{id:"S0342"}),(0,l.yg)("code",null," ::= "),(0,l.yg)("br",null),"    ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.3#S0317"},"generic_association")),(0,l.yg)("br",null),"  | ",(0,l.yg)("em",null,"generic_formal_parameter_"),(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-4/AA-4.1#S0099"},"selector_name"))," =",">"," ","<",">",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"3.2/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)("p",{class:"Indented2"},"Any positional ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0342"},"formal_package_association")),"s shall precede any named ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0342"},"formal_package_association")),"s. ",(0,l.yg)("br",null)),(0,l.yg)("h4",{id:"legality-rules"},"Legality Rules"),(0,l.yg)(g.A,{mdxType:"MarginText"},"4"),(0,l.yg)("p",null,"The ",(0,l.yg)("em",null,"generic_package_"),(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," shall denote a generic package (the ",(0,l.yg)("em",null,"template")," for the formal package); the formal package is an instance of the template.",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"4.1/3"),(0,l.yg)(y.A,{items:["AI05-0025-1"],mdxType:"MarginInfo"}),(0,l.yg)("p",null,"The ",(0,l.yg)("em",null,"generic_formal_parameter_"),(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-4/AA-4.1#S0099"},"selector_name"))," of a ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0342"},"formal_package_association"))," shall denote a ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.1#S0314"},"generic_formal_parameter_declaration"))," of the template. If two or more formal subprograms of the template have the same defining name, then named associations are not allowed for the corresponding actuals.",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"4.2/3"),(0,l.yg)(y.A,{items:["AI95-00398-01"],mdxType:"MarginInfo"}),(0,l.yg)("p",null,"A ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0341"},"formal_package_actual_part"))," shall contain at most one ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0342"},"formal_package_association"))," for each formal parameter. If the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0341"},"formal_package_actual_part"))," does not include \u201c",(0,l.yg)("strong",null,"others")," =",">"," ","<",">","\u201d, each formal parameter without an association shall have a ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-3/AA-3.7#S0063"},"default_expression"))," or ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.6#S0338"},"subprogram_default")),".",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"4.3/3"),(0,l.yg)(y.A,{items:["AI05-0200-1"],mdxType:"MarginInfo"}),(0,l.yg)("p",null,"The rules for matching between ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0342"},"formal_package_association")),"s and the generic formals of the template are as follows: ",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"4.4/3"),(0,l.yg)("ul",null,(0,l.yg)("li",null,"If all of the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0342"},"formal_package_association")),"s are given by generic associations, the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.3#S0318"},"explicit_generic_actual_parameter")),"s of the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0342"},"formal_package_association")),"s shall be legal for an instantiation of the template.",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"4.5/5"),(0,l.yg)(y.A,{items:["AI05-0200-1"],mdxType:"MarginInfo"}),(0,l.yg)("li",null,"If a ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0342"},"formal_package_association"))," for a formal type ",(0,l.yg)("em",null,"T")," of the template is given by ","<",">",", then the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0342"},"formal_package_association"))," for any other ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.1#S0314"},"generic_formal_parameter_declaration"))," of the template that mentions ",(0,l.yg)("em",null,"T")," directly or indirectly shall also be given by ","<",">",".",(0,l.yg)("br",null))),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)(g.A,{mdxType:"MarginText"},"4.a/3"),(0,l.yg)(y.A,{items:["AI05-0200-1"],mdxType:"MarginInfo"}),(0,l.yg)(i.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,l.yg)("strong",null),"The above rule is simple to state, though it does not reflect the fact that the formal package functions like an instantiation of a special kind, where each box association for a ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.1#S0314"},"generic_formal_parameter_declaration"))," ",(0,l.yg)("em",null,"F")," is replaced with a new entity ",(0,l.yg)("em",null,"F"),"' that has the same characteristics as ",(0,l.yg)("em",null,"F"),": if ",(0,l.yg)("em",null,"F")," is a formal discrete type then ",(0,l.yg)("em",null,"F"),"' is a discrete type, if ",(0,l.yg)("em",null,"F")," is a formal subprogram then ",(0,l.yg)("em",null,"F"),"' is a subprogram with a similar signature, etc. In practice this is achieved by making the association into a copy of the declaration of the generic formal.",(0,l.yg)("br",null))),(0,l.yg)(g.A,{mdxType:"MarginText"},"5/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)("p",null,"The actual shall be an instance of the template. If the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0341"},"formal_package_actual_part"))," is (","<",">",") or (",(0,l.yg)("strong",null,"others")," =",">"," ","<",">","), [then the actual may be any instance of the template]; otherwise, certain of the actual parameters of the actual instance shall match the corresponding actual parameters of the formal package, determined as follows:",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"5.1/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)("ul",null,(0,l.yg)("li",null,"If the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0341"},"formal_package_actual_part"))," includes ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.3#S0317"},"generic_association")),"s as well as associations with ","<",">",", then only the actual parameters specified explicitly with ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.3#S0317"},"generic_association")),"s are required to match;",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"5.2/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)("li",null,"Otherwise, all actual parameters shall match[, whether any actual parameter is given explicitly or by default]. ",(0,l.yg)("br",null))),(0,l.yg)(g.A,{mdxType:"MarginText"},"5.3/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)("p",null,"The rules for matching of actual parameters between the actual instance and the formal package are as follows:",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"6/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)("ul",null,(0,l.yg)("li",null,"For a formal object of mode ",(0,l.yg)("strong",null,"in"),", the actuals match if they are static expressions with the same value, or if they statically denote the same constant, or if they are both the literal ",(0,l.yg)("strong",null,"null"),". ",(0,l.yg)("br",null))),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)(g.A,{mdxType:"MarginText"},"6.a"),(0,l.yg)(i.A,{type:"aarm",aarm:"reason",mdxType:"Admonition"},(0,l.yg)("strong",null),"We can't simply require full conformance between the two actual parameter expressions, because the two expressions are being evaluated at different times. ",(0,l.yg)("br",null))),(0,l.yg)(g.A,{mdxType:"MarginText"},"7"),(0,l.yg)("ul",null,(0,l.yg)("li",null,"For a formal subtype, the actuals match if they denote statically matching subtypes. ",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"8"),(0,l.yg)("li",null,"For other kinds of formals, the actuals match if they statically denote the same entity. ",(0,l.yg)("br",null))),(0,l.yg)(g.A,{mdxType:"MarginText"},"8.1/1"),(0,l.yg)(y.A,{items:["AI95-00213-01"],mdxType:"MarginInfo"}),(0,l.yg)("p",null,"{",(0,l.yg)("em",null,"8652/0039"),"}"," For the purposes of matching, any actual parameter that is the name of a formal object of mode ",(0,l.yg)("strong",null,"in")," is replaced by the formal object's actual expression (recursively). ",(0,l.yg)("br",null)),(0,l.yg)("h4",{id:"static-semantics"},"Static Semantics"),(0,l.yg)(g.A,{mdxType:"MarginText"},"9"),(0,l.yg)("p",null,"A ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0340"},"formal_package_declaration"))," declares a generic formal package.",(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"10/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)("p",null,"The visible part of a formal package includes the first list of ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-3/AA-3.11#S0088"},"basic_declarative_item")),"s of the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-7/AA-7.1#S0230"},"package_specification")),". In addition, for each actual parameter that is not required to match, a copy of the declaration of the corresponding formal parameter of the template is included in the visible part of the formal package. If the copied declaration is for a formal type, copies of the implicit declarations of the primitive subprograms of the formal type are also included in the visible part of the formal package.",(0,l.yg)("br",null)),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)(g.A,{mdxType:"MarginText"},"10.a/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)(i.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,l.yg)("strong",null),"If the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0341"},"formal_package_actual_part"))," is (","<",">","), then the declarations that occur immediately within the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.1#S0313"},"generic_formal_part"))," of the template for the formal package are visible outside the formal package, and can be denoted by expanded names outside the formal package. If only some of the actual parameters are given by ","<",">",", then the declaration corresponding to those parameters (but not the others) are made visible. ",(0,l.yg)("br",null))),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)(g.A,{mdxType:"MarginText"},"10.b/3"),(0,l.yg)(y.A,{items:["AI05-0005-1"],mdxType:"MarginInfo"}),(0,l.yg)(i.A,{type:"aarm",aarm:"reason",mdxType:"Admonition"},(0,l.yg)("strong",null),"We always want either the actuals or the formals of an instance to be nameable from outside, but never both. If both were nameable, one would get some funny anomalies since they denote the same entity, but, in the case of types at least, they might have different and inconsistent sets of primitive operators due to predefined operator \u201creemergence\u201d, Formal derived types exacerbate the difference. We want the implicit declarations of the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.1#S0313"},"generic_formal_part"))," as well as the explicit declarations, so we get operations on the formal types. ",(0,l.yg)("br",null))),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)(g.A,{mdxType:"MarginText"},"10.c"),(0,l.yg)(i.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,l.yg)("strong",null),"A generic formal package is a package, and is an instance. Hence, it is possible to pass a generic formal package as an actual to another generic formal package. ",(0,l.yg)("br",null))),(0,l.yg)(g.A,{mdxType:"MarginText"},"11/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)("p",null,"For the purposes of matching, if the actual instance ",(0,l.yg)("em",null,"A")," is itself a formal package, then the actual parameters of ",(0,l.yg)("em",null,"A")," are those specified explicitly or implicitly in the ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0341"},"formal_package_actual_part"))," for ",(0,l.yg)("em",null,"A"),", plus, for those not specified, the copies of the formal parameters of the template included in the visible part of ",(0,l.yg)("em",null,"A"),".",(0,l.yg)("br",null)),(0,l.yg)("h4",{id:"examples"},"Examples"),(0,l.yg)(g.A,{mdxType:"MarginText"},"12/2"),(0,l.yg)(y.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),(0,l.yg)("p",null,(0,l.yg)("em",null,"Example of a generic package with formal package parameters:"),(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"13/2"),(0,l.yg)(o.A,{language:"ada",mdxType:"CodeBlock"},"with Ada.Containers.Ordered_Maps;  -- see ",(0,l.yg)("a",{href:"../AA-A/AA-A.18#Subclause_A.18.6"},"A.18.6"),"\n","generic","\n","   with package Mapping_1 is new Ada.Containers.Ordered_Maps(","<",">",");","\n","   with package Mapping_2 is new Ada.Containers.Ordered_Maps","\n","                                    (Key_Type =",">"," Mapping_1.Element_Type,","\n","                                     others =",">"," ","<",">",");","\n","package Ordered_Join is","\n",'   -- Provide a "join" between two mappings',"\n",(0,l.yg)(g.A,{mdxType:"MarginText"},"14/2"),"subtype Key_Type is Mapping_1.Key_Type;","\n","   subtype Element_Type is Mapping_2.Element_Type;","\n",(0,l.yg)(g.A,{mdxType:"MarginText"},"15/2"),"function Lookup(Key : Key_Type) return Element_Type;","\n",(0,l.yg)(g.A,{mdxType:"MarginText"},"16/2"),"...","\n","end Ordered_Join;","\n"),(0,l.yg)(g.A,{mdxType:"MarginText"},"17/2"),(0,l.yg)(y.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),(0,l.yg)("p",null,(0,l.yg)("em",null,"Example of an instantiation of a package with formal packages:"),(0,l.yg)("br",null)),(0,l.yg)(g.A,{mdxType:"MarginText"},"18/2"),(0,l.yg)(o.A,{language:"ada",mdxType:"CodeBlock"},"with Ada.Containers.Ordered_Maps;","\n","package Symbol_Package is","\n",(0,l.yg)(g.A,{mdxType:"MarginText"},"19/5"),(0,l.yg)(y.A,{items:["AI12-0178-1"],mdxType:"MarginInfo"}),"subtype Key_String is String(1..5);","\n","   type String_Id is ...","\n",(0,l.yg)(g.A,{mdxType:"MarginText"},"20/2"),"type Symbol_Info is ...","\n",(0,l.yg)(g.A,{mdxType:"MarginText"},"21/5"),(0,l.yg)(y.A,{items:["AI12-0178-1"],mdxType:"MarginInfo"}),"package String_Table is new Ada.Containers.Ordered_Maps","\n","           (Key_Type =",">"," Key_String,","\n","            Element_Type =",">"," String_Id);","\n",(0,l.yg)(g.A,{mdxType:"MarginText"},"22/2"),"package Symbol_Table is new Ada.Containers.Ordered_Maps","\n","           (Key_Type =",">"," String_Id,","\n","            Element_Type =",">"," Symbol_Info);","\n",(0,l.yg)(g.A,{mdxType:"MarginText"},"23/2"),"package String_Info is new Ordered_Join(Mapping_1 =",">"," String_Table,","\n","                                           Mapping_2 =",">"," Symbol_Table);","\n",(0,l.yg)(g.A,{mdxType:"MarginText"},"24/2"),'Apple_Info : constant Symbol_Info := String_Info.Lookup("Apple");',"\n",(0,l.yg)(g.A,{mdxType:"MarginText"},"25/2"),"end Symbol_Package;","\n"),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)("h4",{id:"extensions-to-ada-83"},"Extensions to Ada 83")),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)(g.A,{mdxType:"MarginText"},"25.a"),(0,l.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Formal packages are new to Ada 95. ",(0,l.yg)("br",null))),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)("h4",{id:"extensions-to-ada-95"},"Extensions to Ada 95")),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)(g.A,{mdxType:"MarginText"},"25.b/2"),(0,l.yg)(y.A,{items:["AI95-00317-01","AI95-00398-01"],mdxType:"MarginInfo"}),(0,l.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"It's now allowed to mix actuals of a formal package that are specified with those that are not specified. ",(0,l.yg)("br",null))),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)("h4",{id:"wording-changes-from-ada-95"},"Wording Changes from Ada 95")),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)(g.A,{mdxType:"MarginText"},"25.c/2"),(0,l.yg)(y.A,{items:["AI95-00213-01"],mdxType:"MarginInfo"}),(0,l.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"{",(0,l.yg)("em",null,"8652/0039"),"}"," ",(0,l.yg)("strong",null,"Corrigendum:")," Corrected the description of formal package matching to say that formal parameters are always replaced by their actual parameters (recursively). This matches the actual practice of compilers, as the ACATS has always required this behavior.",(0,l.yg)("br",null))),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)(g.A,{mdxType:"MarginText"},"25.d/2"),(0,l.yg)(y.A,{items:["AI95-00317-01"],mdxType:"MarginInfo"}),(0,l.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The description of which operations are visible in a formal package has been clarified. We also specify how matching is done when the actual is a formal package. ",(0,l.yg)("br",null))),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)("h4",{id:"incompatibilities-with-ada-2005"},"Incompatibilities With Ada 2005")),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)(g.A,{mdxType:"MarginText"},"25.e/3"),(0,l.yg)(y.A,{items:["AI05-0025-1","AI05-0200-1"],mdxType:"MarginInfo"}),(0,l.yg)(i.A,{type:"aarm",aarm:"correction",mdxType:"Admonition"},(0,l.yg)("strong",null)," Added missing rules for parameters of generic formal package that parallel those in ",(0,l.yg)("a",{href:"../AA-12/AA-12.3"},"12.3"),", as well as some specific to ","<",">"," parameters. These are technically incompatibilities because generic formal package parameters that Ada 95 and Ada 2005 would have considered legal now have to be rejected. But this should not be an issue in practice as such formal parameters could not have matched any actual generics. And it is quite likely that implementations already enforce some of these rules. ",(0,l.yg)("br",null))),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)("h4",{id:"extensions-to-ada-2005"},"Extensions to Ada 2005")),(0,l.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,l.yg)(g.A,{mdxType:"MarginText"},"25.f/3"),(0,l.yg)(y.A,{items:["AI05-0183-1"],mdxType:"MarginInfo"}),(0,l.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"An optional ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-13/AA-13.1#S0346"},"aspect_specification"))," can be used in a ",(0,l.yg)("code",null,(0,l.yg)("a",{href:"../AA-12/AA-12.7#S0340"},"formal_package_declaration")),". This is described in ",(0,l.yg)("a",{href:"../AA-13/AA-13.1#Subclause_13.1.1"},"13.1.1"),". ",(0,l.yg)("br",null))))}f.isMDXComponent=!0}}]);