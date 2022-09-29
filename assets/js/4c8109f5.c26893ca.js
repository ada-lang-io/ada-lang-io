"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[9537],{1542:(t,e,n)=>{n.r(e),n.d(e,{assets:()=>g,contentTitle:()=>m,default:()=>_,frontMatter:()=>f,metadata:()=>y,toc:()=>b});var a=n(1716),i=n(3050),l=n(8604),r=n(7318),o=n(4768),s=Object.defineProperty,d=Object.defineProperties,A=Object.getOwnPropertyDescriptors,u=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,k=Object.prototype.propertyIsEnumerable,p=(t,e,n)=>e in t?s(t,e,{enumerable:!0,configurable:!0,writable:!0,value:n}):t[e]=n,h=(t,e)=>{for(var n in e||(e={}))c.call(e,n)&&p(t,n,e[n]);if(u)for(var n of u(e))k.call(e,n)&&p(t,n,e[n]);return t};const f={sidebar_position:196},m="J.3 Reduced Accuracy Subtypes",y={unversionedId:"arm/AA-J/AA-J.3",id:"arm/AA-J/AA-J.3",title:"J.3 Reduced Accuracy Subtypes",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-J/AA-J.3.mdx",sourceDirName:"arm/AA-J",slug:"/arm/AA-J/AA-J.3",permalink:"/docs/arm/AA-J/AA-J.3",draft:!1,tags:[],version:"current",sidebarPosition:196,frontMatter:{sidebar_position:196},sidebar:"referenceManualSidebar",previous:{title:"J.2 Allowed Replacements of Characters",permalink:"/docs/arm/AA-J/AA-J.2"},next:{title:"J.4 The Constrained Attribute",permalink:"/docs/arm/AA-J/AA-J.4"}},g={},b=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],x={toc:b};function _(t){var e,n=t,{components:s}=n,p=((t,e)=>{var n={};for(var a in t)c.call(t,a)&&e.indexOf(a)<0&&(n[a]=t[a]);if(null!=t&&u)for(var a of u(t))e.indexOf(a)<0&&k.call(t,a)&&(n[a]=t[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(e=h(h({},x),p),d(e,A({components:s,mdxType:"MDXLayout"}))),(0,a.kt)("h1",h({},{id:"j3-reduced-accuracy-subtypes"}),"J.3 Reduced Accuracy Subtypes"),(0,a.kt)("admonition",h({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",h({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(r.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0050"},"digits_constraint"))," may be used to define a floating point subtype with a new value for its requested decimal precision, as reflected by its Digits attribute. Similarly, a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint"))," may be used to define an ordinary fixed point subtype with a new value for its ",(0,a.kt)("em",null,"delta"),", as reflected by its Delta attribute. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"1.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"It might be more direct to make these attributes specifiable via an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.3#S0349"},"attribute_definition_clause")),", and eliminate the syntax for these ",(0,a.kt)("code",null,"_constraint"),"s. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"syntax"}),"Syntax"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2/4"),(0,a.kt)(o.Z,{items:["AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{mdxType:"CodeBlock"},(0,a.kt)("code",null,"delta_constraint"),(0,a.kt)("a",{id:"S0367"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("strong",null,"delta")," ",(0,a.kt)("em",null,"static_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," [",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0036"},"range_constraint")),"]",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"name-resolution-rules"}),"Name Resolution Rules"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"3/4"),(0,a.kt)(o.Z,{items:["AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint"))," is expected to be of any real type. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"legality-rules"}),"Legality Rules"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"4/4"),(0,a.kt)(o.Z,{items:["AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint"))," shall be static.",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"5"),(0,a.kt)("p",null,"For a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0027"},"subtype_indication"))," with a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint")),", the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0028"},"subtype_mark"))," shall denote an ordinary fixed point subtype.",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"6"),(0,a.kt)("p",null,"For a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0027"},"subtype_indication"))," with a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0050"},"digits_constraint")),", the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0028"},"subtype_mark"))," shall denote either a decimal fixed point subtype or a floating point subtype (notwithstanding the rule given in ",(0,a.kt)("a",{href:"../AA-3/AA-3.5#Subclause_3.5.9"},"3.5.9")," that only allows a decimal fixed point subtype). ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"6.a/2"),(0,a.kt)(o.Z,{items:["AI95-00114-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("em",null,"This paragraph was deleted."),(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"7/4"),(0,a.kt)(o.Z,{items:["AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0027"},"subtype_indication"))," with a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0028"},"subtype_mark"))," that denotes an ordinary fixed point subtype and a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint"))," defines an ordinary fixed point subtype with a ",(0,a.kt)("em",null,"delta")," given by the value of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint")),". If the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint"))," includes a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0036"},"range_constraint")),", then the ordinary fixed point subtype is constrained by the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0036"},"range_constraint")),".",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"8/4"),(0,a.kt)(o.Z,{items:["AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0027"},"subtype_indication"))," with a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0028"},"subtype_mark"))," that denotes a floating point subtype and a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0050"},"digits_constraint"))," defines a floating point subtype with a requested decimal precision (as reflected by its Digits attribute) given by the value of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0050"},"digits_constraint")),". If the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0050"},"digits_constraint"))," includes a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0036"},"range_constraint")),", then the floating point subtype is constrained by the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0036"},"range_constraint")),". ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"9/4"),(0,a.kt)(o.Z,{items:["AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint"))," is ",(0,a.kt)("em",null,"compatible")," with an ordinary fixed point subtype if the value of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," is no less than the ",(0,a.kt)("em",null,"delta")," of the subtype, and the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0036"},"range_constraint")),", if any, is compatible with the subtype.",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"10/4"),(0,a.kt)(o.Z,{items:["AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0050"},"digits_constraint"))," is ",(0,a.kt)("em",null,"compatible")," with a floating point subtype if the value of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," is no greater than the requested decimal precision of the subtype, and the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0036"},"range_constraint")),", if any, is compatible with the subtype.",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"11"),(0,a.kt)("p",null,"The elaboration of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint"))," consists of the elaboration of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0036"},"range_constraint")),", if any. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"11.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"A numeric subtype is considered \u201cconstrained\u201d only if a range constraint applies to it. The only effect of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0050"},"digits_constraint"))," or a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint"))," without a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0036"},"range_constraint"))," is to specify the value of the corresponding Digits or Delta attribute in the new subtype. The set of values of the subtype is not \u201cconstrained\u201d in any way by such ",(0,a.kt)("code",null,"_constraint"),"s. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"11.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"In Ada 83, a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint"))," is called a fixed_point_constraint, and a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0050"},"digits_constraint"))," is called a floating_point_constraint. We have adopted other terms because ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0050"},"digits_constraint")),"s apply primarily to decimal fixed point types now (they apply to floating point types only as an obsolescent feature). ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"11.c/4"),(0,a.kt)(o.Z,{items:["AI12-0152-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("strong",null,"Corrigendum:")," Changed the syntax so that the value following ",(0,a.kt)("strong",null,"delta")," in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.3#S0367"},"delta_constraint"))," is a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),". This is compatible as any expressions that would require extra parentheses are already illegal. The change is necessary to eliminate syntax ambguities in ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.4#S0035"},"derived_type_definition")),"s. The similar change for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0050"},"digits_constraint"))," is documented in ",(0,a.kt)("a",{href:"../AA-3/AA-3.5#Subclause_3.5.9"},"3.5.9"),". ",(0,a.kt)("br",null)))}_.isMDXComponent=!0}}]);