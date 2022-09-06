"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6957],{8706:(t,e,n)=>{n.r(e),n.d(e,{assets:()=>f,contentTitle:()=>h,default:()=>y,frontMatter:()=>k,metadata:()=>p,toc:()=>A});var a=n(1716),i=Object.defineProperty,l=Object.defineProperties,o=Object.getOwnPropertyDescriptors,r=Object.getOwnPropertySymbols,s=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,u=(t,e,n)=>e in t?i(t,e,{enumerable:!0,configurable:!0,writable:!0,value:n}):t[e]=n,c=(t,e)=>{for(var n in e||(e={}))s.call(e,n)&&u(t,n,e[n]);if(r)for(var n of r(e))d.call(e,n)&&u(t,n,e[n]);return t};const k={sidebar_position:193},h="J.3  Reduced Accuracy Subtypes",p={unversionedId:"arm/AA-J.3",id:"arm/AA-J.3",title:"J.3  Reduced Accuracy Subtypes",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-J.3.mdx",sourceDirName:"arm",slug:"/arm/AA-J.3",permalink:"/docs/arm/AA-J.3",draft:!1,tags:[],version:"current",sidebarPosition:193,frontMatter:{sidebar_position:193},sidebar:"tutorialSidebar",previous:{title:"J.2  Allowed Replacements of Characters",permalink:"/docs/arm/AA-J.2"},next:{title:"J.4  The Constrained Attribute",permalink:"/docs/arm/AA-J.4"}},f={},A=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],m={toc:A};function y(t){var e,n=t,{components:i}=n,u=((t,e)=>{var n={};for(var a in t)s.call(t,a)&&e.indexOf(a)<0&&(n[a]=t[a]);if(null!=t&&r)for(var a of r(t))e.indexOf(a)<0&&d.call(t,a)&&(n[a]=t[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(e=c(c({},m),u),l(e,o({components:i,mdxType:"MDXLayout"}))),(0,a.kt)("h1",c({},{id:"j3--reduced-accuracy-subtypes"}),"J.3  Reduced Accuracy Subtypes"),(0,a.kt)("admonition",c({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,a.kt)("a",c({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0050"},"digits_constraint"))," may be used to define a floating point subtype with a new value for its requested decimal precision, as reflected by its Digits attribute. Similarly, a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint"))," may be used to define an ordinary fixed point subtype with a new value for its ",(0,a.kt)("em",null,"delta"),", as reflected by its Delta attribute. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Discussion: "),"It might be more direct to make these attributes specifiable via an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-13.3#S0349"},"attribute_definition_clause")),", and eliminate the syntax for these ",(0,a.kt)("code",null,"_constraint"),"s. "),(0,a.kt)("h4",c({},{id:"syntax"}),"Syntax"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," ",(0,a.kt)("code",null,"delta_constraint"),(0,a.kt)("a",{id:"S0367"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("strong",null,"delta")," ",(0,a.kt)("em",null,"static_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-4.4#S0138"},"simple_expression"))," [",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0036"},"range_constraint")),"]"),(0,a.kt)("h4",c({},{id:"name-resolution-rules"}),"Name Resolution Rules"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-4.4#S0138"},"simple_expression"))," of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint"))," is expected to be of any real type. "),(0,a.kt)("h4",c({},{id:"legality-rules"}),"Legality Rules"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-4.4#S0138"},"simple_expression"))," of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint"))," shall be static."),(0,a.kt)("p",null,"For a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.2#S0027"},"subtype_indication"))," with a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint")),", the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.2#S0028"},"subtype_mark"))," shall denote an ordinary fixed point subtype."),(0,a.kt)("p",null,"For a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.2#S0027"},"subtype_indication"))," with a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0050"},"digits_constraint")),", the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.2#S0028"},"subtype_mark"))," shall denote either a decimal fixed point subtype or a floating point subtype (notwithstanding the rule given in 3.5.9 that only allows a decimal fixed point subtype). "),(0,a.kt)("p",null,(0,a.kt)("em",null,"This paragraph was deleted."),"{",(0,a.kt)("em",null,"AI95-00114-01"),"}"," "),(0,a.kt)("h4",c({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.2#S0027"},"subtype_indication"))," with a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.2#S0028"},"subtype_mark"))," that denotes an ordinary fixed point subtype and a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint"))," defines an ordinary fixed point subtype with a ",(0,a.kt)("em",null,"delta")," given by the value of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-4.4#S0138"},"simple_expression"))," of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint")),". If the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint"))," includes a ",(0,a.kt)("code",null,"[range_constraint](./AA-3.5#S0036)"),", then the ordinary fixed point subtype is constrained by the ",(0,a.kt)("code",null,"[range_constraint](./AA-3.5#S0036)"),"."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.2#S0027"},"subtype_indication"))," with a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.2#S0028"},"subtype_mark"))," that denotes a floating point subtype and a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0050"},"digits_constraint"))," defines a floating point subtype with a requested decimal precision (as reflected by its Digits attribute) given by the value of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-4.4#S0138"},"simple_expression"))," of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0050"},"digits_constraint")),". If the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0050"},"digits_constraint"))," includes a ",(0,a.kt)("code",null,"[range_constraint](./AA-3.5#S0036)"),", then the floating point subtype is constrained by the ",(0,a.kt)("code",null,"[range_constraint](./AA-3.5#S0036)"),". "),(0,a.kt)("h4",c({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint"))," is ",(0,a.kt)("em",null,"compatible")," with an ordinary fixed point subtype if the value of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-4.4#S0138"},"simple_expression"))," is no less than the ",(0,a.kt)("em",null,"delta")," of the subtype, and the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0036"},"range_constraint")),", if any, is compatible with the subtype."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0050"},"digits_constraint"))," is ",(0,a.kt)("em",null,"compatible")," with a floating point subtype if the value of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-4.4#S0138"},"simple_expression"))," is no greater than the requested decimal precision of the subtype, and the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0036"},"range_constraint")),", if any, is compatible with the subtype."),(0,a.kt)("p",null,"The elaboration of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint"))," consists of the elaboration of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0036"},"range_constraint")),", if any. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Reason: "),'A numeric subtype is considered "constrained" only if a range constraint applies to it. The only effect of a ',(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0050"},"digits_constraint"))," or a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint"))," without a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0036"},"range_constraint")),' is to specify the value of the corresponding Digits or Delta attribute in the new subtype. The set of values of the subtype is not "constrained" in any way by such ',(0,a.kt)("code",null,"_constraint"),"s. "),(0,a.kt)("h4",c({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)("p",null,"In Ada 83, a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint"))," is called a fixed_point_constraint, and a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0050"},"digits_constraint"))," is called a floating_point_constraint. We have adopted other terms because ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0050"},"digits_constraint")),"s apply primarily to decimal fixed point types now (they apply to floating point types only as an obsolescent feature). "),(0,a.kt)("h4",c({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0152-1"),"}"," ",(0,a.kt)("strong",null,"Corrigendum:")," Changed the syntax so that the value following ",(0,a.kt)("strong",null,"delta")," in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-J.3#S0367"},"delta_constraint"))," is a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-4.4#S0138"},"simple_expression")),". This is compatible as any expressions that would require extra parentheses are already illegal. The change is necessary to eliminate syntax ambguities in ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.4#S0035"},"derived_type_definition")),"s. The similar change for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.5#S0050"},"digits_constraint"))," is documented in 3.5.9. "))}y.isMDXComponent=!0}}]);