"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8932],{1102:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>g,contentTitle:()=>C,default:()=>_,frontMatter:()=>h,metadata:()=>f,toc:()=>A});var t=r(1716),a=r(7556),i=r(3183),o=Object.defineProperty,s=Object.defineProperties,c=Object.getOwnPropertyDescriptors,d=Object.getOwnPropertySymbols,l=Object.prototype.hasOwnProperty,m=Object.prototype.propertyIsEnumerable,u=(e,n,r)=>n in e?o(e,n,{enumerable:!0,configurable:!0,writable:!0,value:r}):e[n]=r,p=(e,n)=>{for(var r in n||(n={}))l.call(n,r)&&u(e,r,n[r]);if(d)for(var r of d(n))m.call(n,r)&&u(e,r,n[r]);return e};const h={sidebar_position:204},C="J.14 Character and Wide_Character Conversion Functions",f={unversionedId:"arm/AA-J/AA-J.14",id:"arm/AA-J/AA-J.14",title:"J.14 Character and Wide_Character Conversion Functions",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-J/AA-J.14.mdx",sourceDirName:"arm/AA-J",slug:"/arm/AA-J/AA-J.14",permalink:"/docs/arm/AA-J/AA-J.14",draft:!1,tags:[],version:"current",sidebarPosition:204,frontMatter:{sidebar_position:204},sidebar:"referenceManualSidebar",previous:{title:"J.13 Dependence Restriction Identifiers",permalink:"/docs/arm/AA-J/AA-J.13"},next:{title:"J.15 Aspect-related Pragmas",permalink:"/docs/arm/AA-J/AA-J.15"}},g={},A=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],k={toc:A};function _(e){var n,r=e,{components:o}=r,u=((e,n)=>{var r={};for(var t in e)l.call(e,t)&&n.indexOf(t)<0&&(r[t]=e[t]);if(null!=e&&d)for(var t of d(e))n.indexOf(t)<0&&m.call(e,t)&&(r[t]=e[t]);return r})(r,["components"]);return(0,t.kt)("wrapper",(n=p(p({},k),u),s(n,c({components:o,mdxType:"MDXLayout"}))),(0,t.kt)("h1",p({},{id:"j14-character-and-wide_character-conversion-functions"}),"J.14 Character and Wide_Character Conversion Functions"),(0,t.kt)("admonition",p({},{type:"warning"}),(0,t.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,t.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,t.kt)("h4",p({},{id:"static-semantics"}),"Static Semantics"),(0,t.kt)("p",null,"{",(0,t.kt)("em",null,"AI95-00395-01"),"}"," The following declarations exist in the declaration of package Ada.Characters.Handling:"),(0,t.kt)(a.Z,{mdxType:"CodeBlock"},"function Is_Character (Item : in Wide_Character) return Boolean","\n","      renames Conversions.Is_Character;","\n","   function Is_String    (Item : in Wide_String)    return Boolean","\n","      renames Conversions.Is_String;","\n"),(0,t.kt)(a.Z,{mdxType:"CodeBlock"},"function To_Character (Item       : in Wide_Character;","\n","                         Substitute : in Character := ' ')","\n","                         return Character","\n","      renames Conversions.To_Character;","\n"),(0,t.kt)(a.Z,{mdxType:"CodeBlock"},"function To_String    (Item       : in Wide_String;","\n","                          Substitute : in Character := ' ')","\n","                          return String","\n","      renames Conversions.To_String;","\n"),(0,t.kt)(a.Z,{mdxType:"CodeBlock"},"function To_Wide_Character (Item : in Character) return Wide_Character","\n","      renames Conversions.To_Wide_Character;","\n"),(0,t.kt)(a.Z,{mdxType:"CodeBlock"},"function To_Wide_String    (Item : in String)    return Wide_String","\n","      renames Conversions.To_Wide_String;","\n"),(0,t.kt)("h4",p({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,t.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,t.kt)("p",null,"{",(0,t.kt)("em",null,"AI95-00394-01"),"}"," ","{",(0,t.kt)("em",null,"AI05-0299-1"),"}"," This subclause is new. These subprograms were moved to Characters.Conversions (see A.3.4). ")))}_.isMDXComponent=!0}}]);