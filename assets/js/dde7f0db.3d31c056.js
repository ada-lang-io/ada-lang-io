"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[5150],{2264:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>k,contentTitle:()=>g,default:()=>T,frontMatter:()=>h,metadata:()=>f,toc:()=>x});var n=a(1716),i=a(3050),o=a(7318),r=a(4768),d=Object.defineProperty,s=Object.defineProperties,p=Object.getOwnPropertyDescriptors,l=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,m=(e,t,a)=>t in e?d(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,A=(e,t)=>{for(var a in t||(t={}))u.call(t,a)&&m(e,a,t[a]);if(l)for(var a of l(t))c.call(t,a)&&m(e,a,t[a]);return e};const h={sidebar_position:126},g="A.6 Input-Output",f={unversionedId:"arm/AA-A/AA-A.6",id:"arm/AA-A/AA-A.6",title:"A.6 Input-Output",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-A/AA-A.6.mdx",sourceDirName:"arm/AA-A",slug:"/arm/AA-A/AA-A.6",permalink:"/docs/arm/AA-A/AA-A.6",draft:!1,tags:[],version:"current",sidebarPosition:126,frontMatter:{sidebar_position:126},sidebar:"referenceManualSidebar",previous:{title:"A.5 The Numerics Packages",permalink:"/docs/arm/AA-A/AA-A.5"},next:{title:"A.7 External Files and File Objects",permalink:"/docs/arm/AA-A/AA-A.7"}},k={},x=[{value:"Inconsistencies With Ada 83",id:"inconsistencies-with-ada-83",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],y={toc:x};function T(e){var t,a=e,{components:d}=a,m=((e,t)=>{var a={};for(var n in e)u.call(e,n)&&t.indexOf(n)<0&&(a[n]=e[n]);if(null!=e&&l)for(var n of l(e))t.indexOf(n)<0&&c.call(e,n)&&(a[n]=e[n]);return a})(a,["components"]);return(0,n.kt)("wrapper",(t=A(A({},y),m),s(t,p({components:d,mdxType:"MDXLayout"}))),(0,n.kt)("h1",A({},{id:"a6-input-output"}),"A.6 Input-Output"),(0,n.kt)("admonition",A({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,n.kt)("a",A({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)(o.Z,{mdxType:"MarginText"},"1/5"),(0,n.kt)(r.Z,{items:["AI95-00285-01","AI12-0445-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"[ Input-output is provided through language-defined packages, each of which is a child of the root package Ada. The generic packages Sequential_IO and Direct_IO define input-output operations applicable to files containing elements of a given type. The generic package Storage_IO supports reading from and writing to an in-memory buffer. Additional operations for text input-output are supplied in the packages Text_IO, Wide_Text_IO, and Wide_Wide_Text_IO. Heterogeneous input-output is provided through the child packages Streams.Stream_IO and Text_IO.Text_Streams (see also ",(0,n.kt)("a",{href:"../AA-13/AA-13.13"},"13.13"),"). The package IO_Exceptions defines the exceptions used  by the predefined input-output packages.] ",(0,n.kt)("br",null)),(0,n.kt)("h4",A({},{id:"inconsistencies-with-ada-83"}),"Inconsistencies With Ada 83"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"1.a"),(0,n.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The introduction of Append_File as a new element of the enumeration type File_Mode in Sequential_IO and Text_IO, and the introduction of several new declarations in Text_IO, may result in name clashes in the presence of ",(0,n.kt)("strong",null,"use")," clauses. ",(0,n.kt)("br",null)),(0,n.kt)("h4",A({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"1.b"),(0,n.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Text_IO enhancements (Get_Immediate, Look_Ahead, Standard_Error, Modular_IO, Decimal_IO), Wide_Text_IO, and the stream input-output facilities are new in Ada 95. ",(0,n.kt)("br",null)),(0,n.kt)("h4",A({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"1.c"),(0,n.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"RM83-14.6, \u201cLow Level Input-Output\u201d, is removed. This has no semantic effect, since the package was entirely implementation defined, nobody actually implemented it, and if they did, they can always provide it as a vendor-supplied package. ",(0,n.kt)("br",null)),(0,n.kt)("h4",A({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"1.d/2"),(0,n.kt)(r.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,n.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Included package Wide_Wide_Text_IO in this description. ",(0,n.kt)("br",null)))}T.isMDXComponent=!0}}]);