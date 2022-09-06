"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6526],{476:(e,t,i)=>{i.r(t),i.d(t,{assets:()=>f,contentTitle:()=>c,default:()=>k,frontMatter:()=>d,metadata:()=>g,toc:()=>h});var n=i(1716),o=Object.defineProperty,r=Object.defineProperties,s=Object.getOwnPropertyDescriptors,a=Object.getOwnPropertySymbols,l=Object.prototype.hasOwnProperty,m=Object.prototype.propertyIsEnumerable,p=(e,t,i)=>t in e?o(e,t,{enumerable:!0,configurable:!0,writable:!0,value:i}):e[t]=i,u=(e,t)=>{for(var i in t||(t={}))l.call(t,i)&&p(e,i,t[i]);if(a)for(var i of a(t))m.call(t,i)&&p(e,i,t[i]);return e};const d={sidebar_position:168},c="Annex E Distributed Systems",g={unversionedId:"arm/AA-E",id:"arm/AA-E",title:"Annex E Distributed Systems",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-E.mdx",sourceDirName:"arm",slug:"/arm/AA-E",permalink:"/docs/arm/AA-E",draft:!1,tags:[],version:"current",sidebarPosition:168,frontMatter:{sidebar_position:168},sidebar:"tutorialSidebar",previous:{title:"D.16  Multiprocessor Implementation",permalink:"/docs/arm/AA-D.16"},next:{title:"E.1  Partitions",permalink:"/docs/arm/AA-E.1"}},f={},h=[{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Post-Compilation Rules",id:"post-compilation-rules",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4}],b={toc:h};function k(e){var t,i=e,{components:o}=i,p=((e,t)=>{var i={};for(var n in e)l.call(e,n)&&t.indexOf(n)<0&&(i[n]=e[n]);if(null!=e&&a)for(var n of a(e))t.indexOf(n)<0&&m.call(e,n)&&(i[n]=e[n]);return i})(i,["components"]);return(0,n.kt)("wrapper",(t=u(u({},b),p),r(t,s({components:o,mdxType:"MDXLayout"}))),(0,n.kt)("h1",u({},{id:"annex-e-distributed-systems"}),"Annex E Distributed Systems"),(0,n.kt)("admonition",u({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,n.kt)("a",u({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)("p",null,"[This Annex defines facilities for supporting the implementation of distributed systems using multiple partitions working cooperatively as part of a single Ada program.] "),(0,n.kt)("h4",u({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,n.kt)("p",null,"This Annex is new to Ada 95. "),(0,n.kt)("h4",u({},{id:"post-compilation-rules"}),"Post-Compilation Rules"),(0,n.kt)("p",null,"A ",(0,n.kt)("em",null,"distributed system")," is an interconnection of one or more ",(0,n.kt)("em",null,"processing nodes")," (a system resource that has both computational and storage capabilities), and zero or more ",(0,n.kt)("em",null,"storage nodes")," (a system resource that has only storage capabilities, with the storage addressable by one or more processing nodes)."),(0,n.kt)("p",null,"A ",(0,n.kt)("em",null,"distributed program")," comprises one or more partitions that execute independently (except when they communicate) in a distributed system."),(0,n.kt)("p",null,"The process of mapping the partitions of a program to the nodes in a distributed system is called ",(0,n.kt)("em",null,"configuring the partitions of the program"),". "),(0,n.kt)("h4",u({},{id:"implementation-requirements"}),"Implementation Requirements"),(0,n.kt)("p",null,"The implementation shall provide means for explicitly assigning library units to a partition and for the configuring and execution of a program consisting of multiple partitions on a distributed system; the means are implementation defined. "),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Implementation defined: "),"The means for creating and executing distributed programs."),(0,n.kt)("h4",u({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,n.kt)("p",null,"An implementation may require that the set of processing nodes of a distributed system be homogeneous."),(0,n.kt)("p",null,"NOTE 1   ","{",(0,n.kt)("em",null,"AI12-0440-1"),"}"," The partitions comprising a program can be executed on differently configured distributed systems or on a nondistributed system without requiring recompilation. A distributed program can be partitioned differently from the same set of library units without recompilation. The resulting execution is semantically equivalent."),(0,n.kt)("p",null,"NOTE 2   A distributed program retains the same type safety as the equivalent single partition program."))}k.isMDXComponent=!0}}]);