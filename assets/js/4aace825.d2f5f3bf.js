"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[831],{1825:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>y,contentTitle:()=>p,default:()=>T,frontMatter:()=>A,metadata:()=>f,toc:()=>b});var n=a(1716),l=a(3050),i=a(7318),o=a(4768),r=Object.defineProperty,s=Object.defineProperties,u=Object.getOwnPropertyDescriptors,d=Object.getOwnPropertySymbols,m=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,h=(e,t,a)=>t in e?r(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,k=(e,t)=>{for(var a in t||(t={}))m.call(t,a)&&h(e,a,t[a]);if(d)for(var a of d(t))c.call(t,a)&&h(e,a,t[a]);return e};const A={sidebar_position:163},p="D.9 Delay Accuracy",f={unversionedId:"arm/AA-D/AA-D.9",id:"arm/AA-D/AA-D.9",title:"D.9 Delay Accuracy",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-D/AA-D.9.mdx",sourceDirName:"arm/AA-D",slug:"/arm/AA-D/AA-D.9",permalink:"/docs/arm/AA-D/AA-D.9",draft:!1,tags:[],version:"current",sidebarPosition:163,frontMatter:{sidebar_position:163},sidebar:"referenceManualSidebar",previous:{title:"D.8 Monotonic Time",permalink:"/docs/arm/AA-D/AA-D.8"},next:{title:"D.10 Synchronous Task Control",permalink:"/docs/arm/AA-D/AA-D.10"}},y={},b=[{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Documentation Requirements",id:"documentation-requirements",level:4},{value:"Metrics",id:"metrics",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],x={toc:b};function T(e){var t,a=e,{components:r}=a,h=((e,t)=>{var a={};for(var n in e)m.call(e,n)&&t.indexOf(n)<0&&(a[n]=e[n]);if(null!=e&&d)for(var n of d(e))t.indexOf(n)<0&&c.call(e,n)&&(a[n]=e[n]);return a})(a,["components"]);return(0,n.kt)("wrapper",(t=k(k({},x),h),s(t,u({components:r,mdxType:"MDXLayout"}))),(0,n.kt)("h1",k({},{id:"d9-delay-accuracy"}),"D.9 Delay Accuracy"),(0,n.kt)("admonition",k({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,n.kt)("a",k({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)(i.Z,{mdxType:"MarginText"},"1/3"),(0,n.kt)(o.Z,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"[This subclause specifies performance requirements for the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0266"},"delay_statement")),". The rules apply both to ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0268"},"delay_relative_statement"))," and to ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0267"},"delay_until_statement")),". Similarly, they apply equally to a simple ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0266"},"delay_statement"))," and to one which appears in a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.7#S0274"},"delay_alternative")),".] ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,n.kt)(i.Z,{mdxType:"MarginText"},"2"),(0,n.kt)("p",null,"The effect of the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0266"},"delay_statement"))," for Real_Time.Time is defined in terms of Real_Time.Clock: ",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"3"),(0,n.kt)("ul",null,(0,n.kt)("li",null,"If C1 is a value of Clock read before a task executes a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0268"},"delay_relative_statement"))," with duration D, and C2 is a value of Clock read after the task resumes execution following that ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0266"},"delay_statement")),", then C2 \u2013 C1 ",">","= D.",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"4"),(0,n.kt)("li",null,"If C is a value of Clock read after a task resumes execution following a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0267"},"delay_until_statement"))," with Real_Time.Time value T, then C ",">","= T. ",(0,n.kt)("br",null))),(0,n.kt)(i.Z,{mdxType:"MarginText"},"5"),(0,n.kt)("p",null,"A simple ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0266"},"delay_statement"))," with a negative or zero value for the expiration time does not cause the calling task to be blocked; it is nevertheless a potentially blocking operation (see ",(0,n.kt)("a",{href:"../AA-9/AA-9.5#Subclause_9.5.1"},"9.5.1"),").",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"6/3"),(0,n.kt)(o.Z,{items:["AI05-0004-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"When a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0266"},"delay_statement"))," appears in a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.7#S0274"},"delay_alternative"))," of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.7#S0276"},"timed_entry_call"))," the selection of the entry call is attempted, regardless of the specified expiration time. When a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0266"},"delay_statement"))," appears in a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.7#S0272"},"select_alternative")),", and a call is queued on one of the open entries, the selection of that entry call proceeds, regardless of the value of the delay expression. ",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"6.a"),(0,n.kt)(l.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"The effect of these requirements is that one has to always attempt a rendezvous, regardless of the value of the delay expression. This can be tested by issuing a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.7#S0276"},"timed_entry_call"))," with an expiration time of zero, to an open entry. ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"documentation-requirements"}),"Documentation Requirements"),(0,n.kt)(i.Z,{mdxType:"MarginText"},"7"),(0,n.kt)("p",null,"The implementation shall document the minimum value of the delay expression of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0268"},"delay_relative_statement"))," that causes the task to actually be blocked. ",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"7.a/2"),(0,n.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"Documentation Requirement: "),"The minimum value of the delay expression of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0268"},"delay_relative_statement"))," that causes a task to actually be blocked.",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"8"),(0,n.kt)("p",null,"The implementation shall document the minimum difference between the value of the delay expression of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0267"},"delay_until_statement"))," and the value of Real_Time.Clock, that causes the task to actually be blocked. ",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"8.a/2"),(0,n.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("em",null,"This paragraph was deleted."),(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"8.b/2"),(0,n.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"Documentation Requirement: "),"The minimum difference between the value of the delay expression of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0267"},"delay_until_statement"))," and the value of Real_Time.Clock, that causes the task to actually be blocked.",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"metrics"}),"Metrics"),(0,n.kt)(i.Z,{mdxType:"MarginText"},"9"),(0,n.kt)("p",null,"The implementation shall document the following metrics: ",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"10"),(0,n.kt)("ul",null,(0,n.kt)("li",null,"An upper bound on the execution time, in processor clock cycles, of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0268"},"delay_relative_statement"))," whose requested value of the delay expression is less than or equal to zero.",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"11"),(0,n.kt)("li",null,"An upper bound on the execution time, in processor clock cycles, of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0267"},"delay_until_statement"))," whose requested value of the delay expression is less than or equal to the value of Real_Time.Clock at the time of executing the statement. Similarly, for Calendar.Clock.",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"12/5"),(0,n.kt)(o.Z,{items:["AI12-0445-1"],mdxType:"MarginInfo"}),(0,n.kt)("li",null,"An upper bound on the ",(0,n.kt)("em",null,"lateness")," of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0268"},"delay_relative_statement")),", for a positive value of the delay expression, in a situation where the task has sufficient priority to preempt the processor as soon as it becomes ready, and can proceed without waiting  for any other execution resources. The upper bound is expressed as a function of the value of the delay expression. The lateness is obtained by subtracting the value of the delay expression from the ",(0,n.kt)("em",null,"actual duration"),". The actual duration is measured from a point immediately before a task executes the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0266"},"delay_statement"))," to a point immediately after the task resumes execution following this statement.",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"13/5"),(0,n.kt)(o.Z,{items:["AI12-0445-1"],mdxType:"MarginInfo"}),(0,n.kt)("li",null,"An upper bound on the lateness of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0267"},"delay_until_statement")),", in a situation where the value of the requested expiration time is after the time the task begins executing the statement, the task has sufficient priority to preempt the processor as soon as it becomes ready, and it can proceed without waiting  for any other execution resources. The upper bound is expressed as a function of the difference between the requested expiration time and the clock value at the time the statement begins execution. The lateness of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0267"},"delay_until_statement"))," is obtained by subtracting the requested expiration time from the real time that the task resumes execution following this statement. ",(0,n.kt)("br",null))),(0,n.kt)(i.Z,{mdxType:"MarginText"},"13.a/2"),(0,n.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"Documentation Requirement: "),"The metrics for delay statements.",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,n.kt)(i.Z,{mdxType:"MarginText"},"14.a"),(0,n.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The rules regarding a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.7#S0276"},"timed_entry_call"))," with a very small positive Duration value, have been tightened to always require the check whether the rendezvous is immediately possible.",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,n.kt)(i.Z,{mdxType:"MarginText"},"14.b/2"),(0,n.kt)(o.Z,{items:["AI95-00355-01"],mdxType:"MarginInfo"}),(0,n.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The note about \u201cvoluntary round-robin\u2019, while still true, has been deleted as potentially confusing as it is describing a different kind of round-robin than is defined by the round-robin dispatching policy.",(0,n.kt)("br",null)))}T.isMDXComponent=!0}}]);