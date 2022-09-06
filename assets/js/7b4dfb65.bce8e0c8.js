"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8300],{4507:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>g,contentTitle:()=>h,default:()=>_,frontMatter:()=>f,metadata:()=>m,toc:()=>b});var a=n(1716),i=n(7556),r=Object.defineProperty,o=Object.defineProperties,l=Object.getOwnPropertyDescriptors,s=Object.getOwnPropertySymbols,d=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,u=(e,t,n)=>t in e?r(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,p=(e,t)=>{for(var n in t||(t={}))d.call(t,n)&&u(e,n,t[n]);if(s)for(var n of s(t))c.call(t,n)&&u(e,n,t[n]);return e};const f={sidebar_position:124},h="A.7  External Files and File Objects",m={unversionedId:"arm/AA-A.7",id:"arm/AA-A.7",title:"A.7  External Files and File Objects",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-A.7.mdx",sourceDirName:"arm",slug:"/arm/AA-A.7",permalink:"/docs/arm/AA-A.7",draft:!1,tags:[],version:"current",sidebarPosition:124,frontMatter:{sidebar_position:124},sidebar:"tutorialSidebar",previous:{title:"A.6  Input-Output",permalink:"/docs/arm/AA-A.6"},next:{title:"A.8  Sequential and Direct Files",permalink:"/docs/arm/AA-A.8"}},g={},b=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],k={toc:b};function _(e){var t,n=e,{components:r}=n,u=((e,t)=>{var n={};for(var a in e)d.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&s)for(var a of s(e))t.indexOf(a)<0&&c.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=p(p({},k),u),o(t,l({components:r,mdxType:"MDXLayout"}))),(0,a.kt)("h1",p({},{id:"a7--external-files-and-file-objects"}),"A.7  External Files and File Objects"),(0,a.kt)("admonition",p({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,a.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("h4",p({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)("p",null,"Values input from the external environment of the program, or output to the external environment, are considered to occupy ",(0,a.kt)("em",null,"external files"),". An external file can be anything external to the program that can produce a value to be read or receive a value to be written. An external file is identified by a string (the ",(0,a.kt)("em",null,"name"),"). A second string (the ",(0,a.kt)("em",null,"form"),") gives further system-dependent characteristics that may be associated with the file, such as the physical organization or access rights. The conventions governing the interpretation of such strings shall be documented."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," Input and output operations are expressed as operations on objects of some ",(0,a.kt)("em",null,"file type"),", rather than directly in terms of the external files. In the remainder of this clause, the term ",(0,a.kt)("em",null,"file")," is always used to refer to a file object; the term ",(0,a.kt)("em",null,"external file")," is used otherwise."),(0,a.kt)("p",null,"Input-output for sequential files of values of a single element type is defined by means of the generic package Sequential_IO. In order to define sequential input-output for a given element type, an instantiation of this generic unit, with the given type as actual parameter, has to be declared. The resulting package contains the declaration of a file type (called File_Type) for files of such elements, as well as the operations applicable to these files, such as the Open, Read, and Write procedures."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," Input-output for direct access files is likewise defined by a generic package called Direct_IO. Input-output in human-readable form is defined by the (nongeneric) packages Text_IO for Character and String data, Wide_Text_IO for Wide_Character and Wide_String data, and Wide_Wide_Text_IO for Wide_Wide_Character and Wide_Wide_String data. Input-output for files containing streams of elements representing values of possibly different types is defined by means of the (nongeneric) package Streams.Stream_IO."),(0,a.kt)("p",null,"Before input or output operations can be performed on a file, the file first has to be associated with an external file. While such an association is in effect, the file is said to be ",(0,a.kt)("em",null,"open"),", and otherwise the file is said to be ",(0,a.kt)("em",null,"closed"),"."),(0,a.kt)("p",null,"The language does not define what happens to external files after the completion of the main program and all the library tasks (in particular, if corresponding files have not been closed). The effect of input-output for access types is unspecified."),(0,a.kt)("p",null,"An open file has a ",(0,a.kt)("em",null,"current mode"),", which is a value of one of the following enumeration types: "),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"type File_Mode is (In_File, Inout_File, Out_File);  --  for Direct_IO","\n"),(0,a.kt)("p",null,"These values correspond respectively to the cases where only reading, both reading and writing, or only writing are to be performed. "),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},"{","AI95-00285-01","}"," type File_Mode is (In_File, Out_File, Append_File);","\n","--  for Sequential_IO, Text_IO, Wide_Text_IO, Wide_Wide_Text_IO, and Stream_IO","\n"),(0,a.kt)("p",null,"These values correspond respectively to the cases where only reading, only writing, or only appending are to be performed."),(0,a.kt)("p",null,"The mode of a file can be changed. "),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," Several file management operations are common to Sequential_IO, Direct_IO, Text_IO, Wide_Text_IO, and Wide_Wide_Text_IO. These operations are described in subclause A.8.2 for sequential and direct files. Any additional effects concerning text input-output are described in subclause A.10.2."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," The exceptions that can be propagated by the execution of an input-output subprogram are defined in the package IO_Exceptions; the situations in which they can be propagated are described following the description of the subprogram (and in subclause A.13). The exceptions Storage_Error and Program_Error may be propagated. (Program_Error can only be propagated due to errors made by the caller of the subprogram.) Finally, exceptions can be propagated in certain implementation-defined situations. "),(0,a.kt)("p",null,(0,a.kt)("em",null,"This paragraph was deleted.")),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Discussion: "),'The last sentence here is referring to the documentation requirements in A.13, "Exceptions in Input-Output", and the documentation summary item is provided there. '),(0,a.kt)("p",null,"NOTE 1   ","{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," Each instantiation of the generic packages Sequential_IO and Direct_IO declares a different type File_Type. In the case of Text_IO, Wide_Text_IO, Wide_Wide_Text_IO, and Streams.Stream_IO, the corresponding type File_Type is unique."),(0,a.kt)("p",null,"NOTE 2   ","{",(0,a.kt)("em",null,"AI12-0440-1"),"}"," A bidirectional device can often be modeled as two sequential files associated with the device, one of mode In_File, and one of mode Out_File. An implementation can restrict the number of files that can be associated with a given external file. "),(0,a.kt)("h4",p({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," Included package Wide_Wide_Text_IO in this description. "))}_.isMDXComponent=!0}}]);