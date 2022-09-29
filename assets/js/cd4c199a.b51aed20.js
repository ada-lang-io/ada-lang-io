"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6272],{960:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>A,contentTitle:()=>g,default:()=>T,frontMatter:()=>x,metadata:()=>k,toc:()=>y});var r=t(1716),o=t(8604),a=t(7318),i=t(4768),s=Object.defineProperty,d=Object.defineProperties,u=Object.getOwnPropertyDescriptors,p=Object.getOwnPropertySymbols,m=Object.prototype.hasOwnProperty,l=Object.prototype.propertyIsEnumerable,f=(e,n,t)=>n in e?s(e,n,{enumerable:!0,configurable:!0,writable:!0,value:t}):e[n]=t,c=(e,n)=>{for(var t in n||(n={}))m.call(n,t)&&f(e,t,n[t]);if(p)for(var t of p(n))l.call(n,t)&&f(e,t,n[t]);return e};const x={sidebar_position:84},g="9.11 Example of Tasking and Synchronization",k={unversionedId:"arm/AA-9/AA-9.11",id:"arm/AA-9/AA-9.11",title:"9.11 Example of Tasking and Synchronization",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-9/AA-9.11.mdx",sourceDirName:"arm/AA-9",slug:"/arm/AA-9/AA-9.11",permalink:"/docs/arm/AA-9/AA-9.11",draft:!1,tags:[],version:"current",sidebarPosition:84,frontMatter:{sidebar_position:84},sidebar:"referenceManualSidebar",previous:{title:"9.10 Shared Variables",permalink:"/docs/arm/AA-9/AA-9.10"},next:{title:"10 Program Structure and Compilation Issues",permalink:"/docs/arm/AA-10/"}},A={},y=[{value:"Examples",id:"examples",level:4}],h={toc:y};function T(e){var n,t=e,{components:s}=t,f=((e,n)=>{var t={};for(var r in e)m.call(e,r)&&n.indexOf(r)<0&&(t[r]=e[r]);if(null!=e&&p)for(var r of p(e))n.indexOf(r)<0&&l.call(e,r)&&(t[r]=e[r]);return t})(t,["components"]);return(0,r.kt)("wrapper",(n=c(c({},h),f),d(n,u({components:s,mdxType:"MDXLayout"}))),(0,r.kt)("h1",c({},{id:"911-example-of-tasking-and-synchronization"}),"9.11 Example of Tasking and Synchronization"),(0,r.kt)("admonition",c({},{type:"warning"}),(0,r.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,r.kt)("a",c({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,r.kt)("h4",c({},{id:"examples"}),"Examples"),(0,r.kt)(a.Z,{mdxType:"MarginText"},"1/5"),(0,r.kt)(i.Z,{items:["AI12-0440-1"],mdxType:"MarginInfo"}),(0,r.kt)("p",null,"The following example defines a buffer protected object to smooth variations between the speed of output of a producing task and the speed of input of some consuming task. For instance, the producing task can  have the following structure:",(0,r.kt)("br",null)),(0,r.kt)(a.Z,{mdxType:"MarginText"},"2"),(0,r.kt)(o.Z,{language:"ada",mdxType:"CodeBlock"},"task Producer;","\n",(0,r.kt)(a.Z,{mdxType:"MarginText"},"3/2"),(0,r.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"task body Producer is","\n","   Person : Person_Name; -- see ",(0,r.kt)("a",{href:"../AA-3/AA-3.10#Subclause_3.10.1"},"3.10.1"),"\n","begin","\n","   loop","\n","      ... --  simulate arrival of the next customer","\n","      Buffer.Append_Wait(Person);","\n","      exit when Person = null;","\n","   end loop;","\n","end Producer;","\n"),(0,r.kt)(a.Z,{mdxType:"MarginText"},"4/5"),(0,r.kt)(i.Z,{items:["AI12-0440-1"],mdxType:"MarginInfo"}),(0,r.kt)("p",null,"and the consuming task can  have the following structure:",(0,r.kt)("br",null)),(0,r.kt)(a.Z,{mdxType:"MarginText"},"5"),(0,r.kt)(o.Z,{language:"ada",mdxType:"CodeBlock"},"task Consumer;","\n",(0,r.kt)(a.Z,{mdxType:"MarginText"},"6/2"),(0,r.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"task body Consumer is","\n","   Person : Person_Name;","\n","begin","\n","   loop","\n","      Buffer.Remove_First_Wait(Person);","\n","      exit when Person = null;","\n","      ... --  simulate serving a customer","\n","   end loop;","\n","end Consumer;","\n"),(0,r.kt)(a.Z,{mdxType:"MarginText"},"7/2"),(0,r.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),(0,r.kt)("p",null,"The buffer object contains an internal array of person names managed in a round-robin fashion. The array has two indices, an In_Index denoting the index for the next input person name and an Out_Index denoting the index for the next output person name.",(0,r.kt)("br",null)),(0,r.kt)(a.Z,{mdxType:"MarginText"},"7.1/2"),(0,r.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),(0,r.kt)("p",null,"The Buffer is defined as an extension of the Synchronized_Queue interface (see ",(0,r.kt)("a",{href:"../AA-3/AA-3.9#Subclause_3.9.4"},"3.9.4"),"), and as such promises to implement the abstraction defined by that interface. By doing so, the Buffer can be passed to the Transfer class-wide operation defined for objects of a type covered by Queue'Class.",(0,r.kt)("br",null)),(0,r.kt)(a.Z,{mdxType:"MarginText"},"7.2/5"),(0,r.kt)(i.Z,{items:["AI12-0178-1"],mdxType:"MarginInfo"}),(0,r.kt)(o.Z,{language:"ada",mdxType:"CodeBlock"},"type Person_Name_Array is array (Positive range ","<",">",")","\n","   of Person_Name;  -- see ",(0,r.kt)("a",{href:"../AA-3/AA-3.10#Subclause_3.10.1"},"3.10.1"),"\n",(0,r.kt)(a.Z,{mdxType:"MarginText"},"8/2"),(0,r.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"protected Buffer is new Synchronized_Queue with  -- see ",(0,r.kt)("a",{href:"../AA-3/AA-3.9#Subclause_3.9.4"},"3.9.4"),"\n","   entry Append_Wait(Person : in Person_Name);","\n","   entry Remove_First_Wait(Person : out Person_Name);","\n","   function Cur_Count return Natural;","\n","   function Max_Count return Natural;","\n","   procedure Append(Person : in Person_Name);","\n","   procedure Remove_First(Person : out Person_Name);","\n","private","\n","   Pool      : Person_Name_Array(1 .. 100);","\n","   Count     : Natural := 0;","\n","   In_Index, Out_Index : Positive := 1;","\n","end Buffer;","\n",(0,r.kt)(a.Z,{mdxType:"MarginText"},"9/2"),(0,r.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"protected body Buffer is","\n","   entry Append_Wait(Person : in Person_Name)","\n","      when Count ","<"," Pool'Length is","\n","   begin","\n","      Append(Person);","\n","   end Append_Wait;","\n",(0,r.kt)(a.Z,{mdxType:"MarginText"},"9.1/2"),(0,r.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"procedure Append(Person : in Person_Name) is","\n","   begin","\n","      if Count = Pool'Length then","\n",'         raise Queue_Error with "Buffer Full";  -- see ',(0,r.kt)("a",{href:"../AA-11/AA-11.3"},"11.3"),"\n","      end if;","\n","      Pool(In_Index) := Person;","\n","      In_Index       := (In_Index mod Pool'Length) + 1;","\n","      Count          := Count + 1;","\n","   end Append;","\n",(0,r.kt)(a.Z,{mdxType:"MarginText"},"10/2"),(0,r.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"entry Remove_First_Wait(Person : out Person_Name)","\n","      when Count ",">"," 0 is","\n","   begin","\n","      Remove_First(Person);","\n","   end Remove_First_Wait;","\n",(0,r.kt)(a.Z,{mdxType:"MarginText"},"11/2"),(0,r.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"procedure Remove_First(Person : out Person_Name) is","\n","   begin","\n","      if Count = 0 then","\n",'         raise Queue_Error with "Buffer Empty"; -- see ',(0,r.kt)("a",{href:"../AA-11/AA-11.3"},"11.3"),"\n","      end if;","\n","      Person    := Pool(Out_Index);","\n","      Out_Index := (Out_Index mod Pool'Length) + 1;","\n","      Count     := Count - 1;","\n","   end Remove_First;","\n",(0,r.kt)(a.Z,{mdxType:"MarginText"},"12/2"),(0,r.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"function Cur_Count return Natural is","\n","   begin","\n","       return Buffer.Count;","\n","   end Cur_Count;","\n",(0,r.kt)(a.Z,{mdxType:"MarginText"},"13/2"),(0,r.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"function Max_Count return Natural is","\n","   begin","\n","       return Pool'Length;","\n","   end Max_Count;","\n","end Buffer;","\n"))}T.isMDXComponent=!0}}]);