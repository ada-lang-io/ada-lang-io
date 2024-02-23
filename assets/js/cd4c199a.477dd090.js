"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[725],{85020:(e,n,o)=>{o.r(n),o.d(n,{assets:()=>m,contentTitle:()=>u,default:()=>l,frontMatter:()=>d,metadata:()=>g,toc:()=>p});var r=o(58168),a=(o(96540),o(15680)),t=(o(20793),o(91435),o(21432)),i=o(79162),s=o(34421);const d={sidebar_position:84},u="9.11 Example of Tasking and Synchronization",g={unversionedId:"arm/AA-9/AA-9.11",id:"arm/AA-9/AA-9.11",title:"9.11 Example of Tasking and Synchronization",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-9/AA-9.11.mdx",sourceDirName:"arm/AA-9",slug:"/arm/AA-9/AA-9.11",permalink:"/docs/arm/AA-9/AA-9.11",draft:!1,tags:[],version:"current",sidebarPosition:84,frontMatter:{sidebar_position:84},sidebar:"referenceManualSidebar",previous:{title:"9.10 Shared Variables",permalink:"/docs/arm/AA-9/AA-9.10"},next:{title:"10 Program Structure and Compilation Issues",permalink:"/docs/arm/AA-10/"}},m={},p=[{value:"Examples",id:"examples",level:4}],y={toc:p},A="wrapper";function l(e){let{components:n,...o}=e;return(0,a.yg)(A,(0,r.A)({},y,o,{components:n,mdxType:"MDXLayout"}),(0,a.yg)("h1",{id:"911-example-of-tasking-and-synchronization"},"9.11 Example of Tasking and Synchronization"),(0,a.yg)("admonition",{type:"warning"},(0,a.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,a.yg)("h4",{id:"examples"},"Examples"),(0,a.yg)(i.A,{mdxType:"MarginText"},"1/5"),(0,a.yg)(s.A,{items:["AI12-0440-1"],mdxType:"MarginInfo"}),(0,a.yg)("p",null,"The following example defines a buffer protected object to smooth variations between the speed of output of a producing task and the speed of input of some consuming task. For instance, the producing task can have the following structure:",(0,a.yg)("br",null)),(0,a.yg)(i.A,{mdxType:"MarginText"},"2"),(0,a.yg)(t.A,{language:"ada",mdxType:"CodeBlock"},"task Producer;","\n",(0,a.yg)(i.A,{mdxType:"MarginText"},"3/2"),(0,a.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"task body Producer is","\n","   Person : Person","_","Name; -- see ",(0,a.yg)("a",{href:"../AA-3/AA-3.10#Subclause_3.10.1"},"3.10.1"),"\n","begin","\n","   loop","\n","      ... --  simulate arrival of the next customer","\n","      Buffer.Append","_","Wait(Person);","\n","      exit when Person = null;","\n","   end loop;","\n","end Producer;","\n"),(0,a.yg)(i.A,{mdxType:"MarginText"},"4/5"),(0,a.yg)(s.A,{items:["AI12-0440-1"],mdxType:"MarginInfo"}),(0,a.yg)("p",null,"and the consuming task can have the following structure:",(0,a.yg)("br",null)),(0,a.yg)(i.A,{mdxType:"MarginText"},"5"),(0,a.yg)(t.A,{language:"ada",mdxType:"CodeBlock"},"task Consumer;","\n",(0,a.yg)(i.A,{mdxType:"MarginText"},"6/2"),(0,a.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"task body Consumer is","\n","   Person : Person","_","Name;","\n","begin","\n","   loop","\n","      Buffer.Remove","_","First","_","Wait(Person);","\n","      exit when Person = null;","\n","      ... --  simulate serving a customer","\n","   end loop;","\n","end Consumer;","\n"),(0,a.yg)(i.A,{mdxType:"MarginText"},"7/2"),(0,a.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),(0,a.yg)("p",null,"The buffer object contains an internal array of person names managed in a round-robin fashion. The array has two indices, an In","_","Index denoting the index for the next input person name and an Out","_","Index denoting the index for the next output person name.",(0,a.yg)("br",null)),(0,a.yg)(i.A,{mdxType:"MarginText"},"7.1/2"),(0,a.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),(0,a.yg)("p",null,"The Buffer is defined as an extension of the Synchronized","_","Queue interface (see ",(0,a.yg)("a",{href:"../AA-3/AA-3.9#Subclause_3.9.4"},"3.9.4"),"), and as such promises to implement the abstraction defined by that interface. By doing so, the Buffer can be passed to the Transfer class-wide operation defined for objects of a type covered by Queue'Class.",(0,a.yg)("br",null)),(0,a.yg)(i.A,{mdxType:"MarginText"},"7.2/5"),(0,a.yg)(s.A,{items:["AI12-0178-1"],mdxType:"MarginInfo"}),(0,a.yg)(t.A,{language:"ada",mdxType:"CodeBlock"},"type Person","_","Name","_","Array is array (Positive range ","<",">",")","\n","   of Person","_","Name;  -- see ",(0,a.yg)("a",{href:"../AA-3/AA-3.10#Subclause_3.10.1"},"3.10.1"),"\n",(0,a.yg)(i.A,{mdxType:"MarginText"},"8/2"),(0,a.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"protected Buffer is new Synchronized","_","Queue with  -- see ",(0,a.yg)("a",{href:"../AA-3/AA-3.9#Subclause_3.9.4"},"3.9.4"),"\n","   entry Append","_","Wait(Person : in Person","_","Name);","\n","   entry Remove","_","First","_","Wait(Person : out Person","_","Name);","\n","   function Cur","_","Count return Natural;","\n","   function Max","_","Count return Natural;","\n","   procedure Append(Person : in Person","_","Name);","\n","   procedure Remove","_","First(Person : out Person","_","Name);","\n","private","\n","   Pool      : Person","_","Name","_","Array(1 .. 100);","\n","   Count     : Natural := 0;","\n","   In","_","Index, Out","_","Index : Positive := 1;","\n","end Buffer;","\n",(0,a.yg)(i.A,{mdxType:"MarginText"},"9/2"),(0,a.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"protected body Buffer is","\n","   entry Append","_","Wait(Person : in Person","_","Name)","\n","      when Count ","<"," Pool'Length is","\n","   begin","\n","      Append(Person);","\n","   end Append","_","Wait;","\n",(0,a.yg)(i.A,{mdxType:"MarginText"},"9.1/2"),(0,a.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"procedure Append(Person : in Person","_","Name) is","\n","   begin","\n","      if Count = Pool'Length then","\n","         raise Queue","_",'Error with "Buffer Full";  -- see ',(0,a.yg)("a",{href:"../AA-11/AA-11.3"},"11.3"),"\n","      end if;","\n","      Pool(In","_","Index) := Person;","\n","      In","_","Index       := (In","_","Index mod Pool'Length) + 1;","\n","      Count          := Count + 1;","\n","   end Append;","\n",(0,a.yg)(i.A,{mdxType:"MarginText"},"10/2"),(0,a.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"entry Remove","_","First","_","Wait(Person : out Person","_","Name)","\n","      when Count ",">"," 0 is","\n","   begin","\n","      Remove","_","First(Person);","\n","   end Remove","_","First","_","Wait;","\n",(0,a.yg)(i.A,{mdxType:"MarginText"},"11/2"),(0,a.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"procedure Remove","_","First(Person : out Person","_","Name) is","\n","   begin","\n","      if Count = 0 then","\n","         raise Queue","_",'Error with "Buffer Empty"; -- see ',(0,a.yg)("a",{href:"../AA-11/AA-11.3"},"11.3"),"\n","      end if;","\n","      Person    := Pool(Out","_","Index);","\n","      Out","_","Index := (Out","_","Index mod Pool'Length) + 1;","\n","      Count     := Count - 1;","\n","   end Remove","_","First;","\n",(0,a.yg)(i.A,{mdxType:"MarginText"},"12/2"),(0,a.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"function Cur","_","Count return Natural is","\n","   begin","\n","       return Buffer.Count;","\n","   end Cur","_","Count;","\n",(0,a.yg)(i.A,{mdxType:"MarginText"},"13/2"),(0,a.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"function Max","_","Count return Natural is","\n","   begin","\n","       return Pool'Length;","\n","   end Max","_","Count;","\n","end Buffer;","\n"))}l.isMDXComponent=!0}}]);