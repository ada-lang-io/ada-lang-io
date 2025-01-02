"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[725],{7438:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>l,contentTitle:()=>c,default:()=>x,frontMatter:()=>u,metadata:()=>r,toc:()=>h});const r=JSON.parse('{"id":"arm/AA-9/AA-9.11","title":"9.11 Example of Tasking and Synchronization","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-9/AA-9.11.mdx","sourceDirName":"arm/AA-9","slug":"/arm/AA-9/AA-9.11","permalink":"/docs/arm/AA-9/AA-9.11","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":84,"frontMatter":{"sidebar_position":84},"sidebar":"referenceManualSidebar","previous":{"title":"9.10 Shared Variables","permalink":"/docs/arm/AA-9/AA-9.10"},"next":{"title":"10 Program Structure and Compilation Issues","permalink":"/docs/arm/AA-10/"}}');var o=s(4848),i=s(8453),t=(s(3842),s(1435),s(1432)),a=s(9162),d=s(4421);const u={sidebar_position:84},c="9.11 Example of Tasking and Synchronization",l={},h=[{value:"Examples",id:"examples",level:4}];function A(e){const n={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,i.R)(),...e.components};return(0,o.jsxs)(o.Fragment,{children:[(0,o.jsx)(n.header,{children:(0,o.jsx)(n.h1,{id:"911-example-of-tasking-and-synchronization",children:"9.11 Example of Tasking and Synchronization"})}),"\n",(0,o.jsx)(n.admonition,{type:"danger",children:(0,o.jsxs)(n.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,o.jsx)(n.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,o.jsx)(n.h4,{id:"examples",children:"Examples"}),"\n",(0,o.jsx)(a.A,{children:"1/5"}),"\n",(0,o.jsx)(d.A,{items:["AI12-0440-1"]}),"\n",(0,o.jsxs)("p",{children:["The following example defines a buffer protected object to smooth variations between the speed of output of a producing task and the speed of input of some consuming task. For instance, the producing task can have the following structure:",(0,o.jsx)("br",{})]}),"\n",(0,o.jsx)(a.A,{children:"2"}),"\n",(0,o.jsxs)(t.A,{language:"ada",children:[(0,o.jsxs)(n.p,{children:["task Producer;","\n","\n",(0,o.jsx)(a.A,{children:"3/2"})]}),(0,o.jsx)(d.A,{items:["AI95-00433-01"]}),(0,o.jsxs)(n.p,{children:["task body Producer is","\n","   Person : Person","_","Name; -- see ",(0,o.jsx)("a",{href:"/docs/arm/AA-3/AA-3.10#Subclause_3.10.1",children:"3.10.1"}),"\n","begin","\n","   loop","\n","      ... --  simulate arrival of the next customer","\n","      Buffer.Append","_","Wait(Person);","\n","      exit when Person = null;","\n","   end loop;","\n","end Producer;","\n"]})]}),"\n",(0,o.jsx)(a.A,{children:"4/5"}),"\n",(0,o.jsx)(d.A,{items:["AI12-0440-1"]}),"\n",(0,o.jsxs)("p",{children:["and the consuming task can have the following structure:",(0,o.jsx)("br",{})]}),"\n",(0,o.jsx)(a.A,{children:"5"}),"\n",(0,o.jsxs)(t.A,{language:"ada",children:[(0,o.jsxs)(n.p,{children:["task Consumer;","\n","\n",(0,o.jsx)(a.A,{children:"6/2"})]}),(0,o.jsx)(d.A,{items:["AI95-00433-01"]}),(0,o.jsxs)(n.p,{children:["task body Consumer is","\n","   Person : Person","_","Name;","\n","begin","\n","   loop","\n","      Buffer.Remove","_","First","_","Wait(Person);","\n","      exit when Person = null;","\n","      ... --  simulate serving a customer","\n","   end loop;","\n","end Consumer;","\n"]})]}),"\n",(0,o.jsx)(a.A,{children:"7/2"}),"\n",(0,o.jsx)(d.A,{items:["AI95-00433-01"]}),"\n",(0,o.jsxs)("p",{children:["The buffer object contains an internal array of person names managed in a round-robin fashion. The array has two indices, an In","_","Index denoting the index for the next input person name and an Out","_","Index denoting the index for the next output person name.",(0,o.jsx)("br",{})]}),"\n",(0,o.jsx)(a.A,{children:"7.1/2"}),"\n",(0,o.jsx)(d.A,{items:["AI95-00433-01"]}),"\n",(0,o.jsxs)("p",{children:["The Buffer is defined as an extension of the Synchronized","_","Queue interface (see ",(0,o.jsx)("a",{href:"/docs/arm/AA-3/AA-3.9#Subclause_3.9.4",children:"3.9.4"}),"), and as such promises to implement the abstraction defined by that interface. By doing so, the Buffer can be passed to the Transfer class-wide operation defined for objects of a type covered by Queue'Class.",(0,o.jsx)("br",{})]}),"\n",(0,o.jsx)(a.A,{children:"7.2/5"}),"\n",(0,o.jsx)(d.A,{items:["AI12-0178-1"]}),"\n",(0,o.jsxs)(t.A,{language:"ada",children:[(0,o.jsxs)(n.p,{children:["type Person","_","Name","_","Array is array (Positive range ","<",">",")","\n","   of Person","_","Name;  -- see ",(0,o.jsx)("a",{href:"/docs/arm/AA-3/AA-3.10#Subclause_3.10.1",children:"3.10.1"}),"\n","\n",(0,o.jsx)(a.A,{children:"8/2"})]}),(0,o.jsx)(d.A,{items:["AI95-00433-01"]}),(0,o.jsxs)(n.p,{children:["protected Buffer is new Synchronized","_","Queue with  -- see ",(0,o.jsx)("a",{href:"/docs/arm/AA-3/AA-3.9#Subclause_3.9.4",children:"3.9.4"}),"\n","   entry Append","_","Wait(Person : in Person","_","Name);","\n","   entry Remove","_","First","_","Wait(Person : out Person","_","Name);","\n","   function Cur","_","Count return Natural;","\n","   function Max","_","Count return Natural;","\n","   procedure Append(Person : in Person","_","Name);","\n","   procedure Remove","_","First(Person : out Person","_","Name);","\n","private","\n","   Pool      : Person","_","Name","_","Array(1 .. 100);","\n","   Count     : Natural := 0;","\n","   In","_","Index, Out","_","Index : Positive := 1;","\n","end Buffer;","\n","\n",(0,o.jsx)(a.A,{children:"9/2"})]}),(0,o.jsx)(d.A,{items:["AI95-00433-01"]}),(0,o.jsxs)(n.p,{children:["protected body Buffer is","\n","   entry Append","_","Wait(Person : in Person","_","Name)","\n","      when Count ","<"," Pool'Length is","\n","   begin","\n","      Append(Person);","\n","   end Append","_","Wait;","\n","\n",(0,o.jsx)(a.A,{children:"9.1/2"})]}),(0,o.jsx)(d.A,{items:["AI95-00433-01"]}),(0,o.jsxs)(n.p,{children:["procedure Append(Person : in Person","_","Name) is","\n","   begin","\n","      if Count = Pool'Length then","\n","         raise Queue","_",'Error with "Buffer Full";  -- see ',(0,o.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3",children:"11.3"}),"\n","      end if;","\n","      Pool(In","_","Index) := Person;","\n","      In","_","Index       := (In","_","Index mod Pool'Length) + 1;","\n","      Count          := Count + 1;","\n","   end Append;","\n","\n",(0,o.jsx)(a.A,{children:"10/2"})]}),(0,o.jsx)(d.A,{items:["AI95-00433-01"]}),(0,o.jsxs)(n.p,{children:["entry Remove","_","First","_","Wait(Person : out Person","_","Name)","\n","      when Count ",">"," 0 is","\n","   begin","\n","      Remove","_","First(Person);","\n","   end Remove","_","First","_","Wait;","\n","\n",(0,o.jsx)(a.A,{children:"11/2"})]}),(0,o.jsx)(d.A,{items:["AI95-00433-01"]}),(0,o.jsxs)(n.p,{children:["procedure Remove","_","First(Person : out Person","_","Name) is","\n","   begin","\n","      if Count = 0 then","\n","         raise Queue","_",'Error with "Buffer Empty"; -- see ',(0,o.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3",children:"11.3"}),"\n","      end if;","\n","      Person    := Pool(Out","_","Index);","\n","      Out","_","Index := (Out","_","Index mod Pool'Length) + 1;","\n","      Count     := Count - 1;","\n","   end Remove","_","First;","\n","\n",(0,o.jsx)(a.A,{children:"12/2"})]}),(0,o.jsx)(d.A,{items:["AI95-00433-01"]}),(0,o.jsxs)(n.p,{children:["function Cur","_","Count return Natural is","\n","   begin","\n","       return Buffer.Count;","\n","   end Cur","_","Count;","\n","\n",(0,o.jsx)(a.A,{children:"13/2"})]}),(0,o.jsx)(d.A,{items:["AI95-00433-01"]}),(0,o.jsxs)(n.p,{children:["function Max","_","Count return Natural is","\n","   begin","\n","       return Pool'Length;","\n","   end Max","_","Count;","\n","end Buffer;","\n"]})]})]})}function x(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,o.jsx)(n,{...e,children:(0,o.jsx)(A,{...e})}):A(e)}}}]);