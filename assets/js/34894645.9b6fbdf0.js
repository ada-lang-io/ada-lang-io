"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[9860],{78034:(e,a,n)=>{n.r(a),n.d(a,{assets:()=>c,contentTitle:()=>d,default:()=>m,frontMatter:()=>s,metadata:()=>p,toc:()=>g});var t=n(58168),i=(n(96540),n(15680)),r=(n(20793),n(91435),n(21432)),o=n(79162);n(34421);const s={sidebar_position:103},d="12.8 Example of a Generic Package",p={unversionedId:"arm/AA-12/AA-12.8",id:"arm/AA-12/AA-12.8",title:"12.8 Example of a Generic Package",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-12/AA-12.8.mdx",sourceDirName:"arm/AA-12",slug:"/arm/AA-12/AA-12.8",permalink:"/docs/arm/AA-12/AA-12.8",draft:!1,tags:[],version:"current",sidebarPosition:103,frontMatter:{sidebar_position:103},sidebar:"referenceManualSidebar",previous:{title:"12.7 Formal Packages",permalink:"/docs/arm/AA-12/AA-12.7"},next:{title:"13 Representation Issues",permalink:"/docs/arm/AA-13/"}},c={},g=[{value:"Examples",id:"examples",level:4}],l={toc:g},y="wrapper";function m(e){let{components:a,...n}=e;return(0,i.yg)(y,(0,t.A)({},l,n,{components:a,mdxType:"MDXLayout"}),(0,i.yg)("h1",{id:"128-example-of-a-generic-package"},"12.8 Example of a Generic Package"),(0,i.yg)("admonition",{type:"warning"},(0,i.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,i.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,i.yg)("h4",{id:"examples"},"Examples"),(0,i.yg)(o.A,{mdxType:"MarginText"},"1"),(0,i.yg)("p",null,"The following example provides a possible formulation of stacks by means of a generic package. The size of each stack and the type of the stack elements are provided as generic formal parameters.",(0,i.yg)("br",null)),(0,i.yg)(o.A,{mdxType:"MarginText"},"2/1"),(0,i.yg)("p",null,(0,i.yg)("em",null,"This paragraph was deleted."),(0,i.yg)("br",null)),(0,i.yg)(o.A,{mdxType:"MarginText"},"3"),(0,i.yg)(r.A,{language:"ada",mdxType:"CodeBlock"},"generic","\n","   Size : Positive;","\n","   type Item is private;","\n","package Stack is","\n","   procedure Push(E : in  Item);","\n","   procedure Pop (E : out Item);","\n","   Overflow, Underflow : exception;","\n","end Stack;","\n",(0,i.yg)(o.A,{mdxType:"MarginText"},"4"),"package body Stack is","\n",(0,i.yg)(o.A,{mdxType:"MarginText"},"5"),"type Table is array (Positive range ","<",">",") of Item;","\n","   Space : Table(1 .. Size);","\n","   Index : Natural := 0;","\n",(0,i.yg)(o.A,{mdxType:"MarginText"},"6"),"procedure Push(E : in Item) is","\n","   begin","\n","      if Index ",">","= Size then","\n","         raise Overflow;","\n","      end if;","\n","      Index := Index + 1;","\n","      Space(Index) := E;","\n","   end Push;","\n",(0,i.yg)(o.A,{mdxType:"MarginText"},"7"),"procedure Pop(E : out Item) is","\n","   begin","\n","      if Index = 0 then","\n","         raise Underflow;","\n","      end if;","\n","      E := Space(Index);","\n","      Index := Index - 1;","\n","   end Pop;","\n",(0,i.yg)(o.A,{mdxType:"MarginText"},"8"),"end Stack;","\n"),(0,i.yg)(o.A,{mdxType:"MarginText"},"9"),(0,i.yg)("p",null,"Instances of this generic package can be obtained as follows: ",(0,i.yg)("br",null)),(0,i.yg)(o.A,{mdxType:"MarginText"},"10"),(0,i.yg)(r.A,{language:"ada",mdxType:"CodeBlock"},"package Stack_Int  is new Stack(Size =",">"," 200, Item =",">"," Integer);","\n","package Stack_Bool is new Stack(100, Boolean);","\n"),(0,i.yg)(o.A,{mdxType:"MarginText"},"11"),(0,i.yg)("p",null,"Thereafter, the procedures of the instantiated packages can be called as follows: ",(0,i.yg)("br",null)),(0,i.yg)(o.A,{mdxType:"MarginText"},"12"),(0,i.yg)(r.A,{language:"ada",mdxType:"CodeBlock"},"Stack_Int.Push(N);","\n","Stack_Bool.Push(True);","\n"),(0,i.yg)(o.A,{mdxType:"MarginText"},"13"),(0,i.yg)("p",null,"Alternatively, a generic formulation of the type Stack can be given as follows (package body omitted): ",(0,i.yg)("br",null)),(0,i.yg)(o.A,{mdxType:"MarginText"},"14"),(0,i.yg)(r.A,{language:"ada",mdxType:"CodeBlock"},"generic","\n","   type Item is private;","\n","package On_Stacks is","\n","   type Stack(Size : Positive) is limited private;","\n","   procedure Push(S : in out Stack; E : in  Item);","\n","   procedure Pop (S : in out Stack; E : out Item);","\n","   Overflow, Underflow : exception;","\n","private","\n","   type Table is array (Positive range ","<",">",") of Item;","\n","   type Stack(Size : Positive) is","\n","      record","\n","         Space : Table(1 .. Size);","\n","         Index : Natural := 0;","\n","      end record;","\n","end On_Stacks;","\n"),(0,i.yg)(o.A,{mdxType:"MarginText"},"15"),(0,i.yg)("p",null,"In order to use such a package, an instance has to be created and thereafter stacks of the corresponding type can be declared: ",(0,i.yg)("br",null)),(0,i.yg)(o.A,{mdxType:"MarginText"},"16"),(0,i.yg)(r.A,{language:"ada",mdxType:"CodeBlock"},"declare","\n","   package Stack_Real is new On_Stacks(Real); use Stack_Real;","\n","   S : Stack(100);","\n","begin","\n","   ...","\n","   Push(S, 2.54);","\n","   ...","\n","end;","\n"))}m.isMDXComponent=!0}}]);