"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4902],{1307:(e,a,n)=>{n.r(a),n.d(a,{assets:()=>f,contentTitle:()=>g,default:()=>b,frontMatter:()=>u,metadata:()=>x,toc:()=>T});var t=n(91716),r=n(28090),i=n(48424),o=Object.defineProperty,s=Object.defineProperties,p=Object.getOwnPropertyDescriptors,l=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,k=(e,a,n)=>a in e?o(e,a,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[a]=n,m=(e,a)=>{for(var n in a||(a={}))c.call(a,n)&&k(e,n,a[n]);if(l)for(var n of l(a))d.call(a,n)&&k(e,n,a[n]);return e};const u={sidebar_position:103},g="12.8 Example of a Generic Package",x={unversionedId:"arm/AA-12/AA-12.8",id:"arm/AA-12/AA-12.8",title:"12.8 Example of a Generic Package",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-12/AA-12.8.mdx",sourceDirName:"arm/AA-12",slug:"/arm/AA-12/AA-12.8",permalink:"/docs/arm/AA-12/AA-12.8",draft:!1,tags:[],version:"current",sidebarPosition:103,frontMatter:{sidebar_position:103},sidebar:"referenceManualSidebar",previous:{title:"12.7 Formal Packages",permalink:"/docs/arm/AA-12/AA-12.7"},next:{title:"13 Representation Issues",permalink:"/docs/arm/AA-13/"}},f={},T=[{value:"Examples",id:"examples",level:4}],y={toc:T};function b(e){var a,n=e,{components:o}=n,k=((e,a)=>{var n={};for(var t in e)c.call(e,t)&&a.indexOf(t)<0&&(n[t]=e[t]);if(null!=e&&l)for(var t of l(e))a.indexOf(t)<0&&d.call(e,t)&&(n[t]=e[t]);return n})(n,["components"]);return(0,t.kt)("wrapper",(a=m(m({},y),k),s(a,p({components:o,mdxType:"MDXLayout"}))),(0,t.kt)("h1",m({},{id:"128-example-of-a-generic-package"}),"12.8 Example of a Generic Package"),(0,t.kt)("admonition",m({},{type:"warning"}),(0,t.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,t.kt)("a",m({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,t.kt)(i.Z,{mdxType:"MarginText"},"1"),(0,t.kt)("p",null,"The following example provides a possible formulation of stacks by means of a generic package. The size of each stack and the type of the stack elements are provided as generic formal parameters. ",(0,t.kt)("br",null)),(0,t.kt)("h4",m({},{id:"examples"}),"Examples"),(0,t.kt)(i.Z,{mdxType:"MarginText"},"2/1"),(0,t.kt)("p",null,(0,t.kt)("em",null,"This paragraph was deleted."),(0,t.kt)("br",null)),(0,t.kt)(i.Z,{mdxType:"MarginText"},"3"),(0,t.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"generic","\n","   Size : Positive;","\n","   type Item is private;","\n","package Stack is","\n","   procedure Push(E : in  Item);","\n","   procedure Pop (E : out Item);","\n","   Overflow, Underflow : exception;","\n","end Stack;","\n",(0,t.kt)(i.Z,{mdxType:"MarginText"},"4"),"package body Stack is","\n",(0,t.kt)(i.Z,{mdxType:"MarginText"},"5"),"type Table is array (Positive range ","<",">",") of Item;","\n","   Space : Table(1 .. Size);","\n","   Index : Natural := 0;","\n",(0,t.kt)(i.Z,{mdxType:"MarginText"},"6"),"procedure Push(E : in Item) is","\n","   begin","\n","      if Index ",">","= Size then","\n","         raise Overflow;","\n","      end if;","\n","      Index := Index + 1;","\n","      Space(Index) := E;","\n","   end Push;","\n",(0,t.kt)(i.Z,{mdxType:"MarginText"},"7"),"procedure Pop(E : out Item) is","\n","   begin","\n","      if Index = 0 then","\n","         raise Underflow;","\n","      end if;","\n","      E := Space(Index);","\n","      Index := Index - 1;","\n","   end Pop;","\n",(0,t.kt)(i.Z,{mdxType:"MarginText"},"8"),"end Stack;","\n"),(0,t.kt)(i.Z,{mdxType:"MarginText"},"9"),(0,t.kt)("p",null,"Instances of this generic package can be obtained as follows: ",(0,t.kt)("br",null)),(0,t.kt)(i.Z,{mdxType:"MarginText"},"10"),(0,t.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"package Stack_Int  is new Stack(Size =",">"," 200, Item =",">"," Integer);","\n","package Stack_Bool is new Stack(100, Boolean);","\n"),(0,t.kt)(i.Z,{mdxType:"MarginText"},"11"),(0,t.kt)("p",null,"Thereafter, the procedures of the instantiated packages can be called as follows: ",(0,t.kt)("br",null)),(0,t.kt)(i.Z,{mdxType:"MarginText"},"12"),(0,t.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"Stack_Int.Push(N);","\n","Stack_Bool.Push(True);","\n"),(0,t.kt)(i.Z,{mdxType:"MarginText"},"13"),(0,t.kt)("p",null,"Alternatively, a generic formulation of the type Stack can be given as follows (package body omitted): ",(0,t.kt)("br",null)),(0,t.kt)(i.Z,{mdxType:"MarginText"},"14"),(0,t.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"generic","\n","   type Item is private;","\n","package On_Stacks is","\n","   type Stack(Size : Positive) is limited private;","\n","   procedure Push(S : in out Stack; E : in  Item);","\n","   procedure Pop (S : in out Stack; E : out Item);","\n","   Overflow, Underflow : exception;","\n","private","\n","   type Table is array (Positive range ","<",">",") of Item;","\n","   type Stack(Size : Positive) is","\n","      record","\n","         Space : Table(1 .. Size);","\n","         Index : Natural := 0;","\n","      end record;","\n","end On_Stacks;","\n"),(0,t.kt)(i.Z,{mdxType:"MarginText"},"15"),(0,t.kt)("p",null,"In order to use such a package, an instance has to be created and thereafter stacks of the corresponding type can be declared: ",(0,t.kt)("br",null)),(0,t.kt)(i.Z,{mdxType:"MarginText"},"16"),(0,t.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"declare","\n","   package Stack_Real is new On_Stacks(Real); use Stack_Real;","\n","   S : Stack(100);","\n","begin","\n","   ...","\n","   Push(S, 2.54);","\n","   ...","\n","end;","\n"))}b.isMDXComponent=!0}}]);