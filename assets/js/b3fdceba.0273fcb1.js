"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[227],{36801:(e,n,l)=>{l.r(n),l.d(n,{assets:()=>d,contentTitle:()=>i,default:()=>u,frontMatter:()=>o,metadata:()=>r,toc:()=>s});var t=l(58168),a=(l(96540),l(15680));const o={sidebar_position:2},i="Lesson 1: Hello, World!",r={unversionedId:"learn/tutorial/hello-world",id:"learn/tutorial/hello-world",title:"Lesson 1: Hello, World!",description:"Welcome to Ada!",source:"@site/docs/learn/tutorial/01-hello-world.mdx",sourceDirName:"learn/tutorial",slug:"/learn/tutorial/hello-world",permalink:"/docs/learn/tutorial/hello-world",draft:!1,tags:[],version:"current",sidebarPosition:2,frontMatter:{sidebar_position:2},sidebar:"tutorialSidebar",previous:{title:"Tutorial",permalink:"/docs/category/tutorial"},next:{title:"Lesson 2: Command Line Arguments",permalink:"/docs/learn/tutorial/command-line-arguments"}},d={},s=[{value:"Requirements",id:"requirements",level:2},{value:"Objectives",id:"objectives",level:2},{value:"Starting a new project",id:"starting-a-new-project",level:2},{value:"Building a project",id:"building-a-project",level:2},{value:"It&#39;s just an empty program...",id:"its-just-an-empty-program",level:2},{value:"Making it do something",id:"making-it-do-something",level:2},{value:"Need help?",id:"need-help",level:2}],g={toc:s},p="wrapper";function u(e){let{components:n,...l}=e;return(0,a.yg)(p,(0,t.A)({},g,l,{components:n,mdxType:"MDXLayout"}),(0,a.yg)("h1",{id:"lesson-1-hello-world"},"Lesson 1: Hello, World!"),(0,a.yg)("p",null,"Welcome to Ada!"),(0,a.yg)("p",null,"Traditionally, the first program written in a new language is ",(0,a.yg)("a",{parentName:"p",href:"https://en.wikipedia.org/wiki/%22Hello,_World!%22_program"},'"Hello, World."'),"\nThis is a simple program just to print a greeting, and help ensure your environment is\nset up correctly."),(0,a.yg)("h2",{id:"requirements"},"Requirements"),(0,a.yg)("ol",null,(0,a.yg)("li",{parentName:"ol"},(0,a.yg)("p",{parentName:"li"},(0,a.yg)("strong",{parentName:"p"},(0,a.yg)("a",{parentName:"strong",href:"https://alire.ada.dev/"},"Alire"))," - This program provide simple commands to build and\nrun your code, and will install the programs behind this process. The\ndetailed controls are still in the background if you need them, but it helps\nstreamline the process for developers. You will often find just the main\nAlire interface will work well enough for many projects."),(0,a.yg)("p",{parentName:"li"},"If you haven't set it up, you can follow the\n",(0,a.yg)("a",{parentName:"p",href:"https://alire.ada.dev/docs/#installation"},"instructions on the Alire site"),".")),(0,a.yg)("li",{parentName:"ol"},(0,a.yg)("p",{parentName:"li"},"A text editor such as ",(0,a.yg)("a",{parentName:"p",href:"https://code.visualstudio.com/"},"Visual Studio Code")," or ",(0,a.yg)("a",{parentName:"p",href:"https://notepad-plus-plus.org/"},"Notepad++"),"."))),(0,a.yg)("h2",{id:"objectives"},"Objectives"),(0,a.yg)("ol",null,(0,a.yg)("li",{parentName:"ol"},"Make your first running Ada program!"),(0,a.yg)("li",{parentName:"ol"},"Learn how to use standard Ada libraries.")),(0,a.yg)("h2",{id:"starting-a-new-project"},"Starting a new project"),(0,a.yg)("p",null,(0,a.yg)("inlineCode",{parentName:"p"},"Alire")," runs from a command-line terminal. This keeps it slim and allows it\nto be easily usable, such as when making automation to build and test large\nprojects."),(0,a.yg)("admonition",{type:"note"},(0,a.yg)("p",{parentName:"admonition"},"Commands to type in the terminal will follow a ",(0,a.yg)("inlineCode",{parentName:"p"},"$"),", don't type the ",(0,a.yg)("inlineCode",{parentName:"p"},"$"),", that's\njust there to indicate this is a command. Commands in tutorials might also have\nanother block that will show you their output.")),(0,a.yg)("p",null,(0,a.yg)("inlineCode",{parentName:"p"},"Alire")," will generate you a new project. You want to start, or initialize (init),\na new project which runs an executable, which is also called a binary (bin)."),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-bash"},"$ alr init --bin my_hello_world\n")),(0,a.yg)("p",null,"You should respond with something like this:"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-text"},"Success: my_hello_world initialized successfully.\n")),(0,a.yg)("p",null,"This made a new directory for your project, ",(0,a.yg)("inlineCode",{parentName:"p"},"my_hello_world/")," with some contents:"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-text"},"my_hello_world/\n\u251c\u2500\u2500 alire.toml\n\u251c\u2500\u2500 my_hello_world.gpr\n\u251c\u2500\u2500 share\n\u2502   \u2514\u2500\u2500 my_hello_world\n\u2514\u2500\u2500 src\n\u2514\u2500\u2500 my_hello_world.adb\n")),(0,a.yg)("p",null,"There's a few files and a few directories that it made."),(0,a.yg)("table",null,(0,a.yg)("tr",null,(0,a.yg)("td",null,(0,a.yg)("code",null,"alire.toml")),(0,a.yg)("td",null,"Describes what is in your project.")),(0,a.yg)("tr",null,(0,a.yg)("td",null,(0,a.yg)("code",null,"my_hello_world.gpr")),(0,a.yg)("td",null,(0,a.yg)("a",{href:"https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html"},"A GNAT project file."),"This is a more detailed file describing how to build your code into a running program or a library of code for others to use.")),(0,a.yg)("tr",null,(0,a.yg)("td",null,(0,a.yg)("code",null,"my_hello_world.adb")),(0,a.yg)("td",null,'The file with the code executed when your program starts. "adb" is the suffix for the "bodies" of Ada files. "adb" files tell the details of what to do, the other type of file, an "ads" file by convention, describes how to use what is in an "adb" file.')),(0,a.yg)("tr",null,(0,a.yg)("td",null,(0,a.yg)("code",null,"src/")),(0,a.yg)("td",null,"A directory to place additional code your program needs.")),(0,a.yg)("tr",null,(0,a.yg)("td",null,(0,a.yg)("code",null,"share/")),(0,a.yg)("td",null,"Used for additional things by Alire."))),(0,a.yg)("h2",{id:"building-a-project"},"Building a project"),(0,a.yg)("p",null,"Let's try to build the project:"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-bash"},"$ cd my_hello_world\n$ alr build\n")),(0,a.yg)("p",null,"You should see something like:"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-text"},"Note: Synchronizing workspace...\nNothing to update.\n\nNote: Building my_hello_world/my_hello_world.gpr...\nSetup\n   [mkdir]        object directory for project My_Hello_World\n   [mkdir]        exec directory for project My_Hello_World\nCompile\n   [Ada]          my_hello_world.adb\nBind\n   [gprbind]      my_hello_world.bexch\n   [Ada]          my_hello_world.ali\nLink\n   [link]         my_hello_world.adb\nBuild finished successfully in 1.51 seconds.\n")),(0,a.yg)("p",null,"Let's try to run the program to see what happens:"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-bash"},"$ alr run\n")),(0,a.yg)("p",null,"It prints some things, but it's not very interesting. It doesn't look like it\ndid anything."),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-text"},'Note: Building my_hello_world/my_hello_world.gpr...\ngprbuild: "my_hello_world.exe" up to date\nBuild finished successfully in 0.70 seconds.\n')),(0,a.yg)("h2",{id:"its-just-an-empty-program"},"It's just an empty program..."),(0,a.yg)("p",null,"Open up ",(0,a.yg)("inlineCode",{parentName:"p"},"my_hello_world.adb")," and have a look."),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-ada"},"procedure My_Hello_World is\nbegin\n   null;\nend My_Hello_World;\n")),(0,a.yg)("p",null,'In most programming languages, the point where the program begins (the entry point)\nis called "main" but in Ada it can have other names. This is something\nconfigured in the ',(0,a.yg)("inlineCode",{parentName:"p"},"my_hello_world.gpr")," file made when Alire created the project."),(0,a.yg)("p",null,"This is just an empty program. Broken down, it here's what the various words\nmean:"),(0,a.yg)("p",null,(0,a.yg)("inlineCode",{parentName:"p"},"procedure")," - What follows is a block of code that can be run by giving its name."),(0,a.yg)("p",null,(0,a.yg)("inlineCode",{parentName:"p"},"My_Hello_World"),' - This "identifies" the procedure uniquely in this part of the\nprogram. Since it provides identification, it\'s called an ',(0,a.yg)("strong",{parentName:"p"},'"identifier."')),(0,a.yg)("p",null,(0,a.yg)("inlineCode",{parentName:"p"},"is")," ... ",(0,a.yg)("inlineCode",{parentName:"p"},"begin")," - Between these two words is the section of code is where\nadditional variables and constants would go. There' no variables or constants\nused here, so it's just empty."),(0,a.yg)("p",null,(0,a.yg)("inlineCode",{parentName:"p"},"begin")," ... ",(0,a.yg)("inlineCode",{parentName:"p"},"end"),' - Instructions called "statements" go between these words to\ntell the program to do things. They are executed one at a time, and each one\nis separated by semicolon ',(0,a.yg)("inlineCode",{parentName:"p"},";"),"."),(0,a.yg)("p",null,(0,a.yg)("inlineCode",{parentName:"p"},"null;")," - The section between ",(0,a.yg)("inlineCode",{parentName:"p"},"begin")," and ",(0,a.yg)("inlineCode",{parentName:"p"},"end")," cannot be empty, so it ",(0,a.yg)("strong",{parentName:"p"},"MUST"),'\nhave a statement. The "null statement" does nothing.'),(0,a.yg)("p",null,(0,a.yg)("inlineCode",{parentName:"p"},"end My_Hello_World;")," - Indicates the end of the program. In Ada, ",(0,a.yg)("inlineCode",{parentName:"p"},"end"),"s can be\nannotated the name of the thing which started the block being terminated. In\nthis case, it's the procedure ",(0,a.yg)("inlineCode",{parentName:"p"},"My_Hello_World"),", so that's the name used here."),(0,a.yg)("h2",{id:"making-it-do-something"},"Making it do something"),(0,a.yg)("p",null,"There's an empty program, with absolutely nothing in it. How do you make it\ndo something?"),(0,a.yg)("p",null,'The start of every Ada file is what is called the "context clause." This is\nwhere dependencies on other pieces of code get put. In Ada, there are three\npackages provided as part of the language, though with ',(0,a.yg)("inlineCode",{parentName:"p"},"Alire")," you can easily bring\nin additional code from other libraries, but these are the big three:"),(0,a.yg)("table",null,(0,a.yg)("tr",null,(0,a.yg)("td",null,(0,a.yg)("code",null,"Ada")),(0,a.yg)("td",null,"Data structures, basic facilities to read/write files, etc.")),(0,a.yg)("p",null,'{" "}'),(0,a.yg)("tr",null,(0,a.yg)("td",null,(0,a.yg)("code",null,"Standard")),(0,a.yg)("td",null,"The built-in library of extremely basic things to build upon. These are often things the language must handle specially that can't be built out of other things.")),(0,a.yg)("tr",null,(0,a.yg)("td",null,(0,a.yg)("code",null,"Interfaces")),(0,a.yg)("td",null,"Components to talk to other languages, like C."))),(0,a.yg)("p",null,'These "packages" of code can be nested. You want to print things, which is in\nthe ',(0,a.yg)("inlineCode",{parentName:"p"},"Text_IO")," package, which resides within the ",(0,a.yg)("inlineCode",{parentName:"p"},"Ada")," package. A period (full stop)\nbetween names indicates that the thing on the right is contained within the thing\non the left."),(0,a.yg)("p",null,"You want to use procedure to print text to the console, so you bring in ",(0,a.yg)("inlineCode",{parentName:"p"},"Ada.Text_IO"),"\nas a dependency:"),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-ada"},"with Ada.Text_IO;\n\nprocedure My_Hello_World is\nbegin\n   null;\nend My_Hello_World;\n")),(0,a.yg)("p",null,"There's something to do now, so replace that ",(0,a.yg)("inlineCode",{parentName:"p"},"null")," statement with a greeting\nto the world. ",(0,a.yg)("inlineCode",{parentName:"p"},"Put_Line")," is a procedure within the ",(0,a.yg)("inlineCode",{parentName:"p"},"Ada.Text_IO")," package, so\nplace a period after that name to indicate that's where the procedure is located.\n",(0,a.yg)("inlineCode",{parentName:"p"},"Put_Line")," accepts an input, so use parentheses ",(0,a.yg)("inlineCode",{parentName:"p"},"(")," and ",(0,a.yg)("inlineCode",{parentName:"p"},")")," around your greeting\nof ",(0,a.yg)("inlineCode",{parentName:"p"},'"Hello, World!"'),"."),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-ada"},'with Ada.Text_IO;\n\nprocedure My_Hello_World is\nbegin\n   Ada.Text_IO.Put_Line ("Hello, World!");\nend My_Hello_World;\n')),(0,a.yg)("p",null,"Let's run the program again. You don't need to do ",(0,a.yg)("inlineCode",{parentName:"p"},"alr build")," every time, if\nyou've change your program code, you can use ",(0,a.yg)("inlineCode",{parentName:"p"},"alr run")," and it will build your\ncode if it changed automatically before running your program."),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-bash"},"$ alr run\n")),(0,a.yg)("pre",null,(0,a.yg)("code",{parentName:"pre",className:"language-text"},"Note: Building my_hello_world/my_hello_world.gpr...\nCompile\n   [Ada]          my_hello_world.adb\nBind\n   [gprbind]      my_hello_world.bexch\n   [Ada]          my_hello_world.ali\nLink\n   [link]         my_hello_world.adb\nBuild finished successfully in 1.03 seconds.\nHello, World!\n")),(0,a.yg)("p",null,"And there's your greeting to the world!"),(0,a.yg)("h2",{id:"need-help"},"Need help?"),(0,a.yg)("p",null,"Go to the ",(0,a.yg)("a",{parentName:"p",href:"https://gitter.im/ada-lang/Lobby"},"Ada language gitter")," if you have\nquestions or need help."))}u.isMDXComponent=!0}}]);