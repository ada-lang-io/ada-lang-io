"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4195],{2330:(e,t,n)=>{n.d(t,{Z:()=>m});var a=n(6687),l=n(4923);const r="tabItem_Vo3M";var c=Object.defineProperty,s=Object.getOwnPropertySymbols,i=Object.prototype.hasOwnProperty,o=Object.prototype.propertyIsEnumerable,u=(e,t,n)=>t in e?c(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n;function m({children:e,hidden:t,className:n}){return a.createElement("div",((e,t)=>{for(var n in t||(t={}))i.call(t,n)&&u(e,n,t[n]);if(s)for(var n of s(t))o.call(t,n)&&u(e,n,t[n]);return e})({role:"tabpanel",className:(0,l.Z)(r,n)},{hidden:t}),e)}},5195:(e,t,n)=>{n.d(t,{Z:()=>w});var a=n(6687),l=n(4923),r=n(1591),c=n(165),s=n(2257),i=n(3408);const o="tabList_cuNH",u="tabItem_JP6x";var m=Object.defineProperty,v=Object.defineProperties,h=Object.getOwnPropertyDescriptors,d=Object.getOwnPropertySymbols,p=Object.prototype.hasOwnProperty,b=Object.prototype.propertyIsEnumerable,f=(e,t,n)=>t in e?m(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,g=(e,t)=>{for(var n in t||(t={}))p.call(t,n)&&f(e,n,t[n]);if(d)for(var n of d(t))b.call(t,n)&&f(e,n,t[n]);return e};function E(e){var t,n;const{lazy:r,block:m,defaultValue:d,values:p,groupId:b,className:f}=e,E=a.Children.map(e.children,(e=>{if((0,a.isValidElement)(e)&&"value"in e.props)return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)})),w=null!=p?p:E.map((({props:{value:e,label:t,attributes:n}})=>({value:e,label:t,attributes:n}))),y=(0,c.l)(w,((e,t)=>e.value===t.value));if(y.length>0)throw new Error(`Docusaurus error: Duplicate values "${y.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`);const x=null===d?d:null!=(n=null!=d?d:null==(t=E.find((e=>e.props.default)))?void 0:t.props.value)?n:E[0].props.value;if(null!==x&&!w.some((e=>e.value===x)))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${x}" but none of its children has the corresponding value. Available values are: ${w.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);const{tabGroupChoices:O,setTabGroupChoices:_}=(0,s.U)(),[N,H]=(0,a.useState)(x),S=[],{blockElementScrollPositionUntilNextRender:z}=(0,i.o5)();if(null!=b){const e=O[b];null!=e&&e!==N&&w.some((t=>t.value===e))&&H(e)}const P=e=>{const t=e.currentTarget,n=S.indexOf(t),a=w[n].value;a!==N&&(z(t),H(a),null!=b&&_(b,String(a)))},I=e=>{var t,n;let a=null;switch(e.key){case"ArrowRight":{const n=S.indexOf(e.currentTarget)+1;a=null!=(t=S[n])?t:S[0];break}case"ArrowLeft":{const t=S.indexOf(e.currentTarget)-1;a=null!=(n=S[t])?n:S[S.length-1];break}}null==a||a.focus()};return a.createElement("div",{className:(0,l.Z)("tabs-container",o)},a.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,l.Z)("tabs",{"tabs--block":m},f)},w.map((({value:e,label:t,attributes:n})=>{return a.createElement("li",(r=g({role:"tab",tabIndex:N===e?0:-1,"aria-selected":N===e,key:e,ref:e=>S.push(e),onKeyDown:I,onFocus:P,onClick:P},n),c={className:(0,l.Z)("tabs__item",u,null==n?void 0:n.className,{"tabs__item--active":N===e})},v(r,h(c))),null!=t?t:e);var r,c}))),r?(0,a.cloneElement)(E.filter((e=>e.props.value===N))[0],{className:"margin-top--md"}):a.createElement("div",{className:"margin-top--md"},E.map(((e,t)=>(0,a.cloneElement)(e,{key:t,hidden:e.props.value!==N})))))}function w(e){const t=(0,r.Z)();return a.createElement(E,g({key:String(t)},e))}},6565:(e,t,n)=>{n.r(t),n.d(t,{default:()=>L});var a,l,r=n(6687),c=n(1651),s=n(6198),i=n(1591),o=n(7556),u=n(2873),m=n(2330),v=n(5195);function h(){return h=Object.assign?Object.assign.bind():function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var a in n)Object.prototype.hasOwnProperty.call(n,a)&&(e[a]=n[a])}return e},h.apply(this,arguments)}const d=e=>{let{title:t,titleId:n,...c}=e;return r.createElement("svg",h({xmlns:"http://www.w3.org/2000/svg",viewBox:"0 0 426 426","aria-labelledby":n},c),t?r.createElement("title",{id:n},t):null,a||(a=r.createElement("path",{d:"M416 176.619c5.523 0 10-4.477 10-10s-4.477-10-10-10h-49.152v-26.381H416c5.523 0 10-4.477 10-10s-4.477-10-10-10h-49.152V69.152c0-5.523-4.477-10-10-10h-41.086V10c0-5.523-4.477-10-10-10s-10 4.477-10 10v49.152H269.38V10c0-5.523-4.477-10-10-10s-10 4.477-10 10v49.152H223V10c0-5.523-4.477-10-10-10s-10 4.477-10 10v49.152h-26.381V10c0-5.523-4.477-10-10-10s-10 4.477-10 10v49.152h-26.381V10c0-5.523-4.477-10-10-10s-10 4.477-10 10v49.152H69.152c-5.523 0-10 4.477-10 10v41.086H10c-5.523 0-10 4.477-10 10s4.477 10 10 10h49.152v26.381H10c-5.523 0-10 4.477-10 10s4.477 10 10 10h49.152V203H10c-5.523 0-10 4.477-10 10s4.477 10 10 10h49.152v26.381H10c-5.523 0-10 4.477-10 10s4.477 10 10 10h49.152v26.381H10c-5.523 0-10 4.477-10 10s4.477 10 10 10h49.152v41.086c0 5.523 4.477 10 10 10h41.086V416c0 5.523 4.477 10 10 10s10-4.477 10-10v-49.152h26.381V416c0 5.523 4.477 10 10 10s10-4.477 10-10v-49.152H203V416c0 5.523 4.477 10 10 10s10-4.477 10-10v-49.152h26.38V416c0 5.523 4.477 10 10 10s10-4.477 10-10v-49.152h26.381V416c0 5.523 4.477 10 10 10s10-4.477 10-10v-49.152h41.086c5.523 0 10-4.477 10-10v-41.086H416c5.523 0 10-4.477 10-10s-4.477-10-10-10h-49.152v-26.381H416c5.523 0 10-4.477 10-10s-4.477-10-10-10h-49.152V223H416c5.523 0 10-4.477 10-10s-4.477-10-10-10h-49.152v-26.381H416zM346.848 203h-8.821c-5.523 0-10 4.477-10 10s4.477 10 10 10h8.821v26.381h-8.821c-5.523 0-10 4.477-10 10s4.477 10 10 10h8.821v26.381h-8.821c-5.523 0-10 4.477-10 10s4.477 10 10 10h8.821v31.086h-31.086v-8.821c0-5.523-4.477-10-10-10s-10 4.477-10 10v8.821H269.38v-8.821c0-5.523-4.477-10-10-10s-10 4.477-10 10v8.821H223v-8.821c0-5.523-4.477-10-10-10s-10 4.477-10 10v8.821h-26.381v-8.821c0-5.523-4.477-10-10-10s-10 4.477-10 10v8.821h-26.381v-8.821c0-5.523-4.477-10-10-10s-10 4.477-10 10v8.821H79.152v-31.086h8.821c5.523 0 10-4.477 10-10s-4.477-10-10-10h-8.821v-26.381h8.821c5.523 0 10-4.477 10-10s-4.477-10-10-10h-8.821V223h8.821c5.523 0 10-4.477 10-10s-4.477-10-10-10h-8.821v-26.381h8.821c5.523 0 10-4.477 10-10s-4.477-10-10-10h-8.821v-26.381h8.821c5.523 0 10-4.477 10-10s-4.477-10-10-10h-8.821V79.152h31.086v8.821c0 5.523 4.477 10 10 10s10-4.477 10-10v-8.821h26.381v8.821c0 5.523 4.477 10 10 10s10-4.477 10-10v-8.821H203v8.821c0 5.523 4.477 10 10 10s10-4.477 10-10v-8.821h26.38v8.821c0 5.523 4.477 10 10 10s10-4.477 10-10v-8.821h26.381v8.821c0 5.523 4.477 10 10 10s10-4.477 10-10v-8.821h31.086v31.086h-8.821c-5.523 0-10 4.477-10 10s4.477 10 10 10h8.821v26.381h-8.821c-5.523 0-10 4.477-10 10s4.477 10 10 10h8.821V203z"})),l||(l=r.createElement("path",{d:"M266.774 149.225H159.225c-5.523 0-10 4.477-10 10v107.55c0 5.523 4.477 10 10 10h107.549c5.523 0 10-4.477 10-10v-107.55c0-5.523-4.477-10-10-10zm-10 107.55h-87.549v-87.55h87.549v87.55z"})))};var p,b,f;function g(){return g=Object.assign?Object.assign.bind():function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var a in n)Object.prototype.hasOwnProperty.call(n,a)&&(e[a]=n[a])}return e},g.apply(this,arguments)}const E=e=>{let{title:t,titleId:n,...a}=e;return r.createElement("svg",g({xmlns:"http://www.w3.org/2000/svg",viewBox:"0 0 512 512",style:{enableBackground:"new 0 0 512 512"},xmlSpace:"preserve","aria-labelledby":n},a),t?r.createElement("title",{id:n},t):null,p||(p=r.createElement("path",{d:"M128 443.733H93.867c-4.71 0-8.533 3.823-8.533 8.533s3.823 8.533 8.533 8.533H128c4.71 0 8.533-3.823 8.533-8.533s-3.823-8.533-8.533-8.533zM128 392.533H42.667c-4.71 0-8.533 3.823-8.533 8.533s3.823 8.533 8.533 8.533H128c4.71 0 8.533-3.823 8.533-8.533s-3.823-8.533-8.533-8.533zM128 341.333H76.8c-4.71 0-8.533 3.823-8.533 8.533S72.09 358.4 76.8 358.4H128c4.71 0 8.533-3.823 8.533-8.533s-3.823-8.534-8.533-8.534zM93.867 119.467H128a8.536 8.536 0 0 0 8.533-8.533 8.536 8.536 0 0 0-8.533-8.533H93.867a8.536 8.536 0 0 0-8.533 8.533c-.001 4.71 3.822 8.533 8.533 8.533zM179.2 136.533a8.536 8.536 0 0 0 8.533-8.533V68.267h38.665c1.041 0 3.686 1.493 4.702 3.806l25.6 59.307a8.526 8.526 0 0 0 11.213 4.454c4.326-1.869 6.323-6.886 4.454-11.213l-25.617-59.35c-3.567-8.158-12.126-14.071-20.352-14.071h-38.665c-9.412 0-17.067 7.842-17.067 17.493V128c.001 4.71 3.824 8.533 8.534 8.533zM187.733 204.8c0-9.412-7.654-17.067-17.067-17.067-9.412 0-17.067 7.654-17.067 17.067s7.654 17.067 17.067 17.067 17.067-7.655 17.067-17.067zM110.933 68.267H128a8.536 8.536 0 0 0 8.533-8.533A8.536 8.536 0 0 0 128 51.201h-17.067a8.536 8.536 0 0 0-8.533 8.533 8.536 8.536 0 0 0 8.533 8.533zM324.267 187.733c-9.412 0-17.067 7.654-17.067 17.067s7.654 17.067 17.067 17.067c9.412 0 17.067-7.654 17.067-17.067s-7.655-17.067-17.067-17.067zM349.867 290.133h102.4c4.71 0 8.533-3.823 8.533-8.533s-3.823-8.533-8.533-8.533h-102.4c-4.71 0-8.533 3.823-8.533 8.533s3.822 8.533 8.533 8.533zM102.4 204.8c0 9.412 7.654 17.067 17.067 17.067s17.067-7.654 17.067-17.067-7.654-17.067-17.067-17.067S102.4 195.388 102.4 204.8zM290.133 204.8c0-9.412-7.654-17.067-17.067-17.067-9.412 0-17.067 7.654-17.067 17.067s7.654 17.067 17.067 17.067 17.067-7.655 17.067-17.067z"})),b||(b=r.createElement("path",{d:"M459.213 205.201c-5.581-20.881-22.733-51.601-58.146-51.601H69.111l-14.874-37.171c-3.191-8.388-11.273-14.029-20.104-14.029H17.067C7.654 102.4 0 110.054 0 119.467V179.2c0 81.348 63.718 145.067 145.067 145.067 4.71 0 8.533-3.823 8.533-8.533s-3.823-8.533-8.533-8.533c-71.774 0-128-56.226-128-128v-59.733h17.067c1.212 0 3.311.811 4.207 3.174l17.067 42.667a8.543 8.543 0 0 0 7.927 5.359h296.934l6.92 27.674c.085.333.188.666.307.99 5.35 13.901 16.648 22.536 29.483 22.536h21.154c4.71 0 8.533-3.823 8.533-8.533s-3.823-8.533-8.533-8.533h-21.154c-7.868 0-11.87-7.458-13.372-11.145l-5.743-22.989h23.202c34.825 0 42.479 42.291 42.786 44.075a8.53 8.53 0 0 0 8.414 7.125c23.526 0 42.667 19.14 42.667 42.667s-19.14 42.667-42.667 42.667H313.054l10.394-21.948c2.014-4.258.196-9.344-4.062-11.366-4.25-2.005-9.344-.205-11.366 4.062l-76.843 162.227c-.93 2.031-3.541 3.558-4.779 3.558h-38.665v-153.6H281.6c4.71 0 8.533-3.823 8.533-8.533s-3.823-8.533-8.533-8.533H179.2a8.536 8.536 0 0 0-8.533 8.533v162.133c0 9.412 7.654 17.067 17.067 17.067h38.665c8.158 0 16.691-5.683 20.25-13.414l58.317-123.119h147.302c32.939 0 59.733-26.795 59.733-59.733-.001-30.586-23.109-55.887-52.788-59.335z"})),f||(f=r.createElement("path",{d:"M238.933 204.8c0-9.412-7.654-17.067-17.067-17.067-9.412 0-17.067 7.654-17.067 17.067s7.654 17.067 17.067 17.067 17.067-7.655 17.067-17.067z"})))};var w;function y(){return y=Object.assign?Object.assign.bind():function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var a in n)Object.prototype.hasOwnProperty.call(n,a)&&(e[a]=n[a])}return e},y.apply(this,arguments)}const x=e=>{let{title:t,titleId:n,...a}=e;return r.createElement("svg",y({xmlns:"http://www.w3.org/2000/svg",viewBox:"0 0 408.191 408.191",style:{enableBackground:"new 0 0 408.191 408.191"},xmlSpace:"preserve","aria-labelledby":n},a),t?r.createElement("title",{id:n},t):null,w||(w=r.createElement("path",{d:"M363.663 294.916c0-18.823-13.48-34.545-31.289-38.05v-25.655a7.5 7.5 0 0 0-9.452-7.242l-8.355 2.25c-22.41-37.422-57.92-48.108-75.395-51.131v-19.483c5.289-4.545 10.184-10.16 14.557-16.779a100.256 100.256 0 0 0 4.965-8.434c.021.008.039.017.061.023 1.959.635 3.953.936 5.932.936 9.098 0 18.565-6.189 21.023-16.258 5.732-23.485-13.5-28.398-13.83-28.458a148.182 148.182 0 0 0 1.059-17.602C272.938 7.156 233.647 0 204.093 0c-29.551 0-68.838 7.156-68.838 69.035 0 5.963.37 11.844 1.059 17.602-.329.06-19.763 5.184-13.831 28.461 2.559 10.043 11.927 16.255 21.025 16.255a19.22 19.22 0 0 0 5.931-.936l.061-.023c1.543 2.922 3.189 5.746 4.965 8.434 4.373 6.619 9.267 12.234 14.555 16.779v19.483c-17.476 3.023-52.983 13.713-75.394 51.131l-8.354-2.25a7.497 7.497 0 0 0-9.451 7.242v25.654c-17.812 3.504-31.291 19.227-31.291 38.051 0 18.822 13.479 34.549 31.291 38.053v32.199a7.5 7.5 0 0 0 5.383 7.195l120.7 35.523a7.516 7.516 0 0 0 2.117.305h.077a7.751 7.751 0 0 0 2.192-.305l120.701-35.508a7.501 7.501 0 0 0 5.383-7.195V332.97c17.81-3.505 31.289-19.232 31.289-38.054zm-94.399-193.277c2.336 1.52 3.334 5.254 2.176 8.834-1.103 3.402-3.871 5.766-6.535 5.869a127.016 127.016 0 0 0 4.359-14.703zm-132.512 8.835c-1.158-3.579-.16-7.313 2.177-8.834a127.45 127.45 0 0 0 4.358 14.702c-2.662-.106-5.432-2.467-6.535-5.868zM204.093 15c31.995 0 53.399 9.029 53.825 52.721-26.49 5.475-50.976.613-72.852-14.498-12.249-8.462-20.517-18.125-24.52-23.389C169.911 18.174 185.141 15 204.093 15zM166.98 130.559c-10.785-16.326-16.725-38.175-16.725-61.523 0-9.746 1.02-17.805 2.942-24.455 5.171 5.98 12.778 13.604 22.817 20.618 13.961 9.756 34.738 19.657 61.55 19.657 6.219 0 12.766-.541 19.625-1.732-1.896 17.955-7.396 34.447-15.977 47.436-10.152 15.367-23.336 23.83-37.12 23.83-13.78-.001-26.961-8.464-37.112-23.831zm10.279 58.526a7.5 7.5 0 0 0 6.76-7.464v-16.475c6.406 2.791 13.149 4.242 20.074 4.242 6.927 0 13.671-1.451 20.079-4.243v16.476a7.502 7.502 0 0 0 6.76 7.464c3.428.34 9.418 1.203 16.707 3.348-3.697 11.395-22.129 20.362-43.542 20.362-21.414 0-39.847-8.968-43.544-20.362 7.289-2.145 13.278-3.008 16.706-3.348zM59.529 294.916c0-10.499 6.836-19.432 16.291-22.58v45.156c-9.455-3.147-16.291-12.078-16.291-22.576zm136.991 95.75-105.7-31.108v-118.56l105.7 28.468v121.2zm7.576-134.693L108.778 230.3c10.99-16.63 25.02-26.6 37.757-32.561 6.21 17.444 29.267 30.056 57.563 30.056s51.345-12.613 57.552-30.06c12.738 5.96 26.773 15.931 37.764 32.565l-95.318 25.673zm113.278 103.599-105.701 31.095V269.466l105.701-28.468v118.574zm15-42.08v-45.156c9.453 3.149 16.289 12.081 16.289 22.58 0 10.497-6.836 19.428-16.289 22.576z"})))};var O=n(4923);const _="features_t9lD",N="featureSvg_GfXr",H="featureText_ZxAV";function S({icon:e,title:t,children:n}){const a=e;return r.createElement("div",{className:(0,O.Z)("col col--4")},r.createElement("div",{className:"text--center"},r.createElement(a,{className:N,role:"img"})),r.createElement("div",{className:(0,O.Z)("padding-horiz--md",H)},r.createElement("h3",null,t),n))}function z(){return r.createElement("section",{className:_},r.createElement("div",{className:"container"},r.createElement("div",{className:"row"},r.createElement(S,{icon:x,title:"Readability"},r.createElement("p",null,"Express intent with explicitness and keywords over symbols and special structures."),r.createElement("p",null,"Express concepts like meaning in integers. Use built-in design by contract with pre/post-conditions and invariants. Model problems with typechecks and range constraints.")),r.createElement(S,{icon:E,title:"Correctness"},r.createElement("p",null,"Build with technology used in 40 years of reliability in planes, trains, and spaceships."),r.createElement("p",null,"Use the SPARK subset to formally verify part or all of your program, and integrate existing SPARK crates available in the Alire package manager.")),r.createElement(S,{icon:d,title:"Performance"},r.createElement("p",null,"Build native applications and take advantage of other libraries through binding to C and C++."),r.createElement("p",null,"Use inline assembly or compiler intrinsics when you need it. Control resources with scope-based resource control (RAII) and your own memory allocators.")))))}const P="columns_inOX",I="heroBanner_qdFl",M="buttons_AeoN",V="heroLink_mlT7",Z="heroTabs_QNcX",k=[["Win","windows"],["Linux","linux"],["Mac","macos"]];const j=new Map([["windows",{label:"Windows",urlSuffix:"installer-x86_64-windows.exe"}],["macos",{label:"macOS",urlSuffix:"bin-x86_64-macos.zip"}],["linux",{label:"Linux",urlSuffix:"bin-x86_64-linux.zip"}],["appimage",{label:"AppImage",urlSuffix:"x86_64.AppImage"}]]),A="1.2.1",D="https://github.com/alire-project/alire",C=`${D}/releases`;function T(e,t){return`${D}/releases/download/v${e}/alr-${e}-${t}`}function G(){var e;const t=(0,i.Z)(),{siteConfig:n}=(0,s.Z)(),a=t?function(e){for(const[t,n]of k)if(e.startsWith(t))return n;return null}((null==(e=navigator.userAgentData)?void 0:e.platform)||navigator.platform):null,l=null!==a?j.get(a):null,u=null!==l?` for ${l.label}`:"",h=null!==l?T(A,l.urlSuffix):C,d=Array.from(j.values()).map((({label:e,urlSuffix:t})=>r.createElement(c.Z,{className:V,to:T(A,t)},e))),p=r.createElement(c.Z,{className:V,to:C},"others");return r.createElement("header",{className:(0,O.Z)("hero hero--primary",I)},r.createElement("div",{className:(0,O.Z)("container",P)},r.createElement("div",{className:"container"},r.createElement("h1",{className:"hero__title"},n.title),r.createElement("p",{className:"hero__subtitle"},n.tagline),r.createElement("p",null,"Get started with Alire, the Ada package manager"),r.createElement("div",{className:M},r.createElement(c.Z,{className:"button button--secondary button--lg",to:h},"Download Alire ",A.slice(0)," ",u)),r.createElement("div",{className:"container"},r.createElement("small",null,"Download for ",(b=d,f=r.createElement("span",null,", "),b.reduce(((e,t)=>e.length>0?e.concat([f,t]):[t]),[])),", or ",p))),r.createElement("div",{className:(0,O.Z)("container",Z)},r.createElement(v.Z,null,r.createElement(m.Z,{value:"basic",label:"Basic"},r.createElement(o.Z,{showLineNumbers:!0},"with Ada.Text_IO;\n\nprocedure Main is\n   type GUID is new String (1 .. 32)\n     with Dynamic_Predicate => (for all C of GUID => C in '0' .. '9' | 'a' .. 'f');\n\n   ID_1 : constant GUID := \"030000004c050000cc09000011810000\";\nbegin\n   Ada.Text_IO.Put_Line (\"Reading from device \" & String (ID_1) & \"...\");\nend Main;\n")),r.createElement(m.Z,{value:"spark",label:"SPARK"},r.createElement(o.Z,{showLineNumbers:!0},"with Interfaces;\n\npackage Xoshiro128 with Pure, SPARK_Mode => On is\n   use type Interfaces.Unsigned_64;\n\n   type Generator is limited private;\n\n   procedure Next (S : in out Generator; Value : out Interfaces.Unsigned_32)\n     with Global  => null,\n          Depends => (S => S, Value => S);\n\n   procedure Reset (S : out Generator; Seed : Interfaces.Unsigned_64)\n     with Global  => null,\n          Depends => (S => Seed),\n          Pre     => Seed /= 0;\n\nprivate\n   type Generator is array (0 .. 3) of Interfaces.Unsigned_32;\nend Xoshiro128;\n")),r.createElement(m.Z,{value:"embedded",label:"Embedded"},r.createElement(o.Z,{showLineNumbers:!0},"with RP.GPIO;\nwith Pico;\n\nprocedure Main is\nbegin\n   RP.GPIO.Enable;\n\n   Pico.LED.Configure (RP.GPIO.Output);\n   Pico.LED.Set;\n\n   loop\n      Pico.LED.Toggle;\n      delay 0.1;\n   end loop;\nend Main;\n"))))));var b,f}const L=function(){const{siteConfig:e}=(0,s.Z)();return r.createElement(u.Z,{title:e.title,description:e.customFields.description},r.createElement(G,null),r.createElement("main",null,r.createElement(z,null)))}}}]);